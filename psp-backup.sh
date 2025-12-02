#!/usr/bin/env bash

set -euo pipefail

#######################################
# Configuration
#######################################

# NFS export that holds the backup tree.
NFS_REMOTE="home.local:/volume1/Family/sat"
BACKUP_MOUNTPOINT="/mnt/domo/psp-backup"

# Subdirectory inside the mounted backup share used as the PSP backup root.
BACKUP_SUBDIR="psp"

# Local mount point for the PSP USB mass storage.
PSP_MOUNTPOINT="/mnt/psp"

# Default tmux session name (used if we spawn one automatically).
TMUX_SESSION_PREFIX="psp_backup"

#######################################
# Globals
#######################################

SCRIPT_PATH="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)/$(basename -- "${BASH_SOURCE[0]}")"
PSP_DEVICE=""
PSP_MOUNTED=0
EXIT_TRAP_INSTALLED=0

#######################################
# Logging helpers
#######################################

log() {
    # $1: level, $2...: printf-style format string and args
    local level fmt
    level="$1"
    shift
    if [ "$#" -eq 0 ]; then
        return 0
    fi
    fmt="$1"
    shift
    printf '%s [%s] ' "$(date +%Y-%m-%dT%H:%M:%S)" "$level" >&2
    # Format string is controlled by this script.
    printf "$fmt" "$@" >&2
    printf '\n' >&2
}

log_info() {
    log "INFO" "$@"
}

log_warn() {
    log "WARN" "$@"
}

log_error() {
    log "ERROR" "$@"
}

fatal() {
    log_error "$@"
    exit 1
}

#######################################
# Cleanup
#######################################

cleanup_on_exit() {
    # Preserve the original exit status from the caller.
    local status
    status="$1"

    # If running inside a tmux-managed child, persist the exit status for the parent.
    if [ -n "${PSP_TMUX_STATUS_FILE:-}" ]; then
        if ! printf '%s' "$status" >"$PSP_TMUX_STATUS_FILE"; then
            log_warn "Unable to write tmux child status to %s." "$PSP_TMUX_STATUS_FILE"
        fi
    fi

    # Prevent recursive trap invocation if we call exit below.
    trap - EXIT INT TERM

    # Avoid exiting early because of set -e semantics inside the cleanup path.
    set +e

    if [ "$PSP_MOUNTED" -eq 1 ] && command -v mountpoint >/dev/null 2>&1 && mountpoint -q "$PSP_MOUNTPOINT"; then
        log_warn "Cleaning up: attempting to unmount PSP storage mounted at %s..." "$PSP_MOUNTPOINT"
        if umount "$PSP_MOUNTPOINT"; then
            log_info "Cleanup succeeded: unmounted %s." "$PSP_MOUNTPOINT"
            PSP_MOUNTED=0
        else
            log_warn "Cleanup could not unmount %s; manual intervention may be required." "$PSP_MOUNTPOINT"
        fi
    fi

    # Restore error handling defaults for any subsequent commands (defensive; script is exiting).
    set -e

    exit "$status"
}

#######################################
# Prerequisite checks
#######################################

require_cmd() {
    local cmd
    cmd="$1"
    if ! command -v "$cmd" >/dev/null 2>&1; then
        fatal "Required command '$cmd' not found in PATH."
    fi
}

check_prereqs() {
    # Hard requirements for this script.
    require_cmd lsblk
    require_cmd mount
    require_cmd umount
    require_cmd mountpoint
    require_cmd findmnt
    require_cmd find
    require_cmd du
    require_cmd df
    require_cmd unison
    require_cmd awk
    require_cmd sort
    require_cmd date
    require_cmd sudo
    require_cmd numfmt
}

ensure_nfs_common_installed() {
    if dpkg -s nfs-common >/dev/null 2>&1; then
        log_info "nfs-common already installed."
        return 0
    fi

    if ! command -v apt-get >/dev/null 2>&1; then
        fatal "nfs-common is required but apt-get is unavailable; please install nfs-common manually."
    fi

    log_info "nfs-common not detected; attempting installation via apt."

    if ! sudo apt-get update; then
        fatal "Failed to update package lists while installing nfs-common."
    fi

    if ! sudo apt-get install -y nfs-common; then
        fatal "Failed to install nfs-common automatically; please install it manually."
    fi

    log_info "nfs-common installed successfully."
}

#######################################
# Argument parsing
#######################################

usage() {
    cat <<'EOF'
Usage: psp-backup.sh

Back up an ARK-4 PSP (USB mass storage) to an NFS-hosted backup tree using Unison.
EOF
}

parse_args() {
    while [ "$#" -gt 0 ]; do
        case "$1" in
            -h|--help)
                usage
                exit 0
                ;;
            *)
                fatal "Unknown argument: %s" "$1"
                ;;
        esac
        shift
    done
}

#######################################
# tmux handling
#######################################

maybe_reexec_in_tmux() {
    # If PSP_TMUX_CHILD is set, we are already running inside the tmux-managed instance.
    if [ "${PSP_TMUX_CHILD:-0}" -eq 1 ]; then
        return 0
    fi

    # If already in tmux, do nothing.
    if [ -n "${TMUX:-}" ]; then
        log_info "Already running inside tmux session."
        return 0
    fi

    # If tmux is not available, warn and continue.
    if ! command -v tmux >/dev/null 2>&1; then
        log_warn "tmux is not installed; proceeding without tmux protection."
        return 0
    fi

    log_info "It is strongly recommended to run this backup inside tmux."
    printf 'Start a dedicated tmux session for this backup now? [Y/n]: '
    read -r answer || answer="n"
    answer=${answer:-Y}

    case "$answer" in
        [Yy]*)
            local session_name
            session_name="${TMUX_SESSION_PREFIX}_$(date +%Y%m%d_%H%M%S)"
            local tmux_status_file
            tmux_status_file="$(mktemp -t psp_tmux_status.XXXXXX)"

            if [ ! -x "$SCRIPT_PATH" ]; then
                log_error "Script path %s is not executable; cannot spawn tmux session." "$SCRIPT_PATH"
                exit 1
            fi

            log_info "Launching tmux session '%s' running %s..." "$session_name" "$SCRIPT_PATH"
            # PSP_TMUX_CHILD=1 marks the child instance so we do not re-enter tmux.
            # Use SCRIPT_PATH so tmux reliably executes this script even if the parent
            # was invoked via a wrapper (e.g., 'bash psp-backup.sh').
            set +e
            PSP_TMUX_CHILD=1 PSP_TMUX_STATUS_FILE="$tmux_status_file" \
                tmux new-session -s "$session_name" "$SCRIPT_PATH" "$@"
            local tmux_status=$?
            set -e

            if [ "$tmux_status" -ne 0 ]; then
                log_error "tmux session exited with status %d; aborting parent invocation." "$tmux_status"
                exit "$tmux_status"
            fi

            local child_status
            child_status="$tmux_status"
            if [ -e "$tmux_status_file" ]; then
                if read -r child_status <"$tmux_status_file"; then
                    log_info "tmux child exited with status %s." "$child_status"
                else
                    log_warn "Could not read tmux child status file; using tmux exit status %d." "$tmux_status"
                    child_status="$tmux_status"
                fi
                rm -f "$tmux_status_file"
            else
                log_warn "tmux child status file missing; using tmux exit status %d." "$tmux_status"
            fi

            if [ "$child_status" -ne 0 ]; then
                log_error "tmux-managed backup failed with status %d; propagating to parent." "$child_status"
                exit "$child_status"
            fi

            # The parent invocation should not continue once the tmux-managed
            # child finishes. Exit cleanly to avoid running twice.
            exit 0
            ;;
        *)
            log_warn "Proceeding without tmux. If this terminal closes, the backup will abort."
            ;;
    esac
}

#######################################
# NFS mount handling
#######################################

ensure_nfs_mount() {
    if [ -z "$NFS_REMOTE" ] || [ -z "$BACKUP_MOUNTPOINT" ]; then
        fatal "NFS configuration variables must not be empty."
    fi

    mkdir -p "$BACKUP_MOUNTPOINT"

    if mountpoint -q "$BACKUP_MOUNTPOINT"; then
        local current_source
        current_source=$(findmnt -nr -o SOURCE "$BACKUP_MOUNTPOINT" || true)
        if [ "$current_source" != "$NFS_REMOTE" ]; then
            fatal "Mount point %s is already in use by %s (expected %s)." "$BACKUP_MOUNTPOINT" "$current_source" "$NFS_REMOTE"
        fi
        log_info "NFS share already mounted at %s" "$BACKUP_MOUNTPOINT"
    else
        log_info "Mounting NFS share %s at %s..." "$NFS_REMOTE" "$BACKUP_MOUNTPOINT"
        if ! sudo mount -t nfs "$NFS_REMOTE" "$BACKUP_MOUNTPOINT"; then
            fatal "Failed to mount NFS share %s at %s." "$NFS_REMOTE" "$BACKUP_MOUNTPOINT"
        fi
        log_info "NFS share mounted."
    fi

    # Create backup root directory.
    if [ -n "$BACKUP_SUBDIR" ]; then
        BACKUP_ROOT="${BACKUP_MOUNTPOINT}/${BACKUP_SUBDIR}"
    else
        BACKUP_ROOT="$BACKUP_MOUNTPOINT"
    fi

    mkdir -p "$BACKUP_ROOT"
    if [ ! -d "$BACKUP_ROOT" ]; then
        fatal "Backup root directory '%s' does not exist and could not be created." "$BACKUP_ROOT"
    fi
}

#######################################
# PSP USB detection and mount
#######################################

list_usb_block_devices() {
    # List existing USB block devices attached via USB. Some PSP models expose a
    # partition (e.g., /dev/sdX1) while others present the whole disk without a
    # partition table. Accept both.
    lsblk -nrpo NAME,TYPE,RM,TRAN 2>/dev/null \
        | awk '$4 == "usb" && ($2 == "part" || $2 == "disk") { print $1 }' || true
}

select_psp_device() {
    # Baseline list of USB block devices before ARK-4 USB is enabled.
    local -a baseline current new
    mapfile -t baseline < <(list_usb_block_devices)

    log_info "Baseline USB block devices: %s" "${baseline[*]:-(none)}"
    log_info "Step 1: On your PSP running ARK-4, enable USB device mode (e.g., XMB -> USB Connection or ARK-4 Recovery)."
    printf 'Press ENTER once USB mode is active and the PSP is connected via USB... '
    read -r _

    log_info "Step 2: Waiting for new USB storage (PSP) to appear..."
    while :; do
        sleep 2
        mapfile -t current < <(list_usb_block_devices)
        new=()
        local dev bdev found
        for dev in "${current[@]}"; do
            found=0
            for bdev in "${baseline[@]}"; do
                if [ "$dev" = "$bdev" ]; then
                    found=1
                    break
                fi
            done
            if [ "$found" -eq 0 ]; then
                new+=("$dev")
            fi
        done

        if [ "${#new[@]}" -gt 0 ]; then
            break
        fi
    done

    log_info "Detected new USB block device(s):"
    local idx=1
    local dev
    for dev in "${new[@]}"; do
        # Show size and filesystem for each candidate.
        local size fstype
        size=$(lsblk -nrpo SIZE "$dev" 2>/dev/null | head -n1 || printf 'unknown')
        fstype=$(lsblk -nrpo FSTYPE "$dev" 2>/dev/null | head -n1 || printf 'unknown')
        printf '  [%d] %s (size=%s, fstype=%s)\n' "$idx" "$dev" "$size" "$fstype"
        idx=$((idx + 1))
    done

    if [ "${#new[@]}" -eq 1 ]; then
        PSP_DEVICE="${new[0]}"
        log_info "Using detected device %s as PSP USB storage." "$PSP_DEVICE"
    else
        local choice
        while :; do
            printf 'Select the PSP device by number (1-%d): ' "${#new[@]}"
            read -r choice
            if [ -z "$choice" ]; then
                log_error "No selection made."
                continue
            fi
            if ! printf '%s\n' "$choice" | grep -Eq '^[0-9]+$'; then
                log_error "Invalid selection '%s'; please enter a number." "$choice"
                continue
            fi
            if [ "$choice" -lt 1 ] || [ "$choice" -gt "${#new[@]}" ]; then
                log_error "Selection out of range."
                continue
            fi
            PSP_DEVICE="${new[$((choice - 1))]}"
            break
        done
        log_info "Using selected device %s as PSP USB storage." "$PSP_DEVICE"
    fi

    if [ -z "$PSP_DEVICE" ]; then
        fatal "No PSP device selected."
    fi

    # Check if device is already mounted (desktop automount, etc.).
    if findmnt -nr -S "$PSP_DEVICE" >/dev/null 2>&1; then
        log_error "Device %s is already mounted by another process. Please unmount it and re-run." "$PSP_DEVICE"
        fatal "Refusing to remount already-mounted device."
    fi
}

mount_psp_device() {
    mkdir -p "$PSP_MOUNTPOINT"
    if mountpoint -q "$PSP_MOUNTPOINT"; then
        fatal "Mount point '%s' is already in use." "$PSP_MOUNTPOINT"
    fi

    log_info "Step 3: Mounting PSP USB storage %s at %s..." "$PSP_DEVICE" "$PSP_MOUNTPOINT"
    # Let the kernel auto-detect filesystem; supply sensible default ownership.
    if ! mount -o uid="$(id -u)",gid="$(id -g)",umask=000 "$PSP_DEVICE" "$PSP_MOUNTPOINT"; then
        fatal "Failed to mount PSP device $PSP_DEVICE at $PSP_MOUNTPOINT."
    fi
    PSP_MOUNTED=1
    log_info "PSP storage mounted."
}

#######################################
# macOS artifact cleanup
#######################################

clean_macos_cruft() {
    local root
    root="$1"

    if [ ! -d "$root" ]; then
        fatal "Path '%s' is not a directory; cannot clean macOS artifacts." "$root"
    fi

    log_info "Cleaning macOS filesystem artifacts under %s..." "$root"

    # Remove known macOS files and directories; ignore errors but warn if find fails.
    if ! find "$root" \
        \( -name '.DS_Store' \
        -o -name '.Spotlight-V100' \
        -o -name '.Trashes' \
        -o -name '.fseventsd' \
        -o -name '.TemporaryItems' \
        -o -name '.VolumeIcon.icns' \
        -o -name '.AppleDouble' \
        -o -name '.AppleDesktop' \
        -o -name '._*' \) \
        -print -exec rm -rf -- {} + 2>/dev/null
    then
        log_warn "Some macOS artifacts may not have been removed under %s." "$root"
    fi

    log_info "macOS artifact cleanup complete for %s." "$root"
}

#######################################
# Comparison & confirmation
#######################################

summarize_root() {
    local label path
    label="$1"
    path="$2"

    if [ ! -d "$path" ]; then
        fatal "Root '%s' (%s) does not exist or is not a directory." "$label" "$path"
    fi

    local used_bytes used_hr
    used_bytes=$(du -sb "$path" 2>/dev/null | awk '{print $1}' || printf '0')
    used_hr=$(numfmt --to=iec "$used_bytes" 2>/dev/null || printf '0')

    printf '%s: %s (used: %s bytes, ~%s)\n' "$label" "$path" "$used_bytes" "$used_hr"
}

confirm_backup() {
    log_info "Step 4: Summarizing PSP and backup roots before synchronization."

    summarize_root "PSP root" "$PSP_MOUNTPOINT"
    summarize_root "Backup root" "$BACKUP_ROOT"

    # Check available space on backup filesystem with a single df invocation.
    local avail_bytes avail_hr
    avail_bytes=$(df -B1 "$BACKUP_ROOT" 2>/dev/null | awk 'NR==2 {print $4}' || printf '0')
    avail_hr=$(numfmt --to=iec "$avail_bytes" 2>/dev/null || printf '0')

    log_info "Backup filesystem free space at %s: %s bytes (~%s available)." "$BACKUP_ROOT" "$avail_bytes" "$avail_hr"

    printf 'Proceed with Unison synchronization from PSP to backup root? [y/N]: '
    local answer
    read -r answer || answer="n"
    case "$answer" in
        [Yy]*)
            log_info "User confirmed synchronization."
            ;;
        *)
            fatal "User aborted synchronization."
            ;;
    esac
}

#######################################
# Unison sync
#######################################

run_unison_sync() {
    local root_psp root_backup
    root_psp="$PSP_MOUNTPOINT"
    root_backup="$BACKUP_ROOT"

    log_info "Step 5: Starting Unison sync."
    log_info "  PSP root:   %s" "$root_psp"
    log_info "  Backup root: %s" "$root_backup"
    log_info "Unison is configured to NEVER delete files on the backup root."

    # Build Unison command.
    # -auto -batch: non-interactive; accept default actions.
    # -confirmbigdel=false: avoid extra prompts for large deletes (we rely on -nodeletion on backup).
    # -nodeletion <root_backup>: do not delete files on backup root. See unison(1).
    # -fat: appropriate for FAT-like filesystems often used on PSP storage.
    local status
    if unison "$root_psp" "$root_backup" \
        -auto -batch \
        -confirmbigdel=false \
        -nodeletion "$root_backup" \
        -fat
    then
        status=0
    else
        status=$?
    fi

    if [ "$status" -eq 0 ]; then
        log_info "Unison sync completed successfully."
    else
        log_error "Unison sync failed with exit code %d." "$status"
    fi

    return "$status"
}

#######################################
# Eject PSP
#######################################

eject_psp() {
    if [ "$PSP_MOUNTED" -ne 1 ]; then
        log_warn "PSP is not marked as mounted; nothing to eject."
        return 0
    fi

    log_info "Step 6: Flushing buffers and unmounting PSP storage..."
    sync || log_warn "sync(2) reported an error; continuing with unmount."

    if umount "$PSP_MOUNTPOINT"; then
        log_info "Unmounted %s." "$PSP_MOUNTPOINT"
        PSP_MOUNTED=0
    else
        log_warn "Failed to unmount %s; check for open files." "$PSP_MOUNTPOINT"
        return 1
    fi

    # Optionally power off the USB device, if udisksctl is available.
    if [ -n "$PSP_DEVICE" ] && command -v udisksctl >/dev/null 2>&1; then
        if udisksctl power-off -b "$PSP_DEVICE"; then
            log_info "Powered off device %s. It is now safe to disconnect the PSP." "$PSP_DEVICE"
        else
            log_warn "Could not power off device %s via udisksctl. It should still be safe after unmount, but wait for OS indication." "$PSP_DEVICE"
        fi
    else
        log_info "No udisksctl power-off performed. After unmount, it is generally safe to disconnect the PSP."
    fi
}

#######################################
# Main flow
#######################################

main() {
    if [ "$EXIT_TRAP_INSTALLED" -eq 0 ]; then
        trap 'cleanup_on_exit "$?"' EXIT INT TERM
        EXIT_TRAP_INSTALLED=1
    fi

    parse_args "$@"
    check_prereqs

    ensure_nfs_common_installed

    # Ensure tmux protection as early as possible.
    maybe_reexec_in_tmux "$@"

    ensure_nfs_mount
    select_psp_device
    mount_psp_device

    # Clean macOS artifacts on both roots.
    clean_macos_cruft "$PSP_MOUNTPOINT"
    clean_macos_cruft "$BACKUP_ROOT"

    confirm_backup

    # Run unison, allowing retry on failure.
    local status
    while :; do
        if run_unison_sync; then
            status=0
            break
        fi

        status=$?
        printf 'Unison sync failed (exit code %d). Try again? [y/N]: ' "$status"
        local answer
        read -r answer || answer="n"
        case "$answer" in
            [Yy]*)
                log_info "Retrying Unison sync..."
                ;;
            *)
                log_error "User chose not to retry after failure."
                break
                ;;
        esac
    done

    if [ "$status" -eq 0 ]; then
        # Successful sync: eject PSP and tell user to disconnect.
        if eject_psp; then
            printf '\nBackup completed successfully.\n'
            printf 'Step 7: You may now safely disconnect the PSP.\n'
        else
            printf '\nBackup completed, but PSP could not be fully ejected.\n'
            printf 'Step 7: Check for open files, unmount %s manually if needed, then disconnect the PSP.\n' "$PSP_MOUNTPOINT"
        fi
    else
        printf '\nBackup did NOT complete successfully.\n'
        printf 'Step 7: Investigate the errors above. The PSP storage remains mounted at %s.\n' "$PSP_MOUNTPOINT"
        printf 'You can correct the issue and re-run this script to try again.\n'
    fi
}

main "$@"
