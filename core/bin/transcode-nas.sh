#!/usr/bin/env bash

set -Eeuo pipefail

usage() {
    cat <<'EOF'
Usage: transcode-nas.sh list
       transcode-nas.sh transcode DIRECTORY_OR_MKV [...]

List available MKVs recursively, or copy and transcode selected NAS-relative
directories/files to MP4. Paths containing spaces should be quoted.

Examples:
  transcode-nas.sh list
  transcode-nas.sh transcode dune_1983 XFilesS2D1
  transcode-nas.sh transcode 'LongFamilyReunion/title00.mkv'

Environment:
  NAS_HOST        SSH destination (default: ben@nas)
  NAS_MKV_DIR     Absolute remote directory (default: /mnt/nas/tmp/mkvs)
  MKV_DIR         Local source cache (default: ./mkvs)
  MP4_DIR         Local output directory (default: ./mp4s)
  TRANSCODE_DVD   Transcoder executable (default: sibling transcode-dvd.sh)

Requires ssh and rsync on this machine, and find and rsync on the NAS.
Keeps local MKVs and preserves relative paths. Existing MP4s are skipped.
Transfers can be resumed by rerunning. Transcodes use temporary output files
so failed conversions are retried. DVD_TRANSCODE_* settings are passed through.
EOF
}

die() { echo "Error: $*" >&2; exit 1; }

case ${1:-} in
    -h|--help) usage; exit 0 ;;
    list) [[ $# == 1 ]] || { usage >&2; exit 2; } ;;
    transcode) [[ $# -ge 2 ]] || { usage >&2; exit 2; } ;;
    *) usage >&2; exit 2 ;;
esac

host=${NAS_HOST:-ben@nas}
remote_root=${NAS_MKV_DIR:-/mnt/nas/tmp/mkvs}
remote_root=${remote_root%/}
[[ $remote_root == /* ]] || die 'NAS_MKV_DIR must be absolute.'
[[ $host != -* ]] || die 'NAS_HOST must not start with a dash.'
command -v ssh >/dev/null || die 'ssh is required.'

# SSH executes a shell command remotely; quote paths for that shell explicitly.
shell_quote() {
    printf "'%s'" "${1//\'/\'\\\'\'}"
}

listing=$(mktemp)
trap 'rm -f -- "$listing"' EXIT
ssh "$host" "find $(shell_quote "$remote_root") -type f -iname '*.mkv' -print0" > "$listing"
mapfile -d '' -t files < "$listing"

if [[ $1 == list ]]; then
    for file in "${files[@]}"; do
        printf '%s\n' "${file#"$remote_root"/}"
    done
    exit 0
fi
shift

command -v rsync >/dev/null || die 'rsync is required.'
script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
transcoder=${TRANSCODE_DVD:-$script_dir/transcode-dvd.sh}
command -v "$transcoder" >/dev/null || die "Transcoder not found: $transcoder"
mkv_dir=${MKV_DIR:-./mkvs}
mp4_dir=${MP4_DIR:-./mp4s}
selected=()
for selection in "$@"; do
    selection=${selection%/}
    selection=${selection#./}
    [[ -n $selection && $selection != /* && /$selection/ != */../* ]] || die "Expected a NAS-relative path: $selection"
    matched=false
    for file in "${files[@]}"; do
        relative=${file#"$remote_root"/}
        if [[ $relative == "$selection" || $relative == "$selection/"* || $selection == . ]]; then
            selected+=("$relative")
            matched=true
        fi
    done
    $matched || die "No MKVs found for: $selection (use 'list' to see available files)"
done

for relative in "${selected[@]}"; do
    input=$mkv_dir/$relative
    output=$mp4_dir/${relative%.*}.mp4
    if [[ -e $output || -L $output ]]; then
        printf 'Skipping existing output: %s\n' "$output"
        continue
    fi
    mkdir -p -- "$(dirname -- "$input")" "$(dirname -- "$output")"
    printf 'Copying: %s\n' "$relative"
    rsync --protect-args --partial --progress -- "$host:$remote_root/$relative" "$input"
    # Keep the .mp4 suffix so the transcoder selects the MP4 container.
    temporary=${output%.mp4}.partial.mp4
    [[ ! -e $temporary && ! -L $temporary ]] || die "Partial output exists; remove it before retrying: $temporary"
    if "$transcoder" "$input" "$temporary"; then
        mv -- "$temporary" "$output"
    else
        rm -f -- "$temporary"
        die "Transcode failed: $relative"
    fi
done
