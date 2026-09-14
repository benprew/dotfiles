#!/usr/bin/env bash

set -Eeuo pipefail

usage() {
    cat <<'EOF'
Usage: transcode-dvd.sh INPUT.mkv [OUTPUT.mkv]

Transcode a DVD rip to high-quality H.265 video in an MKV container.
The default output is INPUT.h265.mkv.

Optional environment variables:
  DVD_TRANSCODE_ENCODER         HandBrake video encoder (default: x265)
  DVD_TRANSCODE_QUALITY         Constant-quality RF value (default: 18; lower is better)
  DVD_TRANSCODE_PRESET          Encoder speed preset (default: slow)
  DVD_TRANSCODE_AUDIO_LANGS     Comma-separated list of audio languages (default: all)
  DVD_TRANSCODE_SUBTITLE_LANGS  Comma-separated list of subtitle languages (default: all)
  DVD_TRANSCODE_CHAPTERS        Chapter range to transcode (e.g. 1-4)
  DVD_TRANSCODE_EXTRA_ARGS      Additional space-separated args for HandBrakeCLI

Examples:
  transcode-dvd.sh movie.mkv
  transcode-dvd.sh movie.mkv movie-compressed.mkv
  DVD_TRANSCODE_QUALITY=20 transcode-dvd.sh movie.mkv
  DVD_TRANSCODE_AUDIO_LANGS=eng,jpn DVD_TRANSCODE_SUBTITLE_LANGS=eng transcode-dvd.sh movie.mkv
EOF
}

if [[ ${1:-} == "-h" || ${1:-} == "--help" ]]; then
    usage
    exit 0
fi

if (( $# < 1 || $# > 2 )); then
    usage >&2
    exit 2
fi

if ! command -v HandBrakeCLI >/dev/null 2>&1; then
    echo "Error: HandBrakeCLI is not installed or is not in PATH." >&2
    exit 1
fi

input=$1
encoder=${DVD_TRANSCODE_ENCODER:-x265}
quality=${DVD_TRANSCODE_QUALITY:-18}
encoder_preset=${DVD_TRANSCODE_PRESET:-slow}
audio_langs=${DVD_TRANSCODE_AUDIO_LANGS:-}
subtitle_langs=${DVD_TRANSCODE_SUBTITLE_LANGS:-}
chapters=${DVD_TRANSCODE_CHAPTERS:-}

if [[ ! -f $input ]]; then
    echo "Error: input is not a file: $input" >&2
    exit 1
fi

if [[ ! $quality =~ ^([0-9]+)(\.[0-9]+)?$ ]]; then
    echo "Error: DVD_TRANSCODE_QUALITY must be a non-negative number." >&2
    exit 2
fi

case $encoder in
    x264*) codec_name=h264 ;;
    x265*) codec_name=h265 ;;
    svt_av1*) codec_name=av1 ;;
    *) codec_name=compressed ;;
esac

if (( $# == 2 )); then
    output=$2
else
    input_dir=$(dirname -- "$input")
    input_name=$(basename -- "$input")
    input_stem=${input_name%.*}
    [[ -n $input_stem ]] || input_stem=$input_name
    output="${input_dir}/${input_stem}.${codec_name}.mkv"
fi

if [[ -e $output || -L $output ]]; then
    echo "Error: output already exists: $output" >&2
    exit 1
fi

output_dir=$(dirname -- "$output")
if [[ ! -d $output_dir ]]; then
    echo "Error: output directory does not exist: $output_dir" >&2
    exit 1
fi

echo "Input:   $input"
echo "Output:  $output"
echo "Video:   $encoder, RF $quality, preset $encoder_preset"
[[ -n $audio_langs ]] && echo "Audio:   languages ($audio_langs)"
[[ -n $subtitle_langs ]] && echo "Subs:    languages ($subtitle_langs)"
[[ -n $chapters ]] && echo "Chapters: $chapters"

handbrake_args=(
    --input "$input"
    --output "$output"
    --format av_mkv
    --encoder "$encoder"
    --encoder-preset "$encoder_preset"
    --quality "$quality"
    --vfr
    --auto-anamorphic
    --crop-mode none
    --comb-detect
    --decomb
)

if [[ -n $chapters ]]; then
    handbrake_args+=(--chapters "$chapters")
fi

if [[ -n $audio_langs ]]; then
    handbrake_args+=(--audio-lang-list "$audio_langs" --all-audio)
else
    handbrake_args+=(--all-audio)
fi

handbrake_args+=(
    --aencoder copy
    --audio-copy-mask aac,ac3,eac3,truehd,dts,dtshd,mp2,mp3,opus,vorbis,flac,alac,pcm
    --audio-fallback flac16
    --keep-aname
)

if [[ -n $subtitle_langs ]]; then
    handbrake_args+=(--subtitle-lang-list "$subtitle_langs" --all-subtitles)
else
    handbrake_args+=(--all-subtitles)
fi

handbrake_args+=(
    --keep-subname
    --markers
    --keep-metadata
)

if [[ -n ${DVD_TRANSCODE_EXTRA_ARGS:-} ]]; then
    read -r -a extra_args <<< "$DVD_TRANSCODE_EXTRA_ARGS"
    handbrake_args+=("${extra_args[@]}")
fi

HandBrakeCLI "${handbrake_args[@]}"
