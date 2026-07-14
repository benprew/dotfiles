#!/usr/bin/env bash

set -euo pipefail

# Install the Perforce P4 Visual Client for Linux x86_64.
#
# The default uses Perforce's snapshot build. To install a pinned release,
# pass --version (for example, r25.1) or set P4V_VERSION in the environment.

readonly INSTALL_DIR=/opt/p4v
readonly PLATFORM=bin.linux26x86_64
VERSION="${P4V_VERSION:-snapshot}"
FORCE=0

usage() {
    cat <<EOF
Usage: $0 [--version VERSION] [--force]

Install P4V into ${INSTALL_DIR}.

Options:
  --version VERSION  Perforce release directory (default: ${VERSION})
  --force            Replace an existing ${INSTALL_DIR}
  -h, --help         Show this help
EOF
}

while (($#)); do
    case "$1" in
        --version)
            (($# >= 2)) || { echo "--version requires a value" >&2; exit 2; }
            VERSION=$2
            shift 2
            ;;
        --force)
            FORCE=1
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            usage >&2
            exit 2
            ;;
    esac
done

if [[ $EUID -eq 0 ]]; then
    SUDO=()
else
    SUDO=(sudo)
fi

for command in tar sha256sum; do
    command -v "$command" >/dev/null || {
        echo "Required command not found: $command" >&2
        exit 1
    }
done

if command -v curl >/dev/null; then
    download() { curl --fail --location --retry 3 --silent --show-error --output "$2" "$1"; }
elif command -v wget >/dev/null; then
    download() { wget --quiet --output-document="$2" "$1"; }
else
    echo "Required command not found: curl or wget" >&2
    exit 1
fi

if [[ $(uname -m) != x86_64 ]]; then
    echo "P4V's Linux package is currently x86_64; detected $(uname -m)." >&2
    exit 1
fi

if [[ -e $INSTALL_DIR && $FORCE -ne 1 ]]; then
    echo "${INSTALL_DIR} already exists; use --force to replace it." >&2
    exit 1
fi

work_dir=$(mktemp -d "${TMPDIR:-/tmp}/p4v-install.XXXXXX")
cleanup() {
    rm -rf "$work_dir"
}
trap cleanup EXIT

base_url="https://ftp.perforce.com/perforce/${VERSION}/p4v/${PLATFORM}"
archive="$work_dir/p4v.tgz"
checksums="$work_dir/SHA256SUMS"

echo "Downloading P4V (${VERSION})..."
download "${base_url}/p4v.tgz" "$archive"
download "${base_url}/SHA256SUMS" "$checksums"

expected_checksum=$(awk '$2 == "p4v.tgz" || $2 == "*p4v.tgz" { print $1; exit }' "$checksums")
if [[ ! $expected_checksum =~ ^[[:xdigit:]]{64}$ ]]; then
    echo "Could not find a valid p4v.tgz SHA-256 checksum." >&2
    exit 1
fi
actual_checksum=$(sha256sum "$archive" | awk '{print $1}')
if [[ $actual_checksum != "$expected_checksum" ]]; then
    echo "P4V checksum verification failed." >&2
    exit 1
fi

extract_dir="$work_dir/extracted"
mkdir "$extract_dir"
tar -xzf "$archive" -C "$extract_dir"
[[ -f "$extract_dir/p4v" || -n "$(find "$extract_dir" -type f -name p4v -print -quit)" ]] || {
    echo "The P4V archive did not contain a p4v executable." >&2
    exit 1
}

# Normalize archives with or without a top-level directory into a temporary
# destination, then move it into /opt in one operation.
source_dir="$extract_dir"
entries=("$extract_dir"/*)
if [[ ${#entries[@]} -eq 1 && -d ${entries[0]} ]]; then
    source_dir=${entries[0]}
fi
install_dir="$work_dir/p4v"
cp -a "$source_dir" "$install_dir"

if [[ $FORCE -eq 1 ]]; then
    "${SUDO[@]}" rm -rf "$INSTALL_DIR"
fi
"${SUDO[@]}" install -d -m 0755 /opt
"${SUDO[@]}" mv "$install_dir" "$INSTALL_DIR"
"${SUDO[@]}" chmod -R a+rX "$INSTALL_DIR"

echo "P4V installed in ${INSTALL_DIR}."
