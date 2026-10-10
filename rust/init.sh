# Fedora uses the system toolchain; other platforms may use rustup.
if [ ! -f /etc/fedora-release ] && [ -r "${CARGO_HOME:-$HOME/.cargo}/env" ]; then
    . "${CARGO_HOME:-$HOME/.cargo}/env"
fi
