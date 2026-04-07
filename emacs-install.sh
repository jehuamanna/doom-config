h

set -Eeuo pipefail
IFS=$'\n\t'

LOG_FILE="${LOG_FILE:-emacs-build.log}"

exec > >(tee -a "$LOG_FILE") 2>&1

error_handler() {
  echo "Error on line $1"
  exit 1
}
trap 'error_handler $LINENO' ERR

cleanup() {
  if [[ -n "${BUILD_DIR:-}" && -d "${BUILD_DIR:-}" ]]; then
    rm -rf "$BUILD_DIR"
  fi
}
trap cleanup EXIT

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || {
    echo "Missing required command: $1"
    exit 1
  }
}

retry() {
  local attempts=$1
  shift
  local count=0
  until "$@"; do
    count=$((count + 1))
    if ((count >= attempts)); then
      echo "Command failed after $attempts attempts: $*"
      return 1
    fi
    sleep 2
  done
}

EMACS_VERSION="${EMACS_VERSION:-30.2}"
PREFIX="${PREFIX:-/usr/local}"
TARBALL="emacs-${EMACS_VERSION}.tar.xz"
URL="https://ftp.gnu.org/gnu/emacs/${TARBALL}"

require_cmd sudo
require_cmd wget
require_cmd tar
require_cmd make

echo "Removing any existing Emacs installation"

if command -v emacs >/dev/null 2>&1; then
  sudo rm -f /usr/local/bin/emacs || true
  sudo rm -rf /usr/local/share/emacs || true
  sudo rm -rf /usr/local/libexec/emacs || true
fi

sudo apt-get update -y

sudo DEBIAN_FRONTEND=noninteractive apt-get install -y \
  build-essential \
  autoconf \
  texinfo \
  xz-utils \
  wget \
  gcc-13 \
  libgccjit-13-dev \
  libgtk-3-dev \
  libglib2.0-dev \
  libwebkit2gtk-4.1-dev \
  libncurses-dev \
  libtree-sitter-dev \
  libgnutls28-dev \
  libgif-dev \
  libxpm-dev \
  libjpeg-dev \
  libpng-dev \
  libtiff-dev \
  libmagickwand-dev \
  libmagickcore-dev \
  imagemagick \
  libxml2-dev \
  libsqlite3-dev \
  libharfbuzz-dev \
  libcairo2-dev \
  librsvg2-dev \
  liblcms2-dev \
  mailutils \
  libx11-dev \
  libxrandr-dev \
  libxinerama-dev \
  libxcursor-dev \
  libxfixes-dev \
  libxi-dev \
  libxext-dev

BUILD_DIR="$(mktemp -d)"
cd "$BUILD_DIR"

echo "Downloading Emacs ${EMACS_VERSION}"
retry 3 wget -q --show-progress "$URL"

tar -xf "$TARBALL"
cd "emacs-${EMACS_VERSION}"

CC_BIN="${CC:-gcc-13}"

if ! command -v "$CC_BIN" >/dev/null 2>&1; then
  echo "Compiler $CC_BIN not found"
  exit 1
fi

echo "Configuring build (X11)"

./configure \
  --prefix="$PREFIX" \
  CC="$CC_BIN" \
  CFLAGS='-O3 -march=native -mtune=native -fomit-frame-pointer' \
  --enable-link-time-optimization \
  --with-x-toolkit=gtk3 \
  --with-x \
  --with-native-compilation \
  --with-modules \
  --with-threads \
  --with-compress-install \
  --with-cairo \
  --with-harfbuzz \
  --with-rsvg \
  --with-gnutls \
  --with-xml2 \
  --with-zlib \
  --with-jpeg \
  --with-png \
  --with-tiff \
  --with-gif \
  --with-imagemagick \
  --with-sqlite3 \
  --with-tree-sitter \
  --with-mailutils \
  --with-pop \
  --with-sound \
  --with-lcms2

JOBS="$(nproc || echo 2)"
if [[ "$JOBS" -gt 1 ]]; then
  JOBS=$((JOBS - 1))
fi

echo "Building with $JOBS jobs"
make -j"$JOBS"

echo "Installing Emacs"
sudo make install

echo "Verifying installation"

if ! command -v emacs >/dev/null 2>&1; then
  echo "Installation failed: emacs not found in PATH"
  exit 1
fi

if ! emacs --version | grep -q "${EMACS_VERSION}"; then
  echo "Installation verification failed"
  exit 1
fi

echo "Emacs ${EMACS_VERSION} installed successfully (X11 build)"
