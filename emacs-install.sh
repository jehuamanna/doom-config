#!/usr/bin/env bash

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
    if (( count >= attempts )); then
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

if command -v emacs >/dev/null 2>&1; then
  if emacs --version | grep -q "${EMACS_VERSION}"; then
    echo "Emacs ${EMACS_VERSION} already installed"
    exit 0
  fi
fi

require_cmd sudo
require_cmd wget
require_cmd tar
require_cmd make

BUILD_DIR="$(mktemp -d)"
cd "$BUILD_DIR"

sudo apt-get update -y

sudo DEBIAN_FRONTEND=noninteractive apt-get install -y \
  libmagickwand-dev \
  libmagickcore-dev \
  imagemagick \
  libgtk-3-dev \
  libglib2.0-dev \
  libwebkit2gtk-4.1-dev \
  libgccjit-13-dev \
  gcc-13 \
  libxpm-dev \
  libgif-dev \
  libgnutls28-dev \
  libtree-sitter-dev \
  libncurses-dev \
  autoconf \
  texinfo \
  wget \
  xz-utils \
  build-essential

retry 3 wget -q --show-progress "$URL"

tar -xf "$TARBALL"
cd "emacs-${EMACS_VERSION}"

CC_BIN="${CC:-gcc-13}"

if ! command -v "$CC_BIN" >/dev/null 2>&1; then
  echo "Compiler $CC_BIN not found"
  exit 1
fi

./configure \
  --prefix="$PREFIX" \
  CC="$CC_BIN" \
  CFLAGS='-O3 -march=native -mtune=native -fomit-frame-pointer' \
  --enable-link-time-optimization \
  --with-pgtk \
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

make -j"$JOBS"

sudo make install

if ! command -v emacs >/dev/null 2>&1; then
  echo "Installation failed: emacs not found in PATH"
  exit 1
fi

if ! emacs --version | grep -q "${EMACS_VERSION}"; then
  echo "Installation verification failed"
  exit 1
fi

echo "Emacs ${EMACS_VERSION} installed successfully"
