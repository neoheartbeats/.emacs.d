#!/usr/bin/env bash
set -Eeuo pipefail

FORMULA="emacs-igc3"
TAP="local/igc3"

REPO="${EMACS_IGC3_REPO:-https://github.com/emacs-mirror/emacs.git}"
BRANCH="${EMACS_IGC3_BRANCH:-feature/igc3}"

WITH_NATIVE_COMP=1
WITH_NATIVE_FULL_AOT=0
WITH_XWIDGETS=0
WITH_DEBUG=0
WITH_MARCH_NATIVE=0

REINSTALL=0
LINK_APP=1
RUN_TEST=1
BREW_UPDATE=0

usage() {
  cat <<'EOF'
Usage:
  ./install-emacs-igc3.sh [options]

Default:
  - build GNU Emacs feature/igc3
  - enable Cocoa/NS app
  - enable MPS/IGC via --with-mps
  - enable native-comp
  - enable tree-sitter, sqlite, json, rsvg, webp, gnutls
  - install as Homebrew formula: local/igc3/emacs-igc3
  - command name: emacs-igc3
  - app symlink: /Applications/Emacs-IGC3.app

Options:
  --reinstall             Uninstall existing emacs-igc3 first, then build again
  --no-native-comp        Disable native compilation
  --native-full-aot       Enable full native AOT compilation
  --with-xwidgets         Enable NS xwidgets
  --debug                 Add debug symbols and Emacs checking
  --march-native          Add -march=native; local machine only
  --no-link-app           Do not create /Applications/Emacs-IGC3.app symlink
  --no-test               Skip post-install batch test
  --brew-update           Run brew update before building
  --repo URL              Git repo to build from
  --branch NAME           Git branch to build from
  -h, --help              Show this help

Examples:
  ./install-emacs-igc3.sh --reinstall
  ./install-emacs-igc3.sh --with-xwidgets --reinstall
  ./install-emacs-igc3.sh --no-native-comp --debug --reinstall
  ./install-emacs-igc3.sh --repo https://git.savannah.gnu.org/git/emacs.git --branch feature/igc3 --reinstall
EOF
}

die() {
  echo "error: $*" >&2
  exit 1
}

warn() {
  printf '\033[1;33mwarning:\033[0m %s\n' "$*" >&2
}

info() {
  printf '\033[1;34m==>\033[0m %s\n' "$*"
}

rb_bool() {
  if [[ "$1" == "1" ]]; then
    printf "true"
  else
    printf "false"
  fi
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --reinstall)
      REINSTALL=1
      shift
      ;;
    --no-native-comp)
      WITH_NATIVE_COMP=0
      WITH_NATIVE_FULL_AOT=0
      shift
      ;;
    --native-full-aot)
      WITH_NATIVE_COMP=1
      WITH_NATIVE_FULL_AOT=1
      shift
      ;;
    --with-xwidgets)
      WITH_XWIDGETS=1
      shift
      ;;
    --debug)
      WITH_DEBUG=1
      shift
      ;;
    --march-native)
      WITH_MARCH_NATIVE=1
      shift
      ;;
    --no-link-app)
      LINK_APP=0
      shift
      ;;
    --no-test)
      RUN_TEST=0
      shift
      ;;
    --brew-update)
      BREW_UPDATE=1
      shift
      ;;
    --repo)
      [[ $# -ge 2 ]] || die "--repo needs an argument"
      REPO="$2"
      shift 2
      ;;
    --branch)
      [[ $# -ge 2 ]] || die "--branch needs an argument"
      BRANCH="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      die "unknown option: $1"
      ;;
  esac
done

[[ "$(uname -s)" == "Darwin" ]] || die "this script is intended for macOS"

command -v brew >/dev/null 2>&1 || die "Homebrew is required"
command -v git >/dev/null 2>&1 || die "git is required"

xcode-select -p >/dev/null 2>&1 || {
  die "Xcode Command Line Tools are required. Install with: xcode-select --install"
}

BREW_PREFIX="$(brew --prefix)"

if [[ "$BREW_PREFIX" != "/opt/homebrew" && "$BREW_PREFIX" != "/usr/local" ]]; then
  warn "unusual Homebrew prefix: $BREW_PREFIX"
fi

if [[ "$BREW_UPDATE" == "1" ]]; then
  info "Updating Homebrew"
  brew update
fi

info "Checking branch ${BRANCH} in ${REPO}"
if ! git ls-remote --exit-code --heads "$REPO" "$BRANCH" >/dev/null 2>&1; then
  die "cannot find branch '${BRANCH}' in '${REPO}'"
fi

if ! brew tap | grep -qx "$TAP"; then
  info "Creating local Homebrew tap: $TAP"
  brew tap-new "$TAP" >/dev/null
fi

TAP_DIR="$(brew --repository "$TAP")"
FORMULA_DIR="${TAP_DIR}/Formula"
FORMULA_PATH="${FORMULA_DIR}/${FORMULA}.rb"

mkdir -p "$FORMULA_DIR"

info "Writing formula: $FORMULA_PATH"

cat > "$FORMULA_PATH" <<RUBY
# frozen_string_literal: true

class EmacsIgc3 < Formula
  desc "GNU Emacs feature/igc3 with MPS incremental GC"
  homepage "https://www.gnu.org/software/emacs/"
  license "GPL-3.0-or-later"

  head "$REPO", branch: "$BRANCH"

  WITH_NATIVE_COMP = $(rb_bool "$WITH_NATIVE_COMP")
  WITH_NATIVE_FULL_AOT = $(rb_bool "$WITH_NATIVE_FULL_AOT")
  WITH_XWIDGETS = $(rb_bool "$WITH_XWIDGETS")
  WITH_DEBUG = $(rb_bool "$WITH_DEBUG")
  WITH_MARCH_NATIVE = $(rb_bool "$WITH_MARCH_NATIVE")

  depends_on "make" => :build
  depends_on "autoconf" => :build
  depends_on "automake" => :build
  depends_on "pkg-config" => :build
  depends_on "texinfo" => :build
  depends_on "coreutils" => :build
  depends_on "gnu-sed" => :build
  depends_on "gnu-tar" => :build
  depends_on "grep" => :build
  depends_on "awk" => :build
  depends_on "m4" => :build
  depends_on "xz" => :build

  depends_on "libmps"
  depends_on "jansson"
  depends_on "gnutls"
  depends_on "librsvg"
  depends_on "libxml2"
  depends_on "sqlite"
  depends_on "tree-sitter"
  depends_on "webp"
  depends_on "little-cms2"
  depends_on "giflib"
  depends_on "libpng"
  depends_on "jpeg"
  depends_on "zlib"
  depends_on "expat"

  depends_on "gcc" if WITH_NATIVE_COMP
  depends_on "libgccjit" if WITH_NATIVE_COMP
  depends_on "gmp" if WITH_NATIVE_COMP

  def install
    ENV.prepend_path "PATH", Formula["make"].opt_libexec/"gnubin"
    ENV.prepend_path "PATH", Formula["coreutils"].opt_libexec/"gnubin"
    ENV.prepend_path "PATH", Formula["gnu-sed"].opt_libexec/"gnubin"
    ENV.prepend_path "PATH", Formula["gnu-tar"].opt_libexec/"gnubin"
    ENV.prepend_path "PATH", Formula["grep"].opt_libexec/"gnubin"

    ENV.append_to_cflags "-pipe"
    ENV.append_to_cflags "-DFD_SETSIZE=10000"
    ENV.append_to_cflags "-DDARWIN_UNLIMITED_SELECT"

    if WITH_DEBUG
      ENV.append_to_cflags "-g3"
      ENV.append_to_cflags "-O0"
    else
      ENV.append_to_cflags "-O2"
    end

    ENV.append_to_cflags "-march=native" if WITH_MARCH_NATIVE

    ENV.append "CPPFLAGS", "-I#{Formula["libmps"].include}"
    ENV.append "LDFLAGS", "-L#{Formula["libmps"].opt_lib}"

    ENV.append "CPPFLAGS", "-I#{Formula["sqlite"].include}"
    ENV.append "LDFLAGS", "-L#{Formula["sqlite"].opt_lib}"

    ENV.append "CPPFLAGS", "-I#{Formula["zlib"].include}"
    ENV.append "LDFLAGS", "-L#{Formula["zlib"].opt_lib}"

    make_args = []

    args = %W[
      --disable-dependency-tracking
      --disable-silent-rules
      --enable-locallisppath=#{HOMEBREW_PREFIX}/share/emacs/site-lisp
      --infodir=#{info}/emacs
      --prefix=#{prefix}
      --with-ns
      --disable-ns-self-contained
      --without-x
      --with-mps
      --with-modules
      --with-json
      --with-gnutls
      --with-rsvg
      --with-webp
      --with-xml2
      --with-tree-sitter
      --with-sqlite3
      --without-dbus
      --without-imagemagick
      --without-pop
    ]

    args << "--with-xwidgets" if WITH_XWIDGETS
    args << "--enable-checking=yes,glyphs" if WITH_DEBUG

    if WITH_NATIVE_COMP
      args << "--with-native-compilation"

      gcc_version = Formula["gcc"].any_installed_version
      gcc_major = gcc_version.major
      gcc_lib = "#{HOMEBREW_PREFIX}/lib/gcc/#{gcc_major}"

      ENV.append "CPPFLAGS", "-I#{Formula["gcc"].include}"
      ENV.append "CPPFLAGS", "-I#{Formula["libgccjit"].include}"
      ENV.append "CPPFLAGS", "-I#{Formula["gmp"].include}"
      ENV.append "LDFLAGS", "-L#{gcc_lib}"
      ENV.append "LDFLAGS", "-Wl,-rpath,#{gcc_lib}"
      ENV.append "LDFLAGS", "-L#{Formula["libgccjit"].opt_lib}"

      make_args << "NATIVE_FULL_AOT=1" if WITH_NATIVE_FULL_AOT
      make_args << "BYTE_COMPILE_EXTRA_FLAGS=--eval '(setq comp-speed 2)'"
    else
      args << "--without-native-compilation"
    end

    apply_macos_spawn_compat_patch

    system "./autogen.sh"
    system "./configure", *args

    disable_aligned_alloc_on_old_macos

    system "gmake", *make_args

    if WITH_DEBUG
      ns_emacs = buildpath/"nextstep/Emacs.app/Contents/MacOS/Emacs"
      system "dsymutil", ns_emacs.to_s if ns_emacs.exist?
    end

    system "gmake", "install"

    rm_rf prefix/"Emacs.app"
    prefix.install "nextstep/Emacs.app"

    if WITH_NATIVE_COMP && (buildpath/"native-lisp").exist?
      (prefix/"Emacs.app/Contents").install "native-lisp"
    end

    bin.mkpath

    rm_f bin/"emacs"
    (bin/"emacs-igc3").write <<~EOS
      #!/bin/bash
      exec "#{prefix}/Emacs.app/Contents/MacOS/Emacs" "\$@"
    EOS
    chmod 0755, bin/"emacs-igc3"

    if (bin/"emacsclient").exist?
      mv bin/"emacsclient", bin/"emacsclient-igc3"
    end

    %w[ctags etags ebrowse grep-changelog].each do |program|
      mv bin/program, bin/"#{program}-igc3" if (bin/program).exist?
    end

    %w[
      emacs.1 emacsclient.1
      ctags.1 etags.1 ebrowse.1 grep-changelog.1
    ].each do |page|
      rm_f man1/page
      rm_f man1/"#{page}.gz"
    end
  end

  def apply_macos_spawn_compat_patch
    file = buildpath/"src/callproc.c"
    return unless file.exist?

    text = file.read
    return if text.include?("__builtin_available(macOS 26.0")

    old = <<~'C'
      #if defined HAVE_POSIX_SPAWN_FILE_ACTIONS_ADDCHDIR && !defined HAIKU
        error = posix_spawn_file_actions_addchdir (actions, cwd);
      #else
        error = posix_spawn_file_actions_addchdir_np (actions, cwd);
      #endif
    C

    new = <<~'C'
      #if defined HAVE_POSIX_SPAWN_FILE_ACTIONS_ADDCHDIR && !defined HAIKU && defined(__APPLE__)
        if (__builtin_available(macOS 26.0, *))
          error = posix_spawn_file_actions_addchdir (actions, cwd);
        else
          error = posix_spawn_file_actions_addchdir_np (actions, cwd);
      #elif defined HAVE_POSIX_SPAWN_FILE_ACTIONS_ADDCHDIR && !defined HAIKU
        error = posix_spawn_file_actions_addchdir (actions, cwd);
      #else
        error = posix_spawn_file_actions_addchdir_np (actions, cwd);
      #endif
    C

    if text.include?(old)
      inreplace file, old, new, global: false
      ohai "Applied macOS posix_spawn_file_actions_addchdir compatibility patch"
    else
      opoo "macOS spawn compatibility patch pattern not found; assuming upstream changed"
    end
  end

  def disable_aligned_alloc_on_old_macos
    return unless OS.mac?
    return unless MacOS.version <= :mojave

    config_h = buildpath/"src/config.h"
    return unless config_h.exist?

    ohai "Force disabling aligned_alloc/allocation macros on macOS <= Mojave"

    text = config_h.read
      .gsub("#define HAVE_ALIGNED_ALLOC 1", "#undef HAVE_ALIGNED_ALLOC")
      .gsub("#define HAVE_DECL_ALIGNED_ALLOC 1", "#undef HAVE_DECL_ALIGNED_ALLOC")
      .gsub("#define HAVE_ALLOCA 1", "#undef HAVE_ALLOCA")
      .gsub("#define HAVE_ALLOCA_H 1", "#undef HAVE_ALLOCA_H")

    config_h.atomic_write(text)
  end

  def post_install
    emacs_info_dir = info/"emacs"
    Dir.glob(emacs_info_dir/"*.info{,.gz}") do |info_file|
      system "install-info", "--info-dir=#{emacs_info_dir}", info_file
    end

    app = prefix/"Emacs.app"
    if OS.mac? && app.exist?
      ohai "Ad-hoc signing Emacs.app"
      system "codesign", "--force", "--deep", "--sign", "-", app.to_s
    end
  end

  def caveats
    <<~EOS
      Installed as:
        #{opt_bin}/emacs-igc3

      App bundle:
        #{opt_prefix}/Emacs.app

      Optional app symlink:
        ln -s #{opt_prefix}/Emacs.app /Applications/Emacs-IGC3.app

      Daemon:
        brew services start #{name}

      Verify MPS/IGC:
        #{opt_bin}/emacs-igc3 --batch --eval '(princ system-configuration-options)'
    EOS
  end

  service do
    run [opt_bin/"emacs-igc3", "--fg-daemon"]
    keep_alive true
    log_path "/tmp/homebrew.mxcl.emacs-igc3.stdout.log"
    error_log_path "/tmp/homebrew.mxcl.emacs-igc3.stderr.log"
  end

  test do
    output = shell_output("#{bin}/emacs-igc3 --batch --eval='(princ system-configuration-options)'")
    assert_match "--with-mps", output
  end
end
RUBY

if brew list --formula "$FORMULA" >/dev/null 2>&1; then
  if [[ "$REINSTALL" == "1" ]]; then
    info "Uninstalling existing ${FORMULA}"
    brew uninstall --ignore-dependencies "$FORMULA"
  else
    warn "${FORMULA} is already installed"
    warn "run with --reinstall to rebuild from current ${BRANCH}"
    exit 0
  fi
fi

info "Installing ${TAP}/${FORMULA} from ${BRANCH}"
brew install --HEAD --build-from-source "${TAP}/${FORMULA}"

APP_SRC="$(brew --prefix "$FORMULA")/Emacs.app"
APP_DEST="/Applications/Emacs-IGC3.app"

if [[ "$LINK_APP" == "1" ]]; then
  if [[ -d "$APP_SRC" ]]; then
    if [[ -L "$APP_DEST" ]]; then
      rm "$APP_DEST"
    fi

    if [[ ! -e "$APP_DEST" ]]; then
      info "Linking app: $APP_DEST -> $APP_SRC"
      ln -s "$APP_SRC" "$APP_DEST" || warn "failed to link app into /Applications"
    else
      warn "$APP_DEST already exists and is not a symlink; leaving it untouched"
    fi
  else
    warn "app bundle not found at $APP_SRC"
  fi
fi

if [[ "$RUN_TEST" == "1" ]]; then
  EMACS_BIN="$(brew --prefix "$FORMULA")/bin/emacs-igc3"

  info "Verifying build"
  "$EMACS_BIN" --batch --eval '(progn
    (princ (format "Emacs: %s\n" emacs-version))
    (princ (format "MPS/IGC enabled: %s\n"
                   (if (string-match-p "--with-mps" system-configuration-options)
                       "yes"
                     "no")))
    (princ (format "native-comp available: %s\n"
                   (if (fboundp (quote native-compile))
                       "yes"
                     "no")))
    (princ (format "configure options: %s\n" system-configuration-options)))'
fi

info "Done"
info "Run: emacs-igc3"
info "App: /Applications/Emacs-IGC3.app"
