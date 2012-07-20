#!/bin/bash
# ranger supports enhanced previews.  If the option "use_preview_script"
# is set to True and this file exists, this script will be called and its
# output is displayed in ranger.  ANSI color codes are supported.

# NOTES: This script is considered a configuration file.  If you upgrade
# ranger, it will be left untouched. (You must update it yourself.)
# Also, ranger disables STDIN here, so interactive scripts won't work properly

# Meanings of exit codes:
# code | meaning    | action of ranger
# -----+------------+-------------------------------------------
# 0    | success    | success. display stdout as preview
# 1    | no preview | failure. display no preview at all
# 2    | plain text | display the plain content of the file
# 3    | fix width  | success. Don't reload when width changes
# 4    | fix height | success. Don't reload when height changes
# 5    | fix both   | success. Don't ever reload

# Meaningful aliases for arguments:
path="$1"    # Full path of the selected file
width="$2"   # Width of the preview pane (number of fitting characters)
height="$3"  # Height of the preview pane (number of fitting characters)

have() { type -P "$1" > /dev/null; }
trim() { head -n ${1:-$height}; }

success() {
  case ${PIPESTATUS[0]} in
    0|141) exit 0;;
    *)     exit 1;;
  esac
}

extension="${path##*.}"
case "$extension" in
  7z|a|ace|alz|arc|arj|bz|bz2|cab|cpio|deb|gz|jar|lha|lz|lzh|lzma|lzo|\
  rar|rpm|rz|t7z|tar|tbz|tbz2|tgz|tlz|txz|tZ|tzo|war|xpi|xz|Z|zip)
    als "$path" | trim
    success && exit 0 || acat "$path" | trim && exit 3
    exit 1;;

  pdf)
    pdftotext -l 10 -nopgbrk -q "$path" - | trim | fmt -s -w $width
    success && exit 0 || exit 1;;

  doc)
    catdoc "$path" | trim | fmt -s -w $width
    success && exit 0 || exit 1;;

  odt|odp)
    odt2txt "$path" | trim | fmt -s -w $width
    success && exit 0 || exit 1;;

  torrent)
    transmission-show "$path" | trim && exit 3
    success && exit 5 || exit 1;;

  htm|html|xhtml)
    have w3m    && w3m    -dump "$path" | trim | fmt -s -w $width && exit 4
    have lynx   && lynx   -dump "$path" | trim | fmt -s -w $width && exit 4
    have elinks && elinks -dump "$path" | trim | fmt -s -w $width && exit 4
    ;; # fall back to highlight/cat if theres no lynx/elinks
esac

mimetype=$(file --mime-type -Lb "$path")
case "$mimetype" in
  text/*|*/xml)
    highlight --out-format=ansi "$path" | trim
    success && exit 5 || exit 2;;

  image/*)
    img2txt --gamma=0.6 --width="$width" "$path" && exit 4 || exit 1;;

  video/*|audio/*)
    have exiftool && exiftool "$path" && exit 5
    if have mediainfo; then
      mediainfo "$path" | sed 's/  \+:/: /;'
      success && exit 5
    fi
    exit 1;;
esac

exit 1
