#!/bin/sh
draw() {
  ~/.config/lf/draw_img.sh "$@"
  exit 1
}

hash() {
  printf '%s/.cache/lf/%s' "$HOME" \
    "$(stat --printf '%n\0%i\0%F\0%s\0%W\0%Y' -- "$(readlink -f "$1")" | sha256sum | awk '{print $1}')"
}

cache() {
  if [ -f "$1" ]; then
    draw "$@"
  fi
}

file="$1"
shift

case "$(file -Lb --mime-type -- "$file")" in
  */xml) bat --paging=never --line-range :50 "$file" ;;
  *opendocument*) odt2txt "${file}" ;;
  application/bzip2) atool --list -- "${file}" ;;
  application/gzip) atool --list -- "${file}" ;;
  application/json) bat --paging=never --line-range :50 "$file" ;;
  application/lzma) atool --list -- "${file}" ;;
  application/lzop) atool --list -- "${file}" ;;
  application/octet-stream) xxd "$file" ;;
  application/pdf) pdftotext "${file}" /tmp/pdftotext.txt ;;
  application/pgp-encrypted) gpg -d -- "${file}" ;;
  application/x-brotli) atool --list -- "${file}" ;;
  application/x-iso9660-image) ;;
  application/x-lzip) atool --list -- "${file}" ;;
  application/x-tar) atool --list -- "${file}" ;;
  application/zip) atool --list -- "${file}" ;;
  text/troff) man ./ "${file}" | col -b ;;
  text/html) html2text "${file}" ;;
  text/plain) cat "${file}" ;;
  text/*) bat --paging=never --line-range :50 "$file" ;;
  audio/*) mediainfo "$file" ;;
  image/*) mediainfo "$file" ;;
  video/*) mediainfo "$file" ;;
esac

if [ -n "$FIFO_UEBERZUG" ]; then
  case "$(file -Lb --mime-type -- "$file")" in
    application/pdf)
      cache="$(hash "$file").jpg"
      cache "$cache" "$@"
      gs -o "$cache" -sDEVICE=pngalpha -dLastPage=1 "$file" >/dev/null
      draw "$cache" "$@"
      ;;
    font/sfnt | application/octet-stream)
      PREVIEW_TEXT=${FONTPREVIEW_PREVIEW_TEXT:-"ABCDEFGHIJKLM\nNOPQRSTUVWXYZ\nabcdefghijklm\nnopqrstuvwxyz\n1234567890\n!@#$\%^&*,.;:\n_-=+'\"|\\(){}[]"}
      TEXT_ALIGN=${FONTPREVIEW_TEXT_ALIGN:-center}
      [ "$TEXT_ALIGN" = center ] || [ "$TEXT_ALIGN" = south ] || [ "$TEXT_ALIGN" = north ] || PADDING=50
      cache="$(hash "$file").jpg"
      cache "$cache" "$@"
      convert -size "800x800" xc:"#ffffff" -fill "#000000" \
      -pointsize "72" -font "$file" -gravity "$TEXT_ALIGN" \
      -annotate +${PADDING:-0}+0 "$PREVIEW_TEXT" "$cache"
      draw "$cache" "$@" ;;
    image/*)
      orientation="$(identify -format '%[EXIF:Orientation]\n' -- "$file")"
      if [ -n "$orientation" ] && [ "$orientation" != 1 ]; then
        cache="$(hash "$file").jpg"
        cache "$cache" "$@"
        convert -- "$file" -auto-orient "$cache"
        draw "$cache" "$@"
      else
        draw "$file" "$@"
      fi
      ;;
    # video/*)
      # cache="$(hash "$file").jpg"
      # cache "$cache" "$@"
      # ffmpegthumbnailer -i "$file" -o "$cache" -s 0
      # draw "$cache" "$@"
      # ;;
    text/html)
      cache="$(hash "$file").jpg"
      cache "$cache" "$@"
      wkhtmltoimage --quality 70 -f jpg --height 1000 "${file}" "$cache"
      draw "$cache" "$@"
      ;;
  esac
fi

file -Lb -- "$1" | fold -s -w "$width"
exit 0
