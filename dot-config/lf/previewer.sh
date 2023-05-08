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

text_previwer() {
  case "$(file -Lb --mime-type -- "$file")" in
    */xml) bat --color always --paging=never --line-range :50 "$file" ;;
    *opendocument*) odt2txt "${file}" ;;
    application/bzip2) atool --list -- "${file}" ;;
    application/gzip) atool --list -- "${file}" ;;
    application/json) bat --paging=never --line-range :50 "$file" ;;
    application/lzma) atool --list -- "${file}" ;;
    application/lzop) atool --list -- "${file}" ;;
    application/octet-stream) xxd "$file" ;;
    application/pdf) pdftotext -f 1 -l 5 -layout "${file}" /tmp/pdftotext.txt ;;
    # application/pdf) pdftotext "${file}" /tmp/pdftotext.txt ;;
    application/pgp-encrypted) gpg -d -- "${file}" ;;
    application/x-iso9660-image) { isoinfo -d -i "$FILE_PATH"; printf "\nContent:"; isoinfo -l -i "$FILE_PATH"; } | $PAGER ;;
    application/x-lzip) atool --list -- "${file}" ;;
    application/x-mobipocket-ebook) ebook-convert "$file" ;;
    application/x-object) nm "${FILE_PATH}" | $PAGER ;;
    application/x-brotli) atool --list -- "${file}" ;;
    application/x-tar) atool --list -- "${file}" ;;
    application/epub+zip) epub2txt "${file}" | head -n 60;;
    application/x-xz) atool --list -- "${file}";;
    application/zip) atool --list -- "${file}" ;;
    text/troff) man ./ "${file}" | col -b ;;
    text/html) html2text "${file}" ;;
    # text/plain) bat --paging=never --line-range :60 "$file" ;;
    # text/x-diff) ;;
    text/*) bat --color always --paging=never --line-range :60 "$file" ;;
    audio/*) mediainfo "$file" ;;
    image/*) mediainfo "$file" ;;
    video/*) mediainfo "$file" ;;
  esac
}

text_previwer &

if [ -n "$FIFO_UEBERZUG" ]; then
  case "$(file -Lb --mime-type -- "$file")" in
    application/pdf)
      cache="$(hash "$file").jpg"
      cache "$cache" "$@"
      gs -o "$cache" -sDEVICE=pngalpha -dLastPage=1 "$file" >/dev/null
      draw "$cache" "$@"
      ;;
    application/epub+zip|application/x-mobipocket-ebook)
      cache="$(hash "$file").png"
      cache "$cache" "$@"
      ebook-meta "$file" --get-cover="$cache"
      draw "$cache" "$@" ;;
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
    image/vnd.djvu)
      cache="$(hash "$file").jpg"
      cache "$cache" "$@"
      ddjvu -format=tiff -quality=90 -page=1 "$file" "$cache"
      draw "$file" "$@" ;;
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
      */epub+zip|*/mobi*)
        CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/lf/thumb.$(stat --printf '%n\0%i\0%F\0%s\0%W\0%Y' -- "$(readlink -f "$1")" | sha256sum | cut -d' ' -f1)"
        [ ! -f "$CACHE.jpg" ] && gnome-epub-thumbnailer "$1" "$CACHE.jpg"
        image "$CACHE.jpg" "$2" "$3" "$4" "$5" "$1"
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

wait
file -Lb -- "$1" | fold -s -w "$width"
exit 0
