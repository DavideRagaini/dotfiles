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
  audio/* | application/octet-stream) mediainfo "$file" ;;
  application/pgp-encrypted) gpg -d -- "${file}" ;;
  application/zip) atool --list -- "${file}" ;;
  # *opendocument*) odt2txt "${file}" ;;
  text/troff) man ./ "${file}" | col -b ;;
  # text/html) lynx -display_charset=utf-8 -dump "${file}" ;;
  text/* | */xml | application/json) bat --paging=never --line-range :50 "$file" ;;
  video/*) mediainfo "$file" ;;
  # image/*) mediainfo "$file" ;;
esac

if [ -n "$FIFO_UEBERZUG" ]; then
  case "$(file -Lb --mime-type -- "$file")" in
    application/pdf)
      cache="$(hash "$file").jpg"
      cache "$cache" "$@"
      gs -o "$cache" -sDEVICE=pngalpha -dLastPage=1 "$file" >/dev/null
      draw "$cache" "$@"
      ;;
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
  esac
fi

file -Lb -- "$1" | fold -s -w "$width"
exit 0
