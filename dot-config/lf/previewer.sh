#!/bin/sh

image() {
  FILE_PATH="$1"
  X=$4
  Y=$5
  MW=$(($2 - 1))
  MH=$3
  ueberzugpp cmd -s "$UB_SOCKET" -a add -i PREVIEW -x "$X" -y "$Y" --max-width "$MW" --max-height "$MH" -f "$FILE_PATH"
  exit 1
}

batorcat() {
  file="$1"
  shift
  if command -v bat >/dev/null 2>&1; then
    bat --color=always --style=plain --pager=never --line-range :60 "$file" "$@"
  else
    head -n60 "$file"
  fi
}

CACHE="$HOME/.cache/lf/$(stat --printf '%n\0%i\0%F\0%s\0%W\0%Y' -- "$(readlink -f "$1")" | sha256sum | awk '{print $1}'))"

case "$(printf "%s\n" "$(readlink -f "$1")" | tr '[:upper:]' '[:lower:]')" in
  *.gpg | *.pgp) gpg -d -- "${1}" ;;
  *.epub | *.mobi | *.azw | *.azw3)
    [ -f "$CACHE.jpg" ] ||
      gnome-epub-thumbnailer "$1" "${CACHE}.jpg"
    image "$CACHE.jpg" "$2" "$3" "$4" "$5" "$1"
    ;;
  *.ttf | *.otf)
    PREVIEW_TEXT=${FONTPREVIEW_PREVIEW_TEXT:-"ABCDEFGHIJKLM\nNOPQRSTUVWXYZ\nabcdefghijklm\nnopqrstuvwxyz\n1234567890\n!@#$\%^&*,.;:\n_-=+'\"|\\(){}[]"}
    TEXT_ALIGN=${FONTPREVIEW_TEXT_ALIGN:-center}
    [ "$TEXT_ALIGN" = center ] ||
      [ "$TEXT_ALIGN" = south ] ||
      [ "$TEXT_ALIGN" = north ] ||
      PADDING=50
    convert -size "800x800" xc:"#ffffff" -fill "#000000" \
      -pointsize "72" -font "$1" -gravity "$TEXT_ALIGN" \
      -annotate +${PADDING:-0}+0 "$PREVIEW_TEXT" "${CACHE}.jpg"
    image "${CACHE}.jpg" "$2" "$3" "$4" "$5"
    ;;
  *.djvu)
    ddjvu -format=tiff -quality=80 -page=1 "$1" "${CACHE}.jpg"
    image "${CACHE}.jpg" "$2" "$3" "$4" "$5"
    ;;
  *.troff) man ./ "$1" | col -b ;;
  *.html) html2text "$1" ;;
  *.tgz | *.tar.gz) tar tzf "$1" ;;
  *.tar.bz2 | *.tbz2) tar tjf "$1" ;;
  *.tar.txz | *.txz) xz --list "$1" ;;
  *.tar) tar tf "$1" ;;
  *.zip | *.jar | *.war | *.ear | *.oxt) unzip -l "$1" ;;
  *.rar) unrar l "$1" ;;
  *.7z) 7z l "$1" ;;
  *.[1-8]) man "$1" | col -b ;;
  *.o) nm "$1" ;;
  *.torrent) transmission-show "$1" ;;
  *.iso) iso-info --no-header -l "$1" ;;
  *.odt | *.ods | *.odp | *.sxw) odt2txt "$1" ;;
  *.doc) catdoc "$1" ;;
  *.docx) docx2txt "$1" - ;;
  *.xls | *.xlsx)
    ssconvert --export-type=Gnumeric_stf:stf_csv "$1" "fd://1" |
      batorcat --language=csv
    ;;
  *.wav | *.mp3 | *.flac | *.m4a | *.wma | *.ape | *.ac3 | *.og[agx] | *.spx | *.opus | *.as[fx] | *.mka)
    mediainfo "$1" &
    exiftool "$1"
    ;;
  *.pdf)
    pdftotext "${1}" /tmp/pdftotext.txt &
    [ -f "${CACHE}.jpg" ] ||
      pdftoppm -jpeg -f 1 -singlefile "$1" "$CACHE"
    image "${CACHE}.jpg" "$2" "$3" "$4" "$5"
    ;;
  *.avi | *.mp4 | *.wmv | *.dat | *.3gp | *.ogv | *.mkv | *.mpg | *.mpeg | *.vob | *.fl[icv] | *.m2v | *.mov | *.webm | *.ts | *.mts | *.m4v | *.r[am] | *.qt | *.divx)
    mediainfo "$1" &
    [ -f "${CACHE}.jpg" ] ||
      ffmpegthumbnailer -i "$1" -o "${CACHE}.jpg" -s 0 -q 5 -t 00:00:10
    image "${CACHE}.jpg" "$2" "$3" "$4" "$5"
    ;;
  *.bmp | *.jpg | *.jpeg | *.png | *.xpm | *.webp | *.gif | *.jfif)
    mediainfo "$1" &
    image "$1" "$2" "$3" "$4" "$5"
    ;;
  *.svg)
    mediainfo "$1" &
    [ -f "${CACHE}.jpg" ] ||
      convert "$1" "${CACHE}.jpg"
    image "${CACHE}.jpg" "$2" "$3" "$4" "$5"
    ;;
  *) batorcat "$1" ;;
esac
exit 0
