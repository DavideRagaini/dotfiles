#!/bin/sh

batorcat() {
  file="$1"
  shift
  if command -v bat >/dev/null 2>&1; then
    bat --color=always --style=plain --pager=never --line-range :60 "$file" "$@"
  else
    head -n60 "$file"
  fi
}

case "$(file -Lb --mime-type -- "$1")" in
  image/*)
    chafa -f sixel -s "$2x$3" --animate off --polite on "$1"
    exit 1
    ;;
  *)
    batorcat "$1"
    ;;
  esac
