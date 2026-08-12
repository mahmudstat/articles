#!/usr/bin/env bash
# Build both files JSDSE requires: an anonymized version for double-anonymous
# review, and a full version with author details.
#   \anon = 0  -> anonymized      -> moment-anonymous.pdf
#   \anon = 1  -> author details  -> moment-full.pdf
set -euo pipefail
cd "$(dirname "$0")"

build () {  # $1 = anon value, $2 = output basename
  sed "s/^\\\\newcommand{\\\\anon}{[01]}$/\\\\newcommand{\\\\anon}{$1}/" moment.tex > "$2.tex"
  for pass in 1 2 3; do
    pdflatex -interaction=nonstopmode "$2.tex" >/dev/null 2>&1 || true
    [ $pass -eq 1 ] && { bibtex "$2" >/dev/null 2>&1 || true; }
  done
  if grep -qE '^!' "$2.log"; then echo "ERROR in $2 -- see $2.log"; grep -E '^!' "$2.log" | head; exit 1; fi
  rm -f "$2.tex" "$2.aux" "$2.log" "$2.blg" "$2.bbl" "$2.out"
}

build 0 moment-anonymous
build 1 moment-full

echo "built:"
for f in moment-anonymous moment-full; do
  printf "  %-22s %s pages\n" "$f.pdf" "$(pdfinfo $f.pdf | awk '/^Pages/{print $2}')"
done

echo
echo "leak check on the ANONYMOUS file (must report 0):"
hits=$(pdftotext moment-anonymous.pdf - | grep -icE "mahmud|statmania|bishwo|thinker|mymensingh|dhaka" || true)
meta=$(pdfinfo moment-anonymous.pdf | awk -F: '/^Author/{gsub(/ /,"",$2); print $2}')
echo "  identifying strings in text : $hits"
echo "  PDF Author metadata         : '${meta:-}'"
[ "$hits" = "0" ] && [ -z "${meta:-}" ] && echo "  OK" || { echo "  FAILED"; exit 1; }
