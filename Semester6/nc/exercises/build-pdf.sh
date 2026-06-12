#!/usr/bin/env bash
# Render cheatsheet.html -> cheatsheet.pdf (2-page A4 landscape) via headless Chrome.
# Needs internet for the KaTeX CDN. Override the browser with CHROME=/path/to/chrome.
set -euo pipefail

cd "$(dirname "$0")"

CHROME="${CHROME:-/Applications/Google Chrome.app/Contents/MacOS/Google Chrome}"
if [ ! -x "$CHROME" ]; then
  echo "Chrome not found at: $CHROME" >&2
  echo "Set CHROME=/path/to/chrome and re-run." >&2
  exit 1
fi

"$CHROME" --headless --no-sandbox --disable-gpu --no-pdf-header-footer \
  --virtual-time-budget=20000 --run-all-compositor-stages-before-draw \
  --print-to-pdf="cheatsheet.pdf" "file://$(pwd)/cheatsheet.html"

echo "Wrote $(pwd)/cheatsheet.pdf"
command -v pdfinfo >/dev/null && pdfinfo cheatsheet.pdf | grep -E '^Pages'
