#!/bin/bash

# Generate pdfs from tex
latexmk -C gallery
latexmk --shell-escape --lualatex gallery

# Convert to svgs
for f in tikz/*.pdf; do inkscape --without-gui --file=$f --export-plain-svg="${f%-0.pdf}.svg" --export-text-to-path --pdf-poppler; done

# Clean up
latexmk -C gallery
