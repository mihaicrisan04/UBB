# PBT - LaTeX project

## Setup

```bash
brew install --cask mactex
```

## Building the PDF

```bash
# build (handles pdflatex + bibtex passes automatically)
latexmk

# watch mode — recompiles on file changes
latexmk -pvc

# clean aux files
latexmk -c
```

Output: `build/main.pdf`

## Project structure

```
main.tex              # entry point
style.sty             # custom style package
references.bib        # bibliography
chapters/             # chapter .tex files
figures/              # images and figures
build/                # output (generated)
.latexmkrc            # latexmk config
```
