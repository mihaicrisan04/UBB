"""Fallback PDF→text extraction for the two PDFs docling failed on (MPS float64 issue).

Uses pypdfium2 (already installed by docling) which doesn't need any ML model.
"""

from pathlib import Path

import pypdfium2 as pdfium

LECTURES_DIR = Path("/Users/mihai/dev/UBB/Semester6/cvdl/lectures")
OUT_DIR = LECTURES_DIR / "md"

targets = [
    LECTURES_DIR / "Computer vision and deep learning - lecture 2.pdf",
    LECTURES_DIR / "EfficientNetCVPresentation.pdf",
]

for path in targets:
    if not path.exists():
        print(f"MISSING: {path}")
        continue
    out = OUT_DIR / (path.stem + ".md")
    print(f"Extracting: {path.name}")
    pdf = pdfium.PdfDocument(str(path))
    chunks = []
    for i, page in enumerate(pdf):
        text_page = page.get_textpage()
        text = text_page.get_text_bounded() or ""
        chunks.append(f"\n\n## Page {i + 1}\n\n{text.strip()}")
        text_page.close()
        page.close()
    pdf.close()
    md = f"# {path.stem}\n" + "".join(chunks)
    out.write_text(md)
    print(f"  -> wrote {out.name} ({len(md)} chars)")

print("Done.")
