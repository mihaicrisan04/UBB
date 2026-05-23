"""Convert all lecture pptx/pdf files to markdown using docling."""

from pathlib import Path

from docling.document_converter import DocumentConverter

LECTURES_DIR = Path("/Users/mihai/dev/UBB/Semester6/cvdl/lectures")
OUT_DIR = LECTURES_DIR / "md"
OUT_DIR.mkdir(exist_ok=True)

# Skip the partial "5_1" version (we have "5 full") and skip large unrelated files.
SKIP = {
    "Computer Vision and Deep Learning - Lecture 5_1.pptx",
    "hai_ai_index_report_2025.pdf",
    "Stanford AI Index 2025.pptx",
    "VAZ2_1.PDF",
}

converter = DocumentConverter()

inputs = sorted(
    p for p in LECTURES_DIR.iterdir()
    if p.is_file()
    and p.suffix.lower() in {".pptx", ".pdf"}
    and p.name not in SKIP
)

print(f"Found {len(inputs)} lectures to convert")
for path in inputs:
    out = OUT_DIR / (path.stem + ".md")
    if out.exists() and out.stat().st_size > 0:
        print(f"SKIP (exists): {out.name}")
        continue
    print(f"Converting: {path.name}")
    try:
        result = converter.convert(str(path))
        md = result.document.export_to_markdown()
        out.write_text(md)
        print(f"  -> wrote {out.name} ({len(md)} chars)")
    except Exception as e:
        print(f"  ERROR: {e}")

print("Done.")
