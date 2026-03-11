# AGENTS Guide

## Project Overview
- This repository uses Python, not MATLAB, for Numerical Calculus labs.
- Even if lab PDFs mention MATLAB, all implementations here are done in Python notebooks (`.ipynb`).
- Lab notebooks live under course folders such as `lab01/`.

## Environment and Dependency Management
- The project environment is managed with `uv`.
- For the currently approved dependencies and Python version, check `pyproject.toml`.
- Do not add dependencies manually outside of `uv`.
- When adding dependencies, use `uv` so lock/environment state stays consistent.

## Allowed Command Policy
- Prefer `uv`-based commands for Python work.
- Allowed and recommended commands include:
  - `uv sync` (install/update environment from project metadata)
  - `uv add <package>` (add new dependency)
  - `uv remove <package>` (remove dependency)
  - `uv run <command>` (run tools/scripts in project environment)
  - `uv run jupyter lab` or `uv run jupyter notebook` (notebook workflows)
- One-off scripts should be run with `uv run ...` to ensure correct environment resolution.

## Notebook Workflow Conventions
- Solve exercises in notebooks inside the corresponding lab directory.
- Use clear sectioning per exercise; multiple notebooks are acceptable when cleaner.
- Keep code reproducible and runnable top-to-bottom.
- Prefer `numpy`, `scipy`, and `matplotlib` for numerical/plot tasks.
- In notebook Markdown, write math with `$...$` (inline) and `$$...$$` (block).
- Do not use `\(...\)` or `\[...\]` for formulas in this project.

## Difference Table Presentation
- For divided-difference and finite-difference exercises, it is important to present unused triangular table entries as `0` in the notebook output when a full rectangular table is expected.
- It is acceptable to keep internal computations as `NaN` to preserve the triangular structure, but displayed results for this kind of exercise should show `0`.

## MATLAB PDF Mismatch Rule
- Treat MATLAB instructions in PDFs as conceptual guidance only.
- Translate all tasks to Python idioms and notebook-based solutions.

## Code Style and Quality
- Write clean, readable, maintainable code.
- Prefer clear naming, small focused functions, and straightforward control flow.
- Avoid unnecessary complexity and avoid "clever" shortcuts that hurt readability.
- Comments should be minimal: do not add filler comments or obvious comments.
- Add comments only when strictly necessary to explain non-obvious logic, assumptions, or trade-offs.
