# fod_rugby

Repository for the female university rugby CMJ/IMTP monitoring analysis and manuscript package.

## Current layout

- `fod_rugby_analysis.Rmd` — exploratory / analysis notebook kept at repo root so its existing relative data path (`data/CMJ_IMTP_RUGBY.xlsx`) still works.
- `publication_jscr_ready/` — manuscript-ready package.
  - `manuscript_jscr_ready.Rmd` — main manuscript source.
  - `render_manuscript.R` — reproducible DOCX render entry point.
  - `README_notes_and_blockers.txt` — notes on manuscript status and remaining author inputs.
  - `artifacts/` — generated supporting artifacts such as the `sjPlot::tab_model()` HTML export.
  - `derived/` — rendered manuscript outputs and draft exports.
- `Figures/` — retained figure assets already tracked in Git.
- `reports/exploratory/` — generated exploratory HTML output from `fod_rugby_analysis.Rmd`.
- `data/` — private local workbook input (ignored by Git).

## Reproducible manuscript render

From the repository root:

```sh
cd publication_jscr_ready
Rscript render_manuscript.R
```

Expected output:

- `publication_jscr_ready/derived/manuscript_jscr_ready.docx`
- `publication_jscr_ready/artifacts/mixed_models_tab_model.html`

## Notes

- The workbook in `data/` contains athlete identifiers and is intentionally ignored.
- The older NSCA poster scripts are left in place because they use historical absolute paths and were not part of the active JSCR manuscript render path.
- Generated previews, HTML exports, and other bulky derived files are kept out of Git unless there is a specific reason to version them.
