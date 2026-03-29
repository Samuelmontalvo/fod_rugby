JSCR-ready manuscript package (updated 2026-03-28)

Primary files:
- manuscript_jscr_ready.Rmd — reproducible manuscript source for DOCX render
- render_manuscript.R — one-command render script
- derived/manuscript_jscr_ready.docx — rendered Word manuscript
- artifacts/mixed_models_tab_model.html — full sjPlot::tab_model() mixed-model table artifact
- derived/manuscript_jscr_draft.md — earlier hand-written draft retained for reference
- derived/manuscript_jscr_draft.docx — earlier compiled draft retained for reference

What changed in this revision:
1. Added an internal manuscript render pipeline under publication_jscr_ready.
2. Rebuilt Table 1 with gtsummary::tbl_summary() and rendered it to Word via as_flex_table().
3. Fit/reused AR(1) mixed models directly from the rugby workbook and generated mixed-model tables with sjPlot::tab_model().
4. Preserved the full tab_model output as an HTML artifact because sjPlot tables are HTML-native and do not embed cleanly into DOCX by default.
5. Added a DOCX-friendly mixed-model summary table (week-effect p-values by outcome) plus two figures generated during render.
6. Moved rendered manuscript outputs and older draft exports into `derived/` to keep the package tidy while preserving the active render path.

Render command:
- From publication_jscr_ready/: Rscript render_manuscript.R

Known blockers / limitations:
1. The repository still does not supply final author list, affiliations, IRB/ethics wording, consent text, equipment/setup details, or reference list.
2. The Word manuscript embeds gtsummary/flextable tables cleanly, but the full sjPlot coefficient tables are preserved as HTML rather than natively embedded in DOCX.
3. If the final submission needs complete coefficient tables inside Word, a secondary conversion step (for example, a custom model-summary-to-flextable export) will still be needed.
4. Pandoc issued only minor warnings that fig.align is ignored for DOCX output; render otherwise completed successfully.
