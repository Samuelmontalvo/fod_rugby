args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) args[1] else getwd()
setwd(out_dir)
dir.create("derived", showWarnings = FALSE, recursive = TRUE)

rmarkdown::render(
  input = "manuscript_jscr_ready.Rmd",
  output_file = file.path("derived", "manuscript_jscr_ready.docx"),
  params = list(repo_root = ".."),
  envir = new.env(parent = globalenv())
)
