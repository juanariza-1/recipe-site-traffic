# Run from the repository root: Rscript scripts/render-report.R
packages <- c("tidyverse", "GGally", "ggplot2", "tidymodels", "knitr", "kableExtra", "broom", "yardstick", "pROC", "marginaleffects", "stargazer", "mfx", "ranger", "vip", "dplyr", "tibble", "scales", "rmarkdown")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       ". Run Rscript scripts/install-packages.R first.")
}
if (packageVersion("vip") >= package_version("0.5.0")) {
  stop("This report requires vip's 0.4 plotting interface. Run Rscript scripts/install-packages.R to install the compatible release.")
}
if (!rmarkdown::pandoc_available()) {
  stop("Pandoc is required. Use RStudio's bundled Pandoc or install Pandoc and add it to PATH.")
}
dir.create("build", showWarnings = FALSE)
knitr::opts_chunk$set(cache.rebuild = TRUE)
rmarkdown::render(
  "analysis.Rmd",
  output_format = "html_document",
  output_file = "report.html",
  output_dir = "build",
  envir = new.env(parent = globalenv()),
  clean = TRUE
)
