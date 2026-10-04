# Run explicitly from the repository root: Rscript scripts/install-packages.R
packages <- c("tidyverse", "GGally", "ggplot2", "tidymodels", "knitr", "kableExtra", "broom", "yardstick", "pROC", "marginaleffects", "stargazer", "mfx", "ranger", "dplyr", "tibble", "scales", "rmarkdown", "foreach")
missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing)) {
  install.packages(missing, repos = c(CRAN = "https://cloud.r-project.org"))
} else {
  message("All current CRAN packages are already installed.")
}

# vip 0.4.5 uses the ggplot2 interface required by the original report.
# The later 0.5 series changes that interface; retain the compatible release.
if (!requireNamespace("vip", quietly = TRUE) ||
    packageVersion("vip") != package_version("0.4.5")) {
  install.packages(
    "https://cran.r-project.org/src/contrib/Archive/vip/vip_0.4.5.tar.gz",
    repos = NULL,
    type = "source"
  )
}
