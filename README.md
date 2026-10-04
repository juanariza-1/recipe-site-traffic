# Predicting High Recipe Site Traffic

**[View the rendered report online](https://juanariza-1.github.io/recipe-site-traffic/)**

An academic classification project by **Juan Esteban Londoño, Juan Pablo Ariza, and Milan Goossens**. It examines whether recipe nutrition, category, and serving size help predict high website traffic, comparing linear probability models, logistic regression, and a tuned random forest.

The final report includes data preparation, exploratory graphics, a stratified train/test split, 10-fold cross-validation, model estimates, marginal effects, and out-of-sample metrics. Model specifications, random seeds, data handling, collaborators, and results are preserved; the submission's placeholder title has been replaced with a descriptive title.

## Read the report

Open [the report overview](docs/index.html) or [the final HTML report](docs/report.html) in a browser after cloning or downloading the repository. Plots and styles are embedded in the report; viewing it does not require R. The source and saved report now share a descriptive title. Only the saved export's browser title and displayed heading were updated; its author, date, analysis, figures, tables, and numbers are unchanged. The accompanying plot files are available in `docs/Predicting-high-site-traffic_files/`.

## Repository contents

| Path | Contents |
| --- | --- |
| `analysis.Rmd` | Source of the final written report |
| `data/recipe_site_traffic_2212.csv` | Original input dataset |
| `docs/report.html` | Final HTML export with corrected title |
| `docs/Predicting-high-site-traffic_files/` | Original plot exports |
| `styles.css` | Report presentation styles |
| `archive/earlier-analysis.R` | Earlier standalone exploratory version |
| `archive/later-analysis.R` | Later standalone version with model comparison |
| `scripts/install-packages.R` | Explicit R package setup |
| `scripts/render-report.R` | Rebuild the HTML report into `build/` |

The two archived scripts are independent working versions. Their model objects, plotting code, and final comparison section differ from the R Markdown report; they are retained for context and should not be treated as interchangeable reproductions of that report. Package installation is separated from analysis execution, and input paths are relative to the repository root.

## Reproduce the report

Use R and Pandoc. RStudio includes a compatible Pandoc installation. From the repository root, run:

```sh
Rscript scripts/install-packages.R
Rscript scripts/render-report.R
```

The setup script lists all directly used R packages, including `tidyverse`, `GGally`, `tidymodels`, `kableExtra`, `pROC`, `marginaleffects`, `stargazer`, `mfx`, `ranger`, and `vip`. It installs missing current packages from CRAN and uses the compatible archived `vip` 0.4.5 release. The render command writes `build/report.html`, leaving the saved report intact. It forces cache rebuilding; the random forest cross-validation can take several minutes.

To run one of the independent exploratory versions after package setup:

```sh
Rscript archive/earlier-analysis.R
Rscript archive/later-analysis.R
```

Run these from the repository root so the `data/` path resolves. The R Markdown source also declares a PDF output, which additionally requires LaTeX; the original saved deliverable is HTML.

## Data handling

The CSV supplied with the project contains 947 recipes and eight columns: recipe ID, calories, carbohydrate, sugar, protein, category, servings, and high traffic. It contains no names, account information, or contact details. The supplied files do not include a separate data license or attribution document.

For consistency with the submitted analysis, missing traffic labels are treated as low traffic, and rows with incomplete predictors are removed. Numeric conversion of the two serving labels containing `as a snack` makes those rows incomplete; the final analysis uses 892 complete cases. This handling is retained so that publication does not silently change the fitted models or reported results.

## Environment and scope

The preparation environment provides R 4.5.1. No original dependency lockfile was supplied. The setup script describes package requirements without claiming a complete historical environment, and package changes can affect numerical results or random forest tuning. The available original `vip` installation is version 0.4.5, which returns the ggplot object used by the report. [The maintainer documents a breaking plotting change in `vip` 0.5](https://github.com/bgreenwell/vip), so this project installs 0.4.5 from the official CRAN archive rather than switching plot frameworks.

The saved report ends after the random forest variable importance table and an unfinished section marker. A comparative ROC/confusion matrix section exists in the later exploratory script, but is not presented as part of the final written report.

The complete report and both archived scripts completed full runs under the checked environment. Fresh uncached report renders before and after the path/title updates produced identical written results and numerical outputs. The pairwise exploratory plot uses unseeded jitter, so point placement can differ between runs; the saved report retains its original figure. All other regenerated report figures were identical between the two checked runs.
