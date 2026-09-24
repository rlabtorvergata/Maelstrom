options(repos = c(
  FLR = "https://flr.r-universe.dev",
  CRAN = "https://cloud.r-project.org"
))

if (getRversion() < "4.3.0") {
  stop("MAELSTROM v2.3 requires R >= 4.3.0.", call. = FALSE)
}

cran_packages <- c(
  "bslib", "cowplot", "data.table", "dplyr", "ggplot2", "keras3",
  "Metrics", "patchwork", "plotly", "plotrix", "reshape2", "rmarkdown",
  "shiny", "shinyBS", "shinycssloaders", "shinyFiles", "shinyWidgets",
  "tensorflow", "tidyr", "tinytex"
)

installed <- rownames(utils::installed.packages())
missing_cran <- setdiff(cran_packages, installed)
if (length(missing_cran)) utils::install.packages(missing_cran)

if (!requireNamespace("FLCore", quietly = TRUE)) {
  utils::install.packages("FLCore")
}

minimum_versions <- c(shiny = "1.8.1", bslib = "0.8.0", keras3 = "1.0.0")
outdated <- names(minimum_versions)[vapply(names(minimum_versions), function(package) {
  utils::packageVersion(package) < numeric_version(minimum_versions[[package]])
}, logical(1))]
if (length(outdated)) utils::install.packages(outdated)

if (identical(Sys.getenv("MAELSTROM_INSTALL_KERAS"), "true")) {
  keras3::install_keras(backend = "tensorflow", restart_session = FALSE)
}

message("Dependencies are ready. Restart R, then run shiny::runApp().")
