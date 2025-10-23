#* 0b: Setup and Import
#+ 0b.1: Set up R options and repositories
options(repos = c(CRAN = "https://cran.rstudio.com/"))
options(expressions = 10000)
#+ 0b.2: Load utility functions first (needed for dynamic config)
utils_path <- "R/Utilities/"
if (dir.exists(utils_path)) {
  purrr::walk(
    list.files(utils_path, pattern = "\\.[rR]$", full.names = TRUE, recursive = TRUE),
    source
  )
  cat("🔧 Loaded utility functions\n")
}
#+ 0b.3: Validate config object exists (should be loaded in run.R)
if (!exists("config")) {
  stop("Config object not found. Make sure to run load_dynamic_config() first in run.R")
}
.GlobalEnv$CONFIG <- config
#+ 0b.4: Set up global paths from config
output_path <- config$paths$output
scripts_path <- config$paths$scripts
#+ 0b.5: Create output directory if it doesn't exist
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
  cat("📁 Created output directory:", output_path, "\n")
}
#+ 0b.6: Import IVC data using dynamic path resolution
final <- readxl::read_excel(config$paths$GCMS_data, sheet = "Final")
cat("📊 Imported IVC data with", nrow(final), "rows and", ncol(final), "columns\n")
#+ 0b.7: Setup complete
cat("✅ Setup and import complete!\n")