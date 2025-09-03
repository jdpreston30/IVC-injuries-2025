#* 0: Setup
#+ 0.0: Call all utility functions
  purrr::walk(
    list.files(
      here::here("R", "Utilities"),
      pattern = "\\.[rR]$",
      full.names = TRUE,
      recursive = TRUE
    ),
    source
  )
#+ 0.1: Install all dependencies if missing
  #- 0.1.1: CRAN packages vector
    packages <- c(
      "dplyr", "tidyr", "stringr", "readxl", "labelled", "ggplot2",
      "flextable", "officer", "tibble", "forcats", "stats", "purrr",
      "broom", "table1", "readr", "epitools", "openxlsx", "remotes"
    )
  #- 0.1.2: Install any missing packages (CRAN)
    for (pkg in packages) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg)
      }
    }
  #- 0.1.3: Install any missing packages (GitHub)
    if (!requireNamespace("TernTablesR", quietly = TRUE)) {
      remotes::install_github("jdpreston30/TernTablesR")
    }
#+ 0.2: Load Packages
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readxl)
  library(labelled)
  library(ggplot2)
  library(flextable)
  library(officer)
  library(tibble)
  library(forcats)
  library(stats)
  library(purrr)
  library(broom)
  library(table1)
  library(readr)
  library(epitools)
  library(openxlsx)
  library(TernTablesR)
#+ 0.3: Import data
  final <- dynamic_import(raw_path)