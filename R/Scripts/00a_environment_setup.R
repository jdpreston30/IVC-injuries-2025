#* 0a: Environment Setup
#+ 0a.1: Read required packages from DESCRIPTION file 
desc_file <- "DESCRIPTION.txt"
if (!file.exists(desc_file)) {
  stop("DESCRIPTION.txt file not found. Please ensure you're in the project root directory.")
}
#- 0a.1.1: Read DESCRIPTION file 
desc_lines <- readLines(desc_file)
#- 0a.1.2: Extract Imports section 
imports_start <- which(grepl("^Imports:", desc_lines))
if (length(imports_start) == 0) {
  stop("No Imports section found in DESCRIPTION.txt file.")
}
#- 0a.1.3: Find where Imports section ends (next field or end of file) 
next_field <- which(grepl("^[A-Z]", desc_lines[(imports_start + 1):length(desc_lines)]))
if (length(next_field) > 0) {
  imports_end <- imports_start + next_field[1] - 1
} else {
  imports_end <- length(desc_lines)
}
#- 0a.1.4: Extract package names 
imports_lines <- desc_lines[imports_start:imports_end]
imports_text <- paste(imports_lines, collapse = " ")
imports_text <- gsub("Imports:", "", imports_text)
imports_text <- gsub("\\s+", " ", imports_text)
packages <- strsplit(imports_text, ",")[[1]]
packages <- trimws(packages)
packages <- packages[packages != ""]
cat("📋 Found", length(packages), "packages in DESCRIPTION.txt file\n")
#+ 0a.2: Install missing CRAN packages 
#- 0a.2.1: Check for missing packages 
missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) > 0) {
  cat("📦 Installing missing CRAN packages:", paste(missing_packages, collapse = ", "), "\n")
  for (pkg in missing_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cran.rstudio.com/")
    }
  }
} else {
  cat("✅ All CRAN packages already installed\n")
}
#+ 0a.3: Install TernTablesR from GitHub if missing
#- 0a.3.1: Check if TernTablesR is installed
if (!requireNamespace("TernTablesR", quietly = TRUE)) {
  cat("📦 Installing TernTablesR from GitHub: jdpreston30/TernTablesR\n")
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  remotes::install_github("jdpreston30/TernTablesR")
} else {
  cat("✅ TernTablesR already installed\n")
}
#+ 0a.4: Load all required packages 
cat("📚 Loading required packages...\n")
invisible(sapply(packages, library, character.only = TRUE, quietly = TRUE)) 
#+ 0a.5: Environment setup complete 
cat("✅ Environment setup complete! All required packages loaded.\n")