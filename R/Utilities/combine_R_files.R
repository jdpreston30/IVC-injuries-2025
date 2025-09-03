combine_R_files <- function(input_dir, output_file) {
  input_dir <- normalizePath(input_dir, mustWork = TRUE)

  # Find all .R or .r files recursively (case-insensitive)
  r_files <- list.files(
    input_dir,
    pattern = "(?i)\\.r$", full.names = TRUE, recursive = TRUE
  )

  if (length(r_files) == 0) {
    stop("❌ No .R or .r files found in ", input_dir)
  }

  # Natural sort (00, 01, 02 … instead of 1, 10, 2)
  if (!requireNamespace("gtools", quietly = TRUE)) {
    install.packages("gtools")
  }
  r_files <- gtools::mixedsort(r_files)

  # Get relative paths for cleaner headers
  rel_paths <- sub(paste0("^", input_dir, "/?"), "", r_files)

  # Read and combine with headers
  all_contents <- unlist(Map(function(f, rel) {
    c(
      paste0("# ===== File: ", rel, " ====="),
      readLines(f, warn = FALSE),
      ""
    )
  }, r_files, rel_paths))

  # Count total number of lines
  total_lines <- length(all_contents)

  # Ensure output directory exists
  out_dir <- dirname(output_file)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # Write out combined file
  writeLines(all_contents, output_file)
  message(
    "✅ Combined ", length(r_files), " R files into: ", normalizePath(output_file),
    "\n📏 Total lines in combined file: ", total_lines
  )
}