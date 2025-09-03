# ===== File: combine_R_files.R =====
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

# ===== File: dynamic_import.R =====
#' Import Excel file with dynamic defaults
#'
#' @param raw_path Optional path to the Excel file. If NULL, falls back to laptop/desktop defaults.
#' @param sheet Name of the Excel sheet to read (default = "Final").
#'
#' @return A tibble containing the imported sheet.
#' @export
import_excel <- function(raw_path = NULL, sheet = "Final") {
  # Defaults
  laptop_default <- "/Users/jdp2019/Library/CloudStorage/OneDrive-Emory/Research/Manuscripts and Projects/Grady/IVC/raw_data/IVC_JDP.xlsx"
  desktop_default <- "/Users/JoshsMacbook2015/Library/CloudStorage/OneDrive-EmoryUniversity/Research/Manuscripts and Projects/Grady/IVC/raw_data/IVC_JDP.xlsx"

  # Resolve path
  if (is.null(raw_path)) {
    if (file.exists(laptop_default)) {
      raw_path <- laptop_default
    } else if (file.exists(desktop_default)) {
      raw_path <- desktop_default
    } else {
      stop("Could not find Excel file at either default location. Please pass raw_path.")
    }
  }

  # Validate path
  if (!file.exists(raw_path)) {
    stop("Excel file not found at: ", raw_path)
  }

  # Import
  message("Reading Excel from: ", raw_path)
  df <- readxl::read_excel(raw_path, sheet = sheet)

  # Sanity check
  message("Imported '", sheet, "' with ", nrow(df), " rows and ", ncol(df), " columns.")

  return(df)
}

# ===== File: my_render_cat.R =====
my_render_cat <- function(x, ...) {
  c("", sapply(stats.default(x, ...), function(y) {
    with(y, sprintf("%d (%s%%)", FREQ, ifelse(round(PCT / 0.5) * 0.5 %% 1 == 0,
      as.integer(round(PCT / 0.5) * 0.5),
      round(PCT / 0.5) * 0.5
    ))) # Round percentages to integers
  }))
}

# ===== File: run_scripts.R =====
# Run a range/list of numbered scripts in R/Scripts (after sourcing R/Utilities)
# spec examples:
#   "all"                  -> run everything
#   "00-06"                -> run 00 through 06
#   "00,02,05"             -> run exactly 00, 02, 05
#   "00-02,05,07-08"       -> combos ok, any order
run_scripts <- function(
    spec = "all",
    raw_path = NULL,
    scripts_dir = here::here("R", "Scripts"),
    utils_dir = here::here("R", "Utilities")) {
  # 1) Source utilities (quietly skip if folder missing)
  if (dir.exists(utils_dir)) {
    util_files <- list.files(utils_dir, pattern = "\\.[rR]$", full.names = TRUE, recursive = TRUE)
    if (length(util_files)) {
      purrr::walk(util_files, source)
    }
  }

  # 2) Discover scripts and sort by their "NN" prefix
  if (!dir.exists(scripts_dir)) stop("Scripts dir not found: ", scripts_dir)
  scripts <- list.files(scripts_dir, pattern = "\\.[rR]$", full.names = TRUE)
  if (!length(scripts)) stop("No .R scripts found in: ", scripts_dir)

  # Mixed sort if available; otherwise base sort
  if (requireNamespace("gtools", quietly = TRUE)) {
    scripts <- gtools::mixedsort(scripts)
  } else {
    scripts <- sort(scripts)
  }

  # Extract 2-char numeric prefix (e.g., "00", "06") from filenames
  ids <- substr(basename(scripts), 1, 2)
  if (!all(grepl("^\\d\\d$", ids))) {
    warning("Some scripts do not start with a two-digit ID; they will still run but ordering may be unexpected.")
  }

  # 3) Parse spec into indices to run
  parse_spec <- function(spec, ids) {
    spec <- trimws(spec %||% "all")
    if (tolower(spec) == "all") {
      return(seq_along(ids))
    }

    parts <- unlist(strsplit(spec, ","))
    idx <- integer(0)
    for (p in parts) {
      p <- trimws(p)
      if (grepl("-", p)) {
        ab <- unlist(strsplit(p, "-"))
        if (length(ab) != 2) stop("Invalid range piece: ", p)
        a <- which(ids == trimws(ab[1]))
        b <- which(ids == trimws(ab[2]))
        if (!length(a) || !length(b)) stop("Range not found in script IDs: ", p)
        # allow reversed "06-00"
        rng <- if (a <= b) seq(a, b) else seq(b, a)
        idx <- c(idx, rng)
      } else {
        k <- which(ids == p)
        if (!length(k)) stop("Script ID not found: ", p, " (available: ", paste(ids, collapse = ", "), ")")
        idx <- c(idx, k)
      }
    }
    unique(sort(idx))
  }

  to_run <- parse_spec(spec, ids)
  if (!length(to_run)) stop("Nothing to run for spec: ", spec)

  # 4) Make `raw_path` visible to scripts (if provided)
  if (!is.null(raw_path)) assign("raw_path", raw_path, envir = .GlobalEnv)

  # 5) Run the chosen scripts
  for (i in to_run) {
    s <- scripts[i]
    message(">>> Running: ", basename(s))
    tryCatch(
      {
        source(s, local = .GlobalEnv, echo = TRUE, keep.source = TRUE)
      },
      error = function(e) {
        message("❌ Error in ", basename(s), ": ", e$message)
        stop(e)
      }
    )
  }

  message("✅ Finished running scripts: ", spec)
}

# Helper: null-coalescing for base R
`%||%` <- function(a, b) if (!is.null(a)) a else b

