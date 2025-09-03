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