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