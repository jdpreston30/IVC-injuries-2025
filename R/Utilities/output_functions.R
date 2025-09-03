    output_dir <- "Outputs/"
    output_csv <- function(data, filename) {
      write_csv(data, file = file.path(output_dir, filename))
    }
    output_xlsx <- function(data, filename) {
      write_xlsx(data, path = file.path(output_dir, filename))
    }