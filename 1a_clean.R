# Legacy wrapper retained for convenience.
# It resolves the project root from this file, so it can be run from anywhere.
find_this_file <- function() {
  decode_rscript_path <- function(path) {
    gsub("~\\+~", " ", path)
  }

  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(
      decode_rscript_path(sub("^--file=", "", file_arg[[1]])),
      winslash = "/",
      mustWork = TRUE
    ))
  }

  for (frame in rev(sys.frames())) {
    if (!is.null(frame$ofile)) {
      return(normalizePath(frame$ofile, winslash = "/", mustWork = TRUE))
    }
  }

  NULL
}

wrapper_path <- find_this_file()
project_root <- if (!is.null(wrapper_path)) dirname(wrapper_path) else normalizePath(getwd(), winslash = "/", mustWork = TRUE)
source(file.path(project_root, "scripts", "update_trade_data.R"))
