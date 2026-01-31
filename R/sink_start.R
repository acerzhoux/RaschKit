#' sink_start
#'
#' This function starts sinking records to log file.
#'
#' @export

sink_start <- function(){ # chatgpt
  log_dir <- "results"
  if (!dir.exists(log_dir)) dir.create(log_dir)

  log_file <- file.path(log_dir, paste0(Sys.Date(), ".log"))

  # Open a connection
  con <- file(log_file, open = "a")   # append mode

  # Sink output
  sink(con, append = TRUE)                     # capture cat(), print()

  # Write start message
  cat("\n\n===== Log started at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "=====\n")

  # Return connection so we can close later
  assign(".log_con", con, envir = .GlobalEnv)
}
