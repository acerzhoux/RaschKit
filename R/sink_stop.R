#' sink_stop
#'
#' This function stops sinking records to log file.
#'
#' @export

sink_stop <- function(){ # chatgpt
  while (sink.number() > 0) sink()

  if (exists(".log_con", envir = .GlobalEnv)) {
    close(get(".log_con", envir = .GlobalEnv))
    rm(".log_con", envir = .GlobalEnv)
  }
}
