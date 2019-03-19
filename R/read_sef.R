#' Read data files in Station Exchange Format version 0.2.1
#'
#' @param file Character string giving the path of the SEF file.
#'
#' @return A data frame with seven or eigth variables (depending on time
#' resolution): variable code, year, month, day, (time), value,
#' time flag, metadata.
#'
#' @author Yuri Brugnara
#'
#' @import utils
#' @export

read_sef <- function(file = file.choose()) {

  header <- read.table(file = file, quote = "", comment.char = "", sep = "\t",
                       nrows = 10, stringsAsFactors = FALSE, fill = TRUE)
  varcode <- header[9, 2]

  Data <- read.table(file = file, skip = 11, header = TRUE, fill = TRUE,
                     sep = "\t", stringsAsFactors = FALSE)
  times <- unique(Data$TimeF)
  if (length(times) > 1 & 0 %in% times) {
    warning("Cannot convert more than one time resolution")
    Data <- Data[which(Data$TimeF == times[1]), ]
  }
  times <- times[1]
  if (times %in% 0) {
    Data <- Data[, c(1:4,6,5,7)]
    colnames(Data) <- c("Year", "Month", "Day", "Time", "Obs", "TimeF", "Meta")
    Data$Time <- formatC(as.integer(Data$Time), width = 4, flag = "0")
  } else {
    Data <- Data[, c(1:3,6,5,7)]
    colnames(Data) <- c("Year", "Month", "Day", "Obs", "TimeF", "Meta")
  }

  Data <- cbind(varcode, Data)
  colnames(Data)[1] <- "Var"

  return(Data)

}


#' Read metadata from the Station Exchange Format version 0.2.1
#'
#' @param file Character string giving the path of the data file.
#' @param parameter Character vector of required parameters. Accepted
#' values are \code{"version"}, \code{"id"}, \code{"name"}, \code{"lat"},
#' \code{"lon"}, \code{"alt"}, \code{"source"}, \code{"repo"},
#' \code{"var"}, \code{"units"}, \code{"meta"}. By default all parameters
#' are read at once.
#'
#' @return A character vector with the required parameters.
#'
#' @author Yuri Brugnara
#'
#' @import utils
#' @export

read_meta <- function(file = file.choose(), parameter = NA) {

  header <- read.table(file = file, quote = "", comment.char = "", sep = "\t",
                       nrows = 11, stringsAsFactors = FALSE, fill = TRUE)
  pars <-c("version", "id", "name", "lat", "lon", "alt", "source", "repo",
           "var", "units", "meta")
  if (is.na(parameter)) {
    out <- header[, 2]
    names(out) <- pars
  } else {
    out <- header[match(parameter, pars), 2]
    names(out) <- parameter
  }

  return(out)

}
