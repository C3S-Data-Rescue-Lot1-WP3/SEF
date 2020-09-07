#' Write data in Station Exchange Format version 1.0.0
#'
#' @param Data A data frame with 6 variables in this order:
#' year, month, day, hour, minute, value.
#' @param outpath Character string giving the output path (note that the
#' filename is generated from the source identifier, station code, start
#' and end dates, and variable code). By default this is the working
#' directory.
#' @param variable Variable code. This is a required field.
#' @param cod Station code. This is a required field.
#' @param nam Station name.
#' @param lat Station latitude (degrees North in decimal).
#' @param lon Station longitude (degreees East in decimal).
#' @param alt Station altitude (metres).
#' @param sou Character string giving the source identifier.
#' @param link Character string giving an url for metadata (e.g., link to the
#' C3S Data Rescue registry).
#' @param stat Character string giving the statistic code. This is a required
#' field.
#' @param units Character string giving the units. This is a required field.
#' @param metaHead Character string giving metadata entries for the header
#' (pipe separated).
#' @param meta Character vector with length equal to the number of rows
#' of \code{Data}, giving metadata entries for the single observations (pipe
#' separated).
#' @param period Observation time period code. Must be a character vector with
#' length equal to the number of rows of \code{Data} unless all observations
#' have the same period code.
#' @param time_offset Numerical vector of offsets from UTC in hours.
#' This value will be subtracted from the observation times to obtain UTC times,
#' so for instance the offset of Central European Time is +1 hour.
#' Recycled for all observations if only one value is given.
#' @param note Character string to be added to the end of the standard output
#' filename. It will be separated from the rest of the name by an underscore.
#' Blanks will be also replaced by underscores.
#' @param keep_na If FALSE (the default), lines where observations are NA are
#' removed.
#' @param outfile Output filename. If specified, ignores \code{note}.
#'
#' @note
#' Times in SEF files must be expressed in UTC.
#'
#' If \code{outfile} is not specified, the output filename is generated
#' automatically as \code{sou}_\code{cod}_startdate_enddate_\code{variable}.tsv
#'
#' @author Yuri Brugnara
#'
#' @examples
#' # Create a basic SEF file for air temperature in Bern
#' # (assuming the observation times are in local solar time)
#' # The file will be written in the working directory
#' meta_bern <- Meta$ta[which(Meta$ta$id == "Bern"), ]
#' write_sef(Bern$ta[, 2:7], variable = "ta", cod = meta_bern$id, lat = meta_bern$lat,
#'           lon = meta_bern$lon, alt = meta_bern$alt, units = meta_bern$units,
#'           stat = "point", period = "0", time_offset = meta_bern$lon * 24 / 360)
#'
#' @import utils
#' @export

write_sef <- function(Data, outpath = getwd(), variable, cod, nam = "", lat = "",
                      lon = "", alt = "", sou = "", link = "", units,
                      stat, metaHead = "", meta = "", period = "",
                      time_offset = 0, note = "", keep_na = FALSE, outfile = NA) {

  ## Get rid of factors
  for (i in 1:ncol(Data)) Data[,i] <- as.character(Data[,i])

  ## Build header
  header <- array(dim = c(12, 2), data = "")
  header[1, ] <- c("SEF", "1.0.0")
  header[2, ] <- c("ID", trimws(as.character(cod)))
  header[3, ] <- c("Name", trimws(as.character(nam)))
  header[4, ] <- c("Lat", trimws(as.character(lat)))
  header[5, ] <- c("Lon", trimws(as.character(lon)))
  header[6, ] <- c("Alt", trimws(as.character(alt)))
  header[7, ] <- c("Source", trimws(as.character(sou)))
  header[8, ] <- c("Link", trimws(as.character(link)))
  header[9, ] <- c("Vbl", trimws(as.character(variable)))
  header[10, ] <- c("Stat", trimws(as.character(stat)))
  header[11, ] <- c("Units", trimws(as.character(units)))
  header[12, ] <- c("Meta", trimws(as.character(metaHead)))

  ## For instantaneous observations the period must be 0
  if (stat == "point" & !all(as.character(period) == "0")) {
    period <- "0"
    warning("Period forced to 0 because of 'stat'")
  }

  ## Convert times to UTC
  if (!all(time_offset == 0) & !all(is.na(as.integer(Data[,4])+as.integer(Data[,5])))) {
    times <- ISOdate(Data[,1], Data[,2], Data[,3], Data[,4], Data[,5])
    times <- times - time_offset * 3600
    Data[which(!is.na(times)), 1] <- as.integer(substr(times[which(!is.na(times))],1,4))
    Data[which(!is.na(times)), 2] <- as.integer(substr(times[which(!is.na(times))],6,7))
    Data[which(!is.na(times)), 3] <- as.integer(substr(times[which(!is.na(times))],9,10))
    Data[which(!is.na(times)), 4] <- as.integer(substr(times[which(!is.na(times))],12,13))
    Data[which(!is.na(times)), 5] <- as.integer(substr(times[which(!is.na(times))],15,16))
  }

  ## Build data frame with SEF structure
  DataNew <- data.frame(Year = Data[, 1],
                        Month = Data[, 2],
                        Day = Data[, 3],
                        Hour = Data[, 4],
                        Minute = Data[, 5],
                        Period = as.character(period),
                        Value = Data[, 6],
                        Meta = as.character(meta),
                        stringsAsFactors = FALSE)

  ## Remove lines with missing data
  if (!keep_na) DataNew <- DataNew[which(!is.na(DataNew$Value)), ]

  ## Build filename
  if (substr(outpath, nchar(outpath), nchar(outpath)) != "/") {
    outpath <- paste0(outpath, "/")
  }
  if (is.na(outfile)) {
    j <- 3
    if (is.na(as.integer(DataNew[1,3]))) j <- 2
    if (is.na(as.integer(DataNew[1,2]))) j <- 1
    datemin <- paste(formatC(unlist(as.integer(DataNew[1, 1:j])), width=2, flag=0),
                     collapse = "")
    datemax <- paste(formatC(unlist(as.integer(DataNew[dim(DataNew)[1], 1:j])), width=2, flag=0),
                     collapse = "")
    dates <- paste(datemin, datemax, sep = "-")
    filename <- paste(sou, cod, dates, variable, sep = "_")
    if (sou %in% c(NA,"")) filename <- sub("_", "", filename)
    if (note != "") {
      note <- paste0("_", gsub(" ", "_", note))
    }
    filename <- gsub(" ", "", filename)
    filename <- paste0(outpath, filename, note, ".tsv")
  } else {
    filename <- paste0(outpath, outfile)
    if (substr(filename, nchar(filename)-3, nchar(filename)) != ".tsv") {
      filename <- paste0(filename, ".tsv")
    }
  }

  ## Write header to file
  write.table(header, file = filename, quote = FALSE, row.names = FALSE,
              col.names = FALSE, sep = "\t", dec = ".", fileEncoding = "UTF-8")

  ## Write column names to file
  write.table(t(names(DataNew)), file = filename, quote = FALSE, row.names = FALSE,
              col.names = FALSE, sep = "\t", fileEncoding = "UTF-8",
              append = TRUE)

  ## Write data to file
  write.table(DataNew, file = filename, quote = FALSE, row.names = FALSE,
              col.names = FALSE, sep = "\t", dec = ".", fileEncoding = "UTF-8",
              append = TRUE)

  return(print(paste("Data written to file", filename), quote = FALSE))

}
