#' Write data in Station Exchange Format version 0.2.0
#'
#' @param Data A data frame with 7 variables in this order: variable code, 
#' year, month, day, hour, minute, value.
#' @param outpath Character string giving the output path (note that the 
#' filename is generated from the source identifier, station code, start 
#' and end dates, and variable code).
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
#' @param note Character string to be added to the end of the filename.
#' It will be separated from the rest of the name by an underscore.
#' Blanks will be also replaced by underscores.
#'
#' @author Yuri Brugnara
#'
#' @import utils
#' @export

write_sef <- function(Data, outpath = getwd(), cod, nam = "", lat = "", 
                      lon = "", alt = "", sou = "", link = "", units, 
                      stat, metaHead = "", meta = "", period = "", 
                      note = "") {
  
  ## Check that only one variable is given
  variable <- unique(as.character(Data[, 1]))
  if (NA %in% variable) stop("Variable column cannot contain NAs")
  if (length(variable) > 1) {   
    warning("Only one variable can be read. Reading first variable only...")
    variable <- variable[1]
    Data <- subset(Data, Data[, 1] == variable)
  }
   
  ## Build filename
  datemin <- paste(formatC(unlist(Data[1, 2:4]), width=2, flag=0),
                   collapse = "")
  datemax <- paste(formatC(unlist(Data[dim(Data)[1], 2:4]), width=2, flag=0),
                   collapse = "")
  dates <- paste(datemin, datemax, sep = "-")
  filename <- paste(sou, cod, dates, variable, sep = "_")
  if (sou %in% c(NA,"")) filename <- sub("_", "", filename)
  if (substr(outpath, nchar(outpath), nchar(outpath)) != "/") {
    outpath <- paste0(outpath, "/")
  }
  if (note != "") {
    note <- paste0("_", gsub(" ", "_", note))
  }
  filename <- paste0(outpath, filename, note, ".tsv")
  
  ## Build header
  header <- array(dim = c(12, 2), data = "")
  header[1, ] <- c("SEF", packageVersion("SEF"))
  header[2, ] <- c("ID", as.character(cod))
  header[3, ] <- c("Name", as.character(nam))
  header[4, ] <- c("Lat", as.character(lat))
  header[5, ] <- c("Lon", as.character(lon))
  header[6, ] <- c("Alt", as.character(alt))
  header[7, ] <- c("Source", as.character(sou))
  header[8, ] <- c("Link", as.character(link))
  header[9, ] <- c("Vbl", as.character(variable))
  header[10, ] <- c("Stat", as.character(stat))
  header[11, ] <- c("Units", as.character(units))
  header[12, ] <- c("Meta", as.character(metaHead))
  
  # For instantaneous observations the period must be 0
  if (stat == "point") period <- "0"

  ## Build data frame with SEF structure
  DataNew <- data.frame(Year = as.integer(Data[, 2]),
                        Month = as.integer(Data[, 3]),
                        Day = as.integer(Data[, 4]),
                        Hour = as.integer(Data[, 5]),
                        Minute = as.integer(Data[, 6]),
                        Period = as.character(period),
                        Value = as.character(Data[, 7]),
                        Meta = as.character(meta),
                        stringsAsFactors = FALSE)
  
  ## Remove lines with missing data
  DataNew <- DataNew[which(!is.na(DataNew$Value)), ]
  
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
