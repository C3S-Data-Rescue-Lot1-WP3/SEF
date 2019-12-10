#' Sub-daily meteorological observations for Bern
#'
#' Observations of pressure and temperature for the city of Bern (Switzerland)
#' for the period 1800-1827.
#'
#' @format A list of data frames (one data frame per variable).
#' The format of the data frames is that required by the QC functions.
#' @source Institute of Geography - University of Bern
"Bern"

#' Metadata for the station of Bern
#'
#' @format A list of data frames (one data frame per variable)
#' @source Institute of Geography - University of Bern
"Meta"

#' List of standard variable codes
#'
#' @format A data frame with two variables
#' @source C3S Data Rescue Service
"Variables"
utils::globalVariables("Variables")
