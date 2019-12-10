# Run in the package main directory to import example data and metadata,

library(devtools)

# Load and format Bern data from CHIMES project
load("data-raw/Bern_Studer.RData")
n <- dim(df)[1]
df <- df[order(df$year,df$month,df$day,df$hour,df$minutes),]
Bern <- list()
Bunits <- c("C","hPa")
names(Bunits) <- c("ta","p")
Bern$ta <- data.frame(Var = rep("ta", n),
                      Year = df$year,
                      Month = df$month,
                      Day = df$day,
                      Hour = df$hour,
                      Minutes = df$minutes,
                      Obs = df$air_temperature,
                      stringsAsFactors = FALSE)
Bern$ta <- Bern$ta[which(!is.na(Bern$ta$Obs)), ]
Bern$p <- data.frame(Var = rep("p", n),
                     Year = df$year,
                     Month = df$month,
                     Day = df$day,
                     Hour = df$hour,
                     Minutes = df$minutes,
                     Obs = df$air_pressure * 1013.25 / 760,
                     stringsAsFactors = FALSE)
Bern$p <- Bern$p[which(!is.na(Bern$p$Obs)), ]


# Create metadata table
Meta <- list()
ids <- "Bern"
lats <- 46.94812
lons <- 7.45196
alts <- 534
for (variable in c("ta","p")) {
  Meta[[variable]] <- data.frame(id = ids, lat = lats, lon = lons, alt = alts,
                                 var = variable, units = Bunits[variable],
                                 stringsAsFactors = FALSE)
}

# Create vector of variable names
Variables <- read.table("data-raw/variables.txt", sep = "\t", stringsAsFactors = FALSE)
names(Variables) <- c("abbr", "full_name")

# Import using devtools
use_data(Bern, Meta, Variables, overwrite = TRUE)
