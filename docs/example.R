library(dataresqc)


## Step 1: Read the Excel file into R

mydata <- XLConnect::readWorksheetFromFile("example.xls", sheet = 1)

mydata <- as.data.frame(mydata)


## Step 2: Prepare the metadata

latitude <- round(43 + 54/60 + 30/3600, 4)
longitude <- round(-(69 + 57/60 + 24/3600), 4)
altitude <- 25


## Step 3: Change data frame structure

# Create empty list
sefdata <- list()

# Create data frame for temperature (under name 'ta')
sefdata$ta <- data.frame(Year = rep(mydata$Year, each=3),
                         Month = rep(mydata$Month, each=3),
                         Day = rep(mydata$Day, each=3),
                         Value = c(t(mydata[,4:6])))

# Create data frame for pressure (under name 'p')
sefdata$p <- data.frame(Year = rep(mydata$Year, each=3),
                        Month = rep(mydata$Month, each=3),
                        Day = rep(mydata$Day, each=3),
                        Value = c(t(mydata[,7:9])))

# Create data frame for wind (under name 'dd')
sefdata$dd <- data.frame(Year = rep(mydata$Year, each=3),
                         Month = rep(mydata$Month, each=3),
                         Day = rep(mydata$Day, each=3),
                         Value = c(t(mydata[,10:12])))


## Step 4: Add time

# First we create a vector of dates
dates <- as.Date(paste(mydata$Year, mydata$Month, mydata$Day, sep="-"))
# Calculate sunset times (in UTC) for each date
sunsets <- suncalc::getSunlightTimes(dates, latitude, longitude,
                                     keep="sunset", tz="GMT")$sunset
# Convert from UTC to local time
sunsets <- sunsets + 3600 * longitude * 12 / 180

# First we put NA for the sunset times
hours <- rep(c(7, 13, NA), times = 366)
minutes <- rep(c(30, 0, NA), times = 366)
# Then we add the sunset times in place of the NAs
hours[is.na(hours)] <- lubridate::hour(sunsets)
minutes[is.na(minutes)] <- lubridate::minute(sunsets)

for (v in names(sefdata)) {
  sefdata[[v]]$Hour <- hours
  sefdata[[v]]$Minute <- minutes
  sefdata[[v]] <- sefdata[[v]][, c("Year", "Month", "Day",
                                   "Hour", "Minute", "Value")]
}


## Step 5: Add Meta column

original_units <- c("F", "in", "")
obs_times <- c("7:30AM", "1:00PM", "sunset")
names(original_units) <- names(sefdata)
for (v in names(sefdata)) {
  sefdata[[v]]$Meta <- paste0("orig=", sefdata[[v]]$Value, original_units[v],
                              "|orig.time=", rep(obs_times, times=366),
                              "|orig.date=", rep(dates, each=3))
}


## Step 6: Convert into metric units

# Fahrenheit to Celsius
sefdata$ta$Value <- round((sefdata$ta$Value - 32) * 5 / 9, 1)

# Inches to hPa
sefdata$p$Value <- round(convert_pressure(sefdata$p$Value,
                                          f = 25.4,
                                          lat = latitude,
                                          alt = altitude), 1)

directions <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S",
                "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
sefdata$dd$Value <- 22.5 * (match(toupper(sefdata$dd$Value), directions) - 1)


## Step 7: Write SEF files

units <- c("C", "hPa", "degree")
names(units) <- names(sefdata)
for (v in names(sefdata)) {
  write_sef(Data = sefdata[[v]][, 1:6],
            variable = v,
            cod = "Brunswick_Cleaveland",
            nam = "Brunswick, ME",
            lat = latitude,
            lon = longitude,
            alt = altitude,
            sou = "C3S-DRS",
            units = units[v],
            stat = "point",
            metaHead = paste0("Observer=Parker Cleaveland",
                              ifelse(v=="p", "|PTC=N|PGC=Y", "")),
            meta = sefdata[[v]][, 7],
            period = 0,
            time_offset = longitude * 12 / 180)
}


## Step 8: Check SEF files

check_sef("C3S-DRS_Brunswick_Cleaveland_18160101-18161231_ta.tsv")
check_sef("C3S-DRS_Brunswick_Cleaveland_18160101-18161231_p.tsv")
check_sef("C3S-DRS_Brunswick_Cleaveland_18160101-18161231_dd.tsv")
