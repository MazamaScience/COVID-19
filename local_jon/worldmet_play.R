library(dplyr)

library(worldmet)
library(openair)

library(ThermIndex)
library(COVID19)


# ----- Get weather data in Texas ----------------------------------------------


worldmet::getMeta(state = "tx")

# Stations in each cardinal direction and center of TX
stations_tx <- c("720313-03052", "722618-23091", "722516-12928", "723761-23901", "722499-53948")

# Load data from these stations
allData_tx <- list()
for ( code in stations_tx ) {
  data <- worldmet::importNOAA(code = code, year = 2020)
  allData_tx[[code]] <- data
}

# Get the station with the least amount of entries
sumData_tx <- allData_tx[[2]]

# Filter each station to have the same observation times as the one chosen above
allData_tx <- 
  allData_tx %>% 
  purrr::map(~ dplyr::filter(.x, date %in% sumData_tx$date)) %>% 
  dplyr::select(date, air_temp, RH, dew_point)

# Sum each relevant column
for ( code in stations_tx[-2] ) {
  sumData_tx$air_temp <- sumData_tx$air_temp + allData_tx[[code]]$air_temp
  sumData_tx$RH <- sumData_tx$RH + allData_tx[[code]]$RH
  sumData_tx$dew_point <- sumData_tx$dew_point + allData_tx[[code]]$dew_point
}

# Calculate mean for each column
meanData_tx <- data.frame(date = sumData_tx$date,
                          air_temp = sumData_tx$air_temp / length(stations_tx),
                          RH = sumData_tx$RH / length(stations_tx),
                          dew_point = sumData_tx$dew_point / length(stations_tx))


# Convert celsius to fahrenheit  
meanData_tx$air_tempF <- (meanData_tx$air_temp * 9/5) + 32


# ----- Calculate comfort indices ----------------------------------------------


# Thom's Discomfort Index
meanData_tx$discomfortIndex_Thom <- meanData_tx$air_temp - 0.55 * (1 - (0.01 * meanData_tx$RH)) * (meanData_tx$air_temp - 14.5)

# Kawamura's Discomfort Index
meanData_tx$discomfortIndex_Kawamura <- (0.99 * meanData_tx$air_temp) + (0.36 * meanData_tx$dew_point) + 41.5

# Taken from first page of https://journals.ametsoc.org/doi/pdf/10.1175/1520-0477-40.12.620
# Scale: 
# < -5       uncomfortably cool
# -5 to -1   comfortably cool
# 0          ideal
# +1 to +5   comfortably warm
# +6 to +10  uncomfortably warm or humid
# +11 to +15 very oppressive
# > +15      extreme discomfort
meanData_tx$comfortIndex2 <- (0.5 + 0.005 * meanData_tx$RH) * (meanData_tx$air_tempF - 82.0 + 0.11 * meanData_tx$RH) 
meanData_tx$comfortIndex2HighHumidity <- (0.5 + 0.0001 * meanData_tx$RH^2) * (meanData_tx$air_tempF - 80.0 + 0.11 * meanData_tx$RH) 


# ----- Get COVID data ---------------------------------------------------------


# Get COVID data for USA (county level)
covid_USA <- COVID19::covid19(country = "US", level = 3)

# Filter by data in Texas
covid_tx <- covid_USA %>%
  dplyr::filter(administrative_area_level_2 == "Texas") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(totalConfirmed = sum(confirmed, na.rm = T))

# NOTE: column 'totalConfirmed' is the sum to date.
#       the following creates a column 'newCases' 
#       which is the amount of new cases on a given day
covid_tx$newCases <- as.numeric(NA)
for(i in seq(nrow(covid_tx) - 1) + 1) {
  covid_tx$newCases[i] <- covid_tx$totalConfirmed[i] - covid_tx$totalConfirmed[i-1]
}

# NOTE: the dates are stored like "yyyy-mm-dd" 
#       the following gets these dates as POSIXct 
covid_tx$date <- MazamaCoreUtils::parseDatetime(covid_tx$date, timezone = "UTC")


# ----- Plot data --------------------------------------------------------------


# Comfort index
ggplot2::ggplot(data = meanData_tx, ggplot2::aes(x = date, y = comfortIndex2)) +
  ggplot2::geom_line()

# Temp (degrees F)
ggplot2::ggplot(data = meanData_tx, ggplot2::aes(x = date, y = air_tempF)) +
  ggplot2::geom_line()

# Humidity
ggplot2::ggplot(data = meanData_tx, ggplot2::aes(x = date, y = RH)) +
  ggplot2::geom_line()

ggplot2::ggplot(data = covid_tx, ggplot2::aes(x = date, y = newCases)) +
  ggplot2::geom_line()


# ----- Interpretation ---------------------------------------------------------


# At a glance it seems like the comfort index increases until some point in early July where it starts to decrease.
# The comfort index during the beginning of July is telling us that days are getting more uncomfortable. It is still in the
# "comfortably warm" range but that is obviously subjective. Looking at the data, the humidity is staying steady
# but the temperature is fairly high (around 90 F and even goes above 100 F). I would personally say it would be extremely 
# uncomfortable to be outside if it was that hot.
#
# During that time period it looks like the amount of new covid cases increases and then decreases as the 
# comfort index and temperature starts to drop.