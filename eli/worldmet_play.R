library(worldmet)
library(openair)

library(ThermIndex)
library(COVID19)

worldmet::getMeta(site = "seattle")

# "720129-99999" is an airport in austin TX
data <- worldmet::importNOAA(code = "720129-99999", year = 2020)
data$air_tempF <- (data$air_temp * 9/5) + 32
# Thom's Discomfort Index
data$discomfortIndex_Thom <- data$air_temp - 0.55 * (1 - (0.01 * data$RH)) * (data$air_temp - 14.5)

# Kawamura's Discomfort Index
data$discomfortIndex_Kawamura <- (0.99 * data$air_temp) + (0.36 * data$dew_point) + 41.5

# Taken from first page of https://journals.ametsoc.org/doi/pdf/10.1175/1520-0477-40.12.620
# Scale: 
# < -5       uncomfortably cool
# -5 to -1   comfortably cool
# 0          ideal
# +1 to +5   comfortably warm
# +6 to +10  uncomfortably warm or humid
# +11 to +15 very oppressive
# > +15      extreme discomfort
data$comfortIndex2 <- (0.5 + 0.005 * data$RH) * (data$air_tempF - 82.0 + 0.11 * data$RH) 
data$comfortIndex2HighHumidity <- (0.5 + 0.0001 * data$RH^2) * (data$air_tempF - 80.0 + 0.11 * data$RH) 

covid_USA <- COVID19::covid19(country = "US", level = 3)
covid_tx <- covid_USA %>%
  dplyr::filter(administrative_area_level_2 == "Texas") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(totalConfirmed = sum(confirmed, na.rm = T))

covid_tx$newCases <- as.numeric(NA)
for(i in seq(nrow(covid_tx) - 1) + 1) {
  covid_tx$newCases[i] <- covid_tx$totalConfirmed[i] - covid_tx$totalConfirmed[i-1]
}
covid_tx$date <- MazamaCoreUtils::parseDatetime(covid_tx$date, timezone = "UTC")


ggplot2::ggplot(data = data, ggplot2::aes(x = date, y = comfortIndex2)) +
  ggplot2::geom_line()

ggplot2::ggplot(data = covid_tx, ggplot2::aes(x = date, y = newCases)) +
  ggplot2::geom_line()

# At a glance it seems like there is a period of a high density of nice days from July-August.
# During that time it looks like the amount of new covid cases increase and then decrease as the comfort index drops.