# Working with open source NYT county-level data
#   https://github.com/nytimes/covid-19-data
#
# The New York Times is releasing a series of data files with cumulative counts 
# of coronavirus cases in the United States, at the state and county level, over 
# time. We are compiling this time series data from state and local governments 
# and health departments in an attempt to provide a complete record of the 
# ongoing outbreak.


library(dplyr)
library(ggplot2)

ihme <- readr::read_csv("2020_04_05.08.all/Hospitalization_all_locs.csv")

# > names(ihme)
# [1] "V1"            "location_name" "date"          "allbed_mean"   "allbed_lower" 
# [6] "allbed_upper"  "ICUbed_mean"   "ICUbed_lower"  "ICUbed_upper"  "InvVen_mean"  
# [11] "InvVen_lower"  "InvVen_upper"  "deaths_mean"   "deaths_lower"  "deaths_upper" 
# [16] "admis_mean"    "admis_lower"   "admis_upper"   "newICU_mean"   "newICU_lower" 
# [21] "newICU_upper"  "totdea_mean"   "totdea_lower"  "totdea_upper"  "bedover_mean" 
# [26] "bedover_lower" "bedover_upper" "icuover_mean"  "icuover_lower" "icuover_upper"

# ----- Daily by state ---------------------------------------------------------

myState <- "Washington"
predictionDate <- MazamaCoreUtils::parseDatetime("2020-04-05", timezone = "UTC")

firstDate <-
  ihme %>%
  slice(1) %>%
  pull(date)

daily <- 
  ihme %>%
  filter(location_name == myState) %>%
  mutate(isProjection = date > predictionDate) %>%
  mutate(deaths_7day = zoo::rollmean(deaths_mean, 7, align = "right", fill = NA)) %>%
  mutate(day = as.numeric(difftime(date, firstDate, unit = "day"))) %>%
  select(date, day, deaths_mean, deaths_7day, isProjection)

tail(daily)
  
ggplot(daily) +
  geom_area(
    data = subset(daily, isProjection == FALSE),
    mapping = aes(x = date, y = deaths_7day),
    colour = 'black',
    alpha = 0.2
  ) +
  geom_point(aes(x = date, y = deaths_mean, col = isProjection)) +
  ggtitle(sprintf("Daily New Deaths in %s", myState))

