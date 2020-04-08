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

tidyCovid <- readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# > names(tidyCovid)
# [1] "date"   "county" "state"  "fips"   "cases"  "deaths"

King_County <-
  tidyCovid %>%
  filter(state == "Washington") %>%
  filter(county == "King") %>%
  mutate(newCases = c(NA, diff(cases))) %>%
  mutate(newDeaths = c(NA, diff(deaths))) %>%
  select(-c(county, state, fips))
  
# Simplest plot
ggplot(King_County) +
  aes(x = date, y = newDeaths) +
  geom_point() +
  geom_smooth()

# ----- Daily by state ---------------------------------------------------------

firstDate <-
  tidyCovid %>%
  slice(1) %>%
  pull(date)

myState <- "New York"
dailyNew <- 
  tidyCovid %>%
  filter(state == myState) %>%
  group_by(date) %>%
  summarize(allDeaths = sum(deaths)) %>%
  mutate(newDeaths = c(NA, diff(allDeaths))) %>%
  mutate(day = as.numeric(difftime(date, firstDate, unit = "day")))

tail(dailyNew)
  

ggplot(dailyNew) +
  aes(x = date, y = newDeaths) +
  geom_point() +
  geom_smooth() +
  ggtitle(sprintf("Daily New Deaths in %s", myState))

# # Exponential fit
# b <- 0.5
# ggplot(dailyNew) +
#   aes(x = day, y = newDeaths) +
#   geom_point() +
#   geom_smooth(method = "lm", formula = y ~ exp(b * x)) +
#   ggtitle(sprintf("Daily New Deaths in %s", myState))
# 
