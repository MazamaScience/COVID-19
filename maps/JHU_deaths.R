# Quick access to JHU data
#

# ----- Configurables ----------------------------------------------------------

DATESTRING = "4/17/2020"
DATESTAMP <- "UTC_20200417"

# ----- Setup ------------------------------------------------------------------

library(dplyr)

library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("USCensusStates")
loadSpatialData("USCensusCounties")

###library(tidycensus)
library(tmap)

# ----- Download, parse data ---------------------------------------------------

# Data description:
#
# https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data#time-series-summary-csse_covid_19_time_series
#
# Dates are UTC

# Load data
df <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

# > names(df)
# [1] "UID"            "iso2"           "iso3"           "code3"          "FIPS"          
# [6] "Admin2"         "Province_State" "Country_Region" "Lat"            "Long_"         
# [11] "Combined_Key"   "Population"     "1/22/20"        "1/23/20"        "1/24/20"       
# [16] "1/25/20"        "1/26/20"        "1/27/20"        "1/28/20"        "1/29/20"       
# [21] "1/30/20"        "1/31/20"        "2/1/20"         "2/2/20"         "2/3/20"        
# [26] "2/4/20"         "2/5/20"         "2/6/20"         "2/7/20"         "2/8/20"        
# [31] "2/9/20"         "2/10/20"        "2/11/20"        "2/12/20"        "2/13/20"       
# [36] "2/14/20"        "2/15/20"        "2/16/20"        "2/17/20"        "2/18/20"       
# [41] "2/19/20"        "2/20/20"        "2/21/20"        "2/22/20"        "2/23/20"       
# [46] "2/24/20"        "2/25/20"        "2/26/20"        "2/27/20"        "2/28/20"       
# [51] "2/29/20"        "3/1/20"         "3/2/20"         "3/3/20"         "3/4/20"        
# [56] "3/5/20"         "3/6/20"         "3/7/20"         "3/8/20"         "3/9/20"        
# [61] "3/10/20"        "3/11/20"        "3/12/20"        "3/13/20"        "3/14/20"       
# [66] "3/15/20"        "3/16/20"        "3/17/20"        "3/18/20"        "3/19/20"       
# [71] "3/20/20"        "3/21/20"        "3/22/20"        "3/23/20"        "3/24/20"       
# [76] "3/25/20"        "3/26/20"        "3/27/20"        "3/28/20"        "3/29/20"       
# [81] "3/30/20"        "3/31/20"        "4/1/20"         "4/2/20"         "4/3/20"        
# [86] "4/4/20"         "4/5/20"         "4/6/20"         "4/7/20"         "4/8/20"        
# [91] "4/9/20"         "4/10/20"        "4/11/20"        "4/12/20"        "4/13/20"       
# [96] "4/14/20"        "4/15/20"        "4/16/20"       

# Data frame with deaths by county
#  * start with df
#  * remove unneeded columns
#  * use/create Mazama Science standard names
#  * remove old names
#  * filter out records with missing countyNames
byCounty_DF <-
  df %>%
  select(-UID, -iso3, -code3, -Country_Region, -Lat, -Long_, -Combined_Key) %>%
  mutate(
    countryCode = iso2,
    stateCode = stateToCode(Province_State, countryCodes = "US", dataset = "USCensusStates"),
    countyName = Admin2,
    FIPS = sprintf("%05d", FIPS), # Codes are names, not numbers (e.g. '007').
    population = Population
  ) %>%
  select(-iso2, -Province_State, -Admin2, -Population) %>%
  filter(!is.na(countyName)) %>%
  filter(countyName != "Unassigned") %>%
  filter(!stringr::str_detect(countyName, "Out of"))

# ----- Rename and reorder columns ---------------------------------------------

spatialNames <- c("countryCode", "stateCode", "countyName", "FIPS", "population")
dateStamps <- 
  setdiff(names(byCounty_DF), spatialNames) %>%
  lubridate::parse_date_time("mdy", tz = "UTC") %>%
  strftime(format = "UTC_%Y%m%d", tz = "UTC")

spatial_byCounty_DF <- select(byCounty_DF, !!spatialNames)
numeric_byCounty_DF <- select(byCounty_DF, contains("/"))
names(numeric_byCounty_DF) <- dateStamps

deathsByCounty_DF <- bind_cols(spatial_byCounty_DF, numeric_byCounty_DF)

# ----- Create county spatial dataset ------------------------------------------

county_DF <- left_join(
  USCensusCounties@data,
  deathsByCounty_DF,
  by = c("countryCode", "stateCode", "countyName")
)

USCensusCounties@data <- county_DF

# ----- Create a map -----------------------------------------------------------

conus_SPDF <- subset(USCensusCounties, stateCode %in% CONUS)
conus_proj <- sp::CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

# # Population
# breaks <- c(0,10000,20000,50000,100000,200000,500000,Inf)
# 
# tm_shape(conus_SPDF, projection = conus_proj) +
#   tm_polygons(
#     "population",
#     breaks = breaks,
#     #style = "log10_pretty",
#     palette = "YlOrBr",
#     border.col = "white"
#   ) +
#   tm_shape(USCensusStates, projection = conus_proj) +
#   tm_polygons(
#     alpha = 0,
#     border.col = "gray50"
#   )

# Deaths
breaks <- c(0,1,2,5,10,20,50,100,Inf)

tm_shape(conus_SPDF, projection = conus_proj) +
  tm_polygons(
    DATESTAMP,
    breaks = breaks,
    #style = "jenks",
    palette = "YlOrBr",
    border.col = "white"
  ) +
  tm_shape(USCensusStates, projection = conus_proj) +
  tm_polygons(
    alpha = 0,
    border.col = "gray50"
  ) +
  tm_layout(
    title = paste0("Cumulative COVID-19 Deaths per County on ", DATESTRING),
    title.size = 1.1,
    title.position = c("center", "top"),
    frame = FALSE
  )

# ==============================================================================

if ( FALSE ) {
  
  # Mapping idea from
  # https://www.zevross.com/blog/2018/10/02/creating-beautiful-demographic-maps-in-r-with-the-tidycensus-and-tmap-packages/
  
  # Get census map (NEEDS AN API KEY)
  
  # dat16 <- 
  #   get_acs(
  #     "county", 
  #     table = "B27001", 
  #     year = 2016, 
  #     output = "tidy", 
  #     state = NULL, 
  #     geometry = TRUE, 
  #     shift_geo = TRUE
  #   ) %>%
  #   rename(`2016` = estimate) %>%
  #   select(-moe)
  
}


