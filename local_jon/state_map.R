library(dplyr)
library(COVID19)

library(MazamaSpatialPlots)
mazama_initialize(spatialDataDir = "~/Data/Spatial")


# ----- Get COVID data ---------------------------------------------------------

# > names(stateCovid)
#  [1] "id"                                  "date"
#  [3] "tests"                               "confirmed"
#  [5] "recovered"                           "deaths"
#  [7] "hosp"                                "vent"
#  [9] "icu"                                 "population"
# [11] "school_closing"                      "workplace_closing"
# [13] "cancel_events"                       "gatherings_restrictions"
# [15] "transport_closing"                   "stay_home_restrictions"
# [17] "internal_movement_restrictions"      "international_movement_restrictions"
# [19] "information_campaigns"               "testing_policy"
# [21] "contact_tracing"                     "stringency_index"
# [23] "iso_alpha_3"                         "iso_alpha_2"
# [25] "iso_numeric"                         "currency"
# [27] "administrative_area_level"           "administrative_area_level_1"
# [29] "administrative_area_level_2"         "administrative_area_level_3"
# [31] "latitude"                            "longitude"
# [33] "key"                                 "key_google_mobility"
# [35] "key_apple_mobility"                  "key_alpha_2"
# [37] "key_numeric"

stateCovid <-
  COVID19::covid19("US", level = 2) %>%
  rename(
    countryCode = iso_alpha_2,
    stateCode = key_alpha_2
  ) %>%
  # add derived values
  mutate(
    test_per_100 = 100 * tests/population,
    confirmed_per_100 = 100 * confirmed/population,
    recovered_per_100K = 100000 * recovered/population,
    deaths_per_100K = 100000 * deaths/population,
    hosp_per_100K = 100000 * hosp/population,
    vent_per_100K = 100000 * vent/population,
    icu_per_100K = 100000 * icu/population
  )

# Try out a single date
covidDF <-
  stateCovid %>%
  dplyr::filter(date == "2020-11-11")

# ----- Create multi-parameter map ---------------------------------------------

MazamaSpatialPlots::stateMap(
  data = covidDF,
  parameter = c(
    "test_per_100",
    "confirmed_per_100",
    "hosp_per_100K",
    "deaths_per_100K"
  ),
  title = c(
    "Tests",
    "Cases",
    "Hospitalizations",
    "Deaths"
  ),
  main.title = "A Grim Outlook"
)



# ----- Movie??? ---------------------------------------------------------------

datestamps <-
  MazamaCoreUtils::dateSequence(20200601, 20200731, timezone = "UTC") %>%
  MazamaCoreUtils::timeStamp(style = "clock", unit = "day", timezone = "UTC")

imageNameBase <- "covid_confirmed_"

timestep <- 0
for ( datestamp in datestamps ) {

  timestep <- timestep + 1
  fileName <- sprintf("%s%03d.png", imageNameBase, timestep)
  filePath <- file.path(tempdir(), fileName)

  tm <- MazamaSpatialPlots::stateMap(
    data = dplyr::filter(stateCovid, date == datestamp),
    parameter = c(
      "confirmed_per_100"
    ),
    title = datestamp
  )

  ###print(gg)

  # Save plot
  tmap::tmap_save(
    tm = tm,
    filename = filePath,
    width = 10,
    height = 8,
    units = "in",
    dpi = 72,
    scale = NA
  )

  # Results in "PNG image data, 720 x 576, 8-bit/color RGBA, non-interlaced"

}

# ----- Create video with ffmpeg -----------------------------------------------

# Create a filename for the movie
fileName <- sprintf("covid_confirmed.mp4")

frameRate = 6
outputDir <- path.expand(".")

# Define system calls to ffmpeg to create video from frames
cmd_cd <- paste0("cd ", tempdir())
cmd_ffmpeg <- paste0(
  "ffmpeg -y -loglevel quiet -r ",
  frameRate, " -f image2 -s 720x576 -i ",
  imageNameBase, "%03d.png -vcodec libx264 -crf 25 ",
  # https://bugzilla.mozilla.org/show_bug.cgi?id=1368063#c7
  "-pix_fmt yuv420p ",
  outputDir, "/", fileName
)
cmd_rm <- paste0("rm *.png")
cmd <- paste0(cmd_cd, " && ", cmd_ffmpeg, " && ", cmd_rm)

#### Make system calls
###logger.info("Calling ffmpeg to make video from frames")
###logger.trace(cmd)

ffmpegString <- paste(capture.output(system(cmd)), collapse = "\n")

###logger.trace("ffmpeg output:\n\n%s\n", ffmpegString)


