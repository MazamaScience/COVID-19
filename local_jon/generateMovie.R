
if ( FALSE ) {
  
  library(dplyr)
  library(ggplot2)
  
  library(MazamaSpatialPlots)
  mazama_initialize()
  
  # Example:
  stateCovid <- 
    
    COVID19::covid19("US", level = 2) %>%
    
    dplyr::rename(
      countryCode = iso_alpha_2,
      stateCode = key_alpha_2
    ) %>%
    
    # Further dplyr manipulations will be applied per state
    dplyr::group_by(stateCode) %>%
    
    # NOTE:  tests, confirmed, recovered and deaths are cumulative
    # NOTE:  https://covid19datahub.io/articles/doc/data.html
    
    # Create daily statistics from cumulative
    dplyr::mutate(
      t_diff = c(NA, diff(tests)),
      c_diff = c(NA, diff(confirmed)),
      r_diff = c(NA, diff(recovered)),
      d_diff = c(NA, diff(deaths))
    ) %>%
    
    # Add rolling means
    dplyr::mutate(
      t_diff_14 = zoo::rollmean(t_diff, k = 14, align = "right", fill = NA),
      c_diff_14 = zoo::rollmean(c_diff, k = 14, align = "right", fill = NA),
      r_diff_14 = zoo::rollmean(r_diff, k = 14, align = "right", fill = NA),
      d_diff_14 = zoo::rollmean(d_diff, k = 14, align = "right", fill = NA)
    ) %>%
    
    # Stop grouping by state
    dplyr::ungroup()
  
  # Sanity check
  wa <- 
    stateCovid %>% 
    filter(stateCode == "WA") %>% 
    select(date, t_diff, t_diff_14)
  
  gg <- 
    ggplot(wa) + 
    geom_line(aes(x = date, y = t_diff)) + 
    geom_line(aes(x = date, y = t_diff_14), col = 'red')
  
  print(gg)
    
  stateCovid <-
    
    stateCovid %>%
    
    # Add derived values
    mutate(
      tests_per_100K = 100000 * t_diff_14/population,
      confirmed_per_100K = 100000 * c_diff_14/population,
      recovered_per_100K = 100000 * r_diff_14/population,
      deaths_per_100K = 100000 * d_diff_14/population,
      hosp_per_100K = 100000 * hosp/population,
      vent_per_100K = 100000 * vent/population,
      icu_per_100K = 100000 * icu/population
    )
  

  # ----- ALL STATES WHOLE YEAR -----
  generateMovie(
    data = stateCovid, 
    parameter = "confirmed_per_100K", 
    startDate = 20200101, 
    endDate = lubridate::today(tzone = "America/Los_Angeles") - lubridate::ddays(1), 
    saveDir = "/Users/jonathan", 
    # Range is -.003:0.086
    breaks = c(-Inf, 0, 1, 2, 5, 10, 20, 50, 100, Inf), 
    movieFileName = "covid_confirmed_2020",
    main.title = "Confirmed COVID Cases (Per 100,000 Citizens)",
    frame = TRUE,
    frameRate = 7,
    inner.margins = .1
  )
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  data = NULL
  parameter = NULL
  stateCodes = NULL
  startDate = NULL
  endDate = NULL
  breaks = c(0, 0.005, 0.01, 0.03, 0.05, 0.16)
  title = NULL
  main.title = NULL
  frame = FALSE
  imageWidth = 10
  imageHeight = 8
  imageDpi = 75
  frameRate = 6
  saveDir = NULL
  movieFileName = NULL
  verbose = FALSE
  
  data = stateCovid
  parameter = "confirmed_per_100"
  stateCodes = c("WA", "OR")
  startDate = 20200601
  endDate = 20200701
  breaks = seq(0, max(stateCovid$confirmed_per_100, na.rm = TRUE), .1)
  saveDir = "/Users/jonathan"
  movieFileName = "covid_confirmed"
  main.title = "14-day Rolling Mean Confirmed COVID Cases (Per 100,000)"
  frame = TRUE
  inner.margins = .1
  
}

################################################################################
################################################################################
################################################################################

generateMovie <- function(
  data = NULL,
  parameter = NULL,
  stateCodes = NULL,
  startDate = NULL,
  endDate = NULL,
  breaks = c(0, 0.005, 0.01, 0.03, 0.05, 0.16),
  title = NULL,
  main.title = NULL,
  frame = FALSE,
  imageWidth = 10,
  imageHeight = 8,
  imageDpi = 75,
  frameRate = 7,
  saveDir = NULL,
  movieFileName = NULL,
  verbose = FALSE,
  ...
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(data)
  MazamaCoreUtils::stopIfNull(parameter)
  MazamaCoreUtils::stopIfNull(startDate)
  MazamaCoreUtils::stopIfNull(endDate)
  MazamaCoreUtils::stopIfNull(saveDir)
  MazamaCoreUtils::stopIfNull(movieFileName)
  
  if ( !("date" %in% names(data)) )
    stop("Parameter 'data' must have a column called 'date' containing the dates and times of observations.")
  
  if ( !all(parameter %in% names(data)) )
    stop(sprintf("Parameter 'data' does not contain a column called %s", parameter))
  
  parseDates <- try({
    startDate <- MazamaCoreUtils::parseDatetime(startDate, timezone = "UTC")
    endDate <- MazamaCoreUtils::parseDatetime(endDate, timezone = "UTC")
  }, silent = TRUE)
  
  MazamaCoreUtils::stopOnError(parseDates, "Could not correctly parse parameters 'startDate' or 'endDate'.")
  
  if ( tolower(stringr::str_sub(movieFileName, -4)) == ".mp4")
    fileName <- stringr::str_sub(movieFileName, 0, -5)
  
  if ( !is.null(title) && length(parameter) != length(title) )
    stop("The lengths of 'parameter' and 'title' must be equal.")
  
  # ----- Generate animation ---------------------------------------------------
  
  datestamps <- 
    MazamaCoreUtils::dateSequence(startDate, endDate, timezone = "UTC") %>%
    MazamaCoreUtils::timeStamp(style = "clock", unit = "day", timezone = "UTC")
  
  imageNameBase <- "covid_confirmed_"
  
  # Convert to pixels
  imageWidthPx <- imageDpi * imageWidth
  imageHeightPx <- imageDpi * imageHeight
  
  if ( is.null(main.title) )
    main.title <- sprintf("%s in The United States", paste0(parameter, collapse = ", "))
  
  tmap::tmap_options(show.messages = verbose, show.warnings = verbose)
  
  timestep <- 0
  for ( datestamp in datestamps ) {
    
    timestep <- timestep + 1
    fileName <- sprintf("%s%03d.png", imageNameBase, timestep)
    filePath <- file.path(saveDir, fileName)
    
    if( is.null(stateCodes) ) {
      tm <- MazamaSpatialPlots::stateMap(
        data = dplyr::filter(data, date == datestamp),
        parameter = parameter,
        breaks = breaks,
        stateBorderColor = "white"
      )    
    } else {
      tm <- MazamaSpatialPlots::stateMap(
        data = dplyr::filter(data, date == datestamp),
        parameter = parameter,
        breaks = breaks,
        stateBorderColor = "white",
        stateCode = stateCodes
      )
    }
    
    tm <- tm + 
      tmap::tm_layout(
        frame = frame,
        main.title = paste0(main.title, sprintf("\n %s", datestamp)),
        main.title.position = c("center", "top"),
        title = title,
        title.position = c("center", "top"),
        title.fontface = 2,
        fontfamily = "serif",
        legend.position = c('left', 'bottom'),
        ...
      )
    
    # Save plot
    tmap::tmap_save(
      tm = tm,
      filename = filePath,
      width = imageWidth,
      height = imageHeight,
      units = "in",
      dpi = imageDpi,
      scale = NA
    )
    
  }
  
  # ----- Create video with ffmpeg -----------------------------------------------
  
  # Create a filename for the movie
  fileName <- paste0(movieFileName, ".mp4")
  
  outputDir <- path.expand(".")
  
  if ( verbose )
    loglevel <- "verbose"
  else
    loglevel <- "info"
  
  # Define system calls to ffmpeg to create video from frames
  cmd_cd <- paste0("cd ", saveDir)
  cmd_ffmpeg <- paste0(
    "ffmpeg -y -loglevel ", loglevel, " -r ", 
    frameRate, " -f image2 -s ", imageWidthPx, "x", imageHeightPx, " -i ", 
    imageNameBase, "%03d.png -vcodec libx264 -crf 25 ", 
    # https://bugzilla.mozilla.org/show_bug.cgi?id=1368063#c7
    "-pix_fmt yuv420p ",
    outputDir, "/", fileName
  )
  cmd_rm <- paste0("rm *.png")
  cmd <- paste0(cmd_cd, " && ", cmd_ffmpeg, " && ", cmd_rm)
  
  #### Make system calls
  ###warning("Calling ffmpeg to make video from frames")
  ###logger.trace(cmd)
  
  ffmpegString <- paste(capture.output(system(cmd), type = "message"), collapse = "\n")
  
  ###warning("ffmpeg output:\n\n%s\n", ffmpegString)
  
}
