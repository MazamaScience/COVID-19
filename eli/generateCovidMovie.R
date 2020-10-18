

generateMovie <- function(
  data = NULL,
  parameter = NULL,
  stateCodes = NULL,
  startDate = NULL,
  endDate = NULL,
  breaks = c(0, 0.005, 0.01, 0.03, 0.05, 0.16),
  imageWidth = 10,
  imageHeight = 8,
  imageDpi = 75,
  frameRate = 6,
  saveDir = NULL,
  movieFileName = NULL,
  verbose = FALSE
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(data)
  MazamaCoreUtils::stopIfNull(parameter)
  MazamaCoreUtils::stopIfNull(stateCodes)
  MazamaCoreUtils::stopIfNull(startDate)
  MazamaCoreUtils::stopIfNull(endDate)
  MazamaCoreUtils::stopIfNull(saveDir)
  MazamaCoreUtils::stopIfNull(movieFileName)
  
  if ( !("date" %in% names(data)) )
    stop("Parameter 'data' must have a column called 'date' containing the dates and times of observations.")
  
  if ( !(parameter %in% names(data)) )
    stop(sprintf("Parameter 'data' does not contain a column called %s", parameter))
  
  parseDates <- try({
    startDate <- MazamaCoreUtils::parseDatetime(startDate, timezone = "UTC")
    endDate <- MazamaCoreUtils::parseDatetime(endDate, timezone = "UTC")
  }, silent = TRUE)
  
  MazamaCoreUtils::stopOnError(parseDates, "Could not correctly parse parameters 'startDate' or 'endDate'.")

  if ( tolower(stringr::str_sub(movieFileName, -4)) == ".mp4")
    fileName <- stringr::str_sub(movieFileName, 0, -5)
  
  # ----- Generate animation ---------------------------------------------------
  
  datestamps <- 
    MazamaCoreUtils::dateSequence(startDate, endDate, timezone = "UTC") %>%
    MazamaCoreUtils::timeStamp(style = "clock", unit = "day", timezone = "UTC")
  
  imageNameBase <- "covid_confirmed_"
  
  # Convert to pixels
  imageWidthPx <- imageDpi * imageWidth
  imageHeightPx <- imageDpi * imageHeight
  
  tmap::tmap_options(show.messages = verbose, show.warnings = verbose)
  
  timestep <- 0
  for ( datestamp in datestamps ) {
    
    timestep <- timestep + 1
    fileName <- sprintf("%s%03d.png", imageNameBase, timestep)
    filePath <- file.path(saveDir, fileName)
    
    tm <- MazamaSpatialPlots::stateMap(
      data = dplyr::filter(data, date == datestamp),
      parameter = parameter,
      breaks = breaks,
      stateCode = stateCodes,
      title = datestamp
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
  
  # Define system calls to ffmpeg to create video from frames
  cmd_cd <- paste0("cd ", saveDir)
  cmd_ffmpeg <- paste0(
    "ffmpeg -y -loglevel quiet -r ", 
    frameRate, " -f image2 -s ", imageWidthPx, "x", imageHeightPx, " -i ", 
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
  
}
