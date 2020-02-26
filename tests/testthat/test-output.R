test_that("das_read output has expected column names and classes", {
  y.read <- das_read(system.file("das_sample.das", package = "swfscDAS"))
  y.read2 <- as_das_dfr(data.frame(y.read))

  # Same as in as_das_dfr()
  exp.class <- list(
    Event = "character",
    EffortDot = "logical",
    DateTime = c("POSIXct", "POSIXt"),
    Lat = "numeric",
    Lon = "numeric",
    Data1 = "character",
    Data2 = "character",
    Data3 = "character",
    Data4 = "character",
    Data5 = "character",
    Data6 = "character",
    Data7 = "character",
    Data8 = "character",
    Data9 = "character",
    EventNum = "integer",
    file_das = "character",
    line_num = "integer"
  )

  expect_identical(exp.class, lapply(y.read, class))
  expect_identical(exp.class, lapply(y.read2, class))
})


test_that("das_process output has expected column names and classes", {
  y.proc <- das_process(system.file("das_sample.das", package = "swfscDAS"))
  y.proc2 <- as_das_df(data.frame(y.proc))

  # Same as in as_das_df()
  exp.class <- list(
    Event = "character",
    DateTime = c("POSIXct", "POSIXt"),
    Lat = "numeric",
    Lon = "numeric",
    OnEffort = "logical",
    Cruise = "numeric",
    Mode = "character",
    EffType = "character",
    Course = "numeric",
    Bft = "numeric",
    SwellHght = "numeric",
    RainFog = "numeric",
    HorizSun = "numeric",
    VertSun = "numeric",
    Glare = "logical",
    Vis = "numeric",
    Data1 = "character",
    Data2 = "character",
    Data3 = "character",
    Data4 = "character",
    Data5 = "character",
    Data6 = "character",
    Data7 = "character",
    Data8 = "character",
    Data9 = "character",
    EffortDot = "logical",
    EventNum = "integer",
    file_das = "character",
    line_num = "integer"
  )

  expect_identical(exp.class, lapply(y.proc, class))
  expect_identical(exp.class, lapply(y.proc2, class))
})


test_that("das_sight output has expected column names and classes", {
  y.proc <- das_process(system.file("das_sample.das", package = "swfscDAS"))
  y.sight <- das_sight(y.proc)
  y.sight.multi <- das_sight(y.proc, mixed.multi = TRUE)

  exp.name <- c(
    "Event", "DateTime", "Lat", "Lon", "OnEffort",
    "Cruise", "Mode", "EffType", "Course", "Bft", "SwellHght", "RainFog",
    "HorizSun", "VertSun", "Glare", "Vis",
    "EffortDot", "EventNum", "file_das", "line_num",
    "SightNo", "Obs", "Bearing", "Reticle", "DistNm",
    "Cue", "Method", "Photos", "Birds", "Prob", "Mixed", "GsTotal"
  )

  exp.name.nomulti <- c(
    "Sp1", "Sp2","Sp3", "Sp4", "Sp1Perc", "Sp2Perc", "Sp3Perc", "Sp4Perc",
    "GsSp1", "GsSp2", "GsSp3", "GsSp4", "ResightCourse",
    "TurtleSp", "TurtleNum", "TurtleJFR", "TurtleAge", "TurtleCapt",
    "BoatType", "BoatNum"
  )

  exp.name.multi <- c(
    "Species", "GsSpecies", "ResightCourse",
    "TurtleSp", "TurtleNum", "TurtleJFR", "TurtleAge", "TurtleCapt",
    "BoatType", "BoatNum"
  )

  expect_identical(c(exp.name, exp.name.nomulti), names(y.sight))
  expect_identical(c(exp.name, exp.name.multi), names(y.sight.multi))
})


test_that("das_sight output has expected column names and classes with extra column", {
  y.proc <- das_process(system.file("das_sample.das", package = "swfscDAS"))
  y.proc$testrr <- 4
  y.sight <- das_sight(y.proc)
  y.sight.multi <- das_sight(y.proc, mixed.multi = TRUE)

  exp.name <- c(
    "Event", "DateTime", "Lat", "Lon", "OnEffort",
    "Cruise", "Mode", "EffType", "Course", "Bft", "SwellHght", "RainFog",
    "HorizSun", "VertSun", "Glare", "Vis",
    "EffortDot", "EventNum", "file_das", "line_num", "testrr",
    "SightNo", "Obs", "Bearing", "Reticle", "DistNm",
    "Cue", "Method", "Photos", "Birds", "Prob", "Mixed", "GsTotal"
  )

  exp.name.nomulti <- c(
    "Sp1", "Sp2","Sp3", "Sp4", "Sp1Perc", "Sp2Perc", "Sp3Perc", "Sp4Perc",
    "GsSp1", "GsSp2", "GsSp3", "GsSp4", "ResightCourse",
    "TurtleSp", "TurtleNum", "TurtleJFR", "TurtleAge", "TurtleCapt",
    "BoatType", "BoatNum"
  )

  exp.name.multi <- c(
    "Species", "GsSpecies", "ResightCourse",
    "TurtleSp", "TurtleNum", "TurtleJFR", "TurtleAge", "TurtleCapt",
    "BoatType", "BoatNum"
  )

  expect_identical(c(exp.name, exp.name.nomulti), names(y.sight))
  expect_identical(c(exp.name, exp.name.multi), names(y.sight.multi))
})
