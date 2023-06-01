data(lbh1, package = "ohun")
data(lbh2, package = "ohun")
data(lbh_reference, package = "ohun")

#save sound files
tuneR::writeWave(lbh1, file.path(tempdir(),  "lbh1.wav"), extensible = FALSE)
tuneR::writeWave(lbh2, file.path(tempdir(),  "lbh2.wav"), extensible = FALSE)


test_that("peak amplitude works", {
  detec1 <-
    energy_detector(
      files = c("lbh1.wav", "lbh2.wav"),
      path = tempdir(),
      threshold = 0.06,
      hop.size = 6.8,
      bp = c(2, 9),
      min.duration = 0.09,
      smooth = 10,
      peak.amplitude = 10,
      pb = FALSE
    )

  expect_equal(nrow(detec1), 12)
})

test_that("time diagnostics", {
  # diagnose detection
  detec1 <-
    energy_detector(
      files = c("lbh1.wav", "lbh2.wav"),
      path = tempdir(),
      threshold = 0.06,
      hop.size = 6.8,
      bp = c(2, 9),
      min.duration = 0.09,
      smooth = 10,
      peak.amplitude = 80,
      pb = FALSE
    )

  expect_equal(nrow(detec1), 3)

})

test_that("using smoothing and minimum duration", {
  # using smoothing and minimum duration
  detec1 <- energy_detector(
    path = tempdir(),
    threshold = 0.07,
    bp = c(2, 9),
    hop.size = 6.8,
    min.duration = 0.09,
    smooth = 7,
    pb = FALSE
  )

  expect_equal(nrow(detec1), 9)

})

test_that("using hold time", {
  detec2 <-
    energy_detector(
      files = c("lbh1.wav", "lbh2.wav"),
      threshold = 0.10,
      hold.time = 0.15,
      bp = c(2, 9),
      hop.size = 6.8,
      path = tempdir(),
      pb = FALSE
    )

  expect_equal(nrow(detec2), 4)

})



test_that("calculate envelopes first", {
  envs <- get_envelopes(
    files = c("lbh1.wav", "lbh2.wav"),
    bp = c(2, 9),
    hop.size = 6.8,
    path = tempdir()
  )
  detec <-
    energy_detector(
      envelopes = envs,
      threshold = 0.10,
      hold.time = 0.15,
      min.duration = 0.05,
      pb = FALSE
    )

  expect_equal(nrow(detec), 4)

})



test_that("convert files to flac", {

  if (Sys.info()[1] != "Windows"){
   warbleR::wav_2_flac(path = tempdir())

  # change sound file extension to flac
  flac_reference <- lbh_reference
  flac_reference$sound.files <-
    gsub(".wav", ".flac", flac_reference$sound.files)


  detec4 <-
    energy_detector(
      files = c("lbh1.flac", "lbh2.flac"),
      path = tempdir(),
      threshold = 0.06,
      smooth = 6.8,
      bp = c(2, 9),
      hop.size = 6.8,
      min.duration = 0.09,
      pb = FALSE
    )

  unlink(
    list.files(
      path = tempdir(),
      pattern = "\\.wav$|\\.flac$|\\.mp3$|\\.wac$",
      ignore.case = T,
      full.names = TRUE
    )
  )

  expect_equal(nrow(detec4), 8)} else
    expect_true(TRUE)

})
