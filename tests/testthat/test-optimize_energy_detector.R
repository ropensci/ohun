data(lbh1, package = "ohun")
data(lbh2, package = "ohun")
tuneR::writeWave(lbh1, file.path(tempdir(),  "lbh1.wav"), extensible = FALSE) #save sound files
tuneR::writeWave(lbh2, file.path(tempdir(),  "lbh2.wav"), extensible = FALSE) #save sound files


test_that("nothing to merge", {
  data("lbh_reference", "lbh1")

  # using smoothing and minimum duration
  oed <-
    optimize_energy_detector(
      reference = lbh_reference,
      path = tempdir(),
      threshold = c(6, 20),
      hop.size = 6.8,
      peak.amplitude = 40,
      bp = c(2, 9),
      min.duration = c(0.09, 0.1),
      by.sound.file = FALSE
    )

  expect_s3_class(oed, 'data.frame')

  expect_equal(nrow(oed), 4)

  expect_true(all(oed$recall == 1))

  expect_true(all(oed$precision > 0.22))
})


test_that("including previous output in new call", {
  oed <-
    optimize_energy_detector(
      reference = lbh_reference,
      path = tempdir(),
      threshold = c(6, 20),
      hop.size = 6.8,
      peak.amplitude = 40,
      bp = c(2, 9),
      min.duration = c(0.09, 0.1),
      by.sound.file = FALSE
    )

  oed <-
    optimize_energy_detector(
      reference = lbh_reference,
      threshold = 10,
      hold.time = c(15, 20),
      previous.output = oed,
      bp = c(2, 9),
      hop.size = 6.8,
      path = tempdir()
    )

  expect_equal(nrow(oed), 6)

  unlink(
    list.files(
      path = tempdir(),
      pattern = "\\.wav$|\\.flac$|\\.mp3$|\\.wac$",
      ignore.case = T,
      full.names = TRUE
    )
  )

})
