data(lbh1, package = "ohun")
data(lbh2, package = "ohun")
tuneR::writeWave(lbh1, file.path(tempdir(),  "lbh1.wav"), extensible = FALSE) #save sound files
tuneR::writeWave(lbh2, file.path(tempdir(),  "lbh2.wav"), extensible = FALSE) #save sound files


test_that("split files", {
  dd <-
    diagnose_detection(
      reference = lbh_reference[lbh_reference$sound.files != "Phae.long1.wav", ],
      detection = lbh_reference,
      time.diagnostics = TRUE,
      by.sound.file = TRUE,
      path = tempdir()
    )

  # get summary
  sdai <- summarize_diagnostic(dd)

  unlink(
    list.files(
      path = tempdir(),
      pattern = "\\.wav$|\\.flac$|\\.mp3$|\\.wac$",
      ignore.case = T,
      full.names = TRUE
    )
  )

  expect_s3_class(sdai, 'data.frame')

  expect_equal(nrow(sdai), 1)

  expect_equal(ncol(sdai), 10)

})
