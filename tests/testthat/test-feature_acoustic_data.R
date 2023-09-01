data(lbh1, package = "ohun")
data(lbh2, package = "ohun")
# save sound files
tuneR::writeWave(lbh1, file.path(tempdir(),  "lbh1.wav"), extensible = FALSE)
tuneR::writeWave(lbh2, file.path(tempdir(),  "lbh2.wav"), extensible = FALSE)

test_that("default output", {
  expect_null(fad <- summarize_acoustic_data(path = tempdir()))

  unlink(
    list.files(
      path = tempdir(),
      pattern = "\\.wav$|\\.flac$|\\.mp3$|\\.wac$",
      ignore.case = T,
      full.names = TRUE
    )
  )

  expect_error(summarize_acoustic_data(path = tempdir()))

})
