test_that("split files", {
  data(lbh1, package = "ohun")
  data(lbh2, package = "ohun")
  #save sound files
  tuneR::writeWave(lbh1, file.path(tempdir(),  "lbh1.wav"), extensible = FALSE)
  tuneR::writeWave(lbh2, file.path(tempdir(),  "lbh2.wav"), extensible = FALSE)


  #split files in 1 s files
  sad <- split_acoustic_data(sgmt.dur = 1, path = tempdir())

  # Check this folder
  fls <- list.files(path = tempdir(), pattern = ".wav$")

  unlink(
    list.files(
      path = tempdir(),
      pattern = "\\.wav$|\\.flac$|\\.mp3$|\\.wac$",
      ignore.case = T,
      full.names = TRUE
    )
  )

  expect_s3_class(sad, 'data.frame')

  expect_equal(nrow(sad), 10)

  expect_length(fls, 12)

})


test_that("split files and annotations", {
  data(lbh1, package = "ohun")
  data(lbh2, package = "ohun")
  data(lbh_reference, package = "ohun")
  #save sound files
  tuneR::writeWave(lbh1, file.path(tempdir(),  "lbh1.wav"), extensible = FALSE)
  tuneR::writeWave(lbh2, file.path(tempdir(),  "lbh2.wav"), extensible = FALSE)

  #split files in 1 s files
  sad <-
    split_acoustic_data(sgmt.dur = 1,
                        path = tempdir(),
                        X = lbh_reference)

  # Check this folder
  fls <- list.files(path = tempdir(), pattern = ".wav$")

  unlink(
    list.files(
      path = tempdir(),
      pattern = "\\.wav$|\\.flac$|\\.mp3$|\\.wac$",
      ignore.case = T,
      full.names = TRUE
    )
  )

  expect_s3_class(sad, 'data.frame')

  expect_equal(nrow(sad), 20)

  expect_length(fls, 12)

})
