data(lbh1, package = "ohun")
data(lbh2, package = "ohun")
tuneR::writeWave(lbh1, file.path(tempdir(),  "lbh1.wav"), extensible = FALSE) #save sound files
tuneR::writeWave(lbh2, file.path(tempdir(),  "lbh2.wav"), extensible = FALSE) #save sound files


test_that("split files", {
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

  expect_true(is.data.frame(sad))

  expect_equal(nrow(sad), 10)

  expect_length(fls, 12)

})

data(lbh1, package = "ohun")
data(lbh2, package = "ohun")
tuneR::writeWave(lbh1, file.path(tempdir(),  "lbh1.wav"), extensible = FALSE) #save sound files
tuneR::writeWave(lbh2, file.path(tempdir(),  "lbh2.wav"), extensible = FALSE) #save sound files


test_that("split files and annotations", {
  data("lbh_reference")

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

  expect_true(is.data.frame(sad))


  expect_equal(nrow(sad), 20)

  expect_length(fls, 12)

})
