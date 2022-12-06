data(lbh1, package = "ohun")
data(lbh2, package = "ohun")
tuneR::writeWave(lbh1, file.path(tempdir(),  "lbh1.wav"), extensible = FALSE) #save sound files
tuneR::writeWave(lbh2, file.path(tempdir(),  "lbh2.wav"), extensible = FALSE) #save sound files


test_that("default output", {
  fr <- feature_reference(reference = lbh_reference)

  expect_true(is.matrix(fr))

  expect_true(nrow(fr) == 5)

  expect_true(ncol(fr) == 3)
})

test_that("when providing recordings", {
  fr <- feature_reference(reference = lbh_reference, path = tempdir())

  expect_true(is.matrix(fr))

  expect_true(nrow(fr) == 7)

  expect_true(ncol(fr) == 3)
})

test_that("by sound file", {
  fr <-
    feature_reference(reference = lbh_reference,
                      path = tempdir(),
                      by.sound.file = TRUE)

  unlink(
    list.files(
      path = tempdir(),
      pattern = "\\.wav$|\\.flac$|\\.mp3$|\\.wac$",
      ignore.case = T,
      full.names = TRUE
    )
  )

  expect_true(is.data.frame(fr))

  expect_true(nrow(fr) == 2)

  expect_true(ncol(fr) == 18)

  expect_true(!anyNA(fr))
})
