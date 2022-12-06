

test_that("1 false negative", {
  data(lbh1, package = "ohun")
  data(lbh2, package = "ohun")
  tuneR::writeWave(lbh1, file.path(tempdir(),  "lbh1.wav"), extensible = FALSE) #save sound files
  tuneR::writeWave(lbh2, file.path(tempdir(),  "lbh2.wav"), extensible = FALSE) #save sound files

  # EXAMPLES
  lsr <- selection_table(lbh_reference, path = tempdir())

  # an extra one in detection (1 false positive)
  ld <- label_detection(reference = lsr[-1,], detection = lsr)

  expect_true(is_selection_table(ld))

  expect_true(nrow(ld) == 19)

  expect_equal(as.vector(table(ld$detection.class)), c(1, 18))
})


test_that("perfect detection", {
  data(lbh1, package = "ohun")
  data(lbh2, package = "ohun")
  tuneR::writeWave(lbh1, file.path(tempdir(),  "lbh1.wav"), extensible = FALSE) #save sound files
  tuneR::writeWave(lbh2, file.path(tempdir(),  "lbh2.wav"), extensible = FALSE) #save sound files

  # EXAMPLES
  lsr <- selection_table(lbh_reference, path = tempdir())

  ld <- label_detection(reference = lsr, detection = lsr)

  unlink(
    list.files(
      path = tempdir(),
      pattern = "\\.wav$|\\.flac$|\\.mp3$|\\.wac$",
      ignore.case = T,
      full.names = TRUE
    )
  )

  expect_true(is_selection_table(ld))

  expect_true(all(ld$detection.class == "true.positive"))

})
