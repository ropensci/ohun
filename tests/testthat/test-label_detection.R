data(lbh1, package = "ohun")
data(lbh2, package = "ohun")

#save sound files
tuneR::writeWave(lbh1, file.path(tempdir(),  "lbh1.wav"), extensible = FALSE)
tuneR::writeWave(lbh2, file.path(tempdir(),  "lbh2.wav"), extensible = FALSE)

test_that("1 false negative", {
  # EXAMPLES
  lsr <- selection_table(lbh_reference, path = tempdir())

  # an extra one in detection (1 false positive)
  ld <- label_detection(reference = lsr[-1,], detection = lsr)

  expect_s3_class(ld, 'selection_table')

  expect_equal(nrow(ld), 19)

  expect_equal(as.vector(table(ld$detection.class)), c(1, 18))
})


test_that("perfect detection", {
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

  expect_s3_class(ld, 'selection_table')

  expect_match(ld$detection.class, 'true.positive')

})


test_that("bipartite matching", {


  ref <- data.frame(sound.files = "1.wav",
                    start = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                    end = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5)
  )

  ref$selec <- 1:nrow(ref)

  # detection
  det <- data.frame(sound.files = "1.wav",
                    start = c(0.75, 1.4, 3.2, 4.25, 6.2, 7.1, 8.25, 1),
                    end = c(1.25, 3.1, 4.1, 4.8, 6.5, 7.5, 9.25, 1.5)
  )

  det$selec <- 1:nrow(det)

  # diagnose
  ld <- label_detection(reference = ref, detection = det)

  expect_equal(sum(ld$detection.class == "true.positive"), 3)


})
