test_that("output size summarized", {
  # EXAMPLES
  data("lbh_reference")

  # perfect detection
  dd <-
    diagnose_detection(
      reference = lbh_reference,
      detection = lbh_reference,
      time.diagnostics = TRUE,
      by.sound.file = FALSE
    )

  expect_equal(nrow(dd), 1)
})

test_that("output size by sound file", {
  # by sound file
  dd <-
    diagnose_detection(
      reference = lbh_reference,
      detection = lbh_reference,
      time.diagnostics = TRUE,
      by.sound.file = TRUE
    )

  expect_equal(nrow(dd), 2)

})


# perfect detection
dd <-
  diagnose_detection(
    reference = lbh_reference,
    detection = lbh_reference,
    time.diagnostics = TRUE,
    by.sound.file = FALSE
  )

test_that("right recall, precision and f1.score", {
  expect_equal(dd$recall, 1)
  expect_equal(dd$precision, 1)
  expect_equal(dd$f1.score, 1)

  dd <-
    diagnose_detection(
      reference = lbh_reference,
      detection = lbh_reference[-1,],
      time.diagnostics = TRUE,
      by.sound.file = FALSE
    )
  expect_lt(abs(dd$f1.score - 0.972973), 0.001)

})


test_that("right recall, precision and f1.score by sound files", {
  # by sound file
  dd <-
    diagnose_detection(
      reference = lbh_reference,
      detection = lbh_reference,
      time.diagnostics = TRUE,
      by.sound.file = TRUE
    )

  expect_equal(dd$recall, c(1, 1))
  expect_equal(dd$precision, c(1, 1))
  expect_equal(dd$f1.score, c(1, 1))
})
