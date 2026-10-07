data(lbh1, package = "ohun")
data(lbh2, package = "ohun")
data(lbh_reference, package = "ohun")
#save sound files
tuneR::writeWave(lbh1, file.path(tempdir(),  "lbh1.wav"), extensible = FALSE)
tuneR::writeWave(lbh2, file.path(tempdir(),  "lbh2.wav"), extensible = FALSE)

test_that("1 template", {

  # template for the first sound file in 'lbh_reference'
  # generate template correlations
  tc <-
    template_correlator(templates = lbh_reference[1, ], path = tempdir())

  # template detection
  td <-
    template_detector(template.correlations = tc, threshold = 0.4)

  expect_s3_class(td, 'selection_table')

  expect_s3_class(td, 'data.frame')

  expect_equal(nrow(td), 22)

})

test_that("saving txt files", {

  # template for the first sound file in 'lbh_reference'
  # generate template correlations
  tc <- template_correlator(templates = lbh_reference[1, ], path = tempdir())
  
  # template detection
  td <-
    template_detector(template.correlations = tc, threshold = 0.4, save.txt = TRUE, path = tempdir())
  
  txts <- list.files(path = tempdir(), pattern = "txt$", full.names = TRUE)
  
  # one txt per template/sound file combination (names are "template/sound file")
  expect_equal(length(txts), length(tc) - 1)   # last element of tc is call_info
  
  expect_s3_class(td, 'selection_table')
  
  expect_s3_class(td, 'data.frame')
  
  expect_equal(nrow(td), 22)
  
  unlink(txts)
})


test_that("saving txt files no detections", {
  
  # template for the first sound file in 'lbh_reference'
  # generate template correlations (1 template x 2 sound files)
  tc <- template_correlator(templates = lbh_reference[1, ], path = tempdir())
  
  # template detection
  td <-
    template_detector(template.correlations = tc, threshold = 0.99, save.txt = TRUE, path = tempdir())
  
  txts <- list.files(path = tempdir(), pattern = "txt$", full.names = TRUE)
  
  # one (empty) txt per template/sound file combination
  expect_equal(length(txts), length(tc) - 1)   # last element of tc is call_info
  
  # txt files only contain the header
  for (f in txts) {
    expect_equal(nrow(read.table(f, sep = "\t", header = TRUE)), 0)
  }
  
  # no placeholder rows when nothing is detected
  expect_s3_class(td, 'data.frame')
  
  expect_equal(nrow(td), 0)
  
  # running again with the same threshold recomputes (default resume = FALSE) and gets the same result
  td2 <-
    template_detector(template.correlations = tc, threshold = 0.99, save.txt = TRUE, path = tempdir())

  expect_equal(nrow(td2), 0)

  unlink(txts)
})

test_that("resume does not reuse stale results from a different threshold, but resume = TRUE does", {

  # template for the first sound file in 'lbh_reference'
  tc <- template_correlator(templates = lbh_reference[1, ], path = tempdir())

  # low threshold, save txt files with 22 detections
  td1 <-
    template_detector(template.correlations = tc, threshold = 0.4, save.txt = TRUE, path = tempdir())

  expect_equal(nrow(td1), 22)

  # high threshold on the same path: default resume = FALSE must recompute, not reuse the 22-row cache
  td2 <-
    template_detector(template.correlations = tc, threshold = 0.99, save.txt = TRUE, path = tempdir())

  expect_equal(nrow(td2), 0)

  # same high threshold again with resume = TRUE: now it is correct to reuse the cached (0-row) result
  td3 <-
    template_detector(template.correlations = tc, threshold = 0.99, save.txt = TRUE, resume = TRUE, path = tempdir())

  expect_equal(nrow(td3), 0)

  unlink(list.files(path = tempdir(), pattern = "txt$", full.names = TRUE))
})



test_that("2 templates", {

  # template for the fourth sound file in 'lbh_reference'
  # generate template correlations
  tc <- template_correlator(templates = lbh_reference[c(1, 11), ]
                            , path = tempdir())

  # template detection
  td <-
    template_detector(template.correlations = tc, threshold = 0.4)
  unlink(
    list.files(
      path = tempdir(),
      pattern = "\\.wav$|\\.flac$|\\.mp3$|\\.wac$",
      ignore.case = T,
      full.names = TRUE
    )
  )


  expect_s3_class(td, 'selection_table')

  expect_s3_class(td, 'data.frame')

  expect_equal(nrow(td), 42)
})

unlink(c(file.path(tempdir(),  "lbh1.wav"), file.path(tempdir(),  "lbh2.wav")))
