test_that("1 template", {


  data(lbh1, package = "ohun")
  data(lbh2, package = "ohun")
  tuneR::writeWave(lbh1, file.path(tempdir(),  "lbh1.wav"), extensible = FALSE) #save sound files
  tuneR::writeWave(lbh2, file.path(tempdir(),  "lbh2.wav"), extensible = FALSE) #save sound files

  data("lbh_reference", "lbh1")

  # template for the first sound file in 'lbh_reference'
  # generate template correlations
  tc <-
    template_correlator(templates = lbh_reference[1, ], path = tempdir())

  # template detection
  td <-
    template_detector(template.correlations = tc, threshold = 0.4)

  expect_s3_class(td, 'selection_table')

  expect_true(nrow(td) == 22)
  expect_s3_class(td, 'data.frame')

})


test_that("2 templates", {
  data(lbh1, package = "ohun")
  data(lbh2, package = "ohun")
  tuneR::writeWave(lbh1, file.path(tempdir(),  "lbh1.wav"), extensible = FALSE) #save sound files
  tuneR::writeWave(lbh2, file.path(tempdir(),  "lbh2.wav"), extensible = FALSE) #save sound files

  data("lbh_reference", "lbh1")

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

  expect_true(nrow(td) == 42)
  expect_s3_class(td, 'data.frame')
})
