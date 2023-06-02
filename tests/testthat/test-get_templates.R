data(lbh1, package = "ohun")
data(lbh2, package = "ohun")
tuneR::writeWave(lbh1, file.path(tempdir(),  "lbh1.wav"), extensible = FALSE) #save sound files
tuneR::writeWave(lbh2, file.path(tempdir(),  "lbh2.wav"), extensible = FALSE) #save sound files


test_that("measuring PCA on the fly", {
  template <-
    get_templates(reference = lbh_reference,
                  n.sub.spaces =  3,
                  path = tempdir())

  expect_s3_class(template, 'data.frame')

  expect_equal(nrow(template), 4)

})


test_that("measuring PCA a priori", {
  # with custom acoustic space
  spectral_parameters <-
    spectro_analysis(lbh_reference, path = tempdir())

  # remove columns with NAs
  spectral_parameters <-
    spectral_parameters[,!sapply(spectral_parameters, anyNA)]

  # get PCA
  pca <- stats::prcomp(spectral_parameters[, 2:27], scale. = TRUE)


  template <-
    get_templates(
      reference = lbh_reference,
      n.sub.spaces = 4,
      acoustic.space = pca$x[, 1:2],
      path = tempdir()
    )

  unlink(
    list.files(
      path = tempdir(),
      pattern = "\\.wav$|\\.flac$|\\.mp3$|\\.wac$",
      ignore.case = T,
      full.names = TRUE
    )
  )

  expect_s3_class(template, 'selection_table')

  expect_equal(nrow(template), 5)

})
