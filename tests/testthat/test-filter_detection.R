data(lbh1, package = "ohun")
data(lbh2, package = "ohun")
tuneR::writeWave(lbh1, file.path(tempdir(),  "lbh1.wav"), extensible = FALSE) #save sound files
tuneR::writeWave(lbh2, file.path(tempdir(),  "lbh2.wav"), extensible = FALSE) #save sound files


test_that("default output", {
  data("lbh_reference")

  # generate template correlations
  tc <-
    template_correlator(templates = lbh_reference[1,],
                        path = tempdir(),
                        files = "lbh1.wav")

  # template detection
  td <-
    template_detector(template.correlations = tc, threshold = 0.12)

  # this detection generates 2 split positives
  diagnose_detection(reference = lbh_reference[lbh_reference == "lbh1.wav",], detection = td)

  # label detection
  ltd <-
    label_detection(reference = lbh_reference[lbh_reference == "lbh1.wav",], detection = td)

  # now they can be filter to keep the detection with the highest score for each split
  ltd <- selection_table(ltd, path = tempdir(), pb = FALSE)

  ftd <- filter_detection(detection = ltd)

  unlink(
    list.files(
      path = tempdir(),
      pattern = "\\.wav$|\\.flac$|\\.mp3$|\\.wac$",
      ignore.case = T,
      full.names = TRUE
    )
  )

  expect_s3_class(ftd, 'data.frame')

  expect_equal(nrow(ftd), 43)

  expect_equal(ncol(ftd), 9)
})
