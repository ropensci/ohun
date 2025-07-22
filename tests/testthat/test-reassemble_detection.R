test_that("output size summarized", {
  # load example data
  data("lbh1", "lbh2", "lbh_reference", package = "ohun")
  tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
  tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))
   
  # split annotations and files
  split_lbh_reference <- split_acoustic_data(X = lbh_reference, 
   sgmt.dur = 1.5, path = tempdir(), pb = FALSE, files = c("lbh1.wav", "lbh2.wav"))
  
  # reassemble annotations
  tc <- reassemble_detection(detection = split_lbh_reference, 
    Y = attributes(split_lbh_reference)$clip.info, pb = FALSE)
    
  # start and end are the same as in the original unsplit data
  lbh_reference <- lbh_reference[order(lbh_reference$sound.files, lbh_reference$start), ]
  all(tc$end == lbh_reference$end)
  all(tc$start == lbh_reference$start)
    
  testthat::expect_true(all(tc$start == lbh_reference$start))

  testthat::expect_true(all(tc$end == lbh_reference$end))
  
  testthat::expect_equal(nrow(tc), 19)
  
  unlink(
    list.files(
      path = tempdir(),
      pattern = "\\.wav$|\\.flac$|\\.mp3$|\\.wac$",
      ignore.case = T,
      full.names = TRUE
    )
  )
  
})


test_that("using a data frame", {
  
  # load example data
  data("lbh1", "lbh2", "lbh_reference", package = "ohun")
  tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
  tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))
  
  df_ref <- as.data.frame(lbh_reference)
  
  split_df_ref <- split_acoustic_data(X = df_ref, only.sels = TRUE, sgmt.dur = 1.5, path = tempdir(), pb = FALSE, files = c("lbh1.wav", "lbh2.wav"))
  
  Y <- split_acoustic_data(sgmt.dur = 1.5, path = tempdir(), pb = FALSE, files = c("lbh1.wav", "lbh2.wav"))
  
  # reassemble annotations
  tc <- reassemble_detection(detection = split_df_ref, Y = Y, pb = FALSE)
  
  unlink(
    list.files(
      path = tempdir(),
      pattern = "\\.wav$|\\.flac$|\\.mp3$|\\.wac$",
      ignore.case = T,
      full.names = TRUE
    )
  )
    
  # start and end are the same as in the original unsplit data
  lbh_reference <- lbh_reference[order(lbh_reference$sound.files, lbh_reference$start), ]
  all(tc$end == lbh_reference$end)
  all(tc$start == lbh_reference$start)
  
  testthat::expect_true(all(tc$start == lbh_reference$start))
  
  testthat::expect_true(all(tc$end == lbh_reference$end))
  
  testthat::expect_true(is.data.frame(tc))
  

  
  })
