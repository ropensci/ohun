test_that("plot and null", {


  data(list = "lbh1", "lbh_reference")

  # adding labels
  label_spectro(wave = lbh1,
                reference = lbh_reference[lbh_reference$sound.files == "lbh1.wav", ],
                wl = 200, ovlp = 50, flim = c(1, 10))

  # adding envelope
 a <- label_spectro(wave = lbh1,
                detection = lbh_reference[lbh_reference$sound.files == "lbh1.wav", ],
                wl = 200, ovlp = 50, flim = c(1, 10))

 expect_null(a)

})

