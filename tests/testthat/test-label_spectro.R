test_that("plot and null", {


  data(list = "lbh1", "lbh_reference")


 disp_label_spectro <- function() label_spectro(wave = lbh1,
                                               detection = lbh_reference[lbh_reference$sound.files == "lbh1.wav", ],
                                               wl = 200, ovlp = 50, flim = c(1, 10))
 
 vdiffr::expect_doppelganger("get_templates", disp_label_spectro)
 
 
 disp_label_spectro_env <- function() label_spectro(wave = lbh1,
                                                detection = lbh_reference[lbh_reference$sound.files == "lbh1.wav", ],
                                                wl = 200, ovlp = 50, flim = c(1, 10), envelope = TRUE)
 
 vdiffr::expect_doppelganger("get_templates_env", disp_label_spectro_env)
 
 
})

