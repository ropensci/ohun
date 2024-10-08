test_that("nothing to merge", {
  mo <- merge_overlaps(lbh_reference)


  expect_s3_class(mo, 'selection_table')

  expect_equal(nrow(mo), 19)

})


test_that("merging", {
  # modified lbh_selec_table to make selections overlap
  W <- lbh_reference[c(3, 10),]
  end <- W$end
  W$end <- W$end - 0.05
  W$start <- W$start - 0.06
  lbh_reference2 <-
    rbind(as.data.frame(lbh_reference[c(3, 10),]), W)

  lbh_reference2$selec <- 1:nrow(lbh_reference2)

  # merging
  mo <- merge_overlaps(X = lbh_reference2)


  expect_s3_class(mo, 'data.frame')
  expect_equal(nrow(mo), 2)

})
