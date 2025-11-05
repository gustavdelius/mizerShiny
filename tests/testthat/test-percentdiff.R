test_that("percentdiff computes expected percentage change and drops Resource", {
  harvested   <- data.frame(Species = c("A","B","Resource"), value = c(120, 80, 10))
  unharvested <- data.frame(Species = c("A","B","Resource"), value = c(100,100,10))

  res <- percentdiff(harvested, unharvested)

  expect_s3_class(res, "data.frame")
  expect_equal(colnames(res), c("Species", "percentage_diff"))
  expect_true(all(res$Species %in% c("A","B")))
  expect_equal(res$percentage_diff[res$Species=="A"], 20)
  expect_equal(res$percentage_diff[res$Species=="B"], -20)
})


