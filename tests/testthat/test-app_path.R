test_that("app_path prefers installed path but falls back to inst/app", {
  p <- app_path("does_not_exist")
  expect_type(p, "character")
  expect_true(grepl("app", basename(dirname(p))) || grepl("app", basename(dirname(dirname(p)))))
})


