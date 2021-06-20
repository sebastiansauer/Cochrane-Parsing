# test init


test_that("Init gives back character vector",{
  expect_type(init(), "character")
})


test_that("Detect dois in result of init()",{
  doi_found <- str_detect(init(), "doi")
  expect_true(doi_found)
})