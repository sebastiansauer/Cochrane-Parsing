
test_that("parse_individual_parts should return a tibble",
          {
            testurl <- get_dummy_data()[[1]]
            out <- parse_individual_parts(testurl)
            expect_type(out, "list")
            expect_s3_class(out, "data.frame")
          })



test_that("parse_individual_parts should return a tibble with more than zero rows",
          {
            testurl <- get_dummy_data()[[1]]
            out <- parse_individual_parts(testurl)
            expect_gt(nrow(out), 0)
          })