
test_that("parse_dois should return a tibble",
          {
            testurl <- get_dummy_data()[[1]]
            out <- parse_dois(testurl)
            expect_type(out, "list")
            expect_s3_class(out, "data.frame")
          })



test_that("parse_dois should return a tibble with more than zero rows",
          {
            testurl <- get_dummy_data()[[1]]
            out <- parse_dois(testurl)
            expect_gt(nrow(out), 0)
          })


str(out)
class(out)
