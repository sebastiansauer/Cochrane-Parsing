# testing 

test_that("More than zero funs are to be sources",
          {
            expect_true(exists("files_to_source"))
            expect_gt(length(files_to_source), 0)
          })




test_that("Correct number of funs exists in global env.",
          {
            n_funs <- length(files_to_source)
          }
          )
