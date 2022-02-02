test_that("calc reverse max norm works", {

  expect_equal(typeof(calc_reverse_task(matrix(1:9, nrow = 3, ncol = 3),c(1,5,9),1,1,1,1)),
               "double")

})
