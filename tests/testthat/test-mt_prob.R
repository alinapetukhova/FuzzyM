test_that("multiply matrix prob works", {
  expect_equal(multiply_matrix_prob(matrix(1:9, nrow = 3, ncol = 3),
                                    matrix(5:13, nrow = 3, ncol = 3)),
                                    rbind(c(4417,16906,42301),
                                          c(14356,52141,127618),
                                          c(30381,108492,263553)))
  expect_equal(transitive_closure_prob(rbind(c(1,0,3,0),
                                         c(0,1,0,3),
                                         c(2,0,4,0),
                                         c(0,2,0,4)))[1,1],
               1)
  expect_equal(transitive_closure_prob_max(rbind(c(1,0,3,0),
                                             c(0,1,0,3),
                                             c(2,0,4,0),
                                             c(0,2,0,4)))[1,1],
               1)
  expect_equal(probability_matrix_transitive(rbind(c(1,0,3,0),
                                                 c(0,1,0,3),
                                                 c(2,0,4,0),
                                                 c(0,2,0,4)))[1,1],
               1)

})
