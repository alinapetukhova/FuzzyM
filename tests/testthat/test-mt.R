test_that("positive matrix calc works", {

  expect_equal(positive_matrix_calc(matrix(1:4, nrow = 2, ncol = 2)),
               rbind(c(1,0,3,0),
                     c(0,1,0,3),
                     c(2,0,4,0),
                     c(0,2,0,4)))
  expect_equal(positive_matrix_calc(matrix(-4:-1, nrow = 2, ncol = 2)),
               rbind(c(0,4,0,2),
                     c(4,0,2,0),
                     c(0,3,0,1),
                     c(3,0,1,0)))

  expect_equal(eigen_module(matrix(1:4, nrow = 2, ncol = 2)),
               c(5.3722813,0.3722813))
  expect_equal((transitive_closure(rbind(c(1,0,3,0),
                                        c(0,1,0,3),
                                        c(2,0,4,0),
                                        c(0,2,0,4)), tnorm=1, snorm=1, snormMatrix=1,
                                                                   gammaTnormMean=0, algaTnorm=0, gammaTnorm=0, piTnorm=0,
                                                                   gammaSnorm=0, piSnorm=0)[1,2]),
               0)

  expect_equal((matrix_transitive_join(matrix(1:4, nrow = 2, ncol = 2), snorm=1, gammaSnorm=0, piSnorm=0)[1,2]),
               -1)

  expect_equal((consonanse_dissonanse(matrix(1:4, nrow = 2, ncol = 2))[1]),
               1)

  expect_equal(cross_consonanse(matrix(1, nrow = 7, ncol = 14))[1,1],
               1)
  expect_equal(cross_dissonanse(matrix(1, nrow = 7, ncol = 14))[1,1],
               0)
  expect_equal(cross_positive_influence(matrix(1, nrow = 7, ncol = 14))[1,1],
               1)
  expect_equal(cross_negative_influence(matrix(1, nrow = 7, ncol = 14))[1,1],
               -1)
  expect_equal(impuls_vector(c(1,0), matrix(1, nrow = 7, ncol = 14))[1],
               1)
  expect_equal(multiply_vector(matrix(1, nrow = 7, ncol = 14),c(1,0))[1],
               1)

  expect_equal(multiply_matrix(matrix(1:4, nrow = 2, ncol = 2), matrix(1:4, nrow = 2, ncol = 2), tnorm=1, snorm=1, gammaTnormMean=0, algaTnorm=0, gammaTnorm=0, piTnorm=0,
                               gammaSnorm=0, piSnorm=0)[1,1],
               1)
  expect_equal(maximum_matrix(matrix(1:4, nrow = 2, ncol = 2), matrix(1:4, nrow = 2, ncol = 2))[1,2],
               3)
  expect_equal(ik_pos_maximum(matrix(1:4, nrow = 2, ncol = 2), matrix(1:4, nrow = 2, ncol = 2),1,1)[1],
               1)
  expect_equal(ik_neg_maximum(matrix(1:4, nrow = 2, ncol = 2), matrix(1:4, nrow = 2, ncol = 2),1,1)[1],
               1)






})
