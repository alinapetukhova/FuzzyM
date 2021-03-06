test_that("s norm reverse works", {
  expect_equal(get_snorm_reverse('Drastic Sum'), "drastic_sum_snorm_reverse")
  expect_equal(get_snorm_reverse('Bounded Sum'), "bounded_sum_snorm_reverse")
  expect_equal(get_snorm_reverse('Einstein Sum'), "einstein_sum_snorm_reverse")
  expect_equal(get_snorm_reverse('Algebraic Sum'), "algebraic_sum_snorm_reverse")
  expect_equal(get_snorm_reverse('Hamacher Sum'), "hamacher_sum_snorm_reverse")
  expect_equal(get_snorm_reverse('Max'), "max_snorm_reverse")
  expect_equal(get_snorm_reverse('Hamacher-union operator'), "hamacher_union_operator_snorm_reverse")
  expect_equal(get_snorm_reverse('Yager-union operator'), "yager_union_operator_snorm_reverse")

  expect_equal(drastic_sum_snorm_reverse(1,3), 0)
  expect_equal(bounded_sum_snorm_reverse(1,3), 2)
  expect_equal(einstein_sum_snorm_reverse(1,3), 1)
  expect_equal(algebraic_sum_snorm_reverse(3,4), -0.5)
  expect_equal(hamacher_sum_snorm_reverse(3,4), 1.54545455)
  expect_equal(max_snorm_reverse(3,4), 3)
  expect_equal(hamacher_union_operator_snorm_reverse(3,4, gammaSnorm = 0), -5)
  expect_equal(yager_union_operator_snorm_reverse(3,4, piSnorm = 0), 1)
  expect_equal(yager_union_operator_snorm_reverse(3,4, piSnorm = 0), 1)

})
