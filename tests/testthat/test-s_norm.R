test_that("s norm works", {
  expect_equal(typeof(get_snorm('Bounded Sum')), "closure")
  expect_equal(get_snorm('Drastic Sum')(1,5), 1)
  expect_equal(get_snorm('Bounded Sum')(1,5), 1)
  expect_equal(get_snorm('Einstein Sum')(1,5), 1)
  expect_equal(get_snorm('Algebraic Sum')(1,5), 1)
  expect_equal(get_snorm('Hamacher Sum')(1,5), 1)
  expect_equal(get_snorm('Max')(1,5), 5)
  expect_equal(get_snorm('Hamacher-union operator')(1,5,gammaSnorm=0), 1)
  expect_equal(get_snorm('Yager-union operator')(1,5,piSnorm=0), 1)

})

