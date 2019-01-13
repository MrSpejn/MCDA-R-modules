context("Preference")


test_that("check calculating preference with basic funcion", {
  peformanceTable <- matrix(c(
    12, 16, 8,
    9,  10, 3,
    16, 12, 8,
    0,  7,  3,
    0,  6, 18
  ), nrow=5, byrow=TRUE)

  basic = criteriaFunctions[["Basic"]]
  criteriaFn = list(
    criteriaFunctions[["Level"]](2, 3, criterion_gain=TRUE),
    criteriaFunctions[["V-shape-with-indifference"]](4, 8, criterion_gain=FALSE),
    criteriaFunctions[["Gaussian"]](3, criterion_gain=TRUE)
  )

  result = calculatePreference(peformanceTable, criteriaFn)
  expect_equal(dim(result), c(5, 5, 3))
})

test_that("basic function is calculates preference correctly", {
  gain = criteriaFunctions[["Basic"]](criterion_gain=TRUE)
  loss = criteriaFunctions[["Basic"]](criterion_gain=FALSE)

  expect_equal(gain(1, 1), 0)
  expect_equal(gain(2, 1), 1)
  expect_equal(gain(1, 2), 0)

  expect_equal(loss(1, 1), 0)
  expect_equal(loss(2, 1), 0)
  expect_equal(loss(1, 2), 1)
})

test_that("U-shape function is calculates preference correctly", {
  gain = criteriaFunctions[["U-shape"]](2, criterion_gain=TRUE)
  loss = criteriaFunctions[["U-shape"]](2, criterion_gain=FALSE)

  expect_equal(gain(2, 1), 0)
  expect_equal(gain(2, 3), 0)
  expect_equal(gain(4, 2), 0)
  expect_equal(gain(4.1, 2), 1)


  expect_equal(loss(1, 2), 0)
  expect_equal(loss(3, 2), 0)
  expect_equal(loss(2, 4), 0)
  expect_equal(loss(1.99, 4), 1)

})

test_that("V-shape function is calculates preference correctly", {
  gain = criteriaFunctions[["V-shape"]](2, criterion_gain=TRUE)
  loss = criteriaFunctions[["V-shape"]](2, criterion_gain=FALSE)

  expect_equal(gain(5, 4), 0.5)
  expect_equal(gain(4, 5), 0)
  expect_equal(gain(3, 1), 1)
  expect_equal(gain(7, 1), 1)

  expect_equal(loss(4, 5), 0.5)
  expect_equal(loss(2.5, 4), 0.75)
  expect_equal(loss(8, 12), 1)
})

test_that("Level function is calculates preference correctly", {
  gain = criteriaFunctions[["Level"]](2, 3, criterion_gain=TRUE)
  loss = criteriaFunctions[["Level"]](2, 3, criterion_gain=FALSE)

  expect_equal(gain(4, 2), 0)
  expect_equal(gain(4.5, 2), 0.5)
  expect_equal(gain(6, 2), 1)

  expect_equal(loss(7, 8), 0)
  expect_equal(loss(6, 8), 0)
  expect_equal(loss(5, 8), 0.5)
})

test_that("V-shape-with-indifference function is calculates preference correctly", {
  gain = criteriaFunctions[["V-shape-with-indifference"]](2, 3, criterion_gain=TRUE)
  loss = criteriaFunctions[["V-shape-with-indifference"]](2, 3, criterion_gain=FALSE)

  expect_equal(gain(4, 2), 0)
  expect_equal(gain(4.5, 2), .5)
  expect_equal(gain(4.75, 2), .75)
  expect_equal(gain(8, 2), 1)

  expect_equal(loss(2, 4), 0)
  expect_equal(loss(2, 4.5), .5)
  expect_equal(loss(2, 9), 1)
})