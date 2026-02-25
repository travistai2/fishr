test_that("biomass_index calculates correctly", {
  expect_equal(biomass_index(cpue = 10, area_swept = 5), 50)
  expect_equal(biomass_index(cpue = 20, area_swept = 2.5), 50)
})

test_that("biomass_index handles vectors", {
  expect_equal(
    biomass_index(c(10, 20, 30), c(5, 5, 5)),
    c(50, 100, 150)
  )
})

test_that("biomass_index throws error on invalid input", {
  expect_snapshot(biomass_index("ten", 5), error = TRUE)
  expect_snapshot(biomass_index(10, "five"), error = TRUE)
})


test_that("biomass_index uses verbosity when option set to TRUE", {
  withr::local_options(fishr.verbose = TRUE) # reset when this block exits

  expect_snapshot(biomass_index(10, 5))
})

test_that("biomass_index is not verbose when option set to FALSE", {
  withr::local_options(fishr.verbose = FALSE) # reset when this block exits

  expect_silent(biomass_index(10, 5))
})

