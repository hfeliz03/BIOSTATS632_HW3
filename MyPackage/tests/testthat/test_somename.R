test_that("get_width_ci correctly returns 95% interval widths", {
  # Dummy data
  est_dummy <- tibble::tibble(
    Year = 2000:2002,
    iso = c(4, 4, 4),
    U95 = c(5.2, 6.3, 7.1),
    L95 = c(3.2, 4.1, 5.0),
    U80 = c(4.8, 5.8, 6.8),
    L80 = c(3.5, 4.5, 5.5)
  )

  # Expected result
  expected_widths <- tibble::tibble(
    Year = 2000:2002,
    width = c(2.0, 2.2, 2.1)
  )

  # Call the function
  result <- get_width_ci(est_dummy, iso_code = 4, coverage = 95)

  # Check the result
  expect_equal(result, expected_widths)
})

test_that("get_width_ci returns NA for missing U95/L95 when coverage = 95", {
  # Dummy data with missing U95 and L95
  est_dummy_na <- tibble::tibble(
    Year = 2000:2002,
    iso = c(4, 4, 4),
    U95 = c(NA, 6.3, 7.1),
    L95 = c(3.2, NA, 5.0),
    U80 = c(4.8, 5.8, 6.8),
    L80 = c(3.5, 4.5, 5.5)
  )

  # Expected result
  expected_widths_na <- tibble::tibble(
    Year = 2000:2002,
    width = c(NA, NA, 2.1)
  )

  # Call the function
  result <- get_width_ci(est_dummy_na, iso_code = 4, coverage = 95)

  # Check the result
  expect_equal(result, expected_widths_na)
})
