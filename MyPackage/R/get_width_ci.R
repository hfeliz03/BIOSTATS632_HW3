get_width_ci <- function(est, iso_code, coverage = 95) {
  # Filter data for the specified country
  d <- est[est$iso == iso_code, ]

  # Choose appropriate columns based on coverage
  if (coverage == 95) {
    # For 95% interval, use U95 and L95 columns
    d$width <- d$U95 - d$L95
  } else if (coverage == 80) {
    # For 80% interval, use U80 and L80 columns
    d$width <- d$U80 - d$L80
  } else {
    stop("Coverage must be either 80 or 95.")
  }

  # Return a data frame with just the Year and width columns
  result <- d[, c("Year", "width")]

  # Convert to tibble for a cleaner output (optional)
  result <- tibble::as_tibble(result)

  return(result)
}
