# plot_cp <- function(dat, est, iso_code, CI = 95) {
#   # Error (a): Check if `dat` contains required variables
#   if (!all(c("division_numeric_code", "Year", "Indicator") %in% colnames(dat))) {
#     missing_vars <- setdiff(c("division_numeric_code", "Year", "Indicator"), colnames(dat))
#     stop(paste("Input data file dat must contain variable", paste(missing_vars, collapse = " and ")))
#   }
#
#   # Error (a): Check if `est` contains required variables
#   if (!"division_numeric_code" %in% colnames(est)) {
#     stop("Input data file dat and estimates file est must contain variable iso.")
#   }
#
#   # Error (b): Check if `iso_code` is found in `dat` and `est`
#   if (!division_numeric_code %in% dat$division_numeric_code) {
#     stop(paste("iso_code", division_numeric_code, "is not found in input data file dat."))
#   }
#   if (!iso %in% est$iso) {
#     stop(paste("iso_code", iso, "is not found in estimates file est."))
#   }
#
#   # Error (c): Check if `contraceptive_use_modern` in `dat` is numeric
#   if (!is.numeric(dat$contraceptive_use_modern)) {
#     stop("Input contraceptive_use_modern in data file dat must be numeric.")
#   }
#
#   # Error (d): Check if CI is valid
#   if (!is.na(CI) && !(CI %in% c(80, 95))) {
#     stop("CI must be 80, 95, or NA.")
#   }
#
#   # Filter `est` for the given `iso_code`
#   est_filtered <- est %>%
#     filter(iso == iso_code)
#
#   # Filter `dat` for the given `iso_code`
#   observed_filtered <- dat %>%
#     filter(iso == iso_code) %>%
#     mutate(
#       Year = (start_date + end_date) / 2,
#       cp = contraceptive_use_modern * 100
#     )
#
#   # Create the base plot
#   p <- ggplot(est_filtered, aes(x = Year, y = Median)) +
#     geom_line(color = "blue") +
#     geom_point(data = observed_filtered, aes(x = Year, y = cp), color = "black", size = 2) +
#     labs(
#       x = "Time", y = "Modern use (%)",
#       title = unique(est_filtered$`Country or area`)
#     )
#
#   # Add confidence intervals if CI is specified
#   if (!is.na(CI)) {
#     if (CI == 95) {
#       p <- p + geom_ribbon(aes(ymin = L95, ymax = U95), alpha = 0.2, fill = "blue")
#     } else if (CI == 80) {
#       p <- p + geom_ribbon(aes(ymin = L80, ymax = U80), alpha = 0.2, fill = "blue")
#     }
#   }
#
#   return(p)
# }
plot_cp <- function(dat, est, iso_code, CI = 95) {
  # Define required columns
  required_vars_dat <- c("iso", "year", "cp")
  required_vars_est <- c("iso", "Year", "Median", "U95", "L95")

  # Error (a): Check if `dat` contains required variables
  missing_vars_dat <- setdiff(required_vars_dat, colnames(dat))
  if (length(missing_vars_dat) > 0) {
    stop(paste("Input data file dat must contain variable(s):", paste(missing_vars_dat, collapse = ", ")))
  }

  # Error (a): Check if `est` contains required variables
  missing_vars_est <- setdiff(required_vars_est, colnames(est))
  if (length(missing_vars_est) > 0) {
    stop(paste("Estimates file est must contain variable(s):", paste(missing_vars_est, collapse = ", ")))
  }

  # Error (b): Check if `iso_code` is found in `dat` and `est`
  if (!iso_code %in% dat$iso) {
    stop(paste("iso_code", iso_code, "is not found in input data file dat."))
  }
  if (!iso_code %in% est$iso) {
    stop(paste("iso_code", iso_code, "is not found in estimates file est."))
  }

  # Error (c): Check if `cp` in `dat` is numeric
  if (!is.numeric(dat$cp)) {
    stop("Input cp in data file dat must be numeric.")
  }

  # Error (d): Check if CI is valid
  if (!is.na(CI) && !(CI %in% c(80, 95))) {
    stop("CI must be 80, 95, or NA.")
  }

  # Filter `est` for the given `iso_code`
  est_filtered <- est %>%
    filter(iso == iso_code)

  # Filter `dat` for the given `iso_code`
  observed_filtered <- dat %>%
    filter(iso == iso_code) %>%
    mutate(cp = cp * 100)

  # Create the base plot
  p <- ggplot(est_filtered, aes(x = Year, y = Median)) +
    geom_line(color = "blue") +
    geom_point(data = observed_filtered, aes(x = year, y = cp), color = "black", size = 2) +
    labs(
      x = "Time", y = "Modern use (%)",
      title = unique(est_filtered$`Country or area`)
    )

  # Add confidence intervals if CI is specified
  if (!is.na(CI)) {
    if (CI == 95) {
      p <- p + geom_ribbon(aes(ymin = L95, ymax = U95), alpha = 0.2, fill = "blue")
    } else if (CI == 80) {
      p <- p + geom_ribbon(aes(ymin = L80, ymax = U80), alpha = 0.2, fill = "blue")
    }
  }

  return(p)
}
