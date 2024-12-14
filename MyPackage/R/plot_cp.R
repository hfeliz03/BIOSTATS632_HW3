
plot_cp <- function(dat, est, iso_code, CI = 95) {
  # Define required columns
  required_vars_dat <- c("contraceptive_use_modern")
  required_vars_est <- c("iso", "Year", "Median", "U95", "L95", "L80", "U80")


  # Check if CI is valid
  if (!is.na(CI) && !(CI %in% c(80, 95))) {
    stop("CI must be: 80, 95, or NA.")
  }

  missing_vars_dat <- setdiff(required_vars_dat, colnames(dat))
  missing_vars_est <- setdiff(required_vars_est, colnames(est))
  # Error (a): Check if `dat` and est contains required variables
  if (length(missing_vars_dat) > 0 || length(missing_vars_est) > 0) {
    missing_message <- paste(
      "Input data file `dat` must contain variable:",
      paste(missing_vars_dat, collapse = ", "),
      "input data file `est` must contain variable:",
      paste(missing_vars_est, collapse = ", "),
      sep = " "
    )
    stop(missing_message)
  }

  # Error (b): Check if `iso_code` is found in `dat` and `est`
  if (!iso_code %in% dat$iso) {
    stop(paste("iso_code", iso_code, "is not found in the input data file `dat`."))
  }
  if (!iso_code %in% est$iso) {
    stop(paste("iso_code", iso_code, "is not found in the estimates file `est`."))
  }

  # Error (c): Check if `cp` in `dat` is numeric
  if (!is.numeric(dat$contraceptive_use_modern)) {
    stop("Input `cp` in the data file `dat` must be numeric.")
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
      if (!("L95" %in% colnames(est_filtered)) || !("U95" %in% colnames(est_filtered))) {
        stop("Columns `L95` and/or `U95` are missing in the `est` data.")
      }
      p <- p + geom_ribbon(aes(ymin = L95, ymax = U95), alpha = 0.2, fill = "blue")
    } else if (CI == 80) {
      if (!("L80" %in% colnames(est_filtered)) || !("U80" %in% colnames(est_filtered))) {
        stop("Columns `L80` and/or `U80` are missing in the `est` data.")
      }
      p <- p + geom_ribbon(aes(ymin = L80, ymax = U80), alpha = 0.2, fill = "blue")
    }
  }

  return(p)
}
