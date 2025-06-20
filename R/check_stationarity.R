#' Run PP and KPSS Stationarity Tests
#'
#' This function checks stationarity using PP and KPSS tests.
#' If needed, it tests the first difference.
#'
#' @param data A data.frame or tibble with numeric columns
#' @param vars Optional. Character vector of variable names to test
#'
#' @return A tibble of test results
#' @export
check_stationarity <- function(data, vars = NULL) {
  # ---- Ensure Required Packages Are Installed ----
  required_packages <- c("purrr", "fpp3")

  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
  }

  # ---- Load Needed Libraries ----
  suppressPackageStartupMessages({
    library(tibble)
    library(dplyr)
    library(tsibble)
    library(feasts)
    library(fabletools)
    library(purrr)
  })

  # ---- Input Checks ----
  if (!is.data.frame(data)) stop("Input 'data' must be a data.frame or tibble")

  data_tbl <- data %>%
    tibble::as_tibble() %>%
    dplyr::select(-matches("^(year|date|index|time)$", ignore.case = TRUE))

  if (is.null(vars)) {
    # Select only numeric variables with non-zero variance
    numeric_vars <- data_tbl %>% select(where(is.numeric))
    vars <- names(numeric_vars)[sapply(numeric_vars, function(x) stats::var(x, na.rm = TRUE) > 0)]
  }

  # ---- Inner Function for Testing ----
  run_test <- function(vec, test_type) {
    ts_data <- tibble(value = vec) %>%
      mutate(index = row_number()) %>%
      as_tsibble(index = index)

    test_fun <- if (test_type == "PP") unitroot_pp else unitroot_kpss

    tryCatch({
      result <- ts_data %>%
        features(value, test_fun)
      tibble(
        statistic = result[[1]],
        p_value = result[[2]],
        conclusion = case_when(
          test_type == "PP" & result[[2]] < 0.05 ~ "Stationary",
          test_type == "PP" & result[[2]] >= 0.05 ~ "Non-stationary",
          test_type == "KPSS" & result[[2]] < 0.05 ~ "Non-stationary",
          test_type == "KPSS" & result[[2]] >= 0.05 ~ "Stationary",
          TRUE ~ "Inconclusive"
        )
      )
    }, error = function(e) {
      tibble(statistic = NA, p_value = NA, conclusion = "Test failed")
    })
  }

  # ---- Main Loop for All Variables ----
  results <- purrr::map_dfr(vars, function(var) {
    vec <- stats::na.omit(data_tbl[[var]])

    pp_level <- run_test(vec, "PP") %>%
      mutate(test = "PP", difference = "Level")
    kpss_level <- run_test(vec, "KPSS") %>%
      mutate(test = "KPSS", difference = "Level")

    base_row <- bind_rows(pp_level, kpss_level) %>%
      mutate(variable = var)

    # If either test says non-stationary, test the first difference
    need_diff <- any(base_row$conclusion == "Non-stationary")

    if (need_diff && length(vec) > 1) {
      vec_diff <- diff(vec)

      pp_diff <- run_test(vec_diff, "PP") %>%
        mutate(test = "PP", difference = "First Diff")
      kpss_diff <- run_test(vec_diff, "KPSS") %>%
        mutate(test = "KPSS", difference = "First Diff")

      diff_rows <- bind_rows(pp_diff, kpss_diff) %>%
        mutate(variable = var)

      bind_rows(base_row, diff_rows)
    } else {
      base_row
    }
  }) %>%
    select(variable, difference, test, statistic, p_value, conclusion)

  return(results)
}
