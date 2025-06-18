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
   # Check and install required packages
   required_packages <- c("purrr", "fpp3")

   for (pkg in required_packages) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
         install.packages(pkg)
      }
   }

   # Load required packages
   library(purrr)
   library(fpp3)

   # Input validation
   if (!is.data.frame(data)) stop("data must be a data.frame or tibble")

   data_tbl <- data %>%
      tibble::as_tibble() %>%
      dplyr::select(-dplyr::matches("^(year|date|index|time)$", ignore.case = TRUE))

   if (is.null(vars)) {
      vars <- data_tbl %>%
         dplyr::select(where(is.numeric)) %>%
         dplyr::select(-where(~ var(.) == 0)) %>%
         names()
   }

   run_test <- function(vec, test_type) {
      ts_data <- tibble::tibble(value = vec) %>%
         dplyr::mutate(index = dplyr::row_number()) %>%
         tsibble::as_tsibble(index = index)

      tryCatch({
         test_fun <- if (test_type == "PP") fabletools::unitroot_pp else fabletools::unitroot_kpss
         result <- ts_data %>% feasts::features(value, test_fun)
         tibble::tibble(
            statistic = result[[1]],
            p_value = result[[2]],
            conclusion = dplyr::case_when(
               test_type == "PP" & result[[2]] < 0.05 ~ "Stationary",
               test_type == "PP" & result[[2]] >= 0.05 ~ "Non-stationary",
               test_type == "KPSS" & result[[2]] < 0.05 ~ "Non-stationary",
               test_type == "KPSS" & result[[2]] >= 0.05 ~ "Stationary",
               TRUE ~ "Inconclusive"
            )
         )
      }, error = function(e) {
         tibble::tibble(statistic = NA, p_value = NA, conclusion = "Test failed")
      })
   }

   results <- purrr::map_dfr(vars, function(var) {
      vec <- stats::na.omit(data_tbl[[var]])

      pp_level <- run_test(vec, "PP") %>% dplyr::mutate(test = "PP", difference = "Level")
      kpss_level <- run_test(vec, "KPSS") %>% dplyr::mutate(test = "KPSS", difference = "Level")
      base_row <- dplyr::bind_rows(pp_level, kpss_level) %>% dplyr::mutate(variable = var)

      need_diff <- any(base_row$conclusion == "Non-stationary")

      if (need_diff && length(vec) > 1) {
         vec_diff <- diff(vec)
         pp_diff <- run_test(vec_diff, "PP") %>% dplyr::mutate(test = "PP", difference = "First Diff")
         kpss_diff <- run_test(vec_diff, "KPSS") %>% dplyr::mutate(test = "KPSS", difference = "First Diff")
         diff_rows <- dplyr::bind_rows(pp_diff, kpss_diff) %>% dplyr::mutate(variable = var)
         dplyr::bind_rows(base_row, diff_rows)
      } else {
         base_row
      }
   }) %>%
      dplyr::select(variable, difference, test, statistic, p_value, conclusion)

   return(results)
}
