#' Plot Stationarity Test Results
#'
#' This function creates a visual summary of stationarity test results.
#'
#' @param results A tibble as returned by `check_stationarity()`
#'
#' @return A ggplot object showing stationarity status by variable, test, and differencing level
#' @export
plot_stationarity_results <- function(results) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }
  library(ggplot2)

  # Prepare data: Order for cleaner facetting
  results$difference <- factor(results$difference, levels = c("Level", "First Diff"))
  results$conclusion <- factor(results$conclusion,
                               levels = c("Stationary", "Non-stationary", "Inconclusive", "Test failed"))

  # Color palette
  palette <- c("Stationary" = "#2ECC71", 
               "Non-stationary" = "#E74C3C", 
               "Inconclusive" = "#F1C40F", 
               "Test failed" = "#95A5A6")

  # Plot
  ggplot(results, aes(x = test, y = variable, fill = conclusion)) +
    geom_tile(color = "white", size = 0.5) +
    facet_wrap(~difference, ncol = 1) +
    scale_fill_manual(values = palette) +
    theme_minimal(base_size = 13) +
    labs(
      title = "Stationarity Test Results",
      subtitle = "By PP and KPSS, at Level and First Difference",
      x = "Test Type",
      y = "Variable",
      fill = "Conclusion"
    ) +
    theme(
      strip.text = element_text(face = "bold", size = 12),
      legend.position = "bottom",
      panel.grid = element_blank()
    )
}
