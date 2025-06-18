#' Plot Stationarity Test Results
#'
#' This function creates a visual summary of stationarity test results.
#'
#' @param results A tibble as returned by `check_stationarity()`
#'
#' @return A ggplot object showing stationarity status by variable, test, and differencing level
#' @export
#' Visual Summary of Stationarity Test Results
#'
#' Creates a comprehensive visualization of stationarity test results with statistical annotations
#' and enhanced formatting. The plot shows test outcomes by variable, test type, and differencing level,
#' along with summary statistics in the subtitle.
#'
#' @param results A tibble containing stationarity test results as returned by `check_stationarity()`.
#'        Expected columns: variable, test, difference, conclusion, p_value (optional)
#'
#' @return A ggplot object showing stationarity status with:
#'         - Color-coded tiles indicating test conclusions
#'         - Facets by differencing level
#'         - Statistical annotations where available
#'         - Professional theme and formatting
#'         - Dynamic subtitle with summary statistics
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' stationarity_results <- check_stationarity(my_time_series_data)
#' plot_stationarity_results(stationarity_results)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_tile facet_wrap scale_fill_manual 
#'             labs theme_minimal theme element_text element_blank geom_text
#' @importFrom scales percent
#' @importFrom stats reorder
#' @export
plot_stationarity_results <- function(results) {
  # Check and load required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    install.packages("scales")
  }
  
  library(ggplot2)
  library(scales)
  
  # Validate input structure
  required_cols <- c("variable", "test", "difference", "conclusion")
  if (!all(required_cols %in% names(results))) {
    stop("Input data must contain columns: ", paste(required_cols, collapse = ", "))
  }
  
  # Prepare data
  results <- as.data.frame(results)  # Ensure it's a data frame for ggplot
  
  # Order factors for better visualization
  results$difference <- factor(results$difference, 
                              levels = c("Level", "First Diff", "Second Diff"),
                              ordered = TRUE)
  
  results$conclusion <- factor(results$conclusion,
                              levels = c("Stationary", "Non-stationary", 
                                        "Inconclusive", "Test failed"),
                              ordered = TRUE)
  
  # Create summary stats for subtitle
  total_tests <- nrow(results)
  stationary_pct <- mean(results$conclusion == "Stationary", na.rm = TRUE)
  nonstationary_pct <- mean(results$conclusion == "Non-stationary", na.rm = TRUE)
  
  # Color palette with professional colors
  palette <- c("Stationary" = "#2ECC71",      # Green
               "Non-stationary" = "#E74C3C",  # Red
               "Inconclusive" = "#F1C40F",    # Yellow
               "Test failed" = "#95A5A6")     # Gray
  
  # Base plot
  p <- ggplot(results, aes(x = reorder(test, test), 
                       y = reorder(variable, variable), 
                       fill = conclusion)) +
    geom_tile(color = "white", size = 0.7, alpha = 0.9) +
    facet_wrap(~difference, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = palette, drop = FALSE) +
    theme_minimal(base_size = 13) +
    labs(
      title = "Time Series Stationarity Analysis",
      subtitle = sprintf(
        "%d tests performed | %.1f%% Stationary | %.1f%% Non-stationary",
        total_tests, stationary_pct*100, nonstationary_pct*100
      ),
      x = "Test Type",
      y = "Time Series Variable",
      fill = "Test Conclusion",
      caption = "Note: Results may vary by test type and differencing level"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(color = "gray50", size = 10),
      strip.text = element_text(face = "bold", size = 12),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(face = "bold")
    )
  
  # Add p-values if available
  if ("p_value" %in% names(results)) {
    p <- p + 
      geom_text(aes(label = ifelse(!is.na(p_value), 
                                 sprintf("%.3f", p_value), 
                                 "")),
              color = "black", size = 3, check_overlap = TRUE) +
      labs(caption = paste(
        "Note: Numbers show test p-values |",
        "Results may vary by test type and differencing level"
      ))
  }
  
  return(p)
}
