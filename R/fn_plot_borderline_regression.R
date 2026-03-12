#   ____________________________________________________________________________
#   Borderline Regression Plot                                              ####

#' Plot of the Borderline Regression method
#'
#' @description fn_plot_borderline_regression creates a scatterplot of assessment
#' scores against global grades. A regression line is added and its intersection
#' with the borderline group is used to calculate a pass mark.
#'
#' @param x A data frame containing the global grade and score data.
#' @param grade_col Name of the column containing the global grades.
#' @param score_col Name of the column containing the assessment scores.
#'
#' @return A list containing:
#' \item{plt_borderline_regression}{A ggplot object}
#' \item{pass_mark}{The calculated pass mark}
#' Additionally the linear regression model (b_reg_line) is passed to the
#' global environment.
#'
#' @export
fn_plot_borderline_regression <- function(x,
                                         grade_col = "Grade",
                                         score_col = "Score") {

  plot_data <- x[, c(grade_col, score_col)]
  colnames(plot_data) <- c("Grade", "Score")

  if (!is.ordered(plot_data$Grade)) {
    stop(paste("Error:", grade_col, "is not an ordered factor."))
  }
  if (!is.numeric(plot_data$Score)) {
    stop(paste("Error:", score_col, "is not a numeric vector of scores."))
  }

  n_grades       <- length(levels(plot_data$Grade))
  plot_data$n_grade <- as.numeric(plot_data$Grade)

  assign("b_reg_line", lm(Score ~ n_grade, data = plot_data), pos = .GlobalEnv)

  # Capture all derived values locally so ggplot closes over the right values
  # when rendering in a loop — avoids the global env race condition
  .intercept <- unname(b_reg_line$coefficients[1])
  .slope     <- unname(b_reg_line$coefficients[2])
  .cut_pt    <- unname(predict.lm(b_reg_line, newdata = data.frame(n_grade = 2)))
  .r_sq      <- fnGPRound(summary(b_reg_line)[[8]], 3)

  pass_mark_value <- fnGPRound(.cut_pt, 2)

  .y_extremes <- c(.intercept, .intercept + .slope * n_grades)

  # Y axis limits based on actual data range plus regression extremes
  .y_min <- min(c(0, .y_extremes, plot_data$Score), na.rm = TRUE)
  .y_max <- max(c(.y_extremes, plot_data$Score), na.rm = TRUE)
  .y_pad <- (.y_max - .y_min) * 0.05

  .eq_text <- paste0(
    "y = ", fnGPRound(.intercept, 3),
    " + ", fnGPRound(.slope, 3), "x"
  )

  .reg_label <- paste0(
    "Regression line\n", .eq_text,
    "\n(R\u00b2 = ", .r_sq, ")",
    "\nintersects the\nborderline group\nat score of ", fnGPRound(.cut_pt, 4)
  )

  .pass_label <- paste0("Pass mark = ", formatC(pass_mark_value, digits = 2, format = "f"))

  .text_size  <- 10
  .text_basic <- element_text(size = .text_size, colour = "black", face = "plain")
  .text_bold  <- element_text(size = .text_size, colour = "black", face = "bold")

  .grade_colours <- c(
    "Fail"              = "indianred1",
    "Unsatisfactory"    = "indianred1",
    "Borderline"        = "#FFD700",
    "Pass"              = "lightgreen",
    "Satisfactory"      = "lightgreen",
    "Low Satisfactory"  = "lightgreen",
    "High Satisfactory" = "#4444FF",
    "3rd"               = "#FFB347",
    "2:2"               = "#FFD700",
    "2:1"               = "lightgreen",
    "1st"               = "#4444FF",
    "Excellent"         = "#4444FF"
  )

  .grade_levels  <- levels(plot_data$Grade)
  .point_colours <- ifelse(
    .grade_levels %in% names(.grade_colours),
    .grade_colours[.grade_levels],
    "grey70"
  )
  names(.point_colours) <- .grade_levels

  plt_borderline_regression <- ggplot(data = plot_data, aes(x = n_grade, y = Score)) +
    theme_bw() +
    theme(
      text             = .text_basic,
      axis.title       = .text_bold,
      axis.text        = .text_basic,
      panel.border     = element_rect(fill = NA, colour = "#D3D3D3"),
      panel.grid.major = element_line(colour = "#D3D3D3"),
      panel.grid.minor = element_line(colour = "white"),
      legend.title     = .text_bold,
      legend.text      = .text_basic,
      legend.position  = "bottom",
      axis.line.x      = element_line(colour = "#000000"),
      axis.line.y      = element_line(colour = "#000000")
    ) +
    geom_count(aes(colour = Grade), alpha = 0.8) +
    scale_size_area(
      max_size = 8,
      breaks   = function(limits) {
        .max <- floor(limits[2])
        unique(round(seq(1, .max, length.out = min(.max, 5))))
      }
    ) +
    scale_colour_manual(values = .point_colours, name = "Grade") +
    # Regression line
    annotate(
      geom      = "segment",
      x         = 0,
      y         = .intercept,
      xend      = n_grades,
      yend      = .intercept + .slope * n_grades,
      colour    = "black",
      linewidth = 0.7
    ) +
    # Dashed horizontal guide to intersection point
    annotate(
      geom      = "segment",
      x         = 0,
      y         = .cut_pt,
      xend      = 2,
      yend      = .cut_pt,
      colour    = "red",
      linetype  = "dashed",
      linewidth = 0.9
    ) +
    # Regression details in bottom right
    annotate(
      geom  = "text",
      x     = n_grades + 0.4,
      y     = .y_min - .y_pad + 0.5,
      label = .reg_label,
      size  = 3,
      color = "red",
      hjust = "right",
      vjust = "bottom"
    ) +
    # Pass mark label next to the dashed line
    annotate(
      geom     = "text",
      x        = 1,
      y        = .cut_pt - 0.3,
      label    = .pass_label,
      fontface = 2,
      color    = "red",
      vjust    = "top"
    ) +
    scale_x_continuous(
      limits = c(0, n_grades + 0.5),
      breaks = seq(1, n_grades),
      labels = stringr::str_wrap(levels(plot_data$Grade), width = 10),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(.y_min - .y_pad, .y_max + .y_pad),
      breaks = seq(0, ceiling(.y_max / 5) * 5, 5)
    ) +
    labs(x = "Global grade", y = "Score")

  list(
    plt_borderline_regression = plt_borderline_regression,
    pass_mark                 = pass_mark_value
  )
}
