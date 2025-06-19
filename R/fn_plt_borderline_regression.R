#' Plot of the Borderline Regression method.
#'
#' @description fn_plt_borderline_regression creates a scatterplot assessment scores against global grades.
#' A regression line is added to the plot and the intersection of this line with the 'borderline group' is used to calculate a pass mark.
#' The main output from fn_plt_borderline_regression is a list containing a ggplot object and the pass mark.
#' Additionally the function passes the linear regression model into the global environment (see Examples).
#'
#' @usage fn_plt_borderline_regression(x, grade_col="Grade", score_col="Score")
#'
#' @param x A data frame containing the global grade and score data.
#' @param grade_col Name of the column containing the global grades.
#' @param score_col Name of the column containing the assessment scores.
#'
#' @note
#' This function uses \code{\link[ggplot2]{ggplot2}} for the underlying graphics,
#' and uses the str_wrap function from the \code{\link[stringr]{stringr}} package to wrap the x axis labels.
#'
#' @return A list containing:
#' \item{plt_borderline_regression}{A ggplot object which can be saved or customised as needed}
#' \item{pass_mark}{The calculated pass mark from the borderline regression}
#' Additionally the linear regression model (b_reg_line) is passed to the global environment.
#'
#' @examples
#' library("ggplot2")
#' data <- data.frame(Grade = factor(c(rep("Unsatisfactory",5),rep("Borderline",10),rep("Low Satisfactory",20),
#'        rep("High Satisfactory",25),rep("Excellent",10)),ordered=TRUE,
#'        levels = c("Unsatisfactory","Borderline","Low Satisfactory","High Satisfactory","Excellent")),
#'        Score = c(5,8:10,10, 10:14,11:15, 12:16,13:17,13:17,14:18, 13:17,14:18,15:19,16:20,17:21, 18:22,20:23,25),
#'        Grade4 = factor(c(rep("Unsatisfactory",5),rep("Borderline",20),rep("Satisfactory",32),rep("Excellent",13)),
#'        ordered = TRUE, levels = c("Unsatisfactory","Borderline","Satisfactory","Excellent")),
#'        Score4 = c(5,6,8:10, 9:13,10:14, 10:14,11:15,11:15,12:16, 12:16,13:17,13:17,14:18,14:18,15:20, 18:20,20))
#'
#' results <- fn_plt_borderline_regression(data)
#' results$plt_borderline_regression  # The plot
#' results$pass_mark  # The pass mark
#' b_reg_line  # Details of the regression model
#' summary(b_reg_line)   # Summary of the regression model
#' car::outlierTest(b_reg_line)   # Test for significant outliers in the model
#'
#' results2 <- fn_plt_borderline_regression(data, "Grade4", "Score4")
#'
#'
#' @source Written by Martin Roberts (psychometrics@plymouth.ac.uk)
#'
#' @export

fn_plt_borderline_regression <- function(x,
                                         grade_col = "Grade",
                                         score_col = "Score") {
  # Written by: Martin Roberts
  # Last updated: 07/03/2018
  # Required packages: "ggplot2","psychometricsPSMD","stringr"
  # Plots the borderline regression method, reporting the equation, R² and pass mark
  # Passes the linear model to the global environment as b_reg_line
  # Returns a list with the plot and pass mark
  ################################################
  plot_data <- x[, c(grade_col, score_col)]
  colnames(plot_data) <- c("Grade", "Score")

  # Checking for data format errors
  if (!is.ordered(plot_data$Grade)) {
    stop(paste("Error:", grade_col, " is not an ordered factor."))
  }
  if (!is.numeric(plot_data$Score)) {
    stop(paste("Error:", score_col, " is not a numeric vector of scores."))
  }

  n_grades <- length(levels(plot_data$Grade))    # 5 for medical, 4 for dental
  max_score <- n_grades * 5    # 25 for medical, 20 for dental
  plot_data$n_grade <- as.numeric(plot_data$Grade)  # Converts Grade to numeric for plotting

  # Create linear regression model and save to global environment
  assign("b_reg_line", lm(Score ~ n_grade, data = plot_data), pos = .GlobalEnv)

  y_values <- c(b_reg_line$coefficients[1],
                b_reg_line$coefficients[1] + b_reg_line$coefficients[2] * n_grades) # Extreme y values for plot

  cut_point <- predict.lm(b_reg_line, newdata = data.frame(n_grade = c(2)))	# Intersection of regression line with borderline group
  pass_mark_value <- fnGPRound(cut_point, 2)	# Defines the pass mark accurately to 2dp

  b_reg_equation <- paste0(
    "y = ",
    fnGPRound(b_reg_line$coefficients[1], 3),
    " + ",
    fnGPRound(b_reg_line$coefficients[2], 3),
    "x"
  ) # Reg equation as text

  b_reg_text_1 <- paste0(
    "Regression line\n",
    b_reg_equation,
    "\n(R² = ",
    fnGPRound(summary(b_reg_line)[[8]], 3),
    ")\nintersects the\nborderline group \nat score of ",
    fnGPRound(cut_point, 4)
  )        # Text for plot

  b_reg_text_2 <- paste0("Pass mark = ", formatC(pass_mark_value, digits = 2, format = "f"))        # Text for plot

  plt_borderline_regression <- ggplot(data = plot_data, aes(x = n_grade, y = Score)) +
    themeGP() +
    scale_shape_manual(values = 18) +
    geom_count(colour = "gray20", alpha = 0.7) +
    scale_size_area(max_size = 8) +   # Plot duplicate points as bubbles, area ~ number of multiple points
    annotate(
      geom = "segment",
      x = 0,
      xend = 0.05,
      y = seq(0, max_score) ,
      yend = seq(0, max_score)
    ) +  # Minor tick marks
    geom_segment(
      aes(
        x = 0,
        y = b_reg_line$coefficients[1],
        xend = n_grades,
        yend = b_reg_line$coefficients[1] + b_reg_line$coefficients[2] * n_grades
      ),
      colour = "black",
      size = 0.7
    ) +  # Plot regression line
    geom_segment(
      aes(
        x = 0,
        y = cut_point,
        xend = 2,
        yend = cut_point
      ),
      colour = "red",
      linetype = 2,
      size = 0.9
    ) +  # Join intersection point to y axis
    annotate(
      geom = "text",
      x = 1,
      y = cut_point + 0.5,
      label = b_reg_text_1,
      size = 3,
      color = "red",
      vjust = "bottom"
    ) +  # Add text above join line
    annotate(
      geom = "text",
      x = 1,
      y = cut_point - 0.3,
      label = b_reg_text_2,
      fontface = 2,
      color = "red",
      vjust = "top"
    ) +  # Add text below join line
    scale_x_continuous(
      limits = c(0, n_grades + 0.5),
      breaks = seq(1, n_grades),
      labels = stringr::str_wrap(levels(plot_data$Grade), width = 1),
      expand = c(0, 0)
    ) +
    scale_y_continuous(limits = c(min(c(0, y_values)), max(c(max_score, y_values))),
                       breaks = seq(0, max_score, 5)) +
    xlab("Global grade") +
    ylab("Score")

  # Return list with plot and pass mark
  return(list(
    plt_borderline_regression = plt_borderline_regression,
    pass_mark = pass_mark_value
  ))
}
