#' Plot Distribution of Scores by Demographic Variables
#'
#' @description
#' Creates histograms showing the distribution of scores across different demographic variables.
#' Each demographic group is color-coded, and the plots can be customized with various options.
#'
#' @param data A data frame containing the score and demographic columns
#' @param score_col Name of the column containing score values
#' @param cols_demographics Vector of column names for demographic variables
#' @param demographics_names Optional vector of display names for demographic variables (must match length of cols_demographics)
#' @param exclude_na Logical vector indicating whether to exclude NA values for each demographic (single value will be recycled)
#' @param save_path Optional path to save the plots as image files
#' @param width_cm Width of saved plots in centimeters
#' @param height_cm Height of saved plots in centimeters
#' @param dpi Resolution of saved plots in dots per inch
#' @param title_label Prefix text for plot titles
#' @param x_label Label for x-axis
#' @param y_label Label for y-axis
#' @param x_scale Vector of length 2 specifying x-axis limits (min, max)
#' @param x_breaks_major Vector specifying major tick marks on x-axis
#' @param x_breaks_minor Vector specifying minor tick marks on x-axis
#' @param show_means Logical, whether to show group means in the legend
#' @param means_decimals Number of decimal places for mean values in legend
#' @param show_counts Logical, whether to show group counts in the legend
#' @param legend_ncol Number of columns in the legend (NULL for automatic)
#' @param legend_order How to order legend items: "count" (by frequency), "mean" (by average score), or "name" (alphabetically)
#' @param vertical_lines Optional numeric vector of x-positions for vertical reference lines
#' @param vertical_line_colors Optional vector of colors for vertical lines (recycled if needed)
#' @param vertical_line_types Optional vector of line types for vertical lines (recycled if needed)
#' @param color_palette Name of RColorBrewer palette to use (default "Set3") or a vector of custom colors
#' @param use_density Logical, whether to show density plots instead of histograms
#' @param use_facets Logical, whether to create faceted plots for demographics with many categories
#' @param facet_threshold Number of categories above which faceted plots are created (if use_facets is TRUE)
#'
#' @return A list of ggplot objects, one for each demographic variable
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' plots <- fn_plot_scores_by_demographics(
#'   data = student_data,
#'   score_col = "FinalScore",
#'   cols_demographics = c("Gender", "Ethnicity"),
#'   exclude_na = TRUE
#' )
#'
#' # Display one of the plots
#' plots$Gender
#'
#' # Complex example with custom options
#' plots <- fn_plot_scores_by_demographics(
#'   data = student_data,
#'   score_col = "FinalScore",
#'   cols_demographics = c("Gender", "Ethnicity", "Programme"),
#'   demographics_names = c("Student Gender", "Ethnicity Group", "Programme Type"),
#'   exclude_na = TRUE,
#'   show_means = TRUE,
#'   show_counts = TRUE,
#'   vertical_lines = c(60, 70),  # Pass/fail thresholds
#'   color_palette = "Spectral",
#'   use_density = FALSE
#' )
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @import RColorBrewer
#' @import ggprism
#'
#' @export
fn_plot_scores_by_demographics <- function(data,
                                        score_col = "Score",
                                        cols_demographics,
                                        demographics_names = NULL,
                                        exclude_na = NULL,
                                        save_path = NULL,
                                        width_cm = 20,
                                        height_cm = 15,
                                        dpi = 300,
                                        title_label = "Distribution of Scores by",
                                        x_label = "Score (Raw Score)",
                                        y_label = "Frequency (N Students)",
                                        x_scale = c(0, 100),
                                        x_breaks_major = seq(0, 100, 10),
                                        x_breaks_minor = seq(0, 100, 1),
                                        show_means = FALSE,
                                        means_decimals = 2,
                                        show_counts = FALSE,
                                        legend_ncol = NULL,
                                        legend_order = "mean",
                                        vertical_lines = NULL,
                                        vertical_line_colors = "darkgrey",
                                        vertical_line_types = "dashed",
                                        color_palette = "Set3",
                                        use_density = FALSE,
                                        use_facets = FALSE,
                                        facet_threshold = 10) {
  # Validate legend_order parameter
  if (!legend_order %in% c("count", "mean", "name")) {
    warning("Invalid legend_order. Using 'mean'. Valid options are 'count', 'mean', or 'name'.")
    legend_order <- "mean"
  }

  # Create a stripped version of the data with only necessary columns
  data_stripped <- data %>%
    select(all_of(c(score_col, cols_demographics)))

  # Set up display names
  # If demographics_names is NULL, use the original column names
  display_names <- if (is.null(demographics_names)) {
    cols_demographics
  } else if (length(demographics_names) == length(cols_demographics)) {
    demographics_names
  } else {
    warning(
      "demographics_names length does not match cols_demographics; using original column names"
    )
    cols_demographics
  }

  # Set defaults for exclude_na if not provided
  if (is.null(exclude_na)) {
    exclude_na <- rep(FALSE, length(cols_demographics))
  } else if (length(exclude_na) == 1) {
    # If a single value is provided, apply it to all demographics
    exclude_na <- rep(exclude_na, length(cols_demographics))
  } else if (length(exclude_na) != length(cols_demographics)) {
    warning("exclude_na length does not match cols_demographics; using FALSE for all")
    exclude_na <- rep(FALSE, length(cols_demographics))
  }

  # Initialize list to store plots
  plots_list <- list()

  # Create each plot
  for (i in seq_along(cols_demographics)) {
    col <- cols_demographics[i]
    display_name <- display_names[i]

    # Filter out NA values if specified for this demographic
    if (exclude_na[i]) {
      data_for_plot <- data_stripped %>%
        filter(!is.na(!!sym(col)))
    } else {
      data_for_plot <- data_stripped
    }

    # Ensure we're only plotting data within our x_scale range
    data_for_plot <- data_for_plot %>%
      filter(!is.na(!!sym(score_col)) &
               !!sym(score_col) >= x_scale[1] &
               !!sym(score_col) <= x_scale[2])

    # Skip if we have no data after filtering
    if (nrow(data_for_plot) == 0) {
      warning(paste("No data available for", col, "after filtering. Skipping."))
      next
    }

    # Define text elements for ggplot
    text_basic <- element_text(size = 11)
    text_bold <- element_text(size = 11, face = "bold")

    # Calculate category frequencies for color assignment and stacking order
    category_counts <- data_for_plot %>%
      count(!!sym(col)) %>%
      arrange(desc(n))

    # Calculate mean scores for each category - for legend order
    category_means <- data_for_plot %>%
      group_by(!!sym(col)) %>%
      summarise(mean_score = mean(!!sym(score_col), na.rm = TRUE)) %>%
      arrange(desc(mean_score))

    # Get count-based order - descending count (highest N first)
    count_order <- category_counts %>% pull(!!sym(col))

    # Get stacking order - ascending count (lowest N at bottom)
    stacking_order <- category_counts %>% arrange(-n) %>% pull(!!sym(col))

    # Choose appropriate ordering for legend display based on legend_order parameter
    legend_order_values <- switch(
      legend_order,
      "count" = count_order,
      "mean" = category_means %>% arrange(desc(mean_score)) %>% pull(!!sym(col)),
      "name" = sort(unique(data_for_plot[[col]]))
    )

    # Fix for "mean" ordering - use the actual column name instead of "col"
    if (legend_order == "mean") {
      legend_order_values <- category_means[[col]]
    }

    # Format decimal places correctly for means
    mean_format <- paste0("%.", means_decimals, "f")

    # Create label information if requested
    if (show_means || show_counts) {
      # Start with categories in the legend order
      labels_data <- data.frame(category = legend_order_values)

      # Add count information if needed
      if (show_counts) {
        # Fix the join by using the right column names
        count_data <- category_counts %>%
          rename(category = !!sym(col))

        labels_data <- labels_data %>%
          left_join(count_data, by = "category")
      }

      # Add mean information if needed
      if (show_means) {
        mean_data <- category_means %>%
          rename(category = !!sym(col))

        labels_data <- labels_data %>%
          left_join(mean_data, by = "category")
      }

      # Create the label based on what's included
      labels_data <- labels_data %>%
        mutate(
          label = case_when(
            show_counts & show_means ~ paste0(
              category,
              " (N = ",
              n,
              ", Mean = ",
              sprintf(mean_format, mean_score),
              ")"
            ),
            show_counts &
              !show_means ~ paste0(category, " (N = ", n, ")"),
            !show_counts &
              show_means ~ paste0(
                category,
                " (Mean = ",
                sprintf(mean_format, mean_score),
                ")"
              ),
            TRUE ~ as.character(category)
          )
        )

      # Extract the labels in the right order
      category_labels <- labels_data$label

      # Create a named vector for mapping original levels to new labels
      names(category_labels) <- legend_order_values
    }

    # Calculate max count properly by creating temp histogram data for y-axis scaling
    if (!use_density) {
      temp_hist <- hist(data_for_plot[[score_col]],
                        breaks = seq(min(x_scale), max(x_scale), by = 1),
                        plot = FALSE)
      max_count <- max(temp_hist$counts)

      # Calculate y-axis breaks
      y_breaks <- pretty(c(0, max_count))
    }

    # Determine if we should use facets based on category count and settings
    use_facet_for_this_plot <- use_facets &&
      length(count_order) > facet_threshold

    # Convert demographic variable to a factor with appropriate levels BEFORE plotting
    # This is the key fix - convert it to factor with proper levels outside the aes() call
    if (use_facet_for_this_plot) {
      # For faceted plots, just convert to factor
      data_for_plot <- data_for_plot %>%
        mutate(!!col := factor(!!sym(col)))
    } else {
      # For non-faceted plots with legend order
      data_for_plot <- data_for_plot %>%
        mutate(
          # Create a legend_factor with properly ordered levels
          legend_factor = factor(!!sym(col), levels = legend_order_values),
          # Also create stack_factor for stacking order (if using histograms)
          stack_factor = factor(!!sym(col), levels = stacking_order)
        )
    }

    # Determine if we should create density plot or histogram
    if (use_density) {
      # For density plots
      if (use_facet_for_this_plot) {
        # Create faceted density plot
        p <- ggplot(data_for_plot, aes(
          x = !!sym(score_col),
          color = !!sym(col)
        )) +
          geom_density(linewidth = 1) +
          facet_wrap(vars(!!sym(col)), ncol = 3)
      } else {
        # Create overlaid density plot with legend ordered by chosen method
        p <- ggplot(data_for_plot, aes(
          x = !!sym(score_col),
          color = legend_factor  # Use the pre-created factor instead of inline factor()
        )) +
          geom_density(linewidth = 1)
      }
    } else {
      # For histograms
      if (use_facet_for_this_plot) {
        # Create faceted histogram
        p <- ggplot(data_for_plot, aes(
          x = !!sym(score_col),
          fill = !!sym(col)
        )) +
          geom_histogram(
            binwidth = 1,
            color = "black",
            linewidth = 0.5,
            closed = "left",
            boundary = 0.5
          ) +
          facet_wrap(vars(!!sym(col)), ncol = 3)
      } else {
        # Create stacked histogram with the pre-created factors
        p <- ggplot(data_for_plot,
                    aes(
                      x = !!sym(score_col),
                      fill = legend_factor,  # Use pre-created factor for legend
                      group = stack_factor   # Use pre-created factor for stacking
                    )) +
          geom_histogram(
            binwidth = 1,
            position = "stack",
            color = "black",
            linewidth = 0.5,
            closed = "left",
            boundary = 0.5
          )

        # Set Y axis scaling for histograms
        p <- p + scale_y_continuous(
          limits = c(0, max_count * 1.2),
          # Add 20% padding to top
          expand = c(0, 0),
          guide = guide_prism_minor(),
          breaks = y_breaks,
          minor_breaks = seq(0, max_count * 1.2, 1)
        )
      }
    }

    # Add vertical reference lines if specified
    if (!is.null(vertical_lines)) {
      # Ensure color and line type vectors are long enough by recycling
      n_lines <- length(vertical_lines)
      if (length(vertical_line_colors) < n_lines) {
        vertical_line_colors <- rep(vertical_line_colors, length.out = n_lines)
      }
      if (length(vertical_line_types) < n_lines) {
        vertical_line_types <- rep(vertical_line_types, length.out = n_lines)
      }

      # Add a vertical line for each value specified
      for (j in seq_along(vertical_lines)) {
        p <- p + geom_vline(
          xintercept = vertical_lines[j],
          linetype = vertical_line_types[j],
          color = vertical_line_colors[j],
          linewidth = 0.5
        )
      }
    }

    # Continue with common plot elements
    p <- p +
      # Set x axis scale
      scale_x_continuous(
        breaks = x_breaks_major,
        limits = x_scale,
        expand = expansion(mult = c(0, 0.02)),
        guide = guide_prism_minor(),
        minor_breaks = x_breaks_minor
      ) +
      # Theme customization
      theme_bw() +
      theme(
        # Text elements
        text = text_basic,
        axis.title = text_bold,
        axis.text = text_basic,
        legend.title = text_bold,
        legend.text = text_basic,
        # Panel elements
        panel.border = element_rect(fill = NA, colour = "#D3D3D3"),
        panel.grid.major = element_line(colour = "#D3D3D3"),
        panel.grid.minor = element_line(colour = "white"),
        # Legend
        legend.key = element_blank(),
        legend.position = "bottom",
        # Axis lines
        axis.line.x = element_line(colour = "#000000"),
        axis.line.y = element_line(colour = "#000000"),
        # Remove top axis elements
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank()
      ) +
      # Labels
      labs(
        title = paste(title_label, display_name),
        x = x_label,
        y = y_label,
        fill = display_name,
        color = display_name
      )

    # Get color palette - either use the specified RColorBrewer palette or custom colors
    if (length(color_palette) == 1 &&
        color_palette %in% rownames(RColorBrewer::brewer.pal.info)) {
      # Use a predefined RColorBrewer palette
      n_colors_needed <- length(count_order)  # Number of colors needed based on categories

      # Handle cases where fewer than 3 colors are needed
      if (n_colors_needed < 3) {
        # Use a custom minimal palette for 1 or 2 colors
        minimal_palette <- c("#8DD3C7", "#FFFFB3")  # Default colors for 1 or 2 categories
        palette_colors <- minimal_palette[1:n_colors_needed]
      } else {
        # Use RColorBrewer for 3 or more colors
        n_colors_needed <- min(9, n_colors_needed)  # Limit to max 9 colors for RColorBrewer
        palette_colors <- brewer.pal(n_colors_needed, color_palette)
      }

      # If more categories than palette can handle, recycle colors
      if (length(count_order) > n_colors_needed) {
        palette_colors <- colorRampPalette(palette_colors)(length(count_order))
      }

      # Only use as many colors as we need
      palette_colors <- palette_colors[1:length(count_order)]
    } else {
      # Use custom colors, recycling if needed
      palette_colors <- rep(color_palette, length.out = length(count_order))
    }

    # Apply color scale with appropriate legend labels
    if (!use_facet_for_this_plot) {
      # Create color mapping between legend_order_values and palette_colors
      # This is the key to preserving consistent colors for each value
      color_mapping <- setNames(palette_colors[1:length(legend_order_values)], legend_order_values)

      # Only for non-faceted plots
      if (use_density) {
        if (show_means || show_counts) {
          p <- p + scale_color_manual(values = color_mapping, labels = category_labels, drop = FALSE)
        } else {
          p <- p + scale_color_manual(values = color_mapping, drop = FALSE)
        }
      } else {
        if (show_means || show_counts) {
          p <- p + scale_fill_manual(values = color_mapping, labels = category_labels, drop = FALSE)
        } else {
          p <- p + scale_fill_manual(values = color_mapping, drop = FALSE)
        }
      }
    }

    # Determine legend layout (only needed for non-faceted plots)
    if (!use_facet_for_this_plot) {
      if (!is.null(legend_ncol)) {
        # Use the override value if provided
        n_cols <- legend_ncol
      } else {
        # Calculate number of categories
        n_cats <- length(legend_order_values)

        # Calculate estimated label width based on what's shown
        label_width_factor <- 1
        if (show_means)
          label_width_factor <- label_width_factor + 0.8
        if (show_counts)
          label_width_factor <- label_width_factor + 0.5

        # Calculate number of columns based on number of categories and label width
        n_cols <- if (label_width_factor > 1.3) {
          # If both stats or long labels are shown, use fewer columns
          max(1, floor(4 / label_width_factor))
        } else if (n_cats <= 4) {
          n_cats  # Small number of categories can fit in one row
        } else if (n_cats <= 8) {
          4  # Medium number of categories can fit in multiple columns
        } else {
          max(1, min(5, floor(n_cats / 3)))  # For large numbers, limit columns
        }
      }

      # Apply legend guide with specified columns
      if (use_density) {
        p <- p + guides(color = guide_legend(ncol = n_cols))
      } else {
        p <- p + guides(fill = guide_legend(ncol = n_cols))
      }
    }

    # Store the plot in the list with the original column name
    plots_list[[col]] <- p

    # Save the plot if a save path is provided
    if (!is.null(save_path)) {
      # Create directory if it doesn't exist
      if (!dir.exists(save_path)) {
        dir.create(save_path, recursive = TRUE)
      }

      # Convert cm to inches for ggsave
      width_inch <- width_cm / 2.54
      height_inch <- height_cm / 2.54

      # Create filename using the original column name
      plot_type <- if (use_density)
        "Density"
      else
        "Histogram"
      facet_label <- if (use_facet_for_this_plot)
        "_Faceted"
      else
        ""
      filename <- file.path(save_path,
                            paste0("Demographics_", plot_type, facet_label, "_", col, ".png"))

      # Save plot
      ggsave(
        filename = filename,
        plot = p,
        width = width_inch,
        height = height_inch,
        dpi = dpi
      )
    }
  }

  # Return the list of plots
  return(plots_list)
}
