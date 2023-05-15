#' A custom ggplot theme for PSMD.
#' Some edits to come by GP. Most work by DZ
#'
#' @description A custom ggplot theme for PSMD.
#'
#' @usage theme_psmd()
#'
#' @examples  ## Uses data from Data.Example in PSMD.Psychometrics
#' ## Requires ggplot2 to be loaded
#'
#' ggplot(data=Data.Example, aes(x=Grade.UBSE)) +
#'   geom_bar(stat="count", position=position_dodge()) +
#'   ggtitle("Basic Bar-Plot") +
#'   theme_psmd()
#'
#' @author Dr Daniel Zahra, \email{daniel.zahra@plymouth.ac.uk}
#'
#' @export

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### theme_psmd (DZ) ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

themeGP <- function(){

  # Set text size to use as starting point. Other elements are scaled to this.

  Text.Size<-10

  # Specify element_text options to avoid repetition

  Text.Basic <- element_text(size = Text.Size, colour = "black", face = "plain")
  Text.Bold <- element_text(size = Text.Size, colour = "black", face = "bold")
  Text.Title <- element_text(size = 1.1*Text.Size, colour = "black", face = "bold")

  # Start with theme_bw as a base; most of these setting work fine

  theme_bw() +

    # Customise individual elements of theme_bw for new theme
    # Full list of available elements here: http://ggplot2.tidyverse.org/reference/theme.html

    theme(

      legend.key = element_blank(),
      strip.background = element_blank(),

      text = Text.Basic,
      plot.title = Text.Title,

      axis.title = Text.Bold,
      axis.text = Text.Basic,

      legend.title = Text.Bold,
      legend.text = Text.Basic,

      panel.border = element_rect(fill=NA, colour="#D3D3D3"),
      panel.grid.major = element_line(colour = "#D3D3D3"),
      panel.grid.minor = element_line(colour = "#F5F5F5"),

      axis.line.x = element_line(colour = "#000000"),
      axis.line.y = element_line(colour = "#000000"))}
