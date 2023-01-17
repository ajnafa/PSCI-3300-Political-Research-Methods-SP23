# Custom theme for data visualizations
plot_theme <- function(base_size = 12,
                       base_theme,
                       base_font = "serif",
                       title_size = NULL, 
                       xaxis_size = NULL, 
                       yaxis_size = NULL, 
                       strip_size = NULL, 
                       strip_face = NULL, 
                       caption.hjust = 1, 
                       caption.vjust = 0, 
                       x_axis_face = NULL, 
                       y_axis_face = NULL, 
                       transparent = FALSE, 
                       axis_text_size = NULL, 
                       legend_text_size = NULL,
                       subtitle_size = NULL,
                       caption_size = NULL,
                       show.axis == TRUE,
                       ...) {
  
  # Base Theme Settings
  .theme <- base_theme(
    base_size = base_size, 
    base_family = base_font
  ) + 
    # Additional theme settings
    theme(
      # Specify the default settings for the plot title
      plot.title = element_text(
        size = title_size,
        face = "bold",
        family = base_font
      ),
      # Specify the default settings for caption text
      plot.caption = element_text(
        size = caption_size,
        family = base_font,
        hjust = caption.hjust,
        vjust = caption.vjust
      ),
      # Specify the default settings for subtitle text
      plot.subtitle = element_text(
        size = subtitle_size,
        family = base_font
      ),
      # Specify the default settings specific to the x axis title
      axis.title.y = element_text(
        size = yaxis_size, 
        face = y_axis_face, 
        family = base_font,
        margin = margin(r = 10, l = -10)
      ),
      # Specify the default settings specific to the y axis title
      axis.title.x = element_text(
        size = xaxis_size, 
        face = x_axis_face, 
        family = base_font,
        margin = margin(t = 10, b = -10)
      ),
      # Specify the default settings for x axis text
      axis.text.x = element_text(
        size = axis_text_size,
        family = "serif",
        face = x_axis_face
      ),
      # Specify the default settings for y axis text
      axis.text.y = element_text(
        size = axis_text_size,
        family = "serif",
        face = y_axis_face
      ),
      # Specify the default settings for legend titles
      legend.title = element_text(
        size = legend_text_size,
        face = "bold",
        family = base_font
      ),
      # Specify the default settings for legend text
      legend.text = element_text(
        size = legend_text_size,
        family = base_font
      ),
      # Set the strip background fill to blank
      strip.background = element_blank(),
      # Adjust the strip text size settings
      strip.text = element_text(
        family = base_font, 
        size = strip_size,
        face = strip_face
      ),
      # Additional Settings Passed to theme()
      ...
    )
  # Plot Transparency
  if (transparent == TRUE) {
    .theme <- .theme + theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA)
    )
  }
  # Disable X and Y Axis
  if (show.axis == FALSE) {
    .theme <- .theme + theme(
      # Specify the default settings for axis titles
      axis.title = element_blank(),
      # Specify the default settings for axis text
      axis.text = element_blank()
    )
  }
  return(.theme)
}
