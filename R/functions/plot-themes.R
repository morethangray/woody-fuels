
theme_fuels <- function(){
  theme_minimal() +
    theme( 
      # Format the facet strip
      panel.spacing = unit(1.2, "lines"),
      panel.border = element_rect(fill = NA, color = "gray91", size = 2),
      panel.grid = element_line(color = "white"),
      strip.text = element_text(size = 16),
      strip.background = element_rect(fill = "gray91", color = "gray91"),
      # Format the y-axis text 
      axis.title.y = element_text(size = 18, vjust = 1.5),
      axis.text.y = element_text(size = 16, color = "gray50"),
      # Format x-axis 
      axis.title.x = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(),
      # Format the legend
      legend.position = "right", 
      legend.box.spacing = unit(1.2, "lines"), 
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 16, color = "gray30"), 
      legend.spacing.y = unit(0.5, 'cm'))  
}

theme_stacked <- function(){
  theme_minimal() +
    theme( 
      # Format the facet strip
      panel.spacing = unit(1.2, "lines"),
      panel.border = element_rect(fill = NA, color = NA), 
      panel.grid = element_line(color = "white"),
      strip.text = element_text(size = 16),
      strip.background = element_rect(fill = "gray91", color = "gray91"),
      # Format plot frame
      axis.line.x = element_line(color = "gray91", size = 1),
      axis.line.y = element_line(color = "gray91", size = 1),
      # Increase margins around plot to accommodate x-axis title
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
      # Format the y-axis text 
      axis.title.y = element_text(size = 18, vjust = 1.5),
      axis.text.y = element_text(size = 16, color = "gray50"),
      # Format x-axis 
      axis.title.x = element_text(size = 18, vjust = -1.2),
      axis.text.x = element_text(size = 16, color = "gray30"),
      axis.ticks.x = element_blank(),
      # Format the legend
      legend.position = "right", 
      legend.box.spacing = unit(1.2, "lines"), 
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 16, color = "gray30"), 
      legend.spacing.y = unit(0.5, 'cm'))  
}

