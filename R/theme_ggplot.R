# from Corson N. Areshenkoff
# http://areshenk-research-notes.com/publication-quality-plots-in-r/

theme_pub_bw <- function (base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, 
             base_family = base_family) %+replace% 
    
    theme(# Set text size
      plot.title = element_text(size = 18),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16, 
                                  angle = 90),
      
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      
      strip.text.x = element_text(size = 15),
      strip.text.y = element_text(size = 15,
                                  angle = -90),
      
      # Legend text
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 15),
      
      # Configure lines and axes
      axis.ticks.x = element_line(colour = "black"), 
      axis.ticks.y = element_line(colour = "black"), 
      
      # Plot background
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey83", 
                                      size = 0.2), 
      panel.grid.minor = element_line(colour = "grey88", 
                                      size = 0.5), 
      
      # Facet labels        
      legend.key = element_rect(colour = "grey80"), 
      strip.background = element_rect(fill = "grey80", 
                                      colour = "grey50", 
                                      size = 0.2))
}
