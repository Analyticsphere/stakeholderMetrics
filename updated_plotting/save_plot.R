save_plot = function(item, plot_to_save){
  # Create the "plots" folder if it doesn't exist
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  
  # Save the plot as an HTML file in the "plots" folder
  htmlwidgets::saveWidget(plot_to_save, file.path("plots", paste0(item, "_plot.html")), selfcontained = TRUE)
  
  # Save the plot as a PNG file in the "plots" folder
  webshot::webshot(file.path("plots", paste0(item, "_plot.html")), 
                   file = file.path("plots", paste0(item, "_plot.png")), 
                   vwidth = 800, vheight = 600)
}
