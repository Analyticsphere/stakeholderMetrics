## Removes unnecessary buttons/logos in top right of plotly figures and 
## constructs a custom filename for the downloaded figure that incorporates the 
## selected filters and the date

apply_plotly_config <- function(plotly_obj, filters, plot_name) {
  
  # Get current date for the filename
  current_date <- format(Sys.Date(), "%Y-%m-%d")
  
  # Generate dynamic filename
  selected_filters <- filters[filters != "All"]
  
  # Format filter/selection pairings
  filter_pairs <- sapply(names(selected_filters), function(x) {
    filter_value <- tolower(gsub(" ", "_", selected_filters[[x]]))
    filter_name <- tolower(gsub(" ", "_", x))
    paste(filter_name, filter_value, sep = "=")
  })
  
  # Add filters to filename if any, and include datestamp
  if (length(filter_pairs) > 0) {
    dynamic_filename <- paste0(plot_name, ".", paste(filter_pairs, collapse = "&"), ".", "date=", current_date)
  } else {
    dynamic_filename <- paste0(plot_name, ".", "date=", current_date)
  }
  
  # Apply the configuration with the custom filename
  plotly_obj %>%
    config(
      modeBarButtonsToRemove = c(
        "zoom2d", "pan2d", "zoomIn2d", "zoomOut2d", "resetScale2d",  
        "hoverClosestCartesian", "toggleHover", "hoverCompareCartesian", 
        "lasso2d", "select", "autoScale2d"),
      displaylogo = FALSE,
      toImageButtonOptions = list(
        format = 'png',
        filename = dynamic_filename,
        height = 500,
        width = 700,
        scale = 3 # higher for more quality, lower for smaller file
      )
    )
}
