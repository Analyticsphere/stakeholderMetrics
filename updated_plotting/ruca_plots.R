ruca_plots = function(data, items_list){
  for (item in items_list) {
    # Create total_verified scatter plot
    tv_scatter = by_ruca_scatter(data, "total_verified", item)
    print(tv_scatter)  # Render the plot without print()
    
    # Create response_ratio scatter plot
    rr_scatter = by_ruca_scatter(data, "response_ratio", item)
    print(rr_scatter)  # Render the plot without print()
  }
}