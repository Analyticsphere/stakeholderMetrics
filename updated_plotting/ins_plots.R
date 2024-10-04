ins_plots = function(data, items_list){
  for (item in items_list) {
    # Create total_verified scatter plot and save it
    tv_scatter = by_insurance_scatter(data, "total_verified", item)
    save_plot(paste0(item, "_total_verified_by_ins"), tv_scatter)
    
    # Create response_ratio scatter plot and save it
    rr_scatter = by_insurance_scatter(data, "response_ratio", item)
    save_plot(paste0(item, "_response_ratio_by_ins"), rr_scatter)
  }
}