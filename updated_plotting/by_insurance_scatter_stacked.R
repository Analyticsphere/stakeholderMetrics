by_insurance_scatter_stacked = function(data, site_name) {
  # Filter and prepare Total Verified (TV) data
  data_sub_tv <- filter(data, population == "total_verified", site == site_name)
  relevant_columns <- grep("insurance_", colnames(data_sub_tv), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month", "overall_count")
  insurance_data_tv <- data_sub_tv[, relevant_columns]
  
  # Pivot table for Total Verified
  long_ins_tv <- insurance_data_tv %>% pivot_longer(cols = insurance_private_commercial_employer_or_direct_pay:insurance_unknown,
                                    names_to = "insurance_type", 
                                    names_prefix = "insurance_",
                                    values_to = "n")
  
  long_ins_tv$date <- as.Date(paste(long_ins_tv$year, long_ins_tv$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  # Update legend titles for TV data
  long_ins_tv <- long_ins_tv %>%
      mutate(insurance_type = case_when(
        insurance_type == "private_commercial_employer_or_direct_pay" ~ "Private, Commercial, Employer or Direct Pay",
        insurance_type == "medicare" ~ "Medicare",
        insurance_type == "medicaid" ~ "Medicaid",
        insurance_type == "va_champ" ~ "VA Champ",
        insurance_type == "va_and_champ_va" ~ "VA Champ",
        insurance_type == "tricare_military" ~ "Tricare Military",
        insurance_type == "tricare" ~ "Tricare Military",
        insurance_type == "uninsured" ~ "Uninsured",
        insurance_type == "unknown" ~ "Unknown",
        insurance_type == "workers_comp" ~ "Workers Comp",
        insurance_type == "other_public" ~ "Other Public Insurance",
        TRUE ~ insurance_type  # Default case to handle any other values that do not match
      ))
  
  # Order TV data and normalize `n` by `overall_count`
  long_ins_tv <- long_ins_tv[order(long_ins_tv$date), ]
  long_ins_tv$n <- (long_ins_tv$n / long_ins_tv$overall_count)*100
  
  long_ins_tv = filter_extra_months_ins(long_ins_tv)
  
  
  # Filter and prepare Response Ratio (RR) data
  data_sub_rr <- filter(data, population == "response_ratio", site == site_name)
  relevant_columns <- grep("insurance_", colnames(data_sub_rr), value = TRUE)
  relevant_columns <- c(relevant_columns, "year", "month", "overall_count")
  ins_data_rr <- data_sub_rr[, relevant_columns]
  
  # Pivot table for Response Ratio
  long_ins_rr <- ins_data_rr %>%
    pivot_longer(cols = insurance_private_commercial_employer_or_direct_pay:insurance_unknown,
                 names_to = "insurance_type", 
                 names_prefix = "insurance_",
                 values_to = "n")
  
  long_ins_rr$date <- as.Date(paste(long_ins_rr$year, long_ins_rr$month, "01", sep = "-"), format = "%Y-%m-%d")
  
  # Update legend titles for RR data
  long_ins_rr <- long_ins_rr %>%
    mutate(insurance_type = case_when(
      insurance_type == "private_commercial_employer_or_direct_pay" ~ "Private, Commercial, Employer or Direct Pay",
      insurance_type == "medicare" ~ "Medicare",
      insurance_type == "medicaid" ~ "Medicaid",
      insurance_type == "va_champ" ~ "VA Champ",
      insurance_type == "va_and_champ_va" ~ "VA Champ",
      insurance_type == "tricare_military" ~ "Tricare Military",
      insurance_type == "tricare" ~ "Tricare Military",
      insurance_type == "uninsured" ~ "Uninsured",
      insurance_type == "unknown" ~ "Unknown",
      insurance_type == "workers_comp" ~ "Workers Comp",
      insurance_type == "other_public" ~ "Other Public Insurance",
      TRUE ~ insurance_type  # Default case to handle any other values that do not match
    ))
  
  # Order RR data
  long_ins_rr <- long_ins_rr[order(long_ins_rr$date), ]
  long_ins_rr$n = long_ins_rr$n * 100

  long_ins_rr = filter_extra_months_ins(long_ins_rr)
  
  # Identify the number of unique sex values for color mapping
  unique_items <- unique(long_ins_tv$insurance_type)
  n_colors <- length(unique_items)
  
  # Select color palette and map colors
  cols <- select_colors(color_palette2, n_colors)
  color_mapping <- setNames(cols, unique_items)
  
  # Create plot for TV Data (Plot 1)
  plot1 <- plot_ly() %>%
    add_trace(
      data = long_ins_tv,
      x = ~date,
      y = ~n,
      color = ~insurance_type,
      colors = color_mapping,
      type = 'scatter',
      mode = 'lines+markers',
      hoverinfo = 'x+y',
      hovertemplate = '%{x|%B %Y}<br>%{y:.2f}%',
      name = ~insurance_type,  # Set name for the legend
      showlegend = TRUE,
      legendgroup = ~insurance_type  # Group legends by sex
    ) %>%
    layout(
      yaxis = list(title = "% of Total Verified", range = c(0, 100)),
      xaxis = list(title = "Date"),
      showlegend = TRUE
    )
  
  # Create plot for RR Data (Plot 2)
  plot2 <- plot_ly() %>%
    add_trace(
      data = long_ins_rr,
      x = ~date,
      y = ~n,
      color = ~insurance_type,
      colors = color_mapping,
      type = 'scatter',
      mode = 'lines+markers',
      hoverinfo = 'x+y',
      hovertemplate = '%{x|%B %Y}<br>%{y:.2f}%',
      name = ~insurance_type,  # Set name for the legend
      showlegend = FALSE,
      legendgroup = ~insurance_type  # Group legends by sex
    ) %>%
    layout(
      yaxis = list(title = "Response Ratio(%)", range = c(0,10)),
      xaxis = list(title = "Date"),
      showlegend = TRUE
    )
  
  
  # Combine both plots in a stacked layout
  plot <- subplot(plot1, plot2, nrows = 2, shareX = TRUE, titleX = TRUE, titleY = TRUE) %>%
    layout(
      title = paste(site_name, "Total % Of Verified Participants & Response Ratio By Insurance Type"),
      legend = list(title = list(text = "Insurance"))
    )
  
  return(plot)
}
