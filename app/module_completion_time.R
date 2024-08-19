#distribution of module completion times
module_completion_time <- function(data = data, survey ="BOH"){
  library(dplyr) 
  library(ggplot2)
  library(plotly)

    #make distribution for each survey time completions

    if(survey == "LAW"){
    LAW <- data %>%
      filter(!is.na(law_start_date) & !is.na(law_completion_date))
    
    #compute time difference
    LAW <- LAW %>%
      mutate(
        law_start_date = as.POSIXct(law_start_date, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
        law_end_date = as.POSIXct(law_completion_date, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")
      )
    
    LAW$time_difference = difftime(LAW$law_start_date, LAW$law_completion_date, units="mins")

    # Filter the data to include only completion times between 0 and 300 minutes
    LAW <- LAW %>%
      filter(time_difference >= 0 & time_difference <= 300)
    
    # Plot the histogram of filtered completion times using plotly
    plot <- ggplot(LAW, aes(x = time_difference)) +
      geom_histogram(binwidth = 2,fill = color_palette$blue[1], color = color_palette$blue[1], alpha = 0.7) +
      labs(title = "LAW Survey Completion Times (0 to 300 minutes)",
           x = "Completion Time (minutes)",
           y = "Frequency") +
      theme_minimal()
    
    # Convert ggplot to plotly
    plotly_plot <- ggplotly(plot)
    
    # Display the plotly plot
    plotly_plot}else if(survey == "SAS"){
      SAS <- data %>%
        filter(!is.na(sas_start_date) & !is.na(sas_completion_date))
      
      #compute time difference
      SAS <- SAS %>%
        mutate(
          sas_start_date = as.POSIXct(sas_start_date, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
          sas_end_date = as.POSIXct(sas_completion_date, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")
        )
      
      SAS$time_difference = difftime(SAS$sas_start_date, SAS$sas_completion_date, units="mins")
      
      # Filter the data to include only completion times between 0 and 300 minutes
      SAS <- SAS %>%
        filter(time_difference >= 0 & time_difference <= 300)
      
      # Plot the histogram of filtered completion times using plotly
      plot <- ggplot(SAS, aes(x = time_difference)) +
        geom_histogram(binwidth = 2, fill = color_palette$blue[1], color = color_palette$blue[1], alpha = 0.7) +
        labs(title = "SAS Survey Completion Times (0 to 300 minutes)",
             x = "Completion Time (minutes)",
             y = "Frequency") +
        theme_minimal()
      
      # Convert ggplot to plotly
      plotly_plot <- ggplotly(plot)
      
      # Display the plotly plot
      plotly_plot
    }else if(survey == "BOH"){
      #remove NA values
      BOH <- data %>%
        filter(!is.na(boh_start_date) & !is.na(boh_completion_date))
      
      BOH <- BOH %>%
        mutate(
          boh_start_date = as.POSIXct(boh_start_date, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
          boh_end_date = as.POSIXct(boh_completion_date, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")
        )
      
      BOH$time_difference = difftime(BOH$boh_start_date, BOH$boh_completion_date, units="mins")
      
      # Filter the data to include only completion times between 0 and 300 minutes
      BOH <- BOH %>%
        filter(time_difference >= 0 & time_difference <= 300)
      
      # Plot the histogram of filtered completion times using plotly
      plot <- ggplot(BOH, aes(x = time_difference)) +
        geom_histogram(binwidth = 5, fill = color_palette$blue[1], color = color_palette$blue[1], alpha = 0.7) +
        labs(title = "BOH Survey Completion Times (0 to 300 minutes)",
             x = "Completion Time (minutes)",
             y = "Frequency") +
        theme_minimal()
      
      # Convert ggplot to plotly
      plotly_plot <- ggplotly(plot)
      
      # Display the plotly plot
      plotly_plot
    }else if(survey == "MRE"){
      MRE <- data %>%
        filter(!is.na(mre_start_date) & !is.na(mre_completion_date))
      
      MRE <- MRE %>%
        mutate(
          mre_start_date = as.POSIXct(mre_start_date, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
          mre_end_date = as.POSIXct(mre_completion_date, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")) 
      
      MRE$time_difference = difftime(MRE$mre_start_date, MRE$mre_completion_date, units="mins")
      
      # Filter the data to include only completion times between 0 and 300 minutes
      MRE <- MRE %>%
        filter(time_difference >= 0 & time_difference <= 300)
      
      # Plot the histogram of filtered completion times using plotly
      plot <- ggplot(MRE, aes(x = time_difference)) +
        geom_histogram(binwidth = 2,fill = color_palette$blue[1], color = color_palette$blue[1], alpha = 0.7) +
        labs(title = "MRE Survey Completion Times (0 to 300 minutes)",
             x = "Completion Time (minutes)",
             y = "Frequency") +
        theme_minimal()
      
      # Convert ggplot to plotly
      plotly_plot <- ggplotly(plot)
      
      # Display the plotly plot
      plotly_plot
    }
  }
