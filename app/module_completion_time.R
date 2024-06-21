#distribution of module completion times
module_completion_time <- function(data = data, survey ="BOH"){
  library(dplyr) 
  library(ggplot2)
  library(plotly)

    #make distribution for BOH survey time completions
    #BOH start date: d_264644252
    #BOH end date: d_452942800
    #participants table
    query <- paste0("SELECT connect_ID, d_264644252, d_452942800, d_770257102, 
                    d_386488297, d_541836531, d_832139544, d_517311251, d_205553981
                    FROM
                    `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants_JP`
                    WHERE Connect_ID is not NULL")
    project = "nih-nci-dceg-connect-prod-6d04"
    data <- bq_table_download(bq_project_query(project, query = query), bigint = "integer64")
    data$d_264644252 <- as.POSIXct(data$d_264644252, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")#LAW
    data$d_452942800 <- as.POSIXct(data$d_452942800, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")#LAW
    data$d_770257102 <- as.POSIXct(data$d_770257102, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")#SAS
    data$d_386488297 <- as.POSIXct(data$d_386488297, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")#SAS
    data$d_517311251 <- as.POSIXct(data$d_517311251, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")#BOH
    data$d_205553981 <- as.POSIXct(data$d_205553981, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")#BOH    
    data$d_541836531 <- as.POSIXct(data$d_541836531, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")#MRE
    data$d_832139544 <- as.POSIXct(data$d_832139544, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")#MRE
    
    if(survey == "LAW"){
    LAW <- data %>%
      filter(!is.na(d_264644252) & !is.na(d_452942800))
    
    # Calculate the survey completion time in minutes
    LAW <- LAW %>%
      mutate(completion_time = as.numeric(difftime(d_264644252, d_452942800,units = "mins")))
    
    # Filter the data to include only completion times between 0 and 300 minutes
    LAW <- LAW %>%
      filter(completion_time >= 0 & completion_time <= 300)
    
    # Plot the histogram of filtered completion times using plotly
    plot <- ggplot(LAW, aes(x = completion_time)) +
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
        filter(!is.na(d_770257102) & !is.na(d_386488297))
      
      # Calculate the survey completion time in minutes
      SAS <- SAS %>%
        mutate(completion_time = as.numeric(difftime(d_770257102, d_386488297,units = "mins")))
      
      # Filter the data to include only completion times between 0 and 300 minutes
      SAS <- SAS %>%
        filter(completion_time >= 0 & completion_time <= 300)
      
      # Plot the histogram of filtered completion times using plotly
      plot <- ggplot(SAS, aes(x = completion_time)) +
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
      BOH <- data %>%
        filter(!is.na(d_517311251) & !is.na(d_205553981))
      
      # Calculate the survey completion time in minutes
      BOH <- BOH %>%
        mutate(completion_time = as.numeric(difftime(d_517311251, d_205553981,units = "mins")))
      
      # Filter the data to include only completion times between 0 and 300 minutes
      BOH <- BOH %>%
        filter(completion_time >= 0 & completion_time <= 300)
      
      # Plot the histogram of filtered completion times using plotly
      plot <- ggplot(BOH, aes(x = completion_time)) +
        geom_histogram(binwidth = 2, fill = color_palette$blue[1], color = color_palette$blue[1], alpha = 0.7) +
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
        filter(!is.na(d_541836531) & !is.na(d_832139544))
      
      # Calculate the survey completion time in minutes
      MRE <- MRE %>%
        mutate(completion_time = as.numeric(difftime(d_832139544,d_541836531,units = "mins")))
      
      # Filter the data to include only completion times between 0 and 300 minutes
      MRE <- MRE %>%
        filter(completion_time >= 0 & completion_time <= 300)
      
      # Plot the histogram of filtered completion times using plotly
      plot <- ggplot(MRE, aes(x = completion_time)) +
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
