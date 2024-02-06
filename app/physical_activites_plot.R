#physical activities

physical_activities_plot<- function(data){
  # Apply the labels to your data using the expss package
  # Rename the columns to the specified activities without spaces
  d <- data %>%
    rename(
      Walk_or_hike_for_exercise = D_517976064_D_904954920,
      Jog_or_run = D_517976064_D_619501806,
      Play_tennis_squash_or_racquetball = D_517976064_D_203192394,
      Play_golf = D_517976064_D_261267696,
      Swim_laps = D_517976064_D_926584500,
      Ride_a_bicycle = D_517976064_D_420058896,
      Strengthening_exercises = D_517976064_D_868685663,
      Yoga_Pilates_or_Tai_Chi = D_517976064_D_760484278,
      Martial_Arts = D_517976064_D_345916806,
      Dance = D_517976064_D_936042582,
      Downhill_ski_or_snowboard = D_517976064_D_182827107,
      Cross_country_ski = D_517976064_D_734860227,
      Surf_or_bodyboard = D_517976064_D_371531887,
      High_intensity_circuit_training = D_517976064_D_423631576,
      Other_activity = D_517976064_D_181769837,
      None_of_the_above = D_517976064_D_535003378)
  
  activity_columns <- c("Walk_or_hike_for_exercise", "Jog_or_run", "Play_tennis_squash_or_racquetball",
                        "Play_golf", "Swim_laps", "Ride_a_bicycle", "Strengthening_exercises",
                        "Yoga_Pilates_or_Tai_Chi", "Martial_Arts", "Dance", "Downhill_ski_or_snowboard",
                        "Cross_country_ski", "Surf_or_bodyboard", "High_intensity_circuit_training",
                        "Other_activity", "None_of_the_above")
  
  # Assuming 'data' is your dataframe with one column for each activity. 
  d = d[,activity_columns]
  # Aggregate the data to get counts of each activity
  data_numeric <- d %>%
    mutate(across(everything(), ~as.numeric(.x)))
  
  # Now aggregate the data
  activity_counts <- data_numeric %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>%
    pivot_longer(cols = everything(), names_to = "activity", values_to = "count")
  
  # Create a bar plot with ggplot
p<-  ggplot(activity_counts, aes(x = activity, y = count)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    labs(title = "Total Number of Individuals Engaging in Physical Activity in the past 12 Months",
         x = "Activity",
         y = "Count")
p  
}
