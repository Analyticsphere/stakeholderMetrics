filter_extra_months_ses <- function(data) {
  data %>%
    group_by(date, ses_quartile) %>%
    mutate(row_count = n()) %>%  # Count the number of rows for each group (month, year)
    ungroup() %>%
    group_by(date, ses_quartile) %>%
    # Use ifelse to apply the condition correctly across all rows
    filter(ifelse(row_count > 1, overall_count %in% sort(overall_count, decreasing = TRUE)[1], TRUE)) %>%
    ungroup() %>%
    select(-row_count) %>%  # Remove the temporary row_count column
    distinct()              # Remove duplicate rows
}