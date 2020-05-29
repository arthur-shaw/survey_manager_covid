compute_length_stats <- function(
	interviews_df,
    interview_complete,
    interviewers_df,
    length_df,
	group_var,
    overall_txt = ""
	) {

# load required libraries
library(rlang)
library(dplyr)

# statistics by group
stats_by_group <<- 
    full_join(interviews_df, interviewers_df, 
        by = c("interviewer", "supervisor")) %>%
    left_join(length_df, by = c("interview__id", "interview__key")) %>%
    filter(
        !is.na({{group_var}}) &
        {{interview_complete}}    
    ) %>%
    group_by({{group_var}}) %>%
    summarize(
        num_obs = sum(!is.na(interview__id), na.rm = TRUE),
        minimum = fivenum(duration)[1], 
        lower_hinge = fivenum(duration)[2], 
        median = fivenum(duration)[3], 
        upper_hinge = fivenum(duration)[4], 
        maximum = fivenum(duration)[5]
    ) %>%
    ungroup()

# statistics overall
stats_overall <<- 
    full_join(interviews_df, interviewers_df, 
        by = c("interviewer", "supervisor")) %>%
    left_join(length_df, by = c("interview__id", "interview__key")) %>%
    filter({{interview_complete}}) %>%
    summarize(
        num_obs = sum(!is.na(interview__id), na.rm = TRUE),
        minimum = fivenum(duration)[1], 
        lower_hinge = fivenum(duration)[2], 
        median = fivenum(duration)[3], 
        upper_hinge = fivenum(duration)[4], 
        maximum = fivenum(duration)[5]
    ) %>%
    mutate({{group_var}} := overall_txt)

# statistics combined
stats <- rbind(stats_by_group, stats_overall)

}
