compute_attempt_stats <- function(
	df, 
	group_var,
    overall_txt = ""
	) {

# load required libraries
library(rlang)
library(dplyr)

# statistics by group
stats_by_group <- df %>%
    select(interviewer, assignment__id, was_received, any_attempt, no_attempt) %>%
    filter(!is.na({{group_var}})) %>%
    group_by({{group_var}}) %>%
    summarize(
        num_assigned = sum(was_received, na.rm = TRUE),
        num_attempted = sum(any_attempt, na.rm = TRUE),
        num_no_attempted = sum(no_attempt, na.rm = TRUE)
    ) %>% 
    ungroup() %>%
    mutate_at(.vars = vars(matches("attempted")), .funs = ~ ./num_assigned) %>%
    rename_at(.vars = vars(matches("attempted")), .funs = ~ str_replace(., "num", "perc")) 

# statistics overall
stats_overall <- df %>%
    select(interviewer, assignment__id, was_received, any_attempt, no_attempt) %>%
    summarize(
        num_assigned = sum(was_received, na.rm = TRUE),
        num_attempted = sum(any_attempt, na.rm = TRUE),
        num_no_attempted = sum(no_attempt, na.rm = TRUE)
    ) %>% 
    mutate_at(.vars = vars(matches("attempted")), .funs = ~ ./num_assigned) %>%
    rename_at(.vars = vars(matches("attempted")), .funs = ~ str_replace(., "num", "perc")) %>%
    mutate({{group_var}} := overall_txt)

# statistics combined
stats <- rbind(stats_by_group, stats_overall)

}
