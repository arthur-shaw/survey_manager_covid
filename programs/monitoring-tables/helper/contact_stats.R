compute_contact_stats <- function(
	attempt_df,
    contact_df,
	group_var,
    overall_txt = ""
	) {

# load required libraries
library(rlang)
library(dplyr)

# statistics by group
stats_by_group <- attempt_df %>% 
    filter(!is.na({{group_var}})) %>%
    mutate(attempted = !is.na(interview__id)) %>%
    full_join(contact_df, by = "interview__id") %>%
    select({{group_var}}, attempted, reached, not_reached, unreachable) %>%
    group_by({{group_var}}) %>%
    summarize(
        num_attempts = sum(attempted, na.rm = TRUE),
        num_reached = sum(reached, na.rm = TRUE),
        num_not_reached = sum(not_reached, na.rm = TRUE),
        num_unreachable = sum(unreachable, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate_at(.vars = vars(matches("reach")), .funs = ~ ./num_attempts) %>%
    rename_at(.vars = vars(matches("reach")), .funs = ~ str_replace(., "num", "perc")) 

# statistics overall
stats_overall <- attempt_df %>% 
    mutate(attempted = !is.na(interview__id)) %>%
    full_join(contact_df, by = "interview__id") %>%
    select(attempted, reached, not_reached, unreachable) %>%
    summarize(
        num_attempts = sum(attempted, na.rm = TRUE),
        num_reached = sum(reached, na.rm = TRUE),
        num_not_reached = sum(not_reached, na.rm = TRUE),
        num_unreachable = sum(unreachable, na.rm = TRUE)
    ) %>%
    mutate_at(.vars = vars(matches("reach")), .funs = ~ ./num_attempts) %>%
    rename_at(.vars = vars(matches("reach")), .funs = ~ str_replace(., "num", "perc")) %>%
    mutate({{group_var}} := overall_txt)

# statistics combined
stats <- rbind(stats_by_group, stats_overall)

}