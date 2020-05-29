compute_conversion_stats <- function(
	attempt_df,
    conversion_df,
	group_var,
    overall_txt = ""
	) {

# load required libraries
library(rlang)
library(dplyr)

# statistics by group
stats_by_group <- attempt_df %>% 
    filter(!is.na({{group_var}})) %>%
    full_join(conversion_df, by = "interview__id") %>%
    group_by({{group_var}}) %>%
    summarize(
        num_reached = sum(reached, na.rm = TRUE),
        num_agreed = sum(agreed, na.rm = TRUE),
        num_refused = sum(refused, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate_at(.vars = vars(num_agreed, num_refused), .funs = ~ ./num_reached) %>%
    rename_at(.vars = vars(num_agreed, num_refused), .funs = ~ str_replace(., "num", "perc"))

# statistics overall
stats_overall <- attempt_df %>% 
    full_join(conversion_df, by = "interview__id") %>%
    summarize(
        num_reached = sum(reached, na.rm = TRUE),
        num_agreed = sum(agreed, na.rm = TRUE),
        num_refused = sum(refused, na.rm = TRUE)
    ) %>%
    mutate_at(.vars = vars(num_agreed, num_refused), .funs = ~ ./num_reached) %>%
    rename_at(.vars = vars(num_agreed, num_refused), .funs = ~ str_replace(., "num", "perc")) %>%
    mutate({{group_var}} := overall_txt)

# statistics combined
stats <- rbind(stats_by_group, stats_overall)

}
