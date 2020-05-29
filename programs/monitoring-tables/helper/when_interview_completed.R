when_interview_completed <- function(
	file_actions,
	exclusions = NA
) {

# load required libraries
library(haven)
library(dplyr)

# open interview__actions
actions <- read_dta(file = file_actions, encoding = "UTF-8")

# identify last interviewer to touch each interview
interviewerResponsible <- actions %>%
	# exclude interviewers from actions data set
	{if (!all(is.na(exclusions))) filter(., !(originator %in% exclusions)) else .} %>%
	group_by(interview__id, interview__key) %>% # group by interview identifiers
	filter(action == 3) %>% # interview completed
	filter(row_number() == 1L) %>% # take first obs in group
	ungroup() %>%
	rename(interviewer = originator) %>%
	select(interview__id, interview__key, date, interviewer)

# return data
return(interviewerResponsible)

}
