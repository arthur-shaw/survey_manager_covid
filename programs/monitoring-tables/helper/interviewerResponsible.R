
interviewerResponsible <- function(
	dataFile,
	format = "stata",
	exclusions = NA
) {

# load required libraries
library(haven)
library(dplyr)

# open interview__actions
actions <- read_dta(file = dataFile, encoding = "UTF-8")

# identify last interviewer to touch each interview
interviewerResponsible <- actions %>%
	# exclude interviewers from actions data set
	{if (!all(is.na(exclusions))) filter(., !(responsible__name %in% exclusions)) else .} %>%
	group_by(interview__id, interview__key) %>% # group by interview identifiers
	filter(responsible__role == 1) %>% # interviewer provided answers
	filter(row_number() == n()) %>% # take last obs in group
	ungroup() %>%
	rename(interviewer = responsible__name) %>%
	select(interview__id, interview__key, interviewer)

# return data
return(interviewerResponsible)

}
