
dateIntFirstCompleted <- function(
	dataFile, 
	format = "stata"
) {

# load required libraries
library(haven)
library(dplyr)

# open interview__actions
if (format == "stata") {
	actions <- read_dta(file = dataFile, encoding = "UTF-8")
}

# identify first date completed
interviewDate <- actions %>%
	group_by(interview__id, interview__key) %>% # group by interview IDs
	filter(action == 3) %>% # retain obs where interview completed
	filter(row_number() == 1L) %>% # take the first one
	ungroup() %>%
	mutate(date = ymd(date)) %>% # make string date into date class object
	select(interview__id, interview__key, date)

# return data
return(interviewDate)

}
