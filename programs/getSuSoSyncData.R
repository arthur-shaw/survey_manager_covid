
getActionLog <- function(
	interviewerId, 	# 32-character GUID, with or without embedded hyphens
	startDate, 		# date in ymd format
	endDate, 		# date in ymd format
	site, 			# stub site name
	user, 			# name of admin, HQ, or API user
	password 		# password of user
) {

	# load necessary packages
	library(httr)
	library(jsonlite)
	library(dplyr)

	# check inputs
	# TODO:
	# site exists
	# authentication correct
	# dates can be made into ymd()
	# endDate > startDate
	# interviewer ID is correct length: 32 char w/o hyphens, 36 w/ hyphens
	# interviewer ID consists only of allowable characters

	# form request
	request <- paste0(
		"https://", site, ".mysurvey.solutions/api/v1/", 
		"interviewers/", interviewerId, "/actions-log?start=", startDate, "&end=", endDate)

	# make request
	response <- GET(url = request, config = authenticate(user = user, password = password))

	# extract action log from response if OK response received
	if (status_code(response) ==  200) {

		actionLog <- fromJSON(content(response, as = "text"), flatten = TRUE) 

		# if log has any entries, add `interviewerId` column
		if (length(actionLog) > 0) {
			actionLog <- mutate(actionLog, interviewerId = interviewerId)
		# if log has no entries, construct a data.frame with expected columns
		} else if (length(actionLog) == 0) {
			actionLog <- data.frame(
				Time = NA_character_,
				Message = NA_character_, 
				interviewerId = as.character(interviewerId),
				stringsAsFactors = FALSE
			) 
		}
		return(actionLog)

	}

}

# # test for vector/list of interview IDs
# interviewers <- c(
# "22a2f987-4956-4837-9d2a-9d01ae950310",	# interviewer 1
# "c635d7de-e5f3-4cc9-be9d-282e83882381", # interviewer 11
# "aec4a1a9-d679-4301-8d50-84f31a933697"	# interviewer 12
# )
# blah <- purrr::map_dfr(.x = interviewers, .f = getActionLog, startDate = "2019-10-07", endDate = "2019-12-02", site = "cseslsms", user = "admin", password = "d'Z2w4hEO2")

# # test for single case
# getActionLog(interviewerId = "22a2f987-4956-4837-9d2a-9d01ae950310", startDate = "2019-10-07", endDate = "2019-12-02", site = "cseslsms", user = "admin", password = "d'Z2w4hEO2")


calcLastSync <- function(
	data 	# action log data file 
) {

	# load necessary packages
	library(dplyr)
	library(lubridate)
	library(stringr)

	# check inputs 
	# TODO: 
	# object exists in global environment
	# contains expected columns: Time, Message, interviewerId
	# columns expected types: chr for all

	# extract last sync date for each interviewer
	syncDates <- data %>%
		
		# keep only completed syncs
		filter(str_detect(Message, pattern = "^Sync completed") | is.na(Message)) %>%
		# transform character date/time into date
		mutate(date = ymd(str_sub(Time, start = 1L, end = 10L))) %>%
		# keep the first (most recent) sync event
		group_by(interviewerId) %>%
		summarize(lastSync = first(date)) %>%
		ungroup()

	return(syncDates)

}

# test extraction of dates
# calcLastSync(data = blah)

get_last_suso_sync <- function(interviewers, startDate, endDate, site, user, password) {

	library(purrr)

	# get action logs for each interviewer
	actionLogs <- purrr::map_dfr(.x = interviewers$interviewerId, .f = getActionLog, 
		startDate = startDate, endDate = endDate, 
		site = site, user = user, password = password)

	# extract sync date from action logs
	last_SuSo_syncs <- calcLastSync(data = actionLogs)

}

