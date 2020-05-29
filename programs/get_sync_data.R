
# load functions
source(paste0(progsDir, "getSuSoSyncData.R"))

# get list of interviewers if it does not already exist
if (!exists("interviewers")) {
	source(paste0(progsDir, "get_team_composition.R"))
}

# get start and end dates from the setup file
if (is.na(surveyStart) | is.na(surveyEnd)) {

	stop("No data collection dates provided in the setup file")

} 

# fetch sync info
last_SuSo_syncs <- get_last_suso_sync(
	interviewers = interviewers,
	startDate = surveyStart,
	endDate = surveyEnd,
	site = site, user = user, password = password)


