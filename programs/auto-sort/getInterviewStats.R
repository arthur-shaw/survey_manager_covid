get_interview_stats <- function(
	server,
	user,
	password,
	interviewId,
	output_dir
) {

# =============================================================================
# Load necessary libraries
# =============================================================================

# packages needed for this program 
packagesNeeded <- c(
	"RCurl", 	# for checking whether site exists
	"httr", 	# for API communication
	"stringr",  # for checking contentent of interviewId
	"readr"		# for exporting error long
)								

# identify and install those packages that are not already installed
packagesToInstall <- packagesNeeded[!(packagesNeeded %in% installed.packages()[,"Package"])]
if(length(packagesToInstall)) install.packages(packagesToInstall, quiet = TRUE, 
	repos = 'https://cloud.r-project.org/', dep = TRUE)

# load all needed packages
lapply(packagesNeeded, library, character.only = TRUE)

# =============================================================================
# Check user inputs
# =============================================================================	

# -----------------------------------------------------------------------------
# SuSo server exists
# -----------------------------------------------------------------------------

serverAddress <- paste0("https://", server, ".mysurvey.solutions/")

# TODO: figure out why this returns FALSE for SuSo servers now, but not for all other sites, including SuSo portal
# if (!RCurl::url.exists(serverAddress)) {
# 	stop(paste0("Server does not exist at address provided: ", "\n", serverAddress))
# }

# -----------------------------------------------------------------------------
# Credentials valid
# -----------------------------------------------------------------------------

loginCheckAddress <- paste0("https://", server, ".mysurvey.solutions/api/v1/questionnaires")

loginCheck <- httr::GET(
		url = loginCheckAddress, 
		accept_json(), 
		config = authenticate(user = user, password = password), 
		query = list(limit=40, offset=1)
	)

if (httr::status_code(loginCheck) != 200) {
	stop("The user and/or password provided are incorrect. Please correct in program parameters", "\n", 
		"User : ", user, "\n", 
		"Password : ", password, "\n"
	)
}

# -----------------------------------------------------------------------------
# `interview__id` of valid form
# -----------------------------------------------------------------------------

# check length
interviewId_length = nchar(interviewId)
if (interviewId_length != 32) {
	stop(paste(
		"`interviewId` is not of expected character length.", 
		"Expected: 32",
		paste0("Found: ", interviewId_length),
		sep = "\n"))
}

# check content
if (!stringr::str_detect(interviewId, "^([a-z]|[0-9]){32}")) {
	stop(paste(
		"`interviewId` contains different characters than expected.",
		"Expected: [a-z] or [0-9] only.",
		paste0("Provided: ", interviewId),
		sep = "\n"))

}

# =============================================================================
# Make request
# =============================================================================	

# formulate API call
call <- paste0("https://", server, ".mysurvey.solutions/",
	"api/v1/interviews/", interviewId, "/stats")

# make request
print(paste0("Interview ", interviewId))
print("Fetching statistics")
response <- GET(
	url = call, 
	accept_json(),
	config = authenticate(user = user, password = password))

# =============================================================================
# React to response
# =============================================================================	

status_code <- httr::status_code(response) 

# if request failed, capture details on failure
if (status_code != 200) {

	print("Problem rejecting")
	print("")

	# extract interview info and reason for failure
	failed_getStats <- data.frame(
		interview__id = interviewId,
		status_code = status_code,
		reason = content(response)$Message,
		stringsAsFactors = FALSE
	)

	# write failure info to disk
	readr::write_csv(
		failed_getStats, 
		path = paste0(output_dir, "failed_rejections.csv"),
		append = TRUE
	)

	print(paste0("Wrote issue to ", paste0(output_dir, "failed_rejections.csv")))

# if request succeeded, capture interview statistics
} else {

	print("Successfully received stats")
	print("")

	# store interview stats
	stats <- content(response)

	# add `interview__id` whose column name and contents matches other files 
	stats[["interview__id"]] <- interviewId 

	# return data
	return(stats)

}

}
