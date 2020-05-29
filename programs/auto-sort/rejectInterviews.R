
reject_interview <- function(
	server,
	user, 
	password,
	interviewId, 
	comment,
	status, 
	output_dir
) {

# =============================================================================
# Load necessary libraries
# =============================================================================

# packages needed for this program 
packagesNeeded <- c(
	"RCurl", 	# to check that server exists
	"httr", 	# to talk with the server
	"stringr", 	# to check contents of `interviewId`
	"glue", 	# to create more readable error messages
	"dplyr",	# to do basic data wrangling
	"readr" 	# to write failure info to disk
)

# identify and install those packages that are not already installed
packagesToInstall <- packagesNeeded[!(packagesNeeded %in% installed.packages()[,"Package"])]
if(length(packagesToInstall)) 
	install.packages(packagesToInstall, quiet = TRUE, 
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

# -----------------------------------------------------------------------------
# `status` is valid (i.e., interview can be rejected)
# -----------------------------------------------------------------------------

if (!status %in% c(100, 120)) {
	stop(glue(
		"Can only reject interviews that are Completed (100) or ApprovedBySupervisor (120). 
		Interview {interviewId} has status code {status}"))
}

# -----------------------------------------------------------------------------
# `output_dir` exists
# -----------------------------------------------------------------------------

if (!dir.exists(output_dir)) {
	stop(glue(
		"Directory in `output_dir` does not exist in specified location.
		{output_dir}"

	))
}

# =============================================================================
# Make request
# =============================================================================	

# formulate API call
call <- paste0("https://", server, ".mysurvey.solutions", 
	"/api/v1/interviews/", interviewId, 
	case_when(
		status == 100 ~ "/reject", 		# ... if Completed
		status == 120 ~ "/hqreject"), 	# ... if ApprovedBySupervisor
	"?comment=", curlPercentEncode(comment))

# make request
response <- PATCH(url = call, config = authenticate(user = user, password = password))

# check that request worked
# if so, indicate success
if (httr::status_code(response) == 200) {

	print(paste0("Success rejecting interview: ", interviewId))

# if no, indicate failure and write failure to disk
} else {
	
	print(paste0("Problem rejecting interview ", interviewId))
	
	# extract interview info and reason for failure
	reject_failed <- data.frame(
		interview__id = interviewId,
		interview__status = status,
		status_code = httr::status_code(response),
		reason = content(response), # NOTE: previously, this was accessed by content(response)$Message, I believe
		stringsAsFactors = FALSE
	)
	
	# write failure info to disk
	readr::write_csv(
		reject_failed, 
		path = paste0(output_dir, "failed_rejections.csv"),
		append = TRUE
	)

}

}
