# =============================================================================
# Load necessary libraries
# =============================================================================

# packages needed for this program 
packagesNeeded <- c(
	"dplyr",		# for convenient data wrangling
	"haven", 		# for writing results to a Stata file
	"stringr", 		# for truncating (typically Unicode) strings that are too large for haven to save to Stata
	"readr" 		# for writing data to Excel
)									

# identify and install those packages that are not already installed
packagesToInstall <- packagesNeeded[!(packagesNeeded %in% installed.packages()[,"Package"])]
if(length(packagesToInstall)) install.packages(packagesToInstall, quiet = TRUE, repos = 'https://cloud.r-project.org/', dep = TRUE)

# load all needed packages
lapply(packagesNeeded, library, character.only = TRUE)

# =============================================================================
# Confirm inputs exist
# =============================================================================

# to reject
if (!exists("toReject")) {
	stop("List of interviews to reject not found.")
}

# issues
if (!exists("issues")) {
	stop("List of interview issues not found.")
}

# interview info
if (!exists("interviewInfo")) {
	stop("Interview metadata not found.")
}

# =============================================================================
# Add reject message to toReject
# =============================================================================

toReject <- 

	# add issues text to rejection list
	left_join(toReject, issues, by = "interview__id") %>%

	# order by interview, interview type, and interview description
	arrange(interview__id, issueType, issueDesc) %>%

	# limit to issues that will result in rejection
	filter(issueType == 1) %>%

	# remove duplicate issues (e.g., error check issues for variables in a roster)
	distinct(interview__id, interview__key, issueComment, .keep_all = TRUE) %>%

	# create reject message that is vertical concatenation of issue text
	# separated by new line character
	group_by(interview__id) %>%
	summarise(rejectMessage = paste(issueComment, collapse = " \n ")) %>%
	left_join(casesToReview, by = "interview__id")  %>%
	left_join(interviewInfo, by = "interview__id")

# write reject list to Stata
toReject_truncMsg <- toReject %>% mutate(rejectMessage = str_trunc(rejectMessage, width = 500))
write_dta(data = toReject_truncMsg, path = paste0(resultsDir, "toReject.dta"), version = stataVersion)

# write reject list to CSV
write_excel_csv(toReject, path = paste0(resultsDir, "toReject.csv"), col_names = TRUE)

# TODO: Add option for custom ordering
# There are a few methods for this
# First is to have a unique severity rank for each issue and to rank by that
	# This would mean another parameter to arrange() above
# Second is to order by issueDesc
	# This would involve ordering by index, and the index would be provided by match
	# See akrun's answer to this SO question : https://stackoverflow.com/questions/46129322/arranging-rows-in-custom-order-using-dplyr

# =============================================================================
# Confirm outputs exist
# =============================================================================

# check current environment
if (!exists("toReject")) {
	stop("List of interviews to reject not created.")
}

# check local storage
if (!file.exists(paste0(resultsDir, "toReject.dta"))) {
	stop("File toReject.dta not saved to requested folder")
}
