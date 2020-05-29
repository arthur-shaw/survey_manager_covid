# -----------------------------------------------------------------------------
# Load necessary packages
# -----------------------------------------------------------------------------

# packages needed for this program 
packagesNeeded <- c(
	"haven", 	# to injest Stata data
	"dplyr", 	# to perform basic data wrangling
	"purrr", 	# to iterate over interviews
	"stringr" 	# to identify id columns in comments df
)

# identify and install those packages that are not already installed
packagesToInstall <- packagesNeeded[!(packagesNeeded %in% installed.packages()[,"Package"])]
if(length(packagesToInstall)) install.packages(packagesToInstall, quiet = TRUE, repos = 'https://cloud.r-project.org/', dep = TRUE)

# load all needed packages
lapply(packagesNeeded, library, character.only = TRUE)

# -----------------------------------------------------------------------------
# Define common folders and file names
# -----------------------------------------------------------------------------

# folders
constructedDir <- paste0(dataDir, "hhold/derived/")
rawDir <- paste0(dataDir, "hhold/combined/")
rejectProgDir <- paste0(progsDir, "auto-sort/")
resultsDir <- paste0(projDir, "outputs/auto-sort/")

# files

# parameters
maxUnanswered <- 0
statusesToReject <- c(100, 120)
stataVersion <- 14

# -----------------------------------------------------------------------------
# Load functions
# -----------------------------------------------------------------------------

autoSortFunctions <- c(
	"checkForComments.R",
	"getInterviewStats.R",
	"rejectInterviews.R"
)

purrr::walk(.x = autoSortFunctions, .f = ~ source(paste0(rejectProgDir, .x)))

# -----------------------------------------------------------------------------
# Confirm that there are interview that can be processed
# -----------------------------------------------------------------------------

# read cases to review into memory
# filter down to those that are "rejectable"
cases <- read_dta(paste0(constructedDir, "casesToReview.dta")) %>%
	select(interview__id, interview__key, 	# interview identifiers
		interview__status, 					# interview status
		interviewComplete 					# user-defined flag for "complete" interviews
		)	

# completed to review for rejection
casesToReview <- cases %>%
	filter(
		(interview__status %in% statusesToReject) &
		(interviewComplete == 1))

# incomplete questionnaires to return
casesToReturn <- cases %>%
	filter(interviewComplete == 0)

# -----------------------------------------------------------------------------
# Return incomplete interviews
# -----------------------------------------------------------------------------

if (nrow(casesToReturn) > 0) {

# add message to returned cases
interviews_to_return <- casesToReturn %>%
	rename(interviewId = interview__id, status = interview__status) %>%
	mutate(comment = "MESSAGE: Continue work") %>%
	select(interviewId, comment, status)

# return questionnaires
purrr::pwalk(
	.l = interviews_to_return,
	.f = reject_interview,
	server = site,
	user = user,
	password = password,
	output_dir = paste0(projDir, "/logs/")	
)

} else if (nrow(casesToReturn) == 0)  {
	print("No interviews to return to interviewers")
}

# contine only if there is 1 or more
if (nrow(casesToReview) >= 1) {

# -----------------------------------------------------------------------------
# Check interviews for comments
# -----------------------------------------------------------------------------

print("Start comments")

issues <- read_dta(paste0(constructedDir, "issues.dta"))
comments <- read_dta(paste0(rawDir, "interview__comments.dta"))
comment_id_cols <- str_subset(names(comments), "^id[0-9]+$")
# TODO: extract ^id[0-9]+$ column names, and then pass it to the the function below
# See this for motiviation: https://forum.mysurvey.solutions/t/columns-missing-from-interview-comments-and-interview-errors-files/2449
# TODO: have id column names as argument for function

interview_hasComments <- check_for_comments(
	file_interviews = casesToReview, 
	file_comment = comments,
	comment_ids = comment_id_cols,
	file_issues = issues
)

print("End comments")

# -----------------------------------------------------------------------------
# Get interview statistics
# -----------------------------------------------------------------------------

print("Start interview stats")

interviewStats <- purrr::map_dfr(
	.x = casesToReview$interview__id,
	.f = get_interview_stats,
	server = site,
	user = user,
	password = password,
	output_dir = paste0(projDir, "/logs/")
)

write_dta(
	data = interviewStats, 
	path = paste0(constructedDir, "interviewStats.dta"), 
	version = stataVersion)

print("End interview stats")

# -----------------------------------------------------------------------------
# Decide what actions to take for each interview
# -----------------------------------------------------------------------------

print("Start decideAction.R")

source(paste0(rejectProgDir, "decideAction.R"), echo = TRUE)

print("End decideAction.R")

# -----------------------------------------------------------------------------
# Make rejection messages
# -----------------------------------------------------------------------------

print("Start makeRejectMsgs.R")

source(paste0(rejectProgDir, "makeRejectMsgs.R"), echo = TRUE)

print("End makeRejectMsgs.R")

# -----------------------------------------------------------------------------
# Flag issues that appear both in current rejections and in past rejections
# -----------------------------------------------------------------------------

print("Start flagPersistentProblems.R")

source(paste0(rejectProgDir, "flagPersistentProblems.R"), echo = TRUE)

print("End flagPersistentProblems.R")

# -----------------------------------------------------------------------------
# Execute rejections, posting comments and rjecting
# -----------------------------------------------------------------------------

if (to_reject == TRUE) {

	print("Start execute_rejections.R")

	source(paste0(rejectProgDir, "execute_rejections.R"))

	print("End execute_rejections.R")

}

} else if (nrow(casesToReview) == 0) {
	print("Currently no interviews to process that can be rejected")
} 
