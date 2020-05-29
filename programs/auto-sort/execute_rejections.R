# -----------------------------------------------------------------------------
# Load cases
# -----------------------------------------------------------------------------

# read cases to review into memory
casesToReviewPath <- paste0(constructedDir, casesToReviewDta)

# filter down to those that are "rejectable"
casesToReview <- read_stata(casesToReviewPath) %>%

		select(interview__id, interview__key, 	# interview identifiers
			interview__status, 					# interview status
			interviewComplete 					# user-defined flag for "complete" interviews
			) %>%
		filter(
			(interview__status %in% statusesToReject) &
			(interviewComplete == 1))

# contine only if there is 1 or more
if (nrow(casesToReview) >=1) {

# -----------------------------------------------------------------------------
# Load functions
# -----------------------------------------------------------------------------

rejectFunctions <- c(
	"postComments.R",
	"rejectInterviews.R"
)

purrr::walk(.x = rejectFunctions, .f = ~ source(paste0(rejectProgDir, .x)))

# -----------------------------------------------------------------------------
# Post comments
# -----------------------------------------------------------------------------

# gather var-level comments for interviews to reject
if (!exists("comments_to_post")) {
	
	# if any inputs do not exist load them all
	if (!exists("toReject") | !exists("issues") | !exists("toFollowUp")) {

		toReject <- readr::read_csv(file = paste0(resultsDir, "toReject.csv"))
		issues <- read_dta(file = paste0(constructedDir, "issues.dta"))
		toFollowUp <- readr::read_csv(file = paste(resultsDir, "toFollowUp.csv"))

	} 

	# create comments file
	comments_to_post <- toReject %>% 
		left_join(issues, by = c("interview__id", "interview__key")) %>%
		filter(issueType == 2) %>%
		anti_join(toFollowUp, by = c("interview__id", "interview__key"))

}

# if there are any comments, post them
if (nrow(comments_to_post) > 0) {

	post_comments_to_vars(
		file_comments_to_post = comments_to_post, 
		server = server,
		user = login,
		password = password,
		output_dir = paste0(projDir, "/logs/")
	)

}

# -----------------------------------------------------------------------------
# Reject interviews
# -----------------------------------------------------------------------------

# gather interviews to reject
if (!exists("interviews_to_reject")) {

	# if does not exist in memory, load it
	if (!exists("interviews_to_reject")) {
		toReject <- readr::read_csv(file = paste0(resultsDir, "toReject.csv"))
	}

	# modify file to rejection function's expectations
	interviews_to_reject <- toReject %>%
		select(interview__id, rejectMessage, interview__status) %>%
		rename(
			interviewId = interview__id,
			comment = rejectMessage,
			status = interview__status
		)

}

# if there are any interviews, reject them
if (nrow(interviews_to_reject) > 0) {

	purrr::pwalk(
		.l = interviews_to_reject,
		.f = reject_interview,
		server = site,
		user = user,
		password = password,
		output_dir = paste0(projDir, "/logs/")	
	)

}


} else if (nrow(casesToReview) == 0) {
	print("Currently no interviews to process that can be rejected")
} 
