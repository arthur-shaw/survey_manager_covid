# =============================================================================
# 					DESCRIPTION OF PROGRAM
# 					------------------------
#
# DESCRIPTION:	Decides action for each interview: reject, review, or approve
# 				- To reject interviews have: 1 or more major issue but no 
#				(potentially) explanatory comments on variables that are inputs 
#				into the issues
#				- To review interview have: 1 or more issues but with some
#				(potentially) explanatory comments on issue variables; or 
#				any comments on any variables or the interview itself; or
#				any SuSo validation errors
#				- To approve interviews have: no issues, no comments
# AUTHOR: 		Arthur Shaw, jshaw@worldbank.org
# =============================================================================

# =============================================================================
# Load necessary libraries
# =============================================================================

# packages needed for this program 
packagesNeeded <- c(
	"dplyr",	# for convenient data wrangling
	"haven",  	# for loading/writing data to Stata
	"readr", 	# for writing data to Excel
	"stringr" 	# for handling Unicode text written via mutate
)														

# identify and install those packages that are not already installed
packagesToInstall <- packagesNeeded[!(packagesNeeded %in% installed.packages()[,"Package"])]
if(length(packagesToInstall)) install.packages(packagesToInstall, quiet = TRUE, repos = 'https://cloud.r-project.org/', dep = TRUE)

# load all needed packages
lapply(packagesNeeded, library, character.only = TRUE)

# =============================================================================
# Confirm inputs exist
# =============================================================================

# interview actions
interviewActions <- paste0(rawDir, "interview__actions.dta")
if (!file.exists(interviewActions)) {
	stop("Interview actions file cannot be found")
}

# inteview statistics
if (!exists("interviewStats")) {
	stop("Interview stats file cannot be found")
}

# interview issues
if (!exists("issues")) {
	stop("Interview issues not loaded. If needed, read the file from storage.")
}

if (!exists("casesToReview")) {
	stop("Cases to review not loaded. If needed, read the file from storage.")
}

if (!exists("interview_hasComments")) {
	stop("Interviews with comments cannot be loaded. If needed, re-run checkForComments.R")
}

# =============================================================================
# Create interview metadata for inclusion in to[ACTION] files
# =============================================================================

actions <- read_stata(file = paste0(rawDir, "interview__actions.dta"))

interviewInfo <- actions %>%			
	filter(action == 3) %>%			# filter to completed
	group_by(interview__id) %>% 	# group by interview
	slice(n()) %>% 					# take last action
	ungroup() %>%
	rename(interviewer = originator, supervisor = responsible__name) %>%
	select(interview__id, interviewer, supervisor)

# =============================================================================
# Determine whether has rejectable attribute
# =============================================================================

# has at least 1 major issue
interview_hasIssues <- 
	issues %>% filter(issueType == 1) %>%
	distinct(interview__id, interview__key) %>%
	inner_join(casesToReview, by = c("interview__id", "interview__key"))

# has at least N question(s) unanswered

# if (file.exists(paste0(constructedDir, numLegitMissDta))) {

# 	legitMissings <- 
# 		read_stata(paste0(constructedDir, numLegitMissDta))

# 	interview_hasUnanswered <-
# 		interviewStats %>%
# 		inner_join(legitMissings, by = "interview__id") %>%
# 		mutate(Unanswered = NotAnswered - numLegitMiss) %>%
# 		filter(Unanswered > maxUnanswered) %>% 
# 		select(interview__id)

# } else {

	interview_hasUnanswered <-
		interviewStats %>%
		mutate(Unanswered = NotAnswered) %>%
		filter(Unanswered > maxUnanswered) %>% 
		select(interview__id)

# }

# =============================================================================
# Determine whether has attribute that necessitates reivew
# =============================================================================

interview_needsReview <- 
	filter(issues, issueType == 4) %>%
	distinct(interview__id, interview__key) %>%
	inner_join(casesToReview, by = c("interview__id", "interview__key"))	

# =============================================================================
# Reject
# =============================================================================

# identify interviews to reject
toReject <- 

	casesToReview %>%

	# has at least 1 major issue, but no comments
	semi_join(
		anti_join(					
			interview_hasIssues, 	
			interview_hasComments, by = c("interview__id", "interview__key")),
		by = "interview__id") %>%

	# or has at least N unanswered questions
	full_join(interview_hasUnanswered, by = "interview__id") %>%

	distinct(interview__id) %>%
	left_join(casesToReview, by = "interview__id") %>%
	select(interview__id, interview__status)

# write list to disk as a Stata file
write_dta(data = toReject, path = paste0(resultsDir, "toReject.dta"), version = stataVersion)

# create an issue for each interview with unanswered questions
issues_hasUnanswered <- 
	interviewStats %>%
	# inner_join(legitMissings, by = "interview__id") %>%
	mutate(Unanswered = NotAnswered) %>%
	filter(Unanswered > maxUnanswered)	%>%
	mutate(
		issueType = 1, 
		issueDesc = str_conv("Unanswered questions", encoding = "UTF-8"),
		issueComment = str_conv(paste0("ERROR: ", Unanswered, " questions left unanswered"), encoding = "UTF-8"),
		issueLoc = "",
		issueVars = ""
		) %>%
	select(interview__id, Unanswered, NotAnswered, starts_with("issue")) %>%
	left_join(casesToReview, by = "interview__id") %>%
	select(interview__id, interview__key, issueType, issueDesc, issueComment)

# add these issues has issues data base
issues <- full_join(issues, issues_hasUnanswered, by = c("interview__id", "interview__key", "issueType", "issueDesc", "issueComment"))
	# bind_rows(issues, issues_hasUnanswered)

# =============================================================================
# Review
# =============================================================================

# identify interviews to review
toReview <- 

	casesToReview %>%
	
	# has both 1+ major issue and 1+ comments on an issue var
	semi_join(
		intersect(
			interview_hasIssues, 
			interview_hasComments),
		by = "interview__id") %>%

	# or has an attribute that requires review
	full_join(interview_needsReview, by = "interview__id") %>%

	# or has at least 1 comment or validation error
	full_join(
		interviewStats %>% 
			filter(Invalid >= 1 | WithComments >= 1), 
		by = "interview__id") %>%

	# and not on reject list
	anti_join(toReject, by = "interview__id") %>%

	distinct(interview__id) %>%
	left_join(interviewStats, by = "interview__id") %>%
	mutate(numInvalid = Invalid, numComments = WithComments) %>%
	select(interview__id, numComments, numInvalid) %>%
	left_join(casesToReview, by = "interview__id") %>%  
	select(interview__id, interview__key, interview__status, numComments, numInvalid) %>%
	left_join(interviewInfo, by = "interview__id")
	
# create an issue for each interview with unanswered questions
# issues_hasUnanswered <- 
# 	interviewStats %>%
# 	filter(NotAnswered > 1) %>%	# TODO: Make this a parameter
# 	mutate(
# 		issueType = 1, 
# 		issueDesc = "Question(s) laissées sans réponse",
# 		issueComment = paste0("ERREUR: ", NotAnswered, " questions ont été laissées sans réponse")
# 		) %>%
# 	select(interview__id, issueType, issueDesc, issueComment) %>%
# 	left_join(casesToReview, by = "interview__id") %>%  
# 	select(interview__id, interview__key, issueType, issueDesc, issueComment)	

# create an issue for each validation error
errors <- read_stata(paste0(rawDir, "interview__errors.dta"))
issues_hasInvalid <-
	interviewStats %>%
	filter(Invalid > 0) %>%
	left_join(errors, by = c("interview__id")) %>%
	mutate(
		issueType = 3,
		issueDesc = "Validation error", 
		issueComment = message,
		issueVars = variable,
		issueLoc = 
			case_when(
				# interview level
				(id1 == "") ~ "",
				# in level 1 roster 
				# id1 != "" & id2 == "" ~ paste0("[", id1, "]") 
				# ,
				# in level 2 roster
				# id1 != "" & id2 != "" & id3 == "" ~ paste0("[", id1, ", ", id2, "]"),
				# in level 3 roster
				# id1 != "" & id2 != "" & id3 != "" ~ paste0("[", id1, ", ", id2, ", ", id3, "]")	
			)
		) %>%
	left_join(casesToReview, by = c("interview__id", "interview__key")) %>%  
	select(interview__id, interview__key, issueType, issueDesc, issueComment, issueVars)	

# add these issues has issues data base
issues <- full_join(issues, issues_hasInvalid, by = c("interview__id", "interview__key", "issueType", "issueDesc", "issueComment", "issueVars"))
	# bind_rows(issues, issues_hasUnanswered, issues_hasInvalid)

# write list to disk as a Stata file
write_dta(data = toReview, path = paste0(resultsDir, "toReview.dta"), version = stataVersion)

# write to CSV
write_excel_csv(toReview, path = paste0(resultsDir, "toReview.csv"), col_names = TRUE)

# =============================================================================
# Approve
# =============================================================================

# identify interviews to approve
toApprove <- 

	casesToReview %>%

	# has no comments, no unanswered questions, and no validation errors
	inner_join(interviewStats, by = "interview__id") %>%
	filter(WithComments == 0 & NotAnswered == 0 & Invalid == 0) %>%
	
	# has no issues
	anti_join(interview_hasIssues, by = "interview__id") %>%

	select(interview__id) %>%
	left_join(casesToReview, by = "interview__id") %>%
	select(interview__id, interview__status)

# write list to disk as a Stata file
# write_dta(data = toApprove, path = paste0(resultsDir, "toApprove.dta"), version = stataVersion)

# =============================================================================
# Export augmented issues file containing error check and SuSo issues
# =============================================================================

write_dta(data = issues, path = paste0(constructedDir, "issues.dta"), version = stataVersion)

# =============================================================================
# Confirm outputs created
# =============================================================================

# to reject
if (!exists("toReject")) {
	stop("List of interviews to reject was not created.")
}

# to review
if (!exists("toReview")) {
	stop("List of interviews to review was not created.")
}

# to approve
if (!exists("toApprove")) {
	stop("List of interviews to approve was not created.")
}
