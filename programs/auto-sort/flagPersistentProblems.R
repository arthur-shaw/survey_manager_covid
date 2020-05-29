# =============================================================================
# 					DESCRIPTION OF PROGRAM
# 					------------------------
#
# DESCRIPTION:	Flags interviews for follow-up by headquarters. These interviews
# 				are being rejected or reviewed for issues flagged during a
#				prior round of rejection. 
#
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
	"stringr", 	# for cleaning string interview rejection comments
	"tidyr", 	# for expanding rejection comments into their composite issues
	"readr" 	# for writing data to Excel
)														

# identify and install those packages that are not already installed
packagesToInstall <- packagesNeeded[!(packagesNeeded %in% installed.packages()[,"Package"])]
if(length(packagesToInstall)) install.packages(packagesToInstall, quiet = TRUE, repos = 'https://cloud.r-project.org/', dep = TRUE)

# load all needed packages
lapply(packagesNeeded, library, character.only = TRUE)

# =============================================================================
# Confirm inputs exist
# =============================================================================

# folders

if (!exists("rawDir")) {
	stop("Path to combined data files not loaded.")
}

if (!exists("resultsDir")) {
	stop("Path to auto-sort's results not loaded")
}

# interview comments
commentsPath <- paste0(rawDir, "interview__comments.dta")
if (!file.exists(commentsPath)) {
	stop(paste0("Comments file cannot be found at expected location: ", commentsPath))
}

# toReject.dta
toRejectPath <- paste0(resultsDir, "toReject.csv")
if (!file.exists(toRejectPath)) {
	stop(paste0("Rejection file cannot be found at expected location: ", toRejectPath))
}

# =============================================================================
# Process current and past rejection messages
# =============================================================================

# past rejection messages
messages_pastRejects <- 
	
	# load comments file, which contains rejections
	read_stata(paste0(rawDir, "interview__comments.dta")) %>%
	
	# find rejections
	filter((variable %in% c("@@RejectedBySupervisor", "@@RejectedByHeadquarter")) & 
		(role %in% c("Administrator", "Headquarter"))) %>%

  	mutate(														

   		# remove undesirable content from rejection messages
  		comment = str_replace(comment, '^"[ ]*', ""),			# starting quote
  		comment = str_replace(comment, '[ ]*"$', ''),			# ending quote
  		comment = str_replace(comment, 							# ending strange content
  			"\\[WebInterviewUI:CommentYours[\\]]*$", ""),
  		comment = str_replace(comment,
  			"^[\\[]*WebInterviewUI:CommentYours\\] ", ""),		# starting strange content
  		comment = str_replace(comment, "Your comment ", ""),	# more starting strange content
  		comment = str_trim(comment, side = "both"), 			# whitespace padding
  		comment = str_replace(comment, "\\.$", ""), 			# terminal .
  		comment = str_replace(comment, "\\n[ \\.]*$", ""), 		# terminal \n
  		
  		# make date variable into Date type
		date = as.Date(date, format = "%m/%d/%Y")) %>%

  	# expand data set to the error level, where separators are newline characters
  	separate_rows(comment, sep = " \\n ") %>%
  	mutate(comment = as.character(comment)) %>%

  	# keep only the necessary columns
  	select(interview__id, interview__key, date, order, comment)

# current rejection messages
messages_currentRejects <- 
	
	# load rejections file, which contains newline-separated error messages
	read_csv(file = paste0(resultsDir, "toReject.csv"), col_names = TRUE) %>%
	
	# use file creation date as rejection date
	mutate(date = file.info(paste0(resultsDir, "toReject.csv"))$mtime %>% as.Date()) %>%
	
	# rename error message column to match interview__comments
	rename(comment = rejectMessage) %>%

	# expand data set to the error level, where separators are newline characters
	separate_rows(comment, sep = " \\n ") %>%
	mutate(comment = as.character(comment)) %>%

	# keep only the necessary columns
	select(interview__id, interview__key, date, comment)

# combine past and current rejection messages
messages_allRejects <- 

	# merge the two data files
	full_join(messages_pastRejects, messages_currentRejects, 
		by = c("interview__id", "interview__key", "date", "comment")) %>%

	# sort errors into sequential order
	group_by(interview__id) %>%
	arrange(interview__id, date, order, .by_group = TRUE) %>%
	ungroup()

# =============================================================================
# Identify any current messages that have appeared in the past
# =============================================================================

# repeated messages, that appear in current rejections but also in past rejections
messages_repeated <- messages_currentRejects %>%
	select(interview__id, interview__key, comment) %>%
	semi_join(messages_pastRejects, 
		by = c("interview__id", "interview__key", "comment"))

# interviews where follow-up is required for repeated comments
toFollowUp <- 
	semi_join(messages_allRejects, messages_repeated, 
		by = c("interview__id", "interview__key", "comment"))

# =============================================================================
# Write result to file
# =============================================================================

# Stata
toFollowUp_truncMsg <- toFollowUp %>% mutate(comment = str_trunc(comment, width = 500))
write_dta(data = toFollowUp_truncMsg, path = paste0(resultsDir, "toFollowUp.dta"), version = stataVersion)

# CSV
write_excel_csv(x = toFollowUp, path = paste0(resultsDir, "toFollowUp.csv"), col_names = TRUE)

# =============================================================================
# Remove issues interviews with persistent problems for rejection list
# =============================================================================

# filter out recurrent issues from interviews to be rejected
toReject <-
	toReject %>%
	anti_join(toFollowUp, by = c("interview__id", "interview__key"))

# rewrite Stata
toReject_truncMsg <- toReject %>% mutate(rejectMessage = str_trunc(rejectMessage, width = 500))
write_dta(data = toReject_truncMsg, path = paste0(resultsDir, "toReject.dta"), version = stataVersion)

# rewrite CSV
write_excel_csv(x = toReject, path = paste0(resultsDir, "toReject.csv"), col_names = TRUE)

# =============================================================================
# Confirm outputs created
# =============================================================================

if (!exists("toFollowUp")) {
	stop("List of interviews for follow-up not created.")
}

