# future extensions:
# - fileType. Default to "stata". Allow "tab" and "csv". Use different commands to open depending on type. For types without data types, impose column types (e.g., `role` is numeric).
# - who left the comment. Default to interviewer (role == 1).
# - when left comment. Default to when completed (variable == @@Completed)

check_for_comments <- function(
	file_interviews, 
	file_comment, 
	comment_ids,
	file_issues
) {

# =============================================================================
# Load necessary libraries
# =============================================================================

# packages needed for this program 
packagesNeeded <- c(
	"purrr", 	# to iterate over inputs to verify
	"glue", 	# to construct more readable error messages
	"stringr", 	# to check user inputs and identify @@Complete, @@Reject, events
	"dplyr",	# to do basic data wrangling
	"haven", 	# to injest input Stata file, write output Stata files
	"fuzzyjoin", # to check whether comments posted for variables in issues
	"tidyselect" # to pass id column names into select
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
# Determine whether user inputs are data frames or file paths
# -----------------------------------------------------------------------------

df_exists <- function(df) {

	# load required libraries
	if (!"rlang" %in% installed.packages()) {
		install.packages("rlang", quiet = TRUE, repos = 'https://cloud.r-project.org/', dep = TRUE)
	} 
	library(rlang)

	# quote names
	df_name <- as_name(enquo(df))

	# check whether data frame in global environment
	if (exists(df_name) & is.data.frame(df)) {
		return(TRUE)
	# otherwise, fail
	} else {
		return(FALSE)
	}

}

if (!df_exists(file_interviews) & !df_exists(file_comment) & !df_exists(file_issues)) {

# -----------------------------------------------------------------------------
# Check that files exist
# -----------------------------------------------------------------------------

files_expected <- c(file_interviews, file_comment, file_issues)

files_missing <- map_lgl(.x = files_expected, .f = ~ !file.exists(.x))

if (any(files_missing)) {
    stop(paste(
    	"The following file(s) cannot be found at the specified location(s): ", 
    	paste(files_expected[files_missing], collapse = "\n"), 
    	sep = "\n")
	)
}

# -----------------------------------------------------------------------------
# Check that files are of specified type
# -----------------------------------------------------------------------------

file_extension <- "\\.dta$"

files_wrong_type <- map_lgl(.x = files_expected, .f = ~ !str_detect(.x, file_extension))

if (any(files_wrong_type)) {
    stop(paste(
    	paste0("The following file(s) do not have the expected file extensions : ", ".dta"), 
    	paste(files_expected[files_wrong_type], collapse = "\n"), 
    	sep = "\n")
	)
}

# =============================================================================
# Injest files and check their contents
# =============================================================================

# -----------------------------------------------------------------------------
# Define function to injest files from file or global environment
# -----------------------------------------------------------------------------

injestDf <- function(
	data,
	df_name
) {

	# packages needed for this program 
	packagesNeeded <- c(
		"rlang", 	# for quoting and using user inputs
		"haven"  	# for loading Stata data
	)								

	# identify and install those packages that are not already installed
	packagesToInstall <- packagesNeeded[!(packagesNeeded %in% installed.packages()[,"Package"])]
	if(length(packagesToInstall)) install.packages(packagesToInstall, quiet = TRUE, 
		repos = 'https://cloud.r-project.org/', dep = TRUE)

	# load all needed packages
	lapply(packagesNeeded, library, character.only = TRUE)

	# quote names
	data_name <- as_name(enquo(data))
	df <- as_name(enquo(df_name))

	# if already exists in global environment, assign to `df_name`
	if (exists(data_name, envir = globalenv()) & is.data.frame(data)) {
		df <- get0(data, envir = globalenv())
	# if not, load from file to `df_name`
	} else if (file.exists(data)) {
		assign(df, read_dta(file = data, encoding = "UTF-8"), envir = globalenv())
	# otherwise, fail with error
	} else {
		stop(paste0("No data file exists at the indicated location in `data`"))
	}

}

# -----------------------------------------------------------------------------
# Injest files
# -----------------------------------------------------------------------------

df_paths <- c(file_interviews, file_comment, file_issues)
df_names <- c("casesToReview", "comments", "issues")
walk2(.x = df_paths, .y = df_names, .f = injestDf)

}
# -----------------------------------------------------------------------------
# Define function to check df contents against expectations
# -----------------------------------------------------------------------------

checkDf <- function(
	df,
	cols_expected,
	types_expected
) {


# packages needed for this program 
packagesNeeded <- c(
	"rlang", 	# for quoting and using user inputs
	"glue"  	# for more readable error messages
)								

# identify and install those packages that are not already installed
packagesToInstall <- packagesNeeded[!(packagesNeeded %in% installed.packages()[,"Package"])]
if(length(packagesToInstall)) install.packages(packagesToInstall, quiet = TRUE, 
	repos = 'https://cloud.r-project.org/', dep = TRUE)

# load all needed packages
lapply(packagesNeeded, library, character.only = TRUE)

# check all columns present
cols_found <- !(cols_expected %in% colnames(df))
cols_missing <- cols_expected[cols_found]

if (any(cols_found)) {
    stop(paste(
    	paste0("The following columns are expected in `file_interviews`: ", 
    		glue_collapse(single_quote(cols_expected), 
    		sep = ", ", last = ", and ")), 
    	paste0("The following columns are missing: ", 
    		glue_collapse(single_quote(cols_missing), 
    		sep = ", ", last = " and ")), 
    	sep = "\n")
	)
}

# check that columns are expected type
check_col_type <- function(data, column, type) {

	if (type == "character") {
		is.character(data[[column]])
	} else if (type == "numeric") {
		is.numeric(data[[column]])
	}

}

cols_types_wrong <- map2_lgl(
	.x = cols_expected, 
	.y = types_expected, 
	.f = ~ ! check_col_type(data = df, column = .x, type = .y))
cols_types_wrong_names <- cols_expected[cols_types_wrong]
col_types_current <- map_chr(.x = cols_types_wrong_names, .f = ~ typeof(df[[.x]]))


if (any(cols_types_wrong)) {
    stop(paste("The following columns in `file_interviews` are not the expected type: ",
    	glue_collapse(glue("
    		{backtick(cols_expected[cols_types_wrong])} : \\
    		{types_expected[cols_types_wrong]} (expected); \\
    		{col_types_current} (actual)"), sep = "\n"),
    	sep = "\n")
	)
}


}

# -----------------------------------------------------------------------------
# file_interviews
# -----------------------------------------------------------------------------

checkDf(
	df = casesToReview, 
	cols_expected = c("interview__id", "interview__key", "interview__status"), 
	types_expected = c("character", "character", "numeric"))

# -----------------------------------------------------------------------------
# file_comment
# -----------------------------------------------------------------------------

checkDf(
	df = comments, 
	cols_expected = c("interview__id", "variable", "id1", "role"), 
		# removed pending SuSo bug fix: "id3", 
	types_expected = c("character", "character", "numeric", "numeric"))
		# removed pending SuSo bug fix: "numeric", 

# -----------------------------------------------------------------------------
# file_issues
# -----------------------------------------------------------------------------

checkDf(
	df = issues, 
	cols_expected = c("issueVars"), 
	types_expected = c("character"))

# =============================================================================
# Identify comments relevant for rejection decision
# =============================================================================

# -----------------------------------------------------------------------------
# Comments on issue variables
# -----------------------------------------------------------------------------

# create set of unique issue variables (regex patterns)
uniqueIssueVars <- issues %>% 
	filter(issueVars != "") %>%
	distinct(issueVars)

# filter to interviews with any comments at all
commentsForCasesToReview <- comments %>%
	semi_join(casesToReview, by = "interview__id")

# filter to comments left by the interviewer that are the last in their comment string
lastCommentIsFromInt <- commentsForCasesToReview %>% 
	filter(!str_detect(string = variable, pattern = "^@@")) %>% 	# remove Complete/Reject/Approve comments
	group_by(interview__id, variable, all_of(comment_ids)) %>% 		# group by interview-variable-row
		# removed pending SuSo bug fix: , id3
	filter(row_number() == n()) %>% 								# keep last comment within group
	filter(role == 1) %>% 											# retain only Interviewer comments
	ungroup()

# filter to comments concerning variables used in identifying issues
commentsOnIssueVars <- lastCommentIsFromInt  %>%
	regex_semi_join(uniqueIssueVars, by = c("variable" = "issueVars")) %>%
	select(interview__id)

# -----------------------------------------------------------------------------
# Comments on interview overall
# -----------------------------------------------------------------------------

# filter to overall comments
commentsOverall <- commentsForCasesToReview %>%
	filter(variable == "@@Completed") %>%
	group_by(interview__id) %>%
	filter(row_number() == n()) %>%
	ungroup()  %>%
	select(interview__id)

# =============================================================================
# Identify interviews with and without comments on issue variables
# =============================================================================

# interviews with comments
interview_hasComments <- casesToReview %>%
	semi_join(
		full_join(commentsOnIssueVars, commentsOverall, by = "interview__id") %>%
		distinct( interview__id),
	by = "interview__id")

}

