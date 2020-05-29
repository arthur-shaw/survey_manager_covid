
post_comments_to_vars <- function(
	file_comments_to_post, 
	server,
	user,
	password,
	output_dir
) {

# =============================================================================
# Load necessary libraries
# =============================================================================

# packages needed for this program 
packagesNeeded <- c(
	"httr", 	# for API communication
	"dplyr",	# for convenient data wrangling
	"purrr" 	# to iterate over interviews to comment
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

if (!df_exists(file_comments_to_post)) {

# -----------------------------------------------------------------------------
# Check that files exist
# -----------------------------------------------------------------------------

files_expected <- c(file_comments_to_post)

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

# -----------------------------------------------------------------------------
# SuSo server exists
# -----------------------------------------------------------------------------

serverAddress <- paste0("https://", server, ".mysurvey.solutions/")

if (!RCurl::url.exists(serverAddress)) {
	stop(paste0("Server does not exist at address provided: ", "\n", serverAddress))
}

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
# `output_dir` exists
# -----------------------------------------------------------------------------

if (!dir.exists(output_dir)) {
	stop(glue(
		"Directory in `output_dir` does not exist in specified location.
		{output_dir}"

	))
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

df_paths <- c(file_comments_to_post)
df_names <- c("commentsToPost")
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
# file_comments_to_post
# -----------------------------------------------------------------------------

checkDf(
	df = commentsToPost, 
	cols_expected = c("interview__id", "interview__key", "issueVars", "issueLoc", "issueComment"), 
	types_expected = c("character", "character", "character", "character", "character"))

# =============================================================================
# Check whether there are comments to post
# =============================================================================

if (nrow(commentsToPost) == 0) {
	return(cat("No interviews to comment"))
}

# =============================================================================
# Post comments
# =============================================================================

# -----------------------------------------------------------------------------
# Define function
# -----------------------------------------------------------------------------

post_var_comment <- function(
	interview__id,
	interview__key,
	issueVars,
	issueLoc,
	issueComment,	
	server,
	user,
	password
) {

	# packages needed for this program 
	packagesNeeded <- c(
		"httr", 	# to talk with the server
		"glue", 	# to create easy-to-read messages more easily
		"readr" 	# to write failure info to disk
	)

	# identify and install those packages that are not already installed
	packagesToInstall <- packagesNeeded[!(packagesNeeded %in% installed.packages()[,"Package"])]
	if(length(packagesToInstall)) 
		install.packages(packagesToInstall, quiet = TRUE, 
			repos = 'https://cloud.r-project.org/', dep = TRUE)

	# load all needed packages
	lapply(packagesNeeded, library, character.only = TRUE)

	# formulate API call	
	call <- paste0("https://", server, ".mysurvey.solutions", 
		"/api/v1/interviews/", interview__id, 			# interview
		"/comment-by-variable/", issueVars, 			# variable
		"?comment=", curlPercentEncode(issueComment) 	# comment
		)

	# make request
	response <- httr::POST(
		url = call, 
		authenticate(user = login, password = password),
		accept_json(),
		content_type_json(),
		body = paste0("[", issueLoc, "]")
		)

	# react to response

	status_code <- httr::status_code(response) 

	# if request failed, capture details on failure
	if (status_code != 200) {

		print(glue("
			Problem posting comment to interview {interview__id} for \\
			variable {issueVars} at location {issueLoc}
			"))

		# extract interview info and reason for failure
		failed_post_comment <- data.frame(
			interview__id = interview__id,
			status_code = status_code,
			reason = content(response)$Message,
			stringsAsFactors = FALSE
		)

		# write failure info to disk
		readr::write_csv(
			failed_post_comment, 
			path = paste0(output_dir, "failed_post_comment.csv"),
			append = TRUE
		)

	# if request succeeded, indicate success
	} else {

		print(glue(
			"Success posting comment to interview {interview__id} for \\ 
			variable {issueVars} at location {issueLoc}
			"))

	}

}

# -----------------------------------------------------------------------------
# Apply function to all obs in `file_comments_to_post`
# -----------------------------------------------------------------------------

interviews_with_comments <- commentsToPost %>% 
	select(interview__id, interview__key, issueVars, issueLoc, issueComment)

purrr::pwalk(
	.l = interviews_with_comments,
	.f = post_var_comment,
	server = server, user = user, password = password)

}




