
# confirm inputs exist in environment
	# progsDir
	# site
	# user
	# password

# confirm files and folders exist on device
	# progsDir
	# getFieldStaffList.R file

# load necessary libraries
library(purrr)
library(dplyr)

# load functions
source(paste0(progsDir, "getFieldStaffList.R"))

# get supervisors
print("Fetching list of supervisors")
supervisors <- getSupervisors(site = site, user = user, password = password)

# get interviewers
print("Fetching interviewer info for each supervisor")
interviewers <<- purrr::map2_dfr(
	.x = supervisors$supervisorId, 
	.y = supervisors$supervisor, 
	.f = getInterviewers, 
	site = site, user = user, password = password) %>%
	filter(!is.na(interviewer)) %>%		# drop supervisors from pilot/field practice
	filter(supervisor != "Akuffo") 		# drop Akuffo from field practice
