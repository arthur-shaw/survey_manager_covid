
getSupervisors <- function(
	site, 
	user, 
	password,
	limit = 10
) {

# load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

# INITIAL REQUEST TO DETERMINE SIZE

# form request
initRequest <- paste0(
	"https://", site, ".mysurvey.solutions/api/v1/", 
	"supervisors?limit=", 10, "&offset=", 1)

# make request
initResponse <- GET(url = initRequest, config = authenticate(user = user, password = password))

# get total count of supervisors
totSupers <- fromJSON(content(initResponse, as = "text"), flatten = TRUE)$TotalCount

# SUBSEQUENT REQUESTS TO GET SUPERVISORS

requestSupervisors <- function(
	site, 
	user, 
	password,
	limit,
	offset) {

# form request
request <- paste0(
	"https://", site, ".mysurvey.solutions/api/v1/", 
	"supervisors?limit=", limit, "&offset=", offset)

# make request
response <- GET(url = request, config = authenticate(user = user, password = password))

# extract supervisors from response
supervisorResponse <- fromJSON(content(response, as = "text"), flatten = TRUE)$Users

# rename `UserName` column to `supervisor`
supervisorResponse <- supervisorResponse %>% 
	rename( 
		supervisor = UserName,
		supervisorId = UserId)

return(supervisorResponse)

}

supervisors <- purrr::map_dfr(.x = seq(from = 1, to = ceiling(totSupers/limit), by = 1), .f = requestSupervisors, 
	site = site, user = user, password = password, limit = limit)

return(supervisors)

}

# testing
# supervisors <- getSupervisors(site = "arthur", user = "admin", password = "MyLeylaLovesMe2", limit = 10)







getInterviewers <- function(
	site, 
	supervisorId,
	supervisorName,
	user, 
	password,
	limit = 10
) {	

# load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

# INITIAL REQUEST TO DETERMINE SIZE

# form request
initRequest <- paste0(
	"https://", site, ".mysurvey.solutions/api/v1/", 
	"supervisors/", supervisorId, "/interviewers?limit=", limit, "&offset=", 1)

# make request
initResponse <- GET(url = initRequest, config = authenticate(user = user, password = password))

# get total count of interviewers
totInterviewers <- fromJSON(content(initResponse, as = "text"), flatten = TRUE)$TotalCount

# SUBSEQUENT REQUESTS TO GET INTERVIEWERS

requestInterviewers <- function(
	site, 
	supervisorId, 
	supervisorName,
	user, 
	password,
	limit = 10,
	offset) {

# form request
request <- paste0(
	"https://", site, ".mysurvey.solutions/api/v1/", 
	"supervisors/", supervisorId, "/interviewers?limit=", limit, "&offset=", offset)

# make request
response <- GET(url = request, config = authenticate(user = user, password = password))

# extract supervisors from response
interviewerResponse <- fromJSON(content(response, as = "text"), flatten = TRUE)$Users

# manipulate data
interviewerResponse <- interviewerResponse %>%
	rename(
		interviewerId = UserId,
		interviewer = UserName) %>%
	mutate(
		supervisorId = supervisorId,
		supervisor = supervisorName)

return(interviewerResponse)

}

if (totInterviewers > 0) {
interviewers <- purrr::map_dfr(.x = seq(from = 1, to = ceiling(totInterviewers/limit), by = 1L), .f = requestInterviewers, 
	site = site, user = user, password = password, limit = limit, 
	supervisorId = supervisorId, supervisorName = supervisorName)
} else if (totInterviewers == 0 | is.na(totInterviewers)) {
	print(paste0(supervisorName, " has 0 interviewers"))
	interviewers <- data.frame(
		IsLocked = NA,
		CreationDate = NA,
		DeviceId = NA,
		interviewerId = NA,
		interviewer = NA,
		supervisorId = supervisorId,
		supervisor = supervisorName
	)
}

return(interviewers)

}

# testing
# interviewers <- purrr::map2_dfr(.x = supervisors$supervisorId, .y = supervisors$supervisor, .f = getInterviewers, 
# 	site = "arthur", user = "admin", password = "MyLeylaLovesMe2", limit = 10)


