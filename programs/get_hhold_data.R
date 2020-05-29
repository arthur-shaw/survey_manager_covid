# confirm inputs exist in environment
	# progsDir
	# hhold_pattern
	# site
	# user
	# password

# confirm files and folders exist on device
	# progsDir
	# downloadProgDir
	# dl files: "get_qx.R", "dl_one.R", "get_details.R", "dl_similar.R"


# load necessary libraries
library(purrr)

# load functions
downloadProgDir <- paste0(progsDir, "/download/")
downloadProgs <- c(
	"get_qx.R",
	"dl_one.R",
	"get_details.R",
	"dl_similar.R"
)

walk(.x = downloadProgs, .f = ~ source(paste0(downloadProgDir, .x)))

# download data
dl_similar(
	pattern = hhold_pattern, 
	exclude = NULL, 
	export_type = "stata", 
	folder = paste0(dataDir, "hhold/downloaded/"),   
	unzip = TRUE, 
	server = site,
	user = user,  
	password = password,  
	tries = 20
)
