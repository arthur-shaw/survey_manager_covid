
findMainFileName <- function(
	parentDir, 
	pattern = NULL,
	fileFormat = "stata" 	# possible values: "stata", "tab", "spss"
) {

# =============================================================================
# Load necessary libraries
# =============================================================================

# packages needed for this program 
packagesNeeded <- c(
	"readr",	# to injest text file
	"stringr" 	# to subset strings and find indices for content of iterest
)

# identify and install those packages that are not already installed
packagesToInstall <- packagesNeeded[!(packagesNeeded %in% installed.packages()[,"Package"])]
if(length(packagesToInstall)) 
	install.packages(packagesToInstall, quiet = TRUE, 
		repos = 'https://cloud.r-project.org/', dep = TRUE)

# load all needed packages
lapply(packagesNeeded, library, character.only = TRUE)

# =============================================================================
# Compile directories
# =============================================================================

# compile a list of directories in `parentDir`
dataDirs <- list.dirs(path = parentDir, full.names = TRUE, recursive = FALSE)
if (length(dataDirs) == 0) {
	stop("No directories found in `parentDir`")
}

# subset directories that match the indicated pattern
if (!is.null(pattern)) {
	dataDirs <- str_subset(string = dataDirs, pattern = pattern)
	if (length(dataDirs) == 0) {
		stop("No directories found in `parentDir` that match `pattern`")
	}		
}

# =============================================================================
# Look for file manifest in directories until operation fails or succeeds
# =============================================================================

# loop through each directory to find `export__readme.txt`
numDirs <- length(dataDirs)
dirCounter <- 1

while (dirCounter <= numDirs) {
	
	manifestExists <- file.exists(paste0(dataDirs[dirCounter], "/", "export__readme.txt"))

	# if the file is found record the directory and exit the loop
	if (manifestExists == TRUE) {
		dataDir <- dataDirs[dirCounter]
		break
	# otherwise, increment the counter and continue
	} else {
		dirCounter <- dirCounter + 1
	}
}

# if folder with `export__readme.txt` not found, fail
if (!exists("dataDir")) {

	stop("File manifest, `export__readme.txt`, not found in any directory of `parentDir`")

}

# =============================================================================
# Extract main file name from the export file manifest
# =============================================================================

# injest content of manifest
manifestText <- readr::read_lines(file = paste0(dataDir, "/", "export__readme.txt"))

# get indices of lines with name of main file
if (fileFormat == "stata") {
	extension = "\\.dta"
} else if (fileFormat == "spss") {
	extension = "\\.sav"
} else if (fileFormat == "tab") {
	extension = "\\.tab"
}
indices <- str_which(manifestText, pattern = extension)

# return index of first
firstDta <- indices[1]

# return name of first .dta file--which is the main file
mainFileName <- manifestText[firstDta]

return(mainFileName)

}



# testing
# mainFileName <- findMainFileName(parentDir = "C:/Users/wb393438/Cambodia LSMS+/auto-sort/data/downloaded/")
