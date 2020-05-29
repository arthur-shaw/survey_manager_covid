
/*=============================================================================
					DESCRIPTION OF PROGRAM
					------------------------

DESCRIPTION:	Appends together each set of same-named files found in 
				subfolders of the specified folder
				- Collects the names of all sub-folders in the target folder
				- Collects the names of files in collection of all sub-folders
				- Appends together all instances of same-named files, looping
					over file names and folders where those files might occur 

DEPENDENCIES:	None

INPUTS:			Parameters to be specified below:
				- projDir
				- inputDir
				- outputDir

OUTPUTS:		None

SIDE EFFECTS:	Saves appended data sets into target 

AUTHOR: 		Arthur Shaw, jshaw@worldbank.org
=============================================================================*/


capture program drop appendAll
program define appendAll
syntax ,					///
	inputDir(string)		///
	outputDir(string)		///

/*=============================================================================
						FETCH NAMES OF FOLDERS AND FILES
=============================================================================*/

	* collect the names of all folders
	local dataFolders : dir "`inputDir'/" dirs "*", respectcase

	* collect the names of .dta files in each folder
	local dataFiles ""
	foreach dataFolder of local dataFolders {

		local filesInFolder	: dir "`inputDir'/`dataFolder'/" files "*.dta", respectcase
		local dataFiles = `"`dataFiles' `filesInFolder'"'

	}

	* create a list of unique files names with as little decoration as possible
	local dataFiles : list uniq dataFiles
	local dataFiles : list clean dataFiles

/*=============================================================================
				APPEND ALL INSTANCES OF EACH SAME-NAMED FILES
=============================================================================*/

	* append together all same-named files
	* going file by file
	* and for each file, looping over all folders
	foreach dataFile of local dataFiles {

		di ""
		di as error "CURRENT FILE: `dataFile'"

		local i = 0

		foreach dataFolder of local dataFolders {

			di as error "Current folder: `dataFolder'"

			* check that the file is in the current folder
			capture confirm file "`inputDir'/`dataFolder'/`dataFile'"

			* if so, handle the file
			if _rc == 0 {

				di "File exists"

				local ++ i

				di "This is the `i'-th file of this name"

				* if first file found of this name, open it
				if `i' == 1 {
					di "opening..."
					use "`inputDir'/`dataFolder'/`dataFile'", clear
				}

				* if not first found, append it
				if `i' > 1 {
					di "appending ..."
					append using "`inputDir'/`dataFolder'/`dataFile'"
				}

			}

			* if file not found, move to the next folder
			else if _rc != 0 {
				di "no file of this name found. moving to next folder"
				continue
			}

		}

		save "`outputDir'/`dataFile'", replace

	}

end
