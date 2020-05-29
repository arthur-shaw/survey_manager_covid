
/*=============================================================================
					DESCRIPTION OF PROGRAM
					------------------------

DESCRIPTION:	Combines multiple food consumption rosters into one
				- Renames variables
					- Changes variables names that follow a pattern to a 
						harmonized set of names
					- Renames a single variable that does not follow this pattern
					- Rename food ID variable to user-provided name
				- Appends all harmonized food consumption rosters together
				- Creates master variable labels 
					- Collects variable labels from each data set
					- Creates master variable label set
					- Drops exact duplicates
					- Shows duplicates with the same value but not the same
						string
				- Applies master variable labels to final data set

DEPENDENCIES:	

INPUTS:			

OUTPUTS:		

SIDE EFFECTS:	Saves appended data sets into target 

AUTHOR: 		Arthur Shaw, jshaw@worldbank.org
=============================================================================*/

capture program drop combineConsumption
program define combineConsumption
syntax ,					///
	rosterDtaList(string) 	/// list of roster data sets, without .dta extension
	varList(string) 		/// list of variables in each roster
	[diffPattern(string)]	/// variable that cannot be identified with substring. Provide regex pattern to identify and desired variable name, separated by a space
	inputDir(string)		/// where roster data files can be found
	[labelDir(string)] 		/// where labels can exported
	outputDir(string)		/// where combined file should be saved
	outputDta(string) 		/// how the combined file should be named, without dta extension
	outputID(string) 		/// name row ID variable in output file

	/*=============================================================================
							CHECK PROGRAM SET-UP
	=============================================================================*/

	* all folders exist
	foreach desiredDir in inputDir outputDir {

		capture cd "``desiredDir''"
		if _rc != 0 {
			di as error "ERROR: The folder path `desiredDir', specified in the command, does not exist. Please correct or create this directory path"
			error 1
		}
		
	}

	* all input files exist
	foreach roster of local rosterDtaList {
		capture confirm file "`inputDir'/`roster'.dta"
		if _rc !=0 {
			local missingRosters "`missingRosters' `roster'"
		}
	}
	if "`missingRosters'" != "" {
		di as error "The following rosters cannot be found in the inputDir : "
		di as error "`missingRosters'"
		error 1
	}

	/*=============================================================================
							COMBINE CONSUMPTION DATA SETS
	=============================================================================*/

	* initialize roster counter
	local rosterCounter = 1

	* create a data set for each roster with harmonized variable names
	foreach roster of local rosterDtaList {

		*di as error "CURRENT ROSTER: `roster'" 										// TODO: delete after testing

		use "`inputDir'/`roster'.dta", clear

		* rename each variable in varList
		foreach varName of local varList {

			*di as error "CURRENT VAR: `varName'"										// TODO: delete after testing

			capture confirm variable `varName', exact
			if _rc == 0 {
				di as error "Variable already exists: `varName'"

				* save current file
				tempfile `roster'_new
				qui: save "``roster'_new'", replace
				*save "`outputDir'/`roster'_new.dta", replace

				* save label externally
				tempfile `varName'_`rosterCounter'
				label save `varLabel' using "``varName'_`rosterCounter''", replace
				import delimited using "``varName'_`rosterCounter''", ///
					delimiters("\t") varnames(nonames) stripquote(no) encoding("utf-8") clear			
				replace v1 = subinstr(v1, "`variableFound'", "`varName'", .)
				di "saving `varName'_`rosterCounter'"
				qui: save "``varName'_`rosterCounter''", replace
				* save "C:\Users\wb393438\IHS5\auto-sort\data\derived\temp/check_labels1/`varName'_`rosterCounter'.dta", replace 							// TODO: delete after testing

				use "``roster'_new'", clear

				continue
			}

			qui : capture d `varName'*, varlist
			if _rc != 0 {
				di as error "No variable "
				continue
			}

			local variableFound = r(varlist)
			di "`variableFound'"
			local numVarsFound : list sizeof variableFound 
			di "`numVarsFound'"

			* if no matching variable found, show error and move to the next variable
			if `numVarsFound' == 0 {
				di as error "Could not find `varName'_* in `roster'"
				continue
			}

			* if found more than 1 matching variable found, return error
			if `numVarsFound' > 1 {
				local containsOthVar = regexm("`variableFound'", "oth|os") 
				if `containsOthVar' == 0 {
					di as error "More than 1 variable found matching `varName' in `roster'."
					di as error "Please change varList() so that its arguments each identify only one variable"
					error 1
				}
				else if `containsOthVar' == 1 {
					tokenize "`variableFound'"
					if regexm("`1'", "oth|os") {
						local variableFound = "`2'"
						di "OTH IN 1. Remainder is: `variableFound'"
					}
					if regexm("`2'", "oth|os") {
						local variableFound = "`1'"
						di "OTH IN 2. Remainder is: `variableFound'"						
					}
				}

			}

			rename `variableFound' `varName'

			* check whether has label
			local varLabel : value label `varName'

			if ("`varLabel'" !=  "") {

				* save current file
				tempfile `roster'_new
				qui: save "``roster'_new'", replace
				*save "`outputDir'/`roster'_new.dta", replace

				* save label externally
				tempfile `varName'_`rosterCounter'
				label save `varLabel' using "``varName'_`rosterCounter''", replace
				import delimited using "``varName'_`rosterCounter''", ///
					delimiters("\t") varnames(nonames) stripquote(no) encoding("utf-8") clear			
				replace v1 = subinstr(v1, "`variableFound'", "`varName'", .)
				di "saving `varName'_`rosterCounter'"
				qui: save "``varName'_`rosterCounter''", replace
				* save "C:\Users\wb393438\IHS5\auto-sort\data\derived\temp/check_labels1/`varName'_`rosterCounter'.dta", replace 							// TODO: delete after testing

				use "``roster'_new'", clear

			} 

		}

		* rename a single variable that have a different pattern than above
		if ("`diffPattern'" != "") {
			qui : d, varlist
			local allVars = r(varlist)

			tokenize "`diffPattern'"
			local diffRegex1 	= "`1'"
			local diffNewVar1 	= "`2'"
			local diffRegex2 	= "`3'"
			local diffNewVar2 	= "`4'"

			if ("`diffRegex1'" != "" & "`diffRegex2'" == "") {
				local numRegexes = 1
			}
			else if ("`diffRegex1'" != "" & "`diffRegex2'" != "") { 
				local numRegexes = 2
			}

			forvalues i = 1/`numRegexes' {

				local diffVarFound = regexm("`allVars'", "`diffRegex`i''")
				if `diffVarFound' == 0 {
					di as error "No variable found in `roster' found matching this pattern from diffPattern(): `diffRegex`i''"
					error 1
				}
				local diffVariable`i' = regexs(0)

				rename `diffVariable`i'' `diffNewVar`i''

				local varLabel : value label `diffNewVar`i''
				if ("`varLabel'" !=  "") {

					* save current file
					tempfile `roster'_new
					qui: save "``roster'_new'", replace
					*save "`outputDir'/`roster'_new.dta", replace

					* save label externally
					tempfile `diffNewVar`i''_`rosterCounter'
					label save `varLabel' using "``diffNewVar`i''_`rosterCounter''", replace
					import delimited using "``diffNewVar`i''_`rosterCounter''", ///
						delimiters("\t") varnames(nonames) stripquote(no) encoding("utf-8") clear			
					replace v1 = subinstr(v1, "`diffVariable`i''", "`diffNewVar`i''", .)
					di "saving `diffNewVar`i''_`rosterCounter'"
					qui: save "``diffNewVar`i''_`rosterCounter''", replace
					* save "C:\Users\wb393438\IHS5\auto-sort\data\derived\temp/check_labels2/`diffNewVar'_`rosterCounter'.dta", replace 							// TODO: delete after testing

					use "``roster'_new'", clear

				} 

			}



		}

		* rename ID variable to match user input in outputID()
		local inputID = "`roster'" + "__id"
		rename `inputID' `outputID'

		local varLabel : value label `outputID'
		if ("`varLabel'" !=  "") {

			* save current file
			tempfile `roster'_new
			qui: save "``roster'_new'"
			* save "C:\Users\wb393438\IHS5\auto-sort\data\derived\temp/rosters/`roster'_new.dta", replace

			* save label externally
			tempfile `outputID'_`rosterCounter'
			label save `varLabel' using "``outputID'_`rosterCounter''", replace
			import delimited using "``outputID'_`rosterCounter''", ///
				delimiters("\t") varnames(nonames) stripquote(no) encoding("utf-8") clear			
			replace v1 = subinstr(v1, "`inputID'", "`outputID'", .)
			di "saving `outputID'_`rosterCounter'"
			save "``outputID'_`rosterCounter''", replace
			* save "C:\Users\wb393438\IHS5\auto-sort\data\derived\temp/check_labels3/`outputID'_`rosterCounter'.dta", replace 							// TODO: delete after testing

			use "``roster'_new'", clear

		} 


		* save file
		tempfile `roster'_new
		qui: save "``roster'_new'", replace
		*save "`outputDir'/`roster'_new.dta", replace
/*
		* create list of variables with variable labels
		qui : ds, has(vallabel)
		local varsWithVLabels = r(varlist)

		* save the variable labels of each variable
		foreach varWithLabel of local varsWithVLabels {
			
			use "``roster'_new'", clear

			* determine the value label name
			local valLabName : value label `varWithLabel'
			
			* save that value label as a .do file
			tempfile `varWithLabel'_`rosterCounter'
			label save `valLabName' using "``varWithLabel'_`rosterCounter''", replace
			label save `valLabName' using "C:\Users\wb393438\IHS5\auto-sort\temp/`varWithLabel'_`rosterCounter'.do", replace	// TODO: delete after testing
			
			* save contents as a data file
			import delimited using "``varWithLabel'_`rosterCounter''", ///
				delimiters("\t") varnames(nonames) stripquote(no) encoding("utf-8") clear
			if ("`varWithLabel'" == "`outputID'") {
				replace v1 = subinstr(v1, "`produit'__id", "produitID", .)
			}
			if ("`varWithLabel'" != "`outputID'") {}
				replace v1 = subinstr(v1, )
			}
			save "``varWithLabel'_`rosterCounter''", replace
			save "C:\Users\wb393438\IHS5\auto-sort\temp/`varWithLabel'_`rosterCounter'.dta", replace 							// TODO: delete after testing

		}
*/
		local ++rosterCounter		

	}

	* append data files together
	local firstRoster : word 1 of `rosterDtaList'
	local rostersLeft : list rosterDtaList - firstRoster

	local fileCounter = 1

	foreach roster of local rosterDtaList {

		if `fileCounter' == 1 {

			use "``roster'_new'", clear 

	/* TODO: add coercion to type if not expected type */

		}
		else if `fileCounter' > 1 {

			append using "``roster'_new'"

		}

		local ++fileCounter

	}

	qui: save "`outputDir'/`outputDta'", replace

	* confirm that combined consumption file save
	capture confirm file "`outputDir'/`outputDta'"
	if _rc != 0{

		di as error "Problem saving `outputDta' in `outputDir'."
		error 1

	}

	* combine value labels
	keep `outputID' `varList'
	qui : ds, has(vallabel)
	local varsWithVLabels = r(varlist)

	* combine the variable labels for each labelled variable in each file
	foreach varWithLabel of local varsWithVLabels {	

		* combine value label data sets
		local rosterCounter = 1
		local rosterTotal : list sizeof rosterDtaList
		forvalues i = 1/`rosterTotal' {

			di "VARWITHLABEL: `varWithLabel'"
			di "COUNTER: `rosterCounter'"
			di "FILEPATH: ``varWithLabel'_`rosterCounter''"


			if (`i' == 1) {

				use "``varWithLabel'_`rosterCounter''", clear

			}
			else if (`i' > 1) {

				capture confirm file "``varWithLabel'_`rosterCounter''"
				if _rc == 0 {
					append using "``varWithLabel'_`rosterCounter''"
				}
				
			}

			local ++rosterCounter


		}

		* drop duplicate entries
		duplicates drop v1, force

		* warn and then drop entries with duplicate values
		gen labelVal = regexs(1) if regexm(v1, "label define `varWithLabel' ([0-9]+)")
		capture assert (labelVal != "")
		if _rc == 0 {
			di as error "For `varWithLabel', there are value labels with the same value but different labels."
			di as error "They are displayed below for inspection, but have been deleted from the value label"
			duplicates tag labelVal, gen(dupVar)
			capture assert (dupVar == 0)
			if _rc != 0 {
				list v1 if (dupVar > 0), noobs clean
				duplicates drop labelVal, force
			}
			drop dupVar
		}
		else if _rc != 0 {
			list in 1/20
		}
		drop labelVal	

		* convert from data set to .do file
		tempfile `varWithLabel'
		qui: outsheet using "``varWithLabel''", nonames noquote replace
		* outsheet using "C:\Users\wb393438\IHS5\auto-sort\data\derived\temp\dofiles/`varWithLabel'.do", nonames noquote replace		// TODO: delete after testing

		* use .do file to define value labels
		use "`outputDir'/`outputDta'", clear
		include "``varWithLabel''"
		label list `varWithLabel'
		label values `varWithLabel' `varWithLabel'
		qui: save "`outputDir'/`outputDta'", replace

	}


end
