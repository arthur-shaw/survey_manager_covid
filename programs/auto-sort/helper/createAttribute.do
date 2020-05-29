capture program drop createAttribute
program define createAttribute
syntax 								/// 
	using/ , 						/// attribute data set
	[extractAttrib(string asis)] 	/// take value of existing variable
	[genAttrib(string asis)] 		/// function
	[countVars(string asis)] 		///	variables to count - either variable names or variable shortcuts
	[anyVars(string asis)] 			/// variables to inspect
	[varVals(numlist)] 				/// values to find
	[countList(string asis)] 		/// variable name of list
	[listMiss(string)] 				/// missing value marker in list
	[countWhere(string asis)] 		/// condition : count after filter
	[anyWhere(string asis)] 		/// condition : any after filter
	[sumVals(varlist)] 				/// variable to sum withing group
	[byGroup(varlist)] 				/// group within which county/any evaluated
	attribName(string asis) 		/// name of attribute
	[attribVars(string asis)] 		/// attribute name for fuzzy matching with comments dset

	preserve

	* check that specifications are an allowed mode of work
	capture assert ///
		("`extractAttrib'" != "") 					| ///
		("`genAttrib'" != "") 						| ///			
		("`countVars'" != "" & "`varVals'" != "") 	| ///
		("`anyVars'" != "" & "`varVals'" != "") 	| ///
		("`countList'" != "") 						| ///
		("`countWhere'" != "" & "`byGroup'" != "") 	| ///
		("`anyWhere'" != "" & "`byGroup'" != "") 	| ///
		("`sumVals'" != "" & "`byGroup'" != "")
	
	if _rc != 0 {
		di as error "ERROR: Failed to specify necessary set of options"
		error 1
	}

	* if more than 1 obs continue
	if _N > 0 {

		* assign value of existing variable to attribute
		if ("`extractAttrib'" != "") {
			gen attribVal = `extractAttrib'
		}

		* define value of attribute based on existing variables 
		if ("`genAttrib'" != "") {
			gen attribVal = (`genAttrib')
		}

		* count number of values in variable list
		if ("`countVars'" != "" & "`varVals'" != "" & "`attribName'" != "") {
			egen attribVal = anycount(`countVars'), values(`varVals')
		}

		* determine whether any value occurs in variable list
		if ("`anyVars'" != "" & "`varVals'" != "" & "`attribName'" != "") {
			egen attribVal = anymatch(`anyVars'), values(`varVals')
		}

		* count number of non-empty list elements
		if ("`countList'" != "" & "`attribName'" != "") {

			* fetch full varlist
			qui: d `countList'*, varlist
			local listVars = r(varlist)

			* replace list elements with missing
			capture confirm string variable `listVars'
			if _rc == 0 {

				// if list missing marker not specified, use SuSo default
				if "`listMiss'" == "" {
					local listMiss = "##N/A##"
				}

				// replace missing marker with string missing
				foreach listVar of local listVars {
					replace `listVar' = "" if (`listVar' == "`listMiss'")
				}

			}

			* count the number of non-missing entries
			egen attribVal = rownonmiss(`listVars'), strok

		}

		* count number of observations within group where condition is met
		if ("`countWhere'" != "" & "`byGroup'" != "" & "`attribName'" != "") {

			gen attribVal = (`countWhere')
			collapse (sum) attribVal, by(`byGroup')

		}

		* determine whether any observation within group meets condition
		if ("`anyWhere'" != "" & "`byGroup'" != "" & "`attribName'" != "") {

			gen attribVal = (`anyWhere')
			collapse (max) attribVal, by(`byGroup')
			
		}
		* sum values within group
		if ("`sumVals'" != "" & "`byGroup'" != "" & "`attribName'" != "") {		

			collapse (sum) attribVal = `sumVals', by(`byGroup')

		}

		* define attributes
		gen attribName = "`attribName'"
		gen attribVars = "`attribVars'"

		* save attribute to attribute file
		keep interview__id interview__key attribName attribVal attribVars
		order interview__id interview__key attribName attribVal attribVars
		append using "`using'"
		sort interview__id
		save "`using'", replace

	}

	* if there is no data
	else if _N == 0 {

		* exist program with warning
		di as error "WARNING: data set missing. Could not generate indicator `attribName'"

	}

	restore

end
