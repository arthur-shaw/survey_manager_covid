capture program drop createSimpleIssue
program define createSimpleIssue
syntax using/ , ///			/// full path to error file
	flagWhere(string) 		/// condition
	[anyInGroup(varlist)] 	/// variables that define the group
	issueType(integer)		/// value of type in error file
	issueDesc(string)		/// value of description in error file
	issueComm(string)		/// value of comment in error file, limited t0 13,400 characters by Stata macros
	[issueLocIDs(string)] 	/// space-separated list of variables that contain roster indices
	[issueVar(string)] 		/// attribute name for fuzzy matching with comments dset

	preserve

		* filter to observations that satisfy condition
		keep if (`flagWhere')

		* retain only one observation for rosters
		if "`anyInGroup'" != "" {
			bysort `anyInGroup' : keep if _n == 1
		}

		* count how many observations are left after filtering
		qui: d
		local numObs = r(N)
		di "NUMOBS: `numObs'"

		* continue program if N >= 1 obs; stop end program otherwise
		if `numObs' >= 1 {

			* if any variables already exist with same name as error file vars, drop them
			local errorFileVars = "issueType issueDesc issueComment"
			foreach errorFileVar of local errorFileVars {
				capture confirm variable `errorFileVar'
				if _rc == 0 {
					drop `errorFileVar'
				}	
			}
			
			* create variables for error file
			gen issueType = `issueType'
			gen issueDesc = "`issueDesc'"
			gen issueComment = "`issueComm'"
			
			// roster indices
			gen issueLoc = ""

			// if variable not in a roster
			if ("`issueLocIDs'" == "") {

				replace issueLoc = "null" 	/// TODO: Check that this is needed outside of Swagger
			
			}
			// if variable occurs in a roster
			else if ("`issueLocIDs'" != "") {

				// for first-level roster, a single index; example: [1]
				local numLocIndices : list sizeof issueLocIDs
				if (`numLocIndices' == 1) {

					replace issueLoc = "[" + string(`issueLocIDs') + "]"	

				}

				// for second-level roster, an array of two indices; example [2,1]
				else if (`numLocIndices' == 2) {

					tokenize "`issueLocIDs'"
					replace issueLoc = "[" + string(`1') + "," + string(`2') + "]"
	
				}

				// for third-level roster, an array of three indices; example [2,1,4]
				else if (`numLocIndices' == 3) {

					tokenize "`issueLocIDs'"
					replace issueLoc = 	"[" + 	string(`1') + "," + ///
												string(`2') + "," + ///
												string(`3') + "]"
	
				}

				/* TODO: Add handling of more roster levels if needed */
			
			}

			* record issue vars
			if ("`issueVar'" != "") {
				gen issueVars = "`issueVar'"
			}

			* determine variables to keep based on variables created
			local keepVars = "interview__id interview__key issueType issueDesc issueComment"
			if ("`issueLocIDs'" !=  "") {
				local keepVars = "`keepVars' issueLoc"
			}
			if ("`issueVar'" != "") {
				local keepVars = "`keepVars' issueVar"
			}

			* add issues to error file
			keep  `keepVars'
			append using "`using'"
			sort interview__id
			save "`using'", replace	

		}

	restore

end
