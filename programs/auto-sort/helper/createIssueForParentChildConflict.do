capture program drop createIssueForParentChildConflict
program define createIssueForParentChildConflict
syntax , 					/// 
	issuesFile(string) 		/// full path to issue file
	parentRoster(string) 	/// parent file, full file path
	parentId(varname) 		/// item ID in parent file
	[parentCondit(string)] 	/// condition for flag value of 1 in parent file
	childRoster(string) 	/// child file, full file path
	childId(varname) 		/// item ID in child file
	[childCondit(string)] 	/// condition for flag value of 1 in child file
	[itemName(varname)] 	/// variable that contains item name, whether as string var or as labelled numeric
	[issueTypeForInt(integer)] 	/// error type at interview level
	[issueTypeForItem(integer)] 	/// error type at item level
	issueDesc(string) 		/// value of description in error file
	[issueCommForInt(string)]		/// comment for interview summary, limited to 13,400 characters by Stata macros
	[issueCommForItem(string)] 	/// comment for item, limited to 13,400 characters by Stata macros
	[issueLocIDs(string)] 	/// space-separated list of variables that contain roster indices
	[issueVar(string)] 		/// attribute name for fuzzy matching with comments dset

	* make item ID variable names match between parent and child
	use "`parentRoster'", clear
	rename `parentId' item_id
	tempfile parent
	save "`parent'"

	use "`childRoster'", clear
	rename `childId' item_id
	tempfile child
	save "`child'"

	* merge items that exist in parent and child files
	use "`parent", clear
	merge 1:m interview__id interview__key item_id using "`child", keep(3)

	* flag attributes

	// parent condition
	if ("`parentCondit'" != "") {
		gen parentFlag = (`parentCondit')
	}
	else if ("`parentCondit'" == "") { 
		gen parentFlag = 1
	}

	// child condition
	if ("`childCondit'" != "") {
		gen childFlag = (`childCondit')
	}
	else if ("`childCondit'" != "") {
		gen childFlag = 1
	}
	clonevar item__id = item_id

	* aggregate attributes to parent-item level
	collapse (max) parentFlag childFlag item__id, by(interview__id interview__key item__id)

	* retain items where there is a contradiction between parent and child flags
	keep if (parentFlag == 1 & childFlag == 0)

	* continue if there is more than 1 issue; end program execution otherwise
	qui: d
	if _N >= 1 {

		* preserve data in tempfile
		tempfile parentChildConflicts
		save "`parentChildConflicts'"

		* create item-level issue
		if ("`issueTypeForItem'" != "" & "`issueCommForItem'" != "" & "`issueLocIDs'" != "") {

			* capture roster indices

				// roster indices
				gen issueLoc = ""

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

			* capture item name
			if ("`itemName'" != "") {

				// if item name comes from string
				capture confirm string variable `itemName' 
				if _rc == 0 {
					gen itemName = `itemName'
				}
				// if item name comes from non-string
				else if _rc != 0 {

					// if have label use it, otherwise cannot populate itemName
					local varLabel : value label `varName'
					if ("`varLabel'" !=  "") {
						decode `itemName', gen(itemName)
					}
					else ("`varLabel'" ==  "") {
						gen itemName = ""
					}
				}

			}
			
			* construct issue by populating observation with function inputs
			gen issueType 		= `issueTypeForItem'
			gen issueDesc 		= "`issueDesc'"
			if ("`itemName'" != "") {
				gen issueComment 	= "`issueCommForItem'" + ": " + itemName
			}
			else if ("`itemName'" != "") {
				gen issueComment 	= "`issueCommForItem'"
			}
			if ("`issueVars'") != "" {
				gen issueVars 		= "`issueVars'"
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
			append using "`issuesFile'"
			sort interview__id
			save "`issuesFile'", replace	

		}

		* create interview-level issue
		if ("`issueTypeForInt'" != "" & "`issueCommForInt'" != "") {

			use "`parentChildConflicts'", clear 

			* construct issue by populating observation with function inputs
			gen issueType 		= `issueTypeForInt'
			gen issueDesc 		= "`issueDesc'"
			gen issueComment 	= "`issueTypeForInt'"			
			if ("`issueVars'") != "" {
				gen issueVars 		= "`issueVars'"
			}

			* add issues to error file
			duplicates drop interview__id interview__key issueType issueDesc issueComment issueVars item__id, force
			keep interview__id interview__key issueType issueDesc issueComment issueVars
			append using "`issuesFile'"
			sort interview__id
			save "`issuesFile'", replace	

		}		


}

end