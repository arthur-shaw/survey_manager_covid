capture program drop createComplexIssue
program define createComplexIssue
syntax , 					/// 
	issuesFile(string) 		/// full path to issue file
	attributesFile(string) 	/// full path to attributes file
	whichAttributes(string) ///	space-separated list of attribute names (i.e., value of description)
	issueCondit(string) 	///
	issueType(integer) 		/// value of type in error file
	issueDesc(string) 		/// value of description in error file
	issueComm(string) 		/// value of comment in error file, limited t0 13,400 characters by Stata macros
	[issueVars(string)]		/// attribute name for fuzzy matching with comments dset

	preserve

	* confirm that files exist

	// attributes
	capture confirm file "`attributesFile'"
	di "ATTRIBUTES: `_rc'"
	if _rc != 0 {
		di as error "Attributes file does not exist at indicated location: `attributesFile'"
		error 1
	}

	// issues
	capture confirm file "`issuesFile'"
	di "ISSUES: `_rc'"	
	if _rc != 0 {
		di as error "Issues file does not exist at indicated location: `issuesFile'"
		error 1
	}

	* open attributes file
	use "`attributesFile'", clear

	* check that input variables exist
	local attribFileVars = "interview__id interview__key attribName attribVal attribVars"
	foreach attribFileVar of local attribFileVars {

		capture confirm variable `attribFileVar'
		if _rc != 0 {
			local missingVars = "`missingVars' `attribFileVar'"
		}

	}
	if "`missingVars'" != "" {
		di as error "Expected issue file variables do not exist: `missingVars'"
		error 1
	}

	* keep only those issues related related to the compound issue
	local issueRegex = subinstr("`whichAttributes'", " ", "|", .)
	keep if regexm(attribName, "`issueRegex'")

	* combine attribVars for relevant issues if issueVars() not specified
	if ("`issueVars'" == "") {
		sort interview__id interview__key attribName, stable
		by interview__id interview__key : gen issueVars = attribVars if (_n==1)
		by interview__id interview__key : replace issueVars = issueVars[_n-1] + "|" + attribVars if _n>1 & !mi(attribVars[_n-1])
		by interview__id interview__key : replace issueVars = issueVars[_N]
	}

	* keep the necessary columns
	local fileContents = "interview__id interview__key attribName attribVal"
	if ("`issueVars'" == "") {
		local fileContents = "`fileContents' issueVars"
	}
	keep `fileContents'

	* reshape from long-long to wide-wide
	/* NOTE: this would be a one-liner in R. Sigh... */
	
	// reshape so that there is a column attribVal for each value of attribName
	if ("`issueVars'" == "") {
		reshape wide attribVal, i(interview__id interview__key issueVars) j(attribName) string
	}
	else if ("`issueVars'" != "") {
		reshape wide attribVal, i(interview__id interview__key) j(attribName) string
	}

	// rename columns to match string attribute keys
	qui: d attribVal*, varlist
	local attribValues_origin = r(varlist)
	local attribValues_target = subinstr("`attribValues_origin'", "attribVal", "", .)
	rename (`attribValues_origin') (`attribValues_target')

	* retain cases where the error condition is satisfied
	keep if (`issueCondit')

	* continue if there is more than 1 issue; end program execution otherwise
	qui: d
	if _N >= 1 {

		* construct issue by populating observation with function inputs
		gen issueType 		= `issueType'
		gen issueDesc 		= "`issueDesc'"
		gen issueComment 	= "`issueComm'"			
		if ("`issueVars'") != "" {
			gen issueVars 		= "`issueVars'"
		}

		* add issues to error file
		keep interview__id interview__key issueType issueDesc issueComment issueVars
		append using "`issuesFile'"
		sort interview__id
		save "`issuesFile'", replace	

	}	

	restore

end
