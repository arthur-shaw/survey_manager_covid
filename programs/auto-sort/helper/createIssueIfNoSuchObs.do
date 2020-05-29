capture program drop createIssueIfNoSuchObs
program define createIssueIfNoSuchObs
syntax using/ , ///			/// full path to error file
	anyWhere(string asis) 	/// condition : any after filter
	byGroup(varlist) 		/// group within which county/any evaluated
	issueTypeForInt(integer) 	/// error type at interview level
	issueTypeForItem(integer) 	/// error type at item level
	issueDesc(string)		/// value of description in error file
	issueCommForInt(string)		/// comment for interview summary, limited to 13,400 characters by Stata macros
	issueCommForItem(string) 	/// comment for item, limited to 13,400 characters by Stata macros
	issueLocIDs(string) 	/// space-separated list of variables that contain roster indices
	issueVar(string) 		/// attribute name for fuzzy matching with comments dset

* drop any observations that have observations for the group
egen numIDsMissing = rowmiss(`byGroup') // count the number of missing group identifiers
drop if (numIDsMissing >= 1) 			// drop any obs with 1 or more missing identifiers

* see whether attribute exists in group
gen attribute = (`anyWhere')
collapse (max) attributeSummary = attribute, by(`byGroup')
tempfile attributeSummary
save "`attributeSummary'"

* interview-level error
createSimpleIssue using "`using'", ///
	flagWhere(attributeSummary == 0) ///
	issueType(`issueTypeForInt') ///
	issueDesc("`issueDesc'") ///
	issueComm("`issueCommForInt'") ///
	issueVar(`issueVar')

* nested-level error
keep if 

use "`attributeSummary'", clear 
createSimpleIssue using "`using'", ///
	flagWhere(attributeSummary == 0) ///
	issueType(`issueTypeForItem') ///
	issueDesc("`issueDesc'") ///
	issueComm("`issueCommForItem'") ///
	issueLocIDs(`issueLocIDs') ///
	issueVar(`issueVar')


end
