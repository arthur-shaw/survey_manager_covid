
/*=============================================================================
					DESCRIPTION OF PROGRAM
					------------------------

DESCRIPTION:	Create "issues" for each interview. Issues are of three types:
				those that warrant rejection, those that are comments (to be
				posted to rejected interviews), and those that are SuSo
				validation errors. Issues are used in the reject/review/approve
				decision.

DEPENDENCIES:	createSimpleIssue.do, createComplexIssue.do

INPUTS:			

OUTPUTS:		

SIDE EFFECTS:	

AUTHOR: 		Arthur Shaw, jshaw@worldbank.org
=============================================================================*/

/*=============================================================================
LOAD DATA FRAME AND HELPER FUNCTIONS
=============================================================================*/

/*-----------------------------------------------------------------------------
Initialise issues data frame
-----------------------------------------------------------------------------*/

clear
capture erase "`issuesPath'"
gen interview__id = ""
gen interview__key = ""
gen issueType = .
label define types 1 "Critical error" 2 "Comment" 3 "SuSo validation error" 4 "Needs review"
label values issueType types
gen issueDesc = ""
gen issueComment = ""
gen issueLoc = ""
gen issueVars = ""
save "`issuesPath'", replace


/*-----------------------------------------------------------------------------
Load helper functions
-----------------------------------------------------------------------------*/

include "`progDir'/helper/createSimpleIssue.do"

include "`progDir'/helper/createComplexIssue.do"

/*=============================================================================
CRITICAL ISSUES
=============================================================================*/
