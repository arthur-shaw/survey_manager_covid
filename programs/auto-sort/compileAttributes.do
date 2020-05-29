/*=============================================================================
					DESCRIPTION OF PROGRAM
					------------------------

DESCRIPTION:	Compile "attributes" for each interview. These "attributes"
				are indicators or counts that are used in either reports
				or the reject/review/approve decision--or both.

DEPENDENCIES:	createAttribute.do

INPUTS:			

OUTPUTS:		

SIDE EFFECTS:	

AUTHOR: 		Arthur Shaw, jshaw@worldbank.org
=============================================================================*/

/*=============================================================================
LOAD DATA FRAME AND HELPER FUNCTIONS
=============================================================================*/

/*-----------------------------------------------------------------------------
Initialise attributes data set
-----------------------------------------------------------------------------*/

clear
capture erase "`attributesPath'"
gen interview__id = ""
gen interview__key = ""
gen attribName = ""
gen attribVal = .
gen attribVars = ""
order interview__id interview__key attribName attribVal attribVars
save "`attributesPath'", replace

/*-----------------------------------------------------------------------------
Load helper functions
-----------------------------------------------------------------------------*/

include "`progDir'/helper/createAttribute.do"

/*=============================================================================
IDENTIFY CASES TO REVIEW
=============================================================================*/

use "`constructedDir'/casesToReview.dta", clear
tempfile casesToReview
save "`casesToReview'"

/*=============================================================================
CREATE ATTRIBUTES
=============================================================================*/

all_numbers_tried

any_member_answered

