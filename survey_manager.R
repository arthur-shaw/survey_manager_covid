# =============================================================================
# Load necessary libraries
# =============================================================================

# packages needed for this program 
packagesNeeded <- c(
	"shiny", 		# for interactive user interface
	"shinyjs", 		# for hiding/showing content using JS
	"shinyFiles", 	# for selecting project folder
	"fs",  			# for compiling list of accessible drives
	"waiter", 		# for loading screens
	"RStata", 		# for running Stata scripts from R
	"dplyr", 		# for data wrangling
	"stringr", 		# for string wrangling
	"purrr", 		# for iteration
	"rmarkdown", 	# for creating HTML reports from RMarkdown files
	"readr", 		# for injesting from text and for writing R and Stata programs line by line
	"readxl", 		# for injesting Excel data
	"writexl", 		# for writing data to Excel (e.g., LandPKS)
	"haven", 		# for injesting Stata data files
	"rhandsontable" # for capturing data interactively
)

# identify and install those packages that are not already installed
packagesToInstall <- packagesNeeded[!(packagesNeeded %in% installed.packages()[,"Package"])]
if(length(packagesToInstall)) 
	install.packages(packagesToInstall, quiet = TRUE, 
		repos = 'https://cloud.r-project.org/', dep = TRUE)

# load all needed packages
lapply(packagesNeeded, library, character.only = TRUE)

# =============================================================================
# Helper functions
# =============================================================================

# method for getting latest file modification date in each sub-directory
files_last_mod_date <- function(parent_dir, child_dir) {

	library(fs)
	library(dplyr)

	# create df of metadata on files in targe directory
	fs::dir_info(path = paste0(parent_dir, "/", child_dir, "/"), recurse = FALSE) %>%	
	# add directory in `child_dir` format for ease of reporting to end user
	mutate(dir = child_dir) %>%
	filter(type == "file") %>%
	group_by(dir) %>%
	summarize(files_last_mod = max(modification_time)) %>%
	ungroup()

}

# define writing function
writeToDisk <- function(path, content) {

	library(readr)

	if (!file.exists(path)) {
		toAppend = FALSE
	} else {
		toAppend = TRUE
	}
	readr::write_lines(x = content, path = path, append = toAppend)
}

# =============================================================================
# Define interface
# =============================================================================

dl_wait_screen <- tagList(
	spin_wave(), 
	h4("Dowloading data like a boss...")
)

dl_alt_wait_screen <- tagList(
	spin_wave(), 
	h4("Knock, knock, knockin' on the server's door...")	
)

auto_sort_wait <- tagList(
	spin_pong(),
	h4("Playing data quality ping-pong....")
)

report_wait_screen <- tagList(
	spin_cube_grid(),
	h4("Creating report...")
)

ui <- tagList(

	useShinyjs(),
	use_waiter(), 

	navbarPage(title = "Survey manager",
		tabPanel(
			title = "Setup", 
			value = "setup_tab",
			icon = icon("cog"),
				fluidPage(
  					radioButtons("have_setup", label = h2("Does a setup file already exist?"),
    					choices = list("Yes" = 1, "No" = 2)),
					conditionalPanel(condition = "input.have_setup == 1", 
						tags$p("Please load the setup file by finding it in your files"),
						fileInput("setup_file", label = h2("Please select the setup file"), accept = ".R")
					),
					conditionalPanel(condition = "input.have_setup == 2",
						
						# project folder
						h2("Project folder"),
						shinyDirButton("proj_folder", "Select folders", "Please select a folder", icon = icon("folder")),
						
						# Stata
						h2("Stata details"),
						tags$p("Please select the Stata installation file and Stata version."),
						tags$p("These details are needed to execute Stata .do files from this program."),
						shinyFilesButton("stata_exe", "Select Stata exe", "Please select a file", multiple = FALSE, icon = icon("file-code")),
						numericInput(inputId = "stata_version", label = "Specify Stata version", value = 14, min = 12, max = 16),
						
						# SuSo connection
						h2("SuSo server details"),
						
						h3("Credentials"),
						textInput(inputId = "site", label = "Site"),
						textInput(inputId = "user", label = "User name"),
						textInput(inputId = "password", label = "Password"),

						h3("Questionnaires"),
						tags$p("Specify name or regex pattern to identify questionnaire(s) of type indicated."),
						textInput(inputId = "hhold_pattern", label = "Household"),
						textInput(inputId = "call_back_pattern", label = "Call back"),
						textInput(inputId = "mystery_pattern", label = "Mystery respondent"),

						# Preload file details
						h3("Preload data details"),
						tags$p("Record input file names with extension."),
						textInput(inputId = "sample_preload_file", label = "Sample", placeholder = "Stata file"),
						textInput(inputId = "sample_preload_id", label = "Household ID in the sample file", placeholder = "Variable name"),
						textInput(inputId = "hhold_preload_file", label = "Household", placeholder = "Stata file"),
						textInput(inputId = "hhold_preload_id", label = "Household ID in the household file", placeholder = "Variable name"),
						textInput(inputId = "member_preload_file", label = "Members", placeholder = "Stata format"),
						textInput(inputId = "member_preload_id", label = "Household ID in the member file", placeholder = "Variable name"),
						textInput(inputId = "int_lang_file", label = "Interviewers and languages spoken", placeholder = "Excel file"),

						# Questionnaire details
						h2("Designer questionnaire details"),
						tags$p("Please describe key roster and variables in your Designer questionnaire."), 
						textInput(inputId = "qnr_var", label = "Questionnaire variable"),
						textInput(inputId = "attempt_list_var", label = "Call attempt list variable (as it appears in Designer)", placeholder = "Variable name"),
						textInput(inputId = "interview_result", label = "Interview result", placeholder = "Variable name"),
						textInput(inputId = "attempt_roster", label = "Call attempt roster", value = "attempts", placeholder = "Roster ID"),
						textInput(inputId = "attempt_time", label = "Call attempt time", placeholder = "Variable name"),
						textInput(inputId = "answered_var", label = "Whether answered", placeholder = "Variable name"),
						textInput(inputId = "talk_var", label = "Whether talked with hhold member", placeholder = "Variable name"),
						textInput(inputId = "consent", label = "Whether consented", placeholder = "Variable name"),
						textInput(inputId = "number_roster", label = "Phone numbers roster", value = "numbers", placeholder = "Roster ID"),
						textInput(inputId = "member_roster", label = "Member roster", value = "members", placeholder = "Roster ID"),

						h2("Auto-sort details"),
						h3("Survey Solutions status"),
						selectInput(inputId = "suso_status", label = "Status(es) of interview to process", 
							choices = c("Completed", "ApprovedBySupervisor"), 
							selected = c("Completed", "ApprovedBySupervisor"), multiple = TRUE, selectize = TRUE),
						h3("Data-driven status"),
						textInput(inputId = "data_status", label = "Stata condition describing interviews to process", 
							"inlist(result, 1, 3)"),

						h2("Data collection period"),
						dateRangeInput("survey_dates", label = "Survey period"),						

						# write parameters to file
						h3("Save setup file"),
						actionButton(inputId = "writeParams", label = "Save parameters", icon = icon("save"))	

					),
					h2("Load setup file"),
					actionButton("setup_load", label = "Load file", icon = icon("play-circle"))

				)

		), 
		tabPanel(
			title = "Download",
			value = "get_data",
			icon = icon("file-download"),
			h2("Team composition"),
			actionButton("get_teams_data", label = "Download", icon = icon("users")),
			h2("Household"),
			actionButton("get_hhold_data", label = "Download", icon = icon("home")),
			actionButton("check_hhold_data", label = "Check file dates", icon = icon("check-square")),
			actionButton("use_hhold_data", label = "Use these data", icon = icon("play-circle")),
			verbatimTextOutput("hhold_data_found"),
			h2("Audio"),
			actionButton("get_audio", label = "Download", icon = icon("file-audio")),
			actionButton("check_audio_data", label = "Check file dates", icon = icon("check-square")),
			actionButton("use_audio_data", label = "Use these data", icon = icon("play-circle")),
			verbatimTextOutput("audio_files_found"),
		),
		navbarMenu("Assign", icon = icon("exchange-alt"),
			tabPanel("Household", icon = icon("home"), value = "assign_hholds",
				rHandsontableOutput("allocation"),
				actionButton("make_hhold_assign", label = "Make assignments", icon = icon("play-circle"))
			),
			tabPanel("Call back", icon = icon("phone-square"), value = "assign_call_back",
				tags$p("COMING SOON")		
			),
			tabPanel("Mystery assignment", icon = icon("user-secret"), value = "assign_mystery",
				tags$p("COMING SOON")
			)		
		),
		navbarMenu("Act", icon = icon("tasks"), 
			tabPanel("Approve/reject", icon = icon("laptop-code"), value = "action_auto_sort",
				h2("Run auto-sort"),
				checkboxInput("autosort_acts", label = "Approve/reject when run", value = FALSE),
				actionButton("run_autosort", label = "Run", icon = icon("code")), 
				h2("Create auto-sort report"),
				tags$p("NOTE: This button doesn't work yet"),
				actionButton("make_autosort_report", label = "Create", icon = icon("chart-bar"))
			),
			tabPanel("Monitor", icon = icon("table"), value = "action_fcheck",
				conditionalPanel(condition = "output.got_teams == 1",
					h2("Report parameters"),
					dateRangeInput("fcheck_dates", label = "Report period", start = (Sys.Date() - 7)),
					# TODO: require `interviewer` df to exist
					selectInput("fcheck_group_var", label = "Reporting level", 
						choices = c("team"), 
						multiple = FALSE),
					selectInput("fcheck_teams", label = "Team(s)", 
						choices =  "",
							# c("All teams", "team 1", "team 2", "team 3"),
						selected = "All teams",
						multiple = TRUE,
						selectize = TRUE),
					h2("Create report"),
					actionButton("make_fieldcheck_report", label = "Create", icon = icon("table"))
				),
				conditionalPanel(condition = "output.got_teams == 2",
					tags$p("Need to request team compition data before the report can be run")
				)
			),
			tabPanel("Listen", icon = icon("headphones"), value = "action_listen",
				tags$p("COMING SOON")
			)

		)
	)

)

# =============================================================================
# Define server functions
# =============================================================================

server <- function(input, output, session) {

	# initialize Shiny app by hiding...
	# ...buttons
	shinyjs::hide("use_hhold_data")
	# ...assignment actions
	shinyjs::hide(selector = "li a[data-value=assign_call_back]")
	# ...other actions
	shinyjs::hide(selector = "li a[data-value=get_data]")
	shinyjs::hide(selector = "li a[data-value=assign_hholds]")
	shinyjs::hide(selector = "li a[data-value=action_auto_sort]")
	shinyjs::hide(selector = "li a[data-value=action_fcheck]")
	shinyjs::hide(selector = "li a[data-value=action_listen]")

	# selection of project folder
	volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
	shinyDirChoose(input, "proj_folder", roots = volumes, session = session, restrictions = system.file(package = "base"))

	# selection of Stata installation
	shinyFileChoose(input, "stata_exe", roots = volumes, session = session)

	# save user-provided parameters to disk
	observeEvent(input$writeParams, {

		projDir <<- paste0(parseDirPath(volumes, input$proj_folder), "/")
		stata_loc <<- parseFilePaths(volumes, input$stata_exe)$datapath
		stata_windows <<- gsub(pattern = "/", replacement = "\\\\", stata_loc)
		stata_path <<- shQuote(tools::file_path_sans_ext(stata_windows))

		contentLines <<- c(
			"# Project folder",
			paste0("projDir 	<- ", '"', projDir, '"'),
			paste0("dataDir 	<- ", 'paste0(projDir, "/data/")'),
			paste0("progsDir 	<- ", 'paste0(projDir, "/programs/")'),
			paste0("rejectDir	<- ", 'paste0(progsDir, "/auto-sort/")'),
			paste0("fcheckDir	<- ", 'paste0(progsDir, "/monitoring-tables/")'),
			paste0("assignHHDir	<- ", 'paste0(progsDir, "/assign-hholds/")'),
			paste0("callBckDir	<- ", 'paste0(progsDir, "/assign-call-back/")'),
			paste0("mysteryDir	<- ", 'paste0(progsDir, "/assign-mystery/")'),
			"",
			"# Stata",
			paste0("stata_loc	<- ", '"', stata_loc, '"'),
			paste0('options("RStata.StataPath" = shQuote(tools::file_path_sans_ext("', stata_loc, '"))', ")"),
			paste0('options("RStata.StataVersion" = ', input$stata_version, ")"),
			"",
			"# SuSo server details", 
			paste0("site 		<- ", '"', input$site, '"'),
			paste0("user 		<- ", '"', input$user, '"'),
			paste0("password 	<- ", '"', input$password, '"'),
			"",
			"# SuSo survey details",
			paste0("hhold_pattern 		<- ", '"', input$hhold_pattern, '"'),
			paste0("call_back_pattern	<- ", '"', input$call_back_pattern, '"'),
			paste0("mystery_pattern 	<- ", '"', input$mystery_pattern, '"'),
			"",
			"# Preload file details",
			paste0("sample_preload_file	<- ", '"', input$sample_preload_file, '"'),
			paste0("sample_preload_id	<- ", '"', input$sample_preload_id, '"'),
			paste0("hhold_preload_file <- ", '"', input$hhold_preload_file, '"'),
			paste0("hhold_preload_id <- ", '"', input$hhold_preload_id, '"'),
			paste0("member_preload_file	<- ", '"', input$member_preload_file, '"'),
			paste0("member_preload_id	<- ", '"', input$member_preload_id, '"'),
			paste0("int_lang_file	<- ", '"', input$int_lang_file, '"'),
			"", 			
			"# Data details",
			paste0("hhold_file	<- ", '"', paste0(input$qnr_var, ".dta"), '"'),
			paste0("attempt_list_var	<- ", '"', paste0(input$attempt_list_var, "__"), '"'),
			paste0("interview_result	<- ", '"', input$interview_result, '"'),
			paste0("attempt_file <- ", '"', paste0(input$attempt_roster, ".dta"), '"'),
			paste0("attempt_time	<- ", '"', input$attempt_time, '"'),
			paste0("answered_var	<- ", '"', input$answered_var, '"'),
			paste0("talk_var	<- ", '"', input$talk_var, '"'),
			paste0("consent	<- ", '"', input$consent, '"'),
			paste0("number_file	<- ", '"', paste0(input$number_roster, ".dta"), '"'),
			paste0("member_file	<- ", '"', paste0(input$member_roster, ".dta"), '"'),
			"",
			"# Data collection period",
			paste0("surveyStart	<- ", '"', input$survey_dates[1], '"'),
			paste0("surveyEnd	<- ", '"', input$survey_dates[2], '"'),
			""
		)

		# create file path
		outputPath <<- paste0(projDir, "setup_params.R")

		# if file exists at that path, delete it
		if (file.exists(outputPath)) {
			file.remove(outputPath)
		}

		# write file to disk line by line
		purrr::walk(.x = contentLines, .f = writeToDisk, path = outputPath)

		susoStatuses <- input$suso_status %>%
			str_replace(pattern = "Completed", replacement = "100") %>%
			str_replace(pattern = "ApprovedBySupervisor", replacement = "120") %>%
			paste( collapse = ", ")

		# write Stata parameters to file
		contentLinesStata <<- c(
			"* FOLDERS",
			paste0("local rootDir 			", '"', projDir, '"'),
			paste0("local dataDir			", '"`rootDir', "'", "/data/hhold/", '"'),
			paste0("local downloadDir 		", '"`dataDir', "'", "/downloaded/", '"'),
			paste0("local rawDir 			", '"`dataDir', "'", "/combined/", '"'),
			paste0("local constructedDir 	", '"`dataDir', "'", "/derived/", '"'),
			paste0("local resourceDir 		", 	'"`dataDir', "'", "/resource/", '"'),
			paste0("local progDir 			", 	'"`rootDir', "'", "/programs/auto-sort/", '"'),
			paste0("local resultsDir		", 	'"`rootDir', "'", "/outputs/auto-sort/results/", '"'),
			paste0("local logDir			", 	'"`rootDir', "'", "/outputs/auto-sort/logs/", '"'),
			"",
			"* FILES",
			"// user-input",
			paste0("local hholds 	", '"', paste0(input$qnr_var, ".dta"), '"'),
			paste0("local attempts 	", '"', paste0(input$attempt_roster, ".dta"), '"'),
			paste0("local numbers 	", '"', paste0(input$number_roster, ".dta"), '"'),
			paste0("local members 	", '"', paste0(input$member_roster, ".dta"), '"'),
			"",	
			"// constructed",
			'local attributes 		"attributes.dta"',
			'local issues 			"issues.dta"',
			"",
			"* FILE PATHS",
			paste0("local attributesPath 	", '"', "`constructedDir'/`attributes'", '"'),
			paste0("local issuesPath 		", '"', "`constructedDir'/`issues'", '"'),
			"",
			"* IDENTIFY INTERVIEWS TO PROCESS",
			"// By SuSo status",
			paste0("local statusesToReject ", '"', susoStatuses, '"'),
			"",
			"// By data status",
			paste0("local completedInterview ", '`"', input$data_status, '"', "'"),
			"// maximum number of unanswered answers to allow",
			"local maxUnanswered = 0",
			"",
			"* RUN STATA AUTO-SORT PROGRAMS",
			paste0('include "`progDir', "'/runAll.do")
		)

		# create file output path
		outputPathStata <- paste0(projDir, "/programs/auto-sort/", "configure_and_run.do")

		# if file exists at that path, delete it
		if (file.exists(outputPathStata)) {
			file.remove(outputPathStata)
		}

		# write file to disk line by line
		purrr::walk(.x = contentLinesStata, .f = writeToDisk, path = outputPathStata)

		write_append_prog <- function(
			dir_root,
			dir_data,
			file_output
		) {

			# write Stata append instructions to file
			contentLinesAppend <- c(

				"* folders",
				paste0("local rootDir 			", '"', dir_root, '"'),
				paste0("local dataDir			", '"`rootDir', "'", dir_data, '"'), 		# directory relative to project root
																							# "/data/hhold/"
				paste0("local downloadDir 		", '"`dataDir', "'", "/downloaded/", '"'),
				paste0("local rawDir 			", '"`dataDir', "'", "/combined/", '"'),
				paste0("local progDir 			", 	'"`rootDir', "'", "/programs/auto-sort/", '"'),
				"",
				"* load append program",
				paste0("include ", '"', "`progDir'/appendAll.do", '"'),
				"",
				"* execute appendAll to append together same-named files from different template versions",
				"appendAll, 							///",
				paste0("	", "inputDir(", '"', "`downloadDir'", '") 		///	where to find downloaded data'),
				paste0("	", "outputDir(", '"', "`rawDir'", '") 		///	where to save appended data'),
				""
			)

			# create append file path
			outputPathAppend <- file_output # paste0(paste0(projDir, "/programs/"), "append_instructions.do")

			# if file exists at that path, delete it
			if (file.exists(outputPathAppend)) {
				file.remove(outputPathAppend)
			}

			# write file to disk line by line
			purrr::walk(.x = contentLinesAppend, .f = writeToDisk, path = outputPathAppend)

		}

		write_append_prog(
			dir_root = projDir, 
			dir_data = "/data/hhold/", 
			file_output = paste0(paste0(projDir, "/programs/"), "append_hhold.do"))

		write_append_prog(
			dir_root = projDir, 
			dir_data = "/data/call_back/", 
			file_output = paste0(paste0(projDir, "/programs/"), "append_call_back.do"))

		write_append_prog(
			dir_root = projDir, 
			dir_data = "/data/mystery/", 
			file_output = paste0(paste0(projDir, "/programs/"), "append_mystery.do"))

	})

	# load newly created setup file
	observeEvent(input$setup_file_new, {
		source(input$setup_file$datapath)
	})

	# load setup file
	observeEvent(input$setup_load, {
		if (input$have_setup == 1) {
			source(input$setup_file$datapath)
		} else if (input$have_setup == 2) {
			source(outputPath)
		}

		shinyjs::show(selector = "li a[data-value=get_data]")
	})

	# get household data from SuSo
	observeEvent(input$get_hhold_data, {

		waiter_show(html = dl_wait_screen)

		source(paste0(progsDir, "get_hhold_data.R"))

		stata(src = paste0(progsDir, "append_hhold.do"))
		
		waiter_hide()
		
		# assignment actions
		shinyjs::show(selector = "li a[data-value=assign_call_back]")

		# other actions
		shinyjs::show(selector = "li a[data-value=action_auto_sort]")
		shinyjs::show(selector = "li a[data-value=action_fcheck]")

	})

	# check dates of hhold data downloaded on local machine
	observeEvent(input$check_hhold_data, {

		# create hhold data download directory
		hholdDlDir <- paste0(dataDir, "hhold/downloaded/")

		# compile list of folders (each folder corresponding to a file)
		hholdDataDirs <- list.dirs(hholdDlDir, full.names = FALSE, recursive = FALSE)

		# compile last file modification time for each sub-directory
		hhold_last_mod_by_dir <- map_dfr(
			.x = hholdDataDirs,
			.f = ~ files_last_mod_date(parent_dir = hholdDlDir, .x),
		)

		output$hhold_data_found <- renderPrint({ hhold_last_mod_by_dir })

	})

	# enable use data action if data checked
	observeEvent(input$check_hhold_data, {
		shinyjs::show("use_hhold_data")
	})

	# enable actions tab if decide to use currently downloaded data
	observeEvent(input$use_hhold_data, {

		# assignment actions
		shinyjs::show(selector = "li a[data-value=assign_call_back]")

		# other actions
		shinyjs::show(selector = "li a[data-value=action_auto_sort]")
		shinyjs::show(selector = "li a[data-value=action_fcheck]")

	})

	# get audio data from SuSo
	observeEvent(input$get_audio, {

		waiter_show(html = dl_wait_screen)

		source(paste0(progsDir, "get_audio_data.R"))
	
		waiter_hide()
		
		# enable listening to audio
		shinyjs::show(selector = "li a[data-value=action_listen]")

	})

	# check dates of hhold data downloaded on local machine
	observeEvent(input$check_audio_data, {

		# create hhold data download directory
		audioDlDir <- paste0(dataDir, "audio/downloaded/")

		# compile list of folders (each folder corresponding to a file)
		audioDataDirs <- list.dirs(audioDlDir, full.names = FALSE, recursive = FALSE)

		# compile last file modification time for each sub-directory
		audio_last_mod_by_dir <- map_dfr(
			.x = audioDataDirs,
			.f = ~ files_last_mod_date(parent_dir = hholdDlDir, .x),
		)

		output$audio_files_found <- renderPrint({ audio_last_mod_by_dir })

	})

	# enable use data action if data checked
	observeEvent(input$check_audio_data, {
		shinyjs::show("use_audio_data")
	})

	# enable listen to audio if decide to use currently downloaded data
	observeEvent(input$use_audio_data, {

		shinyjs::show(selector = "li a[data-value=action_listen]")

	})

	# get field team composition data
	got_teams_data <- reactiveVal(NA)	# initialize to NA
	team_choices <- reactiveVal(data.frame(supervisor = "All teams"))
	observeEvent(input$get_teams_data, {
		source(paste0(progsDir, "get_team_composition.R"))
		print("Finished running team code")
		if (exists("interviewers")) {
			got_teams_data(TRUE) 			# update to TRUE if df exists
			team_choices(interviewers %>% select(supervisor) %>% distinct() %>% rbind("All teams"))
			shinyjs::show(selector = "li a[data-value=assign_hholds]")
		} else {
			got_teams_data(FALSE) 			# update to FALSE if df DNE after running code
		}
	})
	observe({
		updateSelectInput(session, "fcheck_teams", choices = team_choices(), selected = "All teams")
		updateSelectInput(session, "progress_teams", choices = team_choices(), selected = "All teams")
	})

	output$got_teams <- reactive({
		if_else(condition = got_teams_data() == TRUE,
			true = 1,
			false = 2,
			missing = 2)
	})
	outputOptions(output, "got_teams", suspendWhenHidden = FALSE)

	output$got_teams_print <- renderPrint({
		if_else(
			condition = got_teams_data(),
			true = "Data successfully received",
			false = "Data request failed",
			missing = "Data not requested yet"
		)
	})

	# get SuSo interviewer sync data
	observeEvent(input$get_suso_sync, {
		waiter_show(html = dl_alt_wait_screen)
		source(paste0(progsDir, "get_sync_data.R")) # TODO: make this a common component outside of cluster report prog dir
		waiter_hide()
	})

	# get community data from SuSo
	observeEvent(input$get_comm_data, {

		waiter_show(html = dl_wait_screen)

		# get data
		source(paste0(progsDir, "get_comm_data.R"))
	
		# append data
		stata(src = paste0(progsDir, "append_community.do"))

		waiter_hide()

	})

	# get education data from SuSo
	observeEvent(input$get_educ_data, {

		waiter_show(html = dl_wait_screen)

		# get data
		source(paste0(progsDir, "get_educ_data.R"))

		# append data
		stata(src = paste0(progsDir, "append_education.do"))
		
		waiter_hide()

	})

	# get education data from SuSo
	observeEvent(input$get_health_data, {

		waiter_show(html = dl_wait_screen)

		source(paste0(progsDir, "get_health_data.R"))

		# append data
		stata(src = paste0(progsDir, "append_health.do"))
		
		waiter_hide()

	})

	# get all data
	
	observeEvent(input$get_all_data, {

		waiter_show(html = dl_wait_screen)

		# household
		source(paste0(progsDir, "get_hhold_data.R"))
		stata(src = paste0(progsDir, "append_hhold.do"))

		# call-back
		# TODO: create this program
		# source(paste0(progsDir, "get_call_back_data.R"))
		# stata(src = paste0(progsDir, "append_call_back.do"))

		# mystery respondent
		# TODO: create this program
		# source(paste0(progsDir, "get_mystery_data.R"))
		# stata(src = paste0(progsDir, "append_mystery.do"))

		# team composition
		source(paste0(progsDir, "get_team_composition.R"))

		# audio
		source(paste0(progsDir, "get_audio_data.R"))

		waiter_hide()

		# enable action tabs

		# assignment actions
		shinyjs::show(selector = "li a[data-value=assign_hholds]")
		shinyjs::show(selector = "li a[data-value=assign_call_back]")
		shinyjs::show(selector = "li a[data-value=assign_mystery]")

		# other actions
		shinyjs::show(selector = "li a[data-value=action_auto_sort]")
		shinyjs::show(selector = "li a[data-value=action_fcheck]")
		shinyjs::show(selector = "li a[data-value=action_listen]")

	})

	# run auto-sort
	observeEvent(input$run_autosort, {
		
		waiter_show(html = auto_sort_wait)

		# run Stata programs for creating attributes and issues
		print("Starting execution of auto-sort")
		print("Starting Stata prog")
		stata(src = paste0(rejectDir, "configure_and_run.do"))

		# pass decision on auto-sort behavior from UI to script sourced below
		to_reject <<- input$autosort_acts

		# run R components of auto-sort once Stata has created outputs
		# that serve as R's inputs
		print("Preparing to run R prog")
		print(paste0("To reject is: ", to_reject))
		while(!file.exists(paste0(dataDir, "/hhold/derived/issues.dta"))) {
			print("Waiting for Stata process to finish")
			 Sys.sleep(1)
		}
		print("Running R program")
		source(paste0(rejectDir, "processInterviews.R"))

		waiter_hide()

	})

    output$allocation <- renderRHandsontable({

		allocation_table <- interviewers %>%
			filter(!str_detect(interviewer, "_interv$")) %>%
			arrange(interviewer) %>%
			select(interviewer) %>%
			# TODO: filter to non-archived interviewers
			mutate(num_to_assign = NA_real_)		
		
		rhandsontable(allocation_table) %>%
            hot_col("interviewer", readOnly = TRUE) %>%
            hot_table(highlightRow = TRUE)

    })

	observeEvent(input$make_hhold_assign, {

        data_entered <<- hot_to_r(input$allocation) %>%
            filter(!is.na(num_to_assign))
    
		source(paste0(progsDir, "assign-hholds/", "make_hhold_assignments.R"))

	})

	# create field check report
	observeEvent(input$make_fieldcheck_report, {
		
		waiter_show(html = report_wait_screen)

		fcheck_params = list(
			# TODO: Come back to this once have written to file
			
			# pass user-provided paramters to Rmd
			hhold_file = hhold_file,
			attempt_list_var = attempt_list_var,
			interview_result = interview_result,
			attempt_file = attempt_file,
			attempt_time = attempt_time,
			answered_var = answered_var,
			talk_var = talk_var,
			consent = consent,
			number_file = number_file,
			member_file = member_file,
			reportStart = input$fcheck_dates[1],
			reportEnd = input$fcheck_dates[2],
			supervisors = input$fcheck_teams,
			groupVar = input$fcheck_group_var,
			site = site,
			user = user,
			password = password
		)

		# create report using parameters above
		rmarkdown::render(
			input = 		paste0(fcheckDir, "report.Rmd"), 
			output_dir = 	paste0(projDir, "outputs/", "monitoring-tables/"), 
			output_file = 	paste0("report.html"), 
			params = fcheck_params,
			encoding = "UTF-8")

		waiter_hide()

	})

}

shinyApp(ui = ui, server = server)
