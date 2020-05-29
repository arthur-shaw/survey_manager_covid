# =============================================================================
# Setup
# =============================================================================

# -----------------------------------------------------------------------------
# Load necessary libraries
# -----------------------------------------------------------------------------

# packages needed for this program 
packagesNeeded <- c(
    "purrr",    # to facilitate loops
    "haven", 	# to injest Stata and SPSS files
	"readxl",   # to ingest Exoucel
    "tidyr",    # to reshape data set
    "stringr",  # to manipulate strings
	"dplyr",    # to do basic data wrangling
    "rlang",    # to convert user inputs into different object types
    "glue"      # to compose error messages easily
)

# identify and install those packages that are not already installed
packagesToInstall <- packagesNeeded[!(packagesNeeded %in% installed.packages()[,"Package"])]
if(length(packagesToInstall)) 
	install.packages(packagesToInstall, quiet = TRUE, 
		repos = 'https://cloud.r-project.org/', dep = TRUE)

# load all needed packages
lapply(packagesNeeded, library, character.only = TRUE)

# -----------------------------------------------------------------------------
# Load helper functions
# -----------------------------------------------------------------------------

helper_functions <- c(
    "create_preload_files.R",
    "allocate_assignments.R"
)

purrr::walk(
    .x = helper_functions, 
    .f = ~ source(paste0(assignHHDir, .x)))

# =============================================================================
# Prepare files and folders
# =============================================================================

# folders
assign_data_dir <- paste0(assignHHDir, "data/")
assign_temp_dir <- paste0(assignHHDir, "temp/")
assign_out_dir <- paste0(projDir, "outputs/", "assign-hholds/")

print("data_entered")
print("starting...")
# allocation data entered in Shiny
# check that it exists
if (!exists("data_entered")) {
    stop("Data does not exist")
# check that expected columns exist
} else {
    # determine whether any columns are missing, and if so which
    cols_expected = c("interviewer", "num_to_assign")
    cols_found <- !(cols_expected %in% colnames(data_entered))
    cols_missing <- cols_expected[cols_found]
    if (any(cols_found)) {
        stop(paste(
            paste0("The following columns are expected in `data_entered`: ", 
                glue_collapse(single_quote(cols_expected), 
                sep = ", ", last = ", and ")), 
            paste0("The following columns are missing: ", 
                glue_collapse(single_quote(cols_missing), 
                sep = ", ", last = " and ")), 
            sep = "\n")
        )
    }

    data_entered <- rename(data_entered, interviewer_name = interviewer)

}
print("ending...")

# household sample
hholds_sampled <- haven::read_dta(
    file = paste0(assign_data_dir, sample_preload_file)) %>%
    select(!!sym(sample_preload_id))

hhold_size <- haven::read_dta(
    file = paste0(assign_data_dir, member_preload_file)) %>%
    group_by(!!sym(member_preload_id)) %>%
    summarize(hhsize = n()) %>%
    ungroup() %>%
    select(!!sym(member_preload_id), hhsize)

hholds <- haven::read_dta(
    file = paste0(assign_data_dir, hhold_preload_file)) %>%
    mutate(
        # hhsize = rowSums(select(., starts_with("NOM_PRENOMS__"))),
        has_hh_contacts = (!is.na(s00q12) | !is.na(s00q14))
    ) %>%
    left_join(hhold_size, by = hhold_preload_id) %>%
    semi_join(hholds_sampled, by = hhold_preload_id)

members <- haven::read_dta(
    file = paste0(assign_data_dir, member_preload_file))

# households already assigned
# if no assignments made, create an empty file
# else if assignments made, load file containing them
if (file.exists(paste0(assign_temp_dir, "old_assignments.dta"))) {
    old_assignments <- haven::read_dta(file = paste0(assign_temp_dir, "old_assignments.dta"))
} else {
    old_assignments <- tibble(
        !!sym(hhold_preload_id) := NA_real_,
        `_responsible` = NA_character_,
        `_record_audio` = NA_real_,
        .rows = 0
    )

    haven::write_dta(
        data = old_assignments,
        path = paste0(assign_temp_dir, "old_assignments.dta"))
}

# interviewer attributes
interviewer_attributes <- readxl::read_excel(
        path = paste0(assign_data_dir, "agents_langue.xlsx"), 
        n_max = 18
    ) %>%
    pivot_longer(
        cols = starts_with("s00q28"), 
        names_to = "langue", 
        values_to = "s00q28",
        values_drop_na = TRUE) %>%
    mutate(
        s00q28 = str_extract(langue, "(?<=s00q28_)[0-9]+(?= )"),
        s00q28 = as.numeric(s00q28), 
        interviewer = login
        ) %>%
    select(interviewer, s00q28)    

# =============================================================================
# Make random assignments
# =============================================================================

new_assignments <- tibble(
        !!sym(hhold_preload_id) := NA_real_,
        `_responsible` = NA_character_,
        `_record_audio` = NA_real_,
        .rows = 0
    )

for (i in seq(from = 1, to = nrow(data_entered))) {

    interviewer_name <- data_entered$interviewer_name[[i]]
    num_to_assign <- data_entered$num_to_assign[[i]]

    hholds_assigned_to_int <- allocate(
        df_sample = hholds,
        df_attrib = interviewer_attributes,
        df_assigned = old_assignments,
        interviewer_name = interviewer_name,
        num_to_assign = num_to_assign,
        audio = TRUE,
        audio_interval = 5       
    )
    
    new_assignments <- rbind(new_assignments, hholds_assigned_to_int)
    old_assignments <- rbind(old_assignments, hholds_assigned_to_int)

}

write_dta(data = new_assignments, path = paste0(assign_temp_dir, "new_assignments.dta"), version = 14)

# =============================================================================
# Create preload files based on these assignments
# =============================================================================

# define preload output files based on export file names
out_hhold_preload_file <- str_replace(hhold_file, "\\.dta$", "\\.tab")
out_number_preload_file <- str_replace(number_file, "\\.dta$", "\\.tab")
out_member_preload_file <- str_replace(member_file, "\\.dta$", "\\.tab")

# define how phone numbers should be changed to number types
mutates <- quos(
    source_var %in% c("s00q10", "s00q12") ~ 1,
    source_var %in% c("s00q13", "s00q14") ~ 1,
    source_var %in% c("s00q15", "s00q16") ~ 2,
    source_var %in% c("s00q17", "s00q18") ~ 2    
)

# create preload assignments
create_preload(
    df_hhold = hholds, 
    hhold_id = hhid, 
    hhold_mutate = quos(chef_nom = s00q10),
    hhold_rename = c("hhid" = "hhid", "region" = "s00q01", "langue" = "s00q28"),
    hh_phone_vars = vars(s00q12, s00q14, s00q16, s00q18),
    hh_name_vars = vars(s00q10, s00q13, s00q15, s00q17),
    number_mutate = mutates,
    number_type_var = numero_membre,
    number_var = numeros_liste,
    name_mutate = mutates,
    number_name_var = numero_appartient_txt,
    df_member = members, 
    mem_indiv_id = s01q00a,
    mem_mutate = quos(
        preload_pid = s01q00a, 
        s01q01_open = s01q00b, 
        preload_sex = s01q01, 
        preload_age = AgeAnnee, 
        preload_relation = s01q02,
        s01q07 = s01q02,
        s01q06 = AgeAnnee,
        s01q05 = s01q01,
        s01q01 = s01q00b),
    mem_name = s01q01,
    df_assignments = new_assignments, 
    assign_rename = c("hhid" = "hhid", "_responsible" = "_responsible"),
    out_dir = assign_out_dir,
    out_hhold = out_hhold_preload_file,     
    out_member = out_member_preload_file,      
    out_number = out_number_preload_file      
)

# add new assignments to old assignments
write_dta(data = old_assignments, path = paste0(assign_temp_dir, "old_assignments.dta"), version = 14)
