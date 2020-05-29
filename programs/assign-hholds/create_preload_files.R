create_preload <- function(
    df_hhold,       # household-level data file with columns
    hhold_id,
    hhold_mutate,   # quosure
    hhold_rename,   # named character: c("new_var" = "old_var")
    hh_phone_vars,  # columns passed through vars()
    hh_name_vars,   # columns passed through vars()
    number_mutate,  # quosure that's part of case_when()
    number_type_var, # bare variable name for output
    number_var,
    name_mutate,
    number_name_var,
    df_member,      # member-level data file
    mem_indiv_id,
    mem_mutate,     # quosure 
    mem_name,
    df_assignments,      # assignments data frame with columns hhid, `_responsible`, and `_record_audio`
    assign_rename,  # named character vector: c("new_var" = "old_var")
    out_dir,        # output directory where to save and zip preload files
    out_hhold,      # character name of output file 
    out_member,     # character name of output file 
    out_number      # character name of output file     
) {

# =============================================================================
# Setup
# =============================================================================

# -----------------------------------------------------------------------------
# Load necessary libraries
# -----------------------------------------------------------------------------

# packages needed for this program 
packagesNeeded <- c(
    "haven", 	# to injest Stata and SPSS files
    "purrr",    # to help loop
	"readxl",   # to ingest Excel
	"dplyr",	# to do basic data wrangling
    "rlang",    # to capture, transform, and defer eval of user inputs
	"stringr", 	# to manipulate strings
    "tidyr",    # to reshape data from wide to long, and vice versa
    "readr",    # to write tab-delimited files to disk
    "fs",       # to list tab files in fodler
    "zip"       # to zip tab files together
)

# identify and install those packages that are not already installed
packagesToInstall <- packagesNeeded[!(packagesNeeded %in% installed.packages()[,"Package"])]
if(length(packagesToInstall)) 
	install.packages(packagesToInstall, quiet = TRUE, 
		repos = 'https://cloud.r-project.org/', dep = TRUE)

# load all needed packages
lapply(packagesNeeded, library, character.only = TRUE)

# =============================================================================
# Prepare data
# =============================================================================

# -----------------------------------------------------------------------------
# Rename columns to expected names
# -----------------------------------------------------------------------------

print("Renaming")

print("hholds")
# households
hholds <- df_hhold %>%
    mutate(!!!hhold_mutate) %>%
    rename(!!hhold_rename)

print("members")
# members
members <- df_member %>% mutate(!!!mem_mutate)

print("assigments")
# assignments
assignments <<- df_assignments %>%
    rename(!!assign_rename)

# -----------------------------------------------------------------------------
# Filter data
# -----------------------------------------------------------------------------

print("Filtering")

id_var <- as_name(enquo(hhold_id))

hholds <- inner_join(hholds, assignments, by = id_var) %>%
    mutate(interview__id = row_number())

members <- members %>%
    semi_join(assignments, by = id_var) %>%
    left_join(select(hholds, {{hhold_id}}, interview__id), by = id_var)

# =============================================================================
# Phone number roster
# =============================================================================

# -----------------------------------------------------------------------------
# Roster of numbers
# -----------------------------------------------------------------------------

print("Roster of numbers original")

numbers_source <<- hholds %>%
    # replace missing markers with NA
    # ... for phone numbers
    mutate_at(.vars = hh_phone_vars, 
        .funs = ~ if_else(str_detect(., "(88[ ]*88[ ]*88[ ]*88|99[ ]*99[ ]*99[ ]*99|[Aa][Uu][Cc][Uu][Nn])") | . == "", NA_character_, .)) %>%
    # ... for names
    mutate_at(.vars = hh_name_vars, 
        .funs = ~ if_else(str_detect(., "(^0$|999|[Aa][Uu][Cc][Uu][Nn])") | . == "", NA_character_, .) ) %>%
    select(interview__id, !!!hh_phone_vars, !!!hh_name_vars)

numbers_long <- numbers_source %>% 
    select(interview__id, !!!hh_phone_vars) %>%
    pivot_longer(cols = -interview__id, names_to = "source_var", values_to = "number") %>%
    mutate(
        {{number_type_var}} := case_when(!!!number_mutate),
        {{number_var}} := number
    )

# -----------------------------------------------------------------------------
# Roster of names
# -----------------------------------------------------------------------------

print("Roster of names long")

names_long <- numbers_source %>%
    select(interview__id, !!!hh_name_vars) %>%
    pivot_longer(cols = -interview__id, names_to = "source_var", values_to = "name") %>%
    mutate(
        {{number_type_var}} := case_when(!!!number_mutate),
        {{number_name_var}} := name
    )   

# -----------------------------------------------------------------------------
# Roster of numbers and names
# -----------------------------------------------------------------------------

print("Roster of numbers preload")

number_type <- as_name(enquo(number_type_var))

print(paste0("Number type var: ", number_type))

number_roster_id <- str_replace(out_number, "\\.tab", "") %>% 
    paste0(., "__id") %>% sym()

numbers_preload <- full_join(names_long, numbers_long, by = c("interview__id", number_type)) %>%
    filter(!is.na({{number_var}}) & !is.na({{number_name_var}})) %>%
    # add roster ID
    group_by(interview__id) %>%
    mutate(
        # TODO: create roster name from other inputs
        !!number_roster_id := row_number()
    ) %>%
    ungroup() %>%
    select(interview__id, !!number_roster_id, {{number_var}}, {{number_name_var}}, {{number_type_var}})

# -----------------------------------------------------------------------------
# List of numbers - trigger for number roster
# -----------------------------------------------------------------------------

print("numbers list")

numbers_list <- numbers_preload %>%
    # TODO: create var for roster_id
    mutate(roster_id = !!number_roster_id - 1) %>%
    select(interview__id, roster_id, {{number_var}}) %>%
    pivot_wider(
        id_cols = interview__id, 
        names_from = roster_id, 
        names_prefix = paste0(as_name(enquo(number_var)), "__"),
        values_from = {{number_var}})

# =============================================================================
# Member roster
# =============================================================================

# -----------------------------------------------------------------------------
# Prepare roster for manipulation
# -----------------------------------------------------------------------------

print("Prepare roster for manip")

member_roster_id <- str_replace(out_member, "\\.tab", "") %>% 
    paste0(., "__id") %>% sym()

# source file
member_source <- members %>%
    zap_label() %>%
    zap_labels() %>%
    arrange(interview__id, {{mem_indiv_id}}) %>%
    group_by(interview__id) %>%
    # TODO: programmatically name membres__id
    mutate(!!member_roster_id := row_number()) %>%
    ungroup()

# -----------------------------------------------------------------------------
# Roster of members
# -----------------------------------------------------------------------------

print("Members roster")

member_roster <- member_source %>%
    select(interview__id, !!member_roster_id, names(mem_mutate))

# -----------------------------------------------------------------------------
# List of members - trigger for member roster
# -----------------------------------------------------------------------------

print("Member list")

# string
mem_list_var_chr <- paste0(as_name(enquo(mem_name)), "__")

# symbol
mem_list_var_chr0 <- sym(paste0(mem_list_var_chr, "0"))
mem_list_var_chr1 <- sym(paste0(mem_list_var_chr, "1"))
mem_list_var_chr2 <- sym(paste0(mem_list_var_chr, "2"))
mem_list_var_chr3 <- sym(paste0(mem_list_var_chr, "3"))

member_list <- member_source %>%
    # TODO: programmatically name membres__id
    mutate(roster__id = !!member_roster_id - 1) %>%
    pivot_wider(
        id_cols = interview__id, 
        values_from = {{mem_name}}, 
        names_from = roster__id, 
        names_prefix = mem_list_var_chr) %>%
    mutate(
        num_first_4_mem = rowSums(!is.na(select(., matches(paste0(mem_list_var_chr, "[0-3]$"))))),
        # TODO: figure out how to scale down
        list_first_4_mem = paste(!!mem_list_var_chr0, !!mem_list_var_chr1, !!mem_list_var_chr2, !!mem_list_var_chr3, sep = ", "),
        list_first_4_mem = str_replace_all(list_first_4_mem, ", NA", ""),
        tot_members = rowSums(!is.na(select(., matches(mem_list_var_chr)))),
        preload_mem_list = if_else(
            condition = tot_members > num_first_4_mem, 
            true = paste0(list_first_4_mem, " + ", as.character(tot_members - num_first_4_mem)),
            false = list_first_4_mem
            )
    ) %>%
    select(interview__id, preload_mem_list, starts_with(mem_list_var_chr))

# =============================================================================
# Household
# =============================================================================

print("Household level")

hhold_level <- hholds %>%
    select(interview__id, names(hhold_mutate), names(hhold_rename), `_responsible`, `_record_audio`) %>%
    left_join(numbers_list, by = "interview__id") %>%
    left_join(member_list, by = "interview__id")

# =============================================================================
# Protected variables
# =============================================================================

protected_vars <- data.frame(variable__name = c(
        as_name(enquo(mem_name)), 
        as_name(enquo(number_var))
    ), stringsAsFactors = FALSE)

# =============================================================================
# Write tab files and zip them
# =============================================================================

print("Save and zip preload files")

# household-level
hhold_level %>%
readr::write_tsv(path = paste0(out_dir, out_hhold), na = "")

to_inspect_hhold <<- hhold_level

# member-level
member_roster %>%
readr::write_tsv(path = paste0(out_dir, out_member), na = "")

to_inspect_member <<- member_roster

# number-level
numbers_preload %>%
readr::write_tsv(path = paste0(out_dir, out_number), na = "")

# protected variables
readr::write_tsv(protected_vars, path = paste0(out_dir, "protected__variables.tab"))

# create zip file
# collect all .tab files in output folder
tab_files_to_zip <- fs::dir_ls(path = out_dir, recurse = FALSE, regexp = "\\.tab")
# zip them together
zip::zipr(zipfile = paste0(out_dir, "batch.zip"), files = tab_files_to_zip)

}
