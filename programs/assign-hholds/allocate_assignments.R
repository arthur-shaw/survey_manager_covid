
# =============================================================================
# Select sample via systematic selection
# =============================================================================

select_sample <- function(
    df, 
    n,
    audio = TRUE,
    audio_interval = 5
) {

    library(dplyr)

    N <- nrow(df)

    # select random sample
    # if sample to select is more than available...
    if (n > N) {
        # ... then take all observations
        df <- mutate(df, selected = 1)

    # if less than available...
    } else if (n < N) {
    
        # perform systematic sampling by
        # ... computing an interval
        # ... making a random start
        # ... selecting one per interval

        # compute interval
        interval <- floor(N/n)

        # draw a random start  
        random_start <- sample(1:interval, 1)

        # compute indices of random selections
        selected_rows <- seq(random_start, random_start + interval*(n-1), interval)

        # mark selections in the data set
        df <- df %>%
            mutate(selected = row_number() %in% selected_rows) %>%
            filter(selected == 1)

    }

    # select observations for audio audit of audit enabled
    if (audio == TRUE) {

        # number to select for audio audit
        audio_eligible <- nrow(df)
        audio_num_select <- ifelse(audio_eligible / audio_interval > 1, floor(audio_eligible / audio_interval), 1)

        # rows selected
        audio_rows <- sample(1:audio_eligible, audio_num_select)

        # mark selections
        df <- df %>% 
            mutate(
                # make selected rows TRUE
                audio = row_number() %in% audio_rows,
                # map from TRUE/FALSE to expected 1/0
                audio = if_else(audio == TRUE, 1, 0, 0)
            )

        
    }

    return(df)

}

# =============================================================================
# Allocate households to interviewers via systematic selection
# =============================================================================

allocate <- function(
    df_sample,
    df_attrib,
    df_assigned,
    interviewer_name,
    num_to_assign,
    audio = TRUE,
    audio_interval = 5    
) {

# find the attributes of a particular interviewer
int_attributes <- filter(df_attrib, interviewer == interviewer_name)

# determine which households are available (i.e., not already assigned)
hholds_available <- df_sample %>%
    select(hhid, hhsize, has_hh_contacts, s00q28) %>%
    anti_join(df_assigned, by = "hhid")

# determine which households are eligible for selection (I.e., available and have same attributes as interviewer)
hholds_eligible <- inner_join(hholds_available, int_attributes, by = "s00q28") %>%
    # implicitly stratify by expected difficulty
    # sort by hhold size, lack of direct hhold number
    arrange(hhsize, has_hh_contacts)

#
hholds_assigned_to_int <- select_sample(df = hholds_eligible, n = num_to_assign) %>%
    mutate(
        `_responsible` = interviewer,
        `_record_audio` = audio) %>%
    select(hhid, `_responsible`, `_record_audio`) 
 
return(hholds_assigned_to_int)

}
