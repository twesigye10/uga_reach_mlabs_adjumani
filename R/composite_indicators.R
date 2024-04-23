# function for creating composite indicators
create_composite_indicators <- function(input_df) {
    input_df %>% 
        mutate(i.meta_hoh_gender = ifelse(meta_hoh %in% c("yes"), meta_respondent_gender, meta_hoh_gender), 
               i.meta_hoh_age = ifelse(meta_hoh %in% c("yes"), meta_respondent_age, meta_hoh_age), 
               i.meta_respondent_age = case_when(meta_respondent_age <= 20 ~ "age_18_20",
                                                 meta_respondent_age <= 24 ~ "age_21_24",
                                                 meta_respondent_age <= 59 ~ "age_25_59",
                                                 meta_respondent_age >= 60 ~ "age_greater_60"),
               i.shelter_index = hh_members_live_adjumani_city/shelter_rooms_sleep_number
        )
}