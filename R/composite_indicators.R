# function for creating composite indicators
create_composite_indicators <- function(input_df) {
    input_df %>% 
        mutate(i.meta_hoh_gender = ifelse(meta_hoh %in% c("yes"), meta_respondent_gender, meta_hoh_gender), 
               i.meta_hoh_age = ifelse(meta_hoh %in% c("yes"), meta_respondent_age, meta_hoh_age), 
               i.meta_hoh_educ_level = ifelse(meta_hoh %in% c("yes"), meta_respodent_educ_level, meta_hoh_educ_level), 
               i.meta_respondent_age = case_when(meta_respondent_age <= 20 ~ "age_18_20",
                                                 meta_respondent_age <= 24 ~ "age_21_24",
                                                 meta_respondent_age <= 59 ~ "age_25_59",
                                                 meta_respondent_age >= 60 ~ "age_greater_60")
        )
}