library(tidyverse)
library(glue)
library(cleaningtools)
library(httr)
library(supporteR)

loc_data <- "inputs/UGA2401_Adjumani_ECHO_data.xlsx"

df_tool_data <- readxl::read_excel(loc_data) %>% 
    select(-`...957`)

# tool
loc_tool <- "inputs/UGA2401_Adjumani_ECHO_tool.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

df_filled_cl <- readxl::read_excel("inputs/main_combined_checks_echo_adjumani.xlsx", sheet = "cleaning_log") %>% 
    filter(!is.na(reviewed), !question %in% c("_index"), !uuid %in% c("all"))

# check pii ---------------------------------------------------------------
pii_from_data <- cleaningtools::check_pii(dataset = df_tool_data, element_name = "checked_dataset", uuid_column = "_uuid")
pii_from_data$potential_PII

# then determine wich columns to remove from both the raw and clean data
cols_to_remove <- c("audit", "audit_URL", "pt_num_msg", "pt_num_validation_message",
                    "pt_sample_lat", "pt_sample_lon", "dist_btn_sample_collected", 
                    "threshold_msg_2_positive", "threshold_msg_2_negative",
                    "telephone", "contact", "name", "gps", 
                    "latitude", "longitude", "contact", "geopoint",
                    "instance_name", "_geopoint_latitude", "_geopoint_longitude",
                    "_geopoint_altitude", "_geopoint_precision")


# updating the dataset with new columns -----------------------------------

df_data_with_added_cols <- cts_add_new_sm_choices_to_data(input_df_tool_data = df_tool_data,
                                                          input_df_filled_cl = df_filled_cl, 
                                                          input_df_survey = df_survey,
                                                          input_df_choices = df_choices)

# create a clean data -----------------------------------------------------

# check the cleaning log
df_cl_review <- cleaningtools::review_cleaning_log(
    raw_dataset = df_data_with_added_cols,
    raw_data_uuid_column = "_uuid",
    cleaning_log = df_filled_cl,
    cleaning_log_change_type_column = "change_type",
    change_response_value = "change_response",
    cleaning_log_question_column = "question",
    cleaning_log_uuid_column = "uuid",
    cleaning_log_new_value_column = "new_value")

# filter log for cleaning
df_final_cleaning_log <- df_filled_cl %>% 
    filter(!question %in% c("duration_audit_sum_all_ms", "duration_audit_sum_all_minutes"), !uuid %in% c("all")) %>% 
filter(!str_detect(string = question, pattern = "\\w+\\/$"))

# create the clean data from the raw data and cleaning log
df_cleaning_step <- cleaningtools::create_clean_data(
    raw_dataset = df_data_with_added_cols %>% select(-any_of(cols_to_remove)),
    raw_data_uuid_column = "_uuid",
    cleaning_log = df_final_cleaning_log,
    cleaning_log_change_type_column = "change_type",
    change_response_value = "change_response",
    NA_response_value = "blank_response",
    no_change_value = "no_action",
    remove_survey_value = "remove_survey",
    cleaning_log_question_column = "question",
    cleaning_log_uuid_column = "uuid",
    cleaning_log_new_value_column = "new_value" )

# handle parent question columns ------------------------------------------

df_updating_sm_parents <- cts_update_sm_parent_cols(input_df_cleaning_step_data = df_cleaning_step,
                                                    input_uuid_col = "_uuid",
                                                    input_point_id_col = "point_number",
                                                    input_collected_date_col = "today",
                                                    input_location_col = "meta_village_name")

# output datasets
list_of_datasets <- list("raw_data" = df_tool_data %>% select(-any_of(cols_to_remove)),
                         "cleaned_data" = df_updating_sm_parents$updated_sm_parents)

openxlsx::write.xlsx(list_of_datasets, 
                     paste0("outputs/", butteR::date_file_prefix(), "_UGA2401_echo_adjumani_cleaned_data.xlsx"))

# extra log for recreated select multiple ---------------------------------

openxlsx::write.xlsx(df_updating_sm_parents$extra_log_sm_parents, 
                     paste0("outputs/", butteR::date_file_prefix(), 
                              "_extra_sm_parent_changes_checks_echo_adjumani.xlsx"))
