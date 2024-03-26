library(tidyverse)
library(glue)
library(cleaningtools)
library(httr)

loc_data <- "inputs/UGA2401_Adjumani_ECHO_data.xlsx"

df_tool_data <- readxl::read_excel(loc_data) %>% 
    select(-`...957`)

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



# create a clean data -----------------------------------------------------

df_filled_cl <- readxl::read_excel("inputs/main_combined_checks_echo_adjumani.xlsx", sheet = "cleaning_log") %>% 
    filter(!is.na(reviewed), !question %in% c("_index"), !uuid %in% c("all"))

# check the cleaning log
df_cl_review <- cleaningtools::review_cleaning_log(
    raw_dataset = df_tool_data,
    raw_data_uuid_column = "_uuid",
    cleaning_log = df_filled_cl,
    cleaning_log_change_type_column = "change_type",
    change_response_value = "change_response",
    cleaning_log_question_column = "question",
    cleaning_log_uuid_column = "uuid",
    cleaning_log_new_value_column = "new_value"
)

# create the clean data from the raw data and cleaning log
df_cleaning_data <- cleaningtools::create_clean_data(
    raw_dataset = df_tool_data,
    raw_data_uuid_column = "_uuid",
    cleaning_log = df_filled_cl %>% filter(!question %in% c("duration_audit_sum_all_ms", "duration_audit_sum_all_minutes"), !uuid %in% c("all")),
    cleaning_log_change_type_column = "change_type",
    change_response_value = "change_response",
    NA_response_value = "blank_response",
    no_change_value = "no_action",
    remove_survey_value = "remove_survey",
    cleaning_log_question_column = "question",
    cleaning_log_uuid_column = "uuid",
    cleaning_log_new_value_column = "new_value"
)

openxlsx::write.xlsx(df_cleaning_data, paste0("outputs/", butteR::date_file_prefix(), 
                                              "_cleaning_data_echo_adjumani.xlsx"))
