library(tidyverse)
library(glue)
library(cleaningtools)
library(httr)

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

# gather choice options based on unique choices list
df_grouped_choices<- df_choices %>%
    group_by(list_name) %>%
    summarise(choice_options = paste(name, collapse = " : "))

# get new name and choice pairs to add to the choices sheet
new_vars_sm <- df_filled_cl %>%
    filter(str_detect(string = question, pattern = "\\w+\\/+\\w+")) %>%
    filter(!str_detect(string = question, pattern = "other$")) %>%
    mutate(int.new_value = str_replace_all(string = question, pattern = "\\w+\\/", replacement = ""),
           int.question = str_replace_all(string = question, pattern = "\\/+\\w+", replacement = "")) %>% 
    left_join(df_survey, by = c("int.question" = "name")) %>%
    filter(str_detect(string = type, pattern = "select_one|select one|select_multiple|select multiple")) %>%
    separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop") %>%
    left_join(df_grouped_choices, by = "list_name") %>%
    filter(!str_detect(string = choice_options, pattern = int.new_value)) %>%
    select(question) %>%
    # distinct() %>% # to make sure there are no duplicates
    # arrange(question) %>% 
    group_by(question) %>% 
    summarise(n = n())

# write_csv(new_vars_sm, "outputs/test_new_choices.csv")

# handle when a question had not been answered
df_add_columns_to_data <- df_tool_data %>% 
    butteR:::mutate_batch(nm = new_vars_sm$question, value = NA_character_ ) # %>% 

# parent questions for select multiple
col_changes_parent_vars_sm <- new_vars_sm %>% 
    mutate(question = str_replace_all(string = question, pattern = "/.+", replacement = "")) %>% 
    pull(question) %>% 
    unique()

df_handle_sm_data <- df_add_columns_to_data

for (cur_sm_col in col_changes_parent_vars_sm) {
    df_updated_data <- df_handle_sm_data %>% 
        mutate(
            across(contains(paste0(cur_sm_col, "/")), .fns = ~ifelse(!is.na(!!sym(cur_sm_col)) & is.na(.) , 0, .)),
            across(contains(paste0(cur_sm_col, "/")), .fns = ~ifelse(is.na(!!sym(cur_sm_col)), NA_integer_, .))
        )
    df_handle_sm_data <- df_updated_data
}

df_data_with_added_cols <- df_handle_sm_data



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
    cleaning_log_new_value_column = "new_value"
)

# create the clean data from the raw data and cleaning log
df_cleaning_data <- cleaningtools::create_clean_data(
    raw_dataset = df_data_with_added_cols %>% select(-any_of(cols_to_remove)),
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