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
    filter(!str_detect(string = question, pattern = "other$"), change_type %in% c("change_response")) %>%
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
    cleaning_log_new_value_column = "new_value"
)

# handle parent question columns ------------------------------------------

# parent column names
sm_parent_cols <- df_cleaning_step %>% 
    select(contains("/")) %>% 
    colnames() %>% 
    str_replace_all(pattern = "’", replacement = "") %>% 
    str_replace_all(pattern = "\\/+\\w+", replacement = "") %>% 
    unique()

df_handle_parent_qn_data <- df_cleaning_step

for (cur_parent_sm_col in sm_parent_cols) {
    # test
    print(cur_parent_sm_col)
    
    df_updated_parent_qn_data <- df_handle_parent_qn_data %>% 
        mutate(across(.cols = starts_with(paste0(cur_parent_sm_col, "/")), 
                      .fns = ~ifelse(.x == 1 & !str_detect(string = !!sym(cur_parent_sm_col), pattern = str_replace_all(string = cur_column(), pattern = paste0(cur_parent_sm_col, "/"), replacement = "")), 
                                     str_replace_all(string = cur_column(), pattern = paste0(cur_parent_sm_col, "/"), replacement = ""), 
                                     NA_character_),
                      .names = "check.extra.{.col}"),
               across(.cols = starts_with(paste0(cur_parent_sm_col, "/")), 
                      .fns = ~ifelse(.x == 0 & str_detect(string = !!sym(cur_parent_sm_col), pattern = str_replace_all(string = cur_column(), pattern = paste0(cur_parent_sm_col, "/"), replacement = "")), 
                                     str_replace_all(string = cur_column(), pattern = paste0(cur_parent_sm_col, "/"), replacement = ""), 
                                     NA_character_),
                      .names = "check.removed.{.col}")
        ) %>% 
        unite(!!paste0("check.extra.", cur_parent_sm_col), starts_with(glue::glue("check.extra.{cur_parent_sm_col}/")), remove = TRUE, na.rm = TRUE, sep = " ") %>%
        unite(!!paste0("check.removed.", cur_parent_sm_col), starts_with(glue::glue("check.removed.{cur_parent_sm_col}/")), remove = TRUE, na.rm = TRUE, sep = " ") %>%
        mutate(!!paste0("check.old.", cur_parent_sm_col) := !!sym(cur_parent_sm_col),
               !!paste0("check.reg.", cur_parent_sm_col) := ifelse(!is.na(!!sym(paste0("check.removed.", cur_parent_sm_col))), str_replace_all(string = !!sym(paste0("check.removed.", cur_parent_sm_col)), pattern = " ", replacement = "\\s?|"), NA_character_)) %>% 
        mutate(!!paste0("check.remaining.", cur_parent_sm_col) := ifelse(!(is.na(!!sym(paste0("check.reg.", cur_parent_sm_col))) | !!sym(paste0("check.reg.", cur_parent_sm_col)) %in% c("NA", "")), str_remove_all(string = !!sym(cur_parent_sm_col), pattern = !!sym(paste0("check.reg.", cur_parent_sm_col))), !!sym(cur_parent_sm_col))) %>% 
        unite(!!paste0("check.final.", cur_parent_sm_col), matches(paste0("check.remaining.", cur_parent_sm_col, "$|","check.extra.", cur_parent_sm_col, "$")), remove = FALSE, na.rm = TRUE, sep = " ") %>% 
        mutate(!!paste0("check.final.", cur_parent_sm_col) := str_trim(!!sym(paste0("check.final.", cur_parent_sm_col))),
               !!cur_parent_sm_col := !!sym(paste0("check.final.", cur_parent_sm_col)))
    
    df_handle_parent_qn_data <- df_updated_parent_qn_data
}

df_updated_parent_cols <- df_handle_parent_qn_data

# output datasets

list_of_datasets <- list("raw_data" = df_tool_data %>% select(-any_of(cols_to_remove)),
                         "cleaned_data" = df_updated_parent_cols %>% select(-matches("^int.|^check."))
                         )

openxlsx::write.xlsx(list_of_datasets, 
                     paste0("outputs/", butteR::date_file_prefix(), "_UGA2401_echo_adjumani_cleaned_data.xlsx"))


# extra log for recreated select multiple ---------------------------------

df_log_parent_sm_cols_changes <- purrr::map_dfr(.x = sm_parent_cols, 
                                                .f = ~ {df_updated_parent_cols %>% 
                                                        dplyr::filter(!!sym(paste0("check.old.",.x)) != !!sym(.x)) %>% 
                                                        dplyr::mutate(i.check.uuid = `_uuid`,
                                                                      i.check.enumerator_id = enumerator_id,
                                                                      i.check.point_number = point_number,
                                                                      i.check.today = today,
                                                                      i.check.meta_village_name = meta_village_name,
                                                                      i.check.change_type = "change_response",
                                                                      i.check.question = .x,
                                                                      i.check.old_value = as.character(!!sym(paste0("check.old.",.x))),
                                                                      i.check.new_value = as.character(!!sym(.x)),
                                                                      i.check.issue = "changed parent sm column",
                                                                      i.check.description = "Parent column changed to match children columns",
                                                                      i.check.other_text = "",
                                                                      i.check.comment = "",
                                                                      i.check.reviewed = "1",
                                                                      i.check.so_sm_choices = "") %>%
                                                        dplyr::select(starts_with("i.check."))}) %>% 
    supporteR::batch_select_rename()


openxlsx::write.xlsx(df_log_parent_sm_cols_changes, 
                     paste0("outputs/", butteR::date_file_prefix(), 
                              "_extra_sm_parent_changes_checks_echo_adjumani.xlsx"))