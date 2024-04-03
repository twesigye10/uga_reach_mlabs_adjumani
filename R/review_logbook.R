# Read data and checking log 
logbook_loc <- "outputs/UGA2401_Adjumani_ECHO_logbook.xlsx"
logbook_col_types <- ifelse(str_detect(string = names(readxl::read_excel(path = logbook_loc, n_max = 5000, sheet = "Log book")), pattern = "start_date|checked_date"), "date",  "text")
df_cleaning_logbook <- readxl::read_excel(logbook_loc, col_types = logbook_col_types, sheet = "Log book")

df_deletaion_log <- readxl::read_excel(logbook_loc, sheet = "deletion_log") 

# raw data
loc_data <- "inputs/UGA2401_Adjumani_ECHO_data.xlsx"
data_nms <- names(readxl::read_excel(path = loc_data, n_max = 2000))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")
df_raw_data <- readxl::read_excel(path = loc_data, col_types = c_types) %>% 
    select(-`...957`)

# import clean data
clean_data_path <- "inputs/UGA2401_echo_adjumani_cleaned_data.xlsx"
clean_data_nms <- names(readxl::read_excel(path = clean_data_path, n_max = 2000))
clean_c_types <- ifelse(str_detect(string = clean_data_nms, pattern = "_other$"), "text", "guess")
df_main_clean_data <- readxl::read_excel(path = clean_data_path, col_types = clean_c_types, na = "NA") 

# tool
loc_tool <- "inputs/UGA2401_Adjumani_ECHO_tool.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")


# reviewing cleaning ------------------------------------------------------

compared_df <- cleaningtools::review_cleaning(
    raw_dataset = df_raw_data,
    raw_dataset_uuid_column = "_uuid",
    clean_dataset = df_main_clean_data,
    clean_dataset_uuid_column = "_uuid",
    cleaning_log = df_cleaning_logbook,
    cleaning_log_uuid_column = "uuid",
    cleaning_log_change_type_column = "changed",
    cleaning_log_question_column = "question.name",
    cleaning_log_new_value_column = "new.value",
    cleaning_log_old_value_column = "old.value",
    cleaning_log_added_survey_value = "added_survey",
    cleaning_log_no_change_value = c("no"),
    deletion_log = df_deletaion_log,
    deletion_log_uuid_column = "uuid",
    check_for_deletion_log = T
)


# output

openxlsx::write.xlsx(x = compared_df,
                     file = paste0("support_files/", butteR::date_file_prefix(), 
                                   "_review_logbook.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "")
openxlsx::openXL(file = paste0("support_files/", butteR::date_file_prefix(),"_review_logbook.xlsx"))
