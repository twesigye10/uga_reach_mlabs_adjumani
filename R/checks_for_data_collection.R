library(tidyverse)
library(cleaningtools)
library(httr)
library(supporteR)
library(openxlsx)
library(glue)
library(cluster)

source("R/support_functions.R")
source("support_files/credentials.R")


# read data ---------------------------------------------------------------

loc_data <- "inputs/UGA2401_Adjumani_ECHO_data.xlsx" 


# main data
data_nms <- names(readxl::read_excel(path = loc_data, n_max = 300))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")
df_tool_data <- readxl::read_excel(loc_data, col_types = c_types) %>% 
    select(-`...957`)


# tool

loc_tool <- "inputs/UGA2401_Adjumani_ECHO_tool.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")


# download audit files folder
download_audit_files(df= df_tool_data,
                     uuid_column = "_uuid",
                     audit_dir = "inputs/audit_files",
                     usr = user_acc,
                     pass = user_pss)

# zip audit files folder

if (dir.exists("inputs/audit_files")) {
    zip::zip(zipfile = "inputs/audit_files.zip",
             # files = "inputs/audit_files",
             files = list.dirs(path = "inputs/audit_files/", full.names = TRUE, recursive = FALSE),
             mode = "cherry-pick")

}

# GIS layer for samples
df_sample_data <- sf::st_read("inputs/adjumani_aba_refugee_host_samples.gpkg", quiet = TRUE) 


# check pii ---------------------------------------------------------------

pii_cols <- c("telephone", "contact", "name", "gps", "latitude", "longitude", "contact", "geopoint")
pii_from_data <- cleaningtools::check_pii(dataset = df_tool_data, element_name = "checked_dataset", uuid_column = "_uuid")
pii_from_data$potential_PII


# duration ----------------------------------------------------------------
# read audit file
audit_list_data <- cleaningtools::create_audit_list(audit_zip_path = "inputs/audit_files.zip")

# add duration from audit
df_tool_data_with_audit_time <- cleaningtools::add_duration_from_audit(df_tool_data, uuid_column = "_uuid", audit_list = audit_list_data)

# outliers columns not to check
outlier_cols_not_4_checking <- df_tool_data %>% 
    select(matches("geopoint|gps|_index|_submit|submission|_sample_|^_id$")) %>% 
    colnames()

# logical checks data
df_list_logical_checks <- read_csv("inputs/logical_check_list.csv")

# combine cleaning tools checks
list_log <- df_tool_data_with_audit_time %>%
    # check_pii(uuid_column = "_uuid") %>%
    check_duration(column_to_check = "duration_audit_sum_all_minutes",
                   uuid_column = "_uuid",
                   log_name = "duration_log",
                   lower_bound = 20,
                   higher_bound = 120) %>% 
    check_outliers(uuid_column = "_uuid", sm_separator = "/",
                   strongness_factor = 3, columns_not_to_check = outlier_cols_not_4_checking) %>% 
    check_soft_duplicates(kobo_survey = df_survey,
                          uuid_column = "_uuid",
                          idnk_value = "dk",
                          sm_separator = "/",
                          log_name = "soft_duplicate_log",
                          threshold = 25,
                          return_all_results = FALSE) %>%
    check_value(uuid_column = "_uuid", values_to_look = c(99, 999, 9999)) %>% 
    check_logical_with_list(uuid_column = "_uuid",
                  list_of_check = df_list_logical_checks,
                  check_id_column = "name",
                  check_to_perform_column = "check",
                  columns_to_clean_column = "variables_to_clean",
                  description = "description",
                  bind_checks = TRUE)


# others checks
df_other_checks <- cts_other_specify(input_tool_data = df_tool_data %>% select(-other_expenditure_other), 
                                            input_uuid_col = "_uuid", 
                                            input_survey = df_survey, 
                                            input_choices = df_choices)
# add other checks to the list
list_log$other_log <- df_other_checks


# check duplicate uuids ---------------------------------------------------

df_duplicate_uuids <- cts_checks_duplicate_uuids(input_tool_data = df_tool_data)
list_log$duplicate_uuid_log <- df_duplicate_uuids



# spatial checks ----------------------------------------------------------

if("status" %in% colnames(df_sample_data)){
    sample_pt_nos <- df_sample_data %>%
        mutate(unique_pt_number = paste0(status, "_", Name)) %>%
        pull(unique_pt_number) %>%
        unique()
}else{
    sample_pt_nos <- df_sample_data %>%
        mutate(unique_pt_number = Name) %>%
        pull(unique_pt_number) %>%
        unique()
}

# duplicate point numbers
df_duplicate_pt_nos <- cts_check_duplicate_pt_numbers(input_tool_data = df_tool_data,
                                                              input_uuid_col  = "_uuid",
                                                              input_location_col = "meta_village_name",
                                                              input_point_id_col = "point_number",
                                                              input_sample_pt_nos_list = sample_pt_nos)

list_log$duplicate_pt_nos <- df_duplicate_pt_nos

# point number does not exist in sample
df_pt_number_not_in_sample <- cts_check_pt_number_not_in_samples(input_tool_data = df_tool_data,
                                                                         input_uuid_col  = "_uuid",
                                                                         input_point_id_col = "point_number",
                                                                         input_sample_pt_nos_list = sample_pt_nos)
list_log$pt_number_not_in_sample <- df_pt_number_not_in_sample

# check for exceeded threshold distance
df_greater_thresh_distance <- cts_check_threshold_distance(input_sample_data = df_sample_data,
                                                                   input_tool_data = df_tool_data,
                                                                   input_uuid_col  = "_uuid",
                                                                   input_point_id_col = "point_number",
                                                                   input_threshold_dist = 150)
list_log$greater_thresh_distance <- df_greater_thresh_distance





# silhouette --------------------------------------------------------------

# NOTE: the column for "col_admin" is kept in the data

omit_cols_sil <- c("start", "end", "today", "duration", "duration_minutes",
                   "deviceid", "audit", "audit_URL", "instance_name", "end_survey",
                   "geopoint", "_geopoint_latitude", "_geopoint_altitude", "_geopoint_precision", 
                   "_id" ,"_submission_time","_validation_status","_notes","_status","_submitted_by","_tags","_index",
                   "meta_village_name")

data_similartiy_sil <- df_tool_data %>% 
    select(- any_of(omit_cols_sil), - matches("_note$|^note_"))

df_sil_data <- calculateEnumeratorSimilarity(data = data_similartiy_sil,
                                             input_df_survey = df_survey, 
                                             col_enum = "enumerator_id",
                                             col_admin = "meta_division_name") %>% 
    mutate(si2= abs(si))

df_sil_processed <- df_sil_data[order(df_sil_data$`si2`, decreasing = TRUE),!colnames(df_sil_data)%in%"si2"] %>%  
    # filter(si > 0.6) %>% 
    mutate(i.check.uuid = "all",
           i.check.question = NA_character_,
           i.check.issue = paste("silhouette flag"),
           i.check.description = glue::glue("Potential similar responses for enumerator:{enumerator_id}, location:{meta_division_name}. si: {si}")) %>% 
    batch_select_rename()

# add other checks to the list
list_log$enum_similarity <- df_sil_processed

# combine the checks ------------------------------------------------------

df_combined_log <- create_combined_log_keep_change_type(dataset_name = "checked_dataset", list_of_log = list_log)

# # add_info_to_cleaning_log()
# add_with_info <- add_info_to_cleaning_log(list_of_log = df_combined_log,
#                                                   dataset = "checked_dataset",
#                                                   cleaning_log = "cleaning_log",
#                                                   dataset_uuid_column = "_uuid",
#                                                   cleaning_log_uuid_column = "uuid",
#                                                   information_to_add = c("enumerator_id", "today", "meta_village_name")
# )
# 
# # create_xlsx_cleaning_log()
# add_with_info |>
#     create_xlsx_cleaning_log(
#         kobo_survey = df_survey,
#         kobo_choices = df_choices,
#         use_dropdown = TRUE,
#         output_path = paste0("outputs/", butteR::date_file_prefix(), 
#                              "_combined_checks_adjumani_echo_assessment.xlsx")
#     )



# create workbook ---------------------------------------------------------
# prep data
cols_to_add_to_log <- c("enumerator_id", "point_number", "today", "meta_village_name")

tool_support <- df_combined_log$checked_dataset %>% 
    select(uuid = `_uuid`, any_of(cols_to_add_to_log))

df_prep_checked_data <- df_combined_log$checked_dataset
df_prep_cleaning_log <- df_combined_log$cleaning_log %>%
    left_join(tool_support, by = "uuid") %>% 
    relocate(any_of(cols_to_add_to_log), .after = uuid) %>% 
    add_qn_label_to_cl(input_cl_name_col = "question",
                       input_tool = df_survey, 
                       input_tool_name_col = "name", 
                       input_tool_label_col = "label") %>% 
    mutate(enumerator_id = ifelse(issue %in% c("silhouette flag"), 
                                  str_replace(string = str_extract(string = description, pattern = "enumerator:[0-9]{1,3}"), pattern = "enumerator:", ""),
                                  enumerator_id))

df_prep_readme <- tibble::tribble(
    ~change_type_validation,                       ~description,
    "change_response", "Change the response to new_value",
    "blank_response",       "Remove and NA the response",
    "remove_survey",                "Delete the survey",
    "no_action",               "No action to take."
)

wb_log <- createWorkbook()

hs1 <- createStyle(fgFill = "#E34443", textDecoration = "Bold", fontName = "Arial Narrow", fontColour = "white", fontSize = 12, wrapText = F)

modifyBaseFont(wb = wb_log, fontSize = 11, fontName = "Arial Narrow")

addWorksheet(wb_log, sheetName="checked_dataset")
setColWidths(wb = wb_log, sheet = "checked_dataset", cols = 1:ncol(df_prep_checked_data), widths = 24.89)
writeDataTable(wb = wb_log, sheet = "checked_dataset", 
               x = df_prep_checked_data, 
               startRow = 1, startCol = 1, 
               tableStyle = "TableStyleLight9",
               headerStyle = hs1)
# freeze pane
freezePane(wb = wb_log, "checked_dataset", firstActiveRow = 2, firstActiveCol = 2)


addWorksheet(wb_log, sheetName="cleaning_log")
setColWidths(wb = wb_log, sheet = "cleaning_log", cols = 1:ncol(df_prep_cleaning_log), widths = 24.89)
writeDataTable(wb = wb_log, sheet = "cleaning_log", 
               x = df_prep_cleaning_log, 
               startRow = 1, startCol = 1, 
               tableStyle = "TableStyleLight9",
               headerStyle = hs1)
# freeze pane
freezePane(wb = wb_log, "cleaning_log", firstActiveRow = 2, firstActiveCol = 2)

addWorksheet(wb_log, sheetName="readme")
setColWidths(wb = wb_log, sheet = "readme", cols = 1:ncol(df_prep_readme), widths = 24.89)
writeDataTable(wb = wb_log, sheet = "readme", 
               x = df_prep_readme, 
               startRow = 1, startCol = 1, 
               tableStyle = "TableStyleLight9",
               headerStyle = hs1)
# freeze pane
freezePane(wb = wb_log, "readme", firstActiveRow = 2, firstActiveCol = 2)

# openXL(wb_log)

saveWorkbook(wb_log, paste0("outputs/", butteR::date_file_prefix(),"_combined_checks_echo_adjumani.xlsx"), overwrite = TRUE)
openXL(file = paste0("outputs/", butteR::date_file_prefix(),"_combined_checks_echo_adjumani.xlsx"))
