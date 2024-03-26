# read in log and data

library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)
library(openxlsx)

options("openxlsx.borderStyle" = "thin")

# Read data and checking log 
log_path <- "inputs/main_combined_checks_echo_adjumani.xlsx"
log_data_nms <- names(readxl::read_excel(path = log_path, n_max = 2000))
log_c_types <- case_when(str_detect(string = log_data_nms, pattern = "sheet|new_value|other_text|enumerator_id") ~ "text",
                         str_detect(string = log_data_nms, pattern = "index") ~ "numeric",
                         TRUE ~ "guess")
df_cleaning_log <- readxl::read_excel(log_path, col_types = log_c_types) %>%  
    filter(reviewed %in% c("1"))


# raw data
loc_data <- "inputs/UGA2401_Adjumani_ECHO_data.xlsx"

# cols_to_escape <- c("index", "start", "end", "today", "starttime",	"endtime", "_submission_time", "_submission__submission_time")

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

# filter the log for deletion and remaining entries -----------------------

df_log_del_confirmed <- df_cleaning_log %>% 
    filter(change_type %in% c("remove_survey"))

df_cleaning_log_updated <- df_cleaning_log %>% 
    filter(!uuid %in% df_log_del_confirmed$uuid)

# create variable summary -------------------------------------------------
# also need to add composite indicators
# need to determine new choices added and how many entries were affected
df_variable_summary <- df_survey %>%  
    filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) %>%  
    mutate(variable = name, action = "checked", description = "", observations_affected = "") %>%  
    select(variable, action, description, observations_affected)

# extract data ------------------------------------------------------------
df_data_extract <- df_raw_data %>%  
    mutate(`enumerator ID` = enumerator_id) %>% 
    select(uuid = `_uuid`, `enumerator ID`)

# log ---------------------------------------------------------------------

df_formatted_log <- df_cleaning_log_updated %>%  
    mutate(int.adjust_log = ifelse(change_type %in% c("no_action"), "no", "yes"),
           `enumerator ID` = enumerator_id, 
           question.name = question, 
           Issue = issue, 
           `Type of Issue` = change_type, 
           feedback = comment, 
           changed = int.adjust_log, 
           old.value = old_value, 
           new.value = new_value,
           new.value = ifelse(changed %in% c("no"), old.value, new.value)) %>%  
    select(uuid, `enumerator ID`, question.name, Issue, `Type of Issue`, 
           feedback, changed, old.value, new.value)

# deletion log ------------------------------------------------------------

df_deletion_log <- df_log_del_confirmed %>%  
    group_by(uuid) %>%  
    filter(row_number() == 1) %>%  
    ungroup() %>%  
    select(uuid, `enumerator ID` = enumerator_id, Issue = issue, `Type of Issue (Select from dropdown list)` = change_type, 
           feedback = comment)

# enumerator performance --------------------------------------------------
# Number of surveys collected by enumerators
df_surveys_by_enum <- df_raw_data %>%  
    group_by(enumerator_id) %>%  
    summarise(Number = n())
# Number of changes by enumerators
df_changes_by_enum <- df_cleaning_log_updated %>%  
    filter(!change_type %in% c("no_action")) %>%  
    group_by(enumerator_id) %>%  
    summarise(Number = n())
# Number of changes by enumerators filtered by issues
df_changes_by_enum_issue <- df_cleaning_log_updated %>%  
    filter(!change_type %in% c("no_action")) %>%  
    group_by(enumerator_id, issue) %>%  
    summarise(Number = n())
# Number of deletions by enumerators
df_deletion_by_enum <- df_deletion_log %>%  
    group_by(uuid) %>%  
    filter(row_number() == 1) %>%  
    ungroup() %>%  
    mutate(enumerator_id = `enumerator ID`) %>% 
    group_by(enumerator_id) %>%  
    summarise(Number = n())
# Number of deletions due to time by enumerator
df_deletion_by_enum_time <- df_deletion_log %>%  
    filter(Issue %in% c("Duration is lower or higher than the thresholds")) %>%  
    group_by(uuid) %>%  
    filter(row_number() == 1) %>%  
    ungroup() %>% 
    mutate(enumerator_id = `enumerator ID`) %>%
    group_by(enumerator_id) %>%  
    summarise(Number = n())

 
# format the logbook and export -------------------------------------------
df_variable_tracker <- tibble::tribble(
                                  ~Variable,   ~Action,                                      ~Rationale,
                                 "deviceid", "Removed",    "Blanked columns related to the survey and PII",
                                    "audit", "Removed",    "Blanked columns related to the survey and PII",
                                "audit_URL", "Removed",    "Blanked columns related to the survey and PII",
                            "instance_name", "Removed",    "Blanked columns related to the survey and PII",
                            "geopoint", "Removed",    "Blanked columns related to the survey and PII",
                            "_geopoint_latitude", "Removed",    "Blanked columns related to the survey and PII",
                           "_geopoint_longitude", "Removed",    "Blanked columns related to the survey and PII",
                            "_geopoint_altitude", "Removed",    "Blanked columns related to the survey and PII",
                           "_geopoint_precision", "Removed",    "Blanked columns related to the survey and PII")


# create workbook ---------------------------------------------------------

wb_log <- createWorkbook()

hs1 <- createStyle(fgFill = "#E34443", textDecoration = "Bold", fontName = "Arial Narrow", fontColour = "white", fontSize = 12, wrapText = T)
hs2 <- createStyle(fgFill = "#C4BD97", textDecoration = "Bold", fontName = "Roboto Condensed", fontColour = "white", fontSize = 11, wrapText = T)
hs3 <- createStyle(fgFill = "#D8E4BC", textDecoration = "Bold", fontName = "Roboto Condensed", fontSize = 11, wrapText = T)
hs4 <- createStyle(fgFill = "#D9D9D9", textDecoration = "Bold", fontName = "Roboto Condensed", fontSize = 11, wrapText = T)


# deletion_log ------------------------------------------------------------

addWorksheet(wb_log, sheetName="deletion_log")
writeData(wb_log, sheet = "deletion_log", df_deletion_log, startRow = 1, startCol = 1)
addStyle(wb_log, sheet = "deletion_log", hs1, rows = 1, cols = 1:6, gridExpand = FALSE)
setColWidths(wb = wb_log, sheet = "deletion_log", cols = 1, widths = 36)
setColWidths(wb = wb_log, sheet = "deletion_log", cols = 2, widths = 20)
setColWidths(wb = wb_log, sheet = "deletion_log", cols = 3, widths = 50)
setColWidths(wb = wb_log, sheet = "deletion_log", cols = 4, widths = 40)
setColWidths(wb = wb_log, sheet = "deletion_log", cols = 5, widths = 40)

# log book ----------------------------------------------------------------

addWorksheet(wb_log, sheetName="Log book")
writeData(wb_log, sheet = "Log book", df_formatted_log, startRow = 1, startCol = 1)
addStyle(wb_log, sheet = "Log book", hs1, rows = 1, cols = 1:9, gridExpand = FALSE)
setColWidths(wb = wb_log, sheet = "Log book", cols = 1, widths = 36)
setColWidths(wb = wb_log, sheet = "Log book", cols = 2:9, widths = 20)


# data_extract ------------------------------------------------------------

addWorksheet(wb_log, sheetName="data_extract")
writeData(wb_log, sheet = "data_extract", df_data_extract, startRow = 1, startCol = 1)
addStyle(wb_log, sheet = "data_extract", hs1, rows = 1, cols = 1:2, gridExpand = FALSE)
setColWidths(wb = wb_log, sheet = "data_extract", cols = 1, widths = 36)
setColWidths(wb = wb_log, sheet = "data_extract", cols = 2, widths = 20)


# variable_tracker --------------------------------------------------------

addWorksheet(wb_log, sheetName="variable_tracker")
writeData(wb_log, sheet = "variable_tracker", df_variable_tracker, startRow = 1, startCol = 1)
addStyle(wb_log, sheet = "variable_tracker", hs1, rows = 1, cols = 1:3, gridExpand = FALSE)
setColWidths(wb = wb_log, sheet = "variable_tracker", cols = 1:3, widths = 25)

# Enumerator - performance ------------------------------------------------

addWorksheet(wb_log, sheetName="Enumerator - performance")

setColWidths(wb = wb_log, sheet = "Enumerator - performance", cols = 1, widths = 14)
setColWidths(wb = wb_log, sheet = "Enumerator - performance", cols = 5, widths = 14)
setColWidths(wb = wb_log, sheet = "Enumerator - performance", cols = 9, widths = 14)
setColWidths(wb = wb_log, sheet = "Enumerator - performance", cols = 14, widths = 14)
setColWidths(wb = wb_log, sheet = "Enumerator - performance", cols = 18, widths = 14)
setColWidths(wb = wb_log, sheet = "Enumerator - performance", cols = 22, widths = 14)

# Dataset
writeData(wb_log, sheet = "Enumerator - performance", "Dataset", startRow = 1, startCol = 1)
addStyle(wb_log, sheet = "Enumerator - performance", hs2, rows = 1, cols = 1:2, gridExpand = FALSE)

mergeCells(wb_log, sheet = "Enumerator - performance", rows = 3:4, cols = 1:2)
writeData(wb_log, sheet = "Enumerator - performance", "Number of surveys collected by enumerators", startRow = 3, startCol = 1)
addStyle(wb_log, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 1:2, gridExpand = FALSE)

writeDataTable(wb = wb_log, sheet = "Enumerator - performance",
               x = df_surveys_by_enum ,
               startRow = 6, startCol = 1,
               tableStyle = "TableStyleLight10", headerStyle = hs4)

# Cleaning log 1
writeData(wb_log, sheet = "Enumerator - performance", "Cleaning log", startRow = 1, startCol = 5)
addStyle(wb_log, sheet = "Enumerator - performance", hs2, rows = 1, cols = 5:6, gridExpand = FALSE)

mergeCells(wb_log, sheet = "Enumerator - performance", rows = 3:4, cols = 5:6)
writeData(wb_log, sheet = "Enumerator - performance", "Number of changes by enumerators", startRow = 3, startCol = 5)
addStyle(wb_log, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 5:6, gridExpand = FALSE)

writeDataTable(wb = wb_log, sheet = "Enumerator - performance",
               x = df_changes_by_enum ,
               startRow = 6, startCol = 5,
               tableStyle = "TableStyleLight10", headerStyle = hs4)

# Cleaning log 2
writeData(wb_log, sheet = "Enumerator - performance", "Cleaning log", startRow = 1, startCol = 9)
addStyle(wb_log, sheet = "Enumerator - performance", hs2, rows = 1, cols = 9:11, gridExpand = FALSE)

mergeCells(wb_log, sheet = "Enumerator - performance", rows = 3:4, cols = 9:11)
writeData(wb_log, sheet = "Enumerator - performance", "Number of changes by enumerators filtered by issues", startRow = 3, startCol = 9)
addStyle(wb_log, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 9:10, gridExpand = FALSE)
addStyle(wb_log, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 11, gridExpand = FALSE)

writeDataTable(wb = wb_log, sheet = "Enumerator - performance",
               x = df_changes_by_enum_issue ,
               startRow = 6, startCol = 9,
               tableStyle = "TableStyleLight10", headerStyle = hs4)

# Deletion log 1
writeData(wb_log, sheet = "Enumerator - performance", "Deletion log", startRow = 1, startCol = 14)
addStyle(wb_log, sheet = "Enumerator - performance", hs2, rows = 1, cols = 14:15, gridExpand = FALSE)

mergeCells(wb_log, sheet = "Enumerator - performance", rows = 3:4, cols = 14:15)
writeData(wb_log, sheet = "Enumerator - performance", "Number of deletions by enumerators", startRow = 3, startCol = 14)
addStyle(wb_log, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 14:15, gridExpand = FALSE)

writeDataTable(wb = wb_log, sheet = "Enumerator - performance",
               x = df_deletion_by_enum ,
               startRow = 6, startCol = 14,
               tableStyle = "TableStyleLight10", headerStyle = hs4)

# Deletion log 2
writeData(wb_log, sheet = "Enumerator - performance", "Deletion log", startRow = 1, startCol = 18)
addStyle(wb_log, sheet = "Enumerator - performance", hs2, rows = 1, cols = 18:19, gridExpand = FALSE)

mergeCells(wb_log, sheet = "Enumerator - performance", rows = 3:4, cols = 18:19)
writeData(wb_log, sheet = "Enumerator - performance", "Number of deletions due to time by enumerator", startRow = 3, startCol = 18)
addStyle(wb_log, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 18:19, gridExpand = FALSE)

writeDataTable(wb = wb_log, sheet = "Enumerator - performance",
               x = df_deletion_by_enum_time ,
               startRow = 6, startCol = 18,
               tableStyle = "TableStyleLight10", headerStyle = hs4)

# Deletion log 3
writeData(wb_log, sheet = "Enumerator - performance", "Deletion log", startRow = 1, startCol = 22)
addStyle(wb_log, sheet = "Enumerator - performance", hs2, rows = 1, cols = 22:24, gridExpand = FALSE)

mergeCells(wb_log, sheet = "Enumerator - performance", rows = 3:4, cols = 22:24)
writeData(wb_log, sheet = "Enumerator - performance", "Needs to be reviewed by Research Manager / lead AO before submission to HQ", startRow = 3, startCol = 22)
addStyle(wb_log, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 22:23, gridExpand = FALSE)
addStyle(wb_log, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 24, gridExpand = FALSE)

writeDataTable(wb = wb_log, sheet = "Enumerator - performance",
               x = df_deletion_by_enum %>%  filter(Number > 20) %>%
                   mutate(`issue(s) followed up in country y/n` = "Yes",
                          `further comments` = NA_character_) %>%  select(-Number),
               startRow = 6, startCol = 22,
               tableStyle = "TableStyleLight10", headerStyle = hs4)


saveWorkbook(wb_log, paste0("outputs/", butteR::date_file_prefix(),"_UGA2401_Adjumani_ECHO_logbook.xlsx"), overwrite = TRUE)
openXL(file = paste0("outputs/", butteR::date_file_prefix(),"_UGA2401_Adjumani_ECHO_logbook.xlsx"))

# renamed file
saveWorkbook(wb_log, paste0("outputs/UGA2401_Adjumani_ECHO_logbook.xlsx"), overwrite = TRUE)

