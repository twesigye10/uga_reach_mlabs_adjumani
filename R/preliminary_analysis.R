library(tidyverse)
library(srvyr)
library(supporteR)
library(analysistools)

source("R/composite_indicators.R")

# clean data
clean_data_path <- "inputs/UGA2401_echo_adjumani_cleaned_data.xlsx"
clean_data_nms <- names(readxl::read_excel(path = clean_data_path, n_max = 2000, sheet = "cleaned_data"))
clean_c_types <- ifelse(str_detect(string = clean_data_nms, pattern = "_other$"), "text", "guess")
df_main_clean_data <- readxl::read_excel(path = clean_data_path, col_types = clean_c_types, na = "NA", sheet = "cleaned_data") 

# tool
loc_tool <- "inputs/UGA2401_Adjumani_ECHO_tool.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

# dap
dap <- read_csv("inputs/r_dap_echo_adjumani.csv") %>% 
    filter(!group_var %in% c("reason_for_health_facility_choice",
                            "hh_length_stay_adjumani_city"))

# pop
df_ref_pop <- read_csv("inputs/refugee_population_echo_adjumani.csv")
df_host_pop <- read_csv("inputs/host_population_echo_adjumani.csv")

# data with composites
df_data_with_composites <- df_main_clean_data %>% 
    create_composite_indicators() %>% 
    mutate(strata = paste0(status, "_", meta_division_name)) #%>% 
    # addindicators::add_lcsi(lcsi_stress_vars = c("lcsi_stress1_sold_household_assets", "lcsi_stress2_borrowed_money", "lcsi_stress3_spent_savings", "lcsi_stress4_sold_more_animals_than_usual"),
    #                         lcsi_crisis_vars = c("lcsi_crisis1_reduced_expenditure_on_health_and_education", "lcsi_crisis2_sold_productive_assets_or_means_of_transport", "lcsi_crisis3_withdrew_children_from_school"),
    #                         lcsi_emergency_vars = c("lcsi_emergency1_increase_the_number_of_family_members_searching_for_work_outside_your_village", "lcsi_emergency2_purchased_food_on_credit", "lcsi_emergency3_begged_or_relied_on_charity"),
    #                         yes_val = "yes",
    #                         no_val = "no_had_no_need",
    #                         exhausted_val = "no_exhausted",
    #                         not_applicable_val = "not_applicable",
    #                         ignore_NA = TRUE) 

# refugee analysis --------------------------------------------------------

# weights
df_ref_with_weights <- analysistools::add_weights(dataset = df_data_with_composites %>% 
                                                      filter(status %in% c("refugee")),
                                                  sample_data = df_ref_pop,
                                                  strata_column_dataset = "strata",
                                                  strata_column_sample = "strata",
                                                  population_column =  "population")
ref_svy <- as_survey(.data = df_ref_with_weights, strata = strata, weights = weights)

df_analysis_refugee <- analysistools::create_analysis(design = ref_svy, 
                                                      loa = dap,
                                                      sm_separator = "/")

# host analysis -----------------------------------------------------------

# weights
df_host_with_weights <- analysistools::add_weights(dataset = df_data_with_composites %>% 
                                                       filter(status %in% c("host_community")),
                                                   sample_data = df_host_pop,
                                                   strata_column_dataset = "strata",
                                                   strata_column_sample = "strata",
                                                   population_column =  "population")

host_svy <- as_survey(.data = df_host_with_weights, strata = strata, weights = weights)

df_analysis_host <- analysistools::create_analysis(design = host_svy, 
                                                      loa = dap,
                                                      sm_separator = "/") 

# analysis tables ---------------------------------------------------------

df_refugee_analysis_table <- presentresults::create_table_variable_x_group(results_table = df_analysis_refugee$results_table)

presentresults::create_xlsx_variable_x_group(table_group_x_variable = df_refugee_analysis_table,
                                             file_path = paste0("outputs/", butteR::date_file_prefix(), "_analysis_tables_UGA2401_echo_adjumani_refugee.xlsx"),
                                             table_sheet_name = "refugee"
                                             
)

df_host_analysis_table <- presentresults::create_table_variable_x_group(results_table = df_analysis_host$results_table)

presentresults::create_xlsx_variable_x_group(table_group_x_variable = df_host_analysis_table,
                                             file_path = paste0("outputs/", butteR::date_file_prefix(), "_analysis_tables_UGA2401_echo_adjumani_host.xlsx"),
                                             table_sheet_name = "host"
                                             
)
