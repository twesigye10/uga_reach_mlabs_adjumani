library(tidyverse)
library(srvyr)
library(supporteR)
library(analysistools)

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
dap <- read_csv("inputs/r_dap_echo_adjumani.csv")

# pop
df_ref_pop <- read_csv("inputs/refugee_population_echo_adjumani.csv")
df_host_pop <- read_csv("inputs/host_population_echo_adjumani.csv")

# data with composites
df_data_with_composites <- df_main_clean_data %>% 
    create_composite_indicators() %>% 
    addindicators::add_lcsi(lcsi_stress_vars = ,
                            lcsi_crisis_vars = ,
                            lcsi_emergency_vars = ,
                            yes_val = ,
                            no_val = ,
                            exhausted_val = ,
                            not_applicable_val = ,
                            ignore_NA = TRUE) 
    






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


# output analysis with out tables -----------------------------------------

analysis_out_list <- list("Refugee analysis" = df_analysis_refugee,
                                 "Host analysis" = df_analysis_host)

openxlsx::write.xlsx(analysis_out_list, paste0("outputs/", butteR::date_file_prefix(), "_analysis_UGA2401_echo_adjumani.xlsx"))