# download audit files ----------------------------------------------------

download_audit_files <- function(df, uuid_column = "_uuid", audit_dir, usr, pass){
    if (!"httr" %in% installed.packages()) 
        stop("The package is httr is required!")
    if (is.na(audit_dir) || audit_dir == "") 
        stop("The path for storing audit files can't be empty!")
    if (is.na(usr) || usr == "") 
        stop("Username can't be empty!")
    if (is.na(pass) || pass == "") 
        stop("Password can't be empty!")
    
    # checking if the output directory is already available
    if (!dir.exists(audit_dir)) {
        dir.create(audit_dir)
        if (dir.exists(audit_dir)) {
            cat("Attention: The audit file directory was created in", audit_dir,"\n")
        }
    }
    
    # checking if creating output directory was successful
    if (!dir.exists(audit_dir))
        stop("download_audit_fils was not able to create the output directory!")
    # checking if uuid column exists in data set
    if (!uuid_column %in% names(df))
        stop("The column ", uuid_column, " is not available in data set.")
    # checking if column audit_URL exists in data set
    if (!uuid_column %in% names(df))
        stop("The column ", uuid_column, " is not available in data set.")
    if (!"audit_URL" %in% names(df))
        stop("Error: the column audit_URL is not available in data set.")
    
    # getting the list of uuids that are already downloaded
    available_audits <- dir(audit_dir)
    
    # excluding uuids that their audit files are already downloaded
    df <- df[!df[[uuid_column]] %in% available_audits,]
    
    audits_endpoint_link <- df[["audit_URL"]]
    names(audits_endpoint_link) <- df[[uuid_column]]
    audits_endpoint_link <- na.omit(audits_endpoint_link)
    
    if (length(audits_endpoint_link) > 0) {
        # iterating over each audit endpoint from data
        for (i in 1:length(audits_endpoint_link)) {
            uuid = names(audits_endpoint_link[i])
            endpoint_link_i <- audits_endpoint_link[i]
            cat("Downloading audit file for", uuid, "\n")
            
            # requesting data
            audit_file <- content(GET(endpoint_link_i,
                                      authenticate(usr, pass),
                                      timeout(1000),
                                      progress()), "text", encoding = "UTF-8")
            
            if (!is.na(audit_file)) {
                if (length(audit_file) > 2) {
                    dir.create(paste0(audit_dir, "/", uuid), showWarnings = F)
                    write.csv(audit_file, paste0(audit_dir, "/", uuid, "/audit.csv"), row.names = F)
                }else if(!audit_file == "Attachment not found"){
                    if (grepl("[eventnodestartend]", audit_file)) {
                        dir.create(paste0(audit_dir, "/", uuid), showWarnings = F)
                        write.table(audit_file, paste0(audit_dir, "/", uuid, "/audit.csv"), row.names = F, col.names = FALSE, quote = F)
                    } else{
                        cat("Error: Downloading audit was unsucessful!\n")
                    }
                }
            } else{
                cat("Error: Downloading audit was unsucessful!\n")
            }
        }
    } else{
        cat("Attention: All audit files for given data set is downloaded!")
    }
}

# silhouette analysis based on gower distance between surveys -------------

# silhouette analysis based on gower distance between surveys
# METHOD: check for anomalies using the silhouette function. We assume the dataset is clustered using the 
# enumerator IDs as the cluster IDs and we calculate the silhouette for this clustering scenario. A 
# silhouette value close to 1 indicates that the entries of the cluster are very similar to each other and 
# very dissimilar from entries of other clusters. Thus, we need to raise a flag if the silhouette value gets 
# close to 1 for any of the clusters/enumerators.
# https://en.wikipedia.org/wiki/Silhouette_(clustering)
# https://dpmartin42.github.io/posts/r/cluster-mixed-types
# https://medium.com/@rumman1988/clustering-categorical-and-numerical-datatype-using-gower-distance-ab89b3aa90d9

calculateEnumeratorSimilarity <- function(data, input_df_survey, col_enum, col_admin){
    
    # helper function
    convertColTypes <- function(data, input_df_survey){
        # select_multiple: numeric or factor?
        col.types <- data.frame(column=colnames(data)) %>% 
            left_join(select(input_df_survey, name, type), by=c("column"="name")) %>% 
            mutate(type.edited = case_when(type %in% c("integer", "decimal", "calculate") ~ "numeric",
                                           str_starts(type, "select_") ~ "factor",
                                           str_detect(column, "/") ~ "factor",
                                           TRUE ~ "text"))
        
        cols <- col.types[col.types$type.edited=="numeric", "column"]
        data[,cols] <- lapply(data[,cols], as.numeric)
        cols <- col.types[col.types$type.edited=="text", "column"]
        data[,cols] <- lapply(data[,cols], as.character)
        cols <- col.types[col.types$type.edited=="factor", "column"]
        data[,cols] <- lapply(data[,cols], as.factor)
        
        return(data)
    }
    
    # convert columns using the tool
    data <- convertColTypes(data, input_df_survey)
    # keep only relevant columns
    cols <- data.frame(column=colnames(data)) %>% 
        left_join(select(input_df_survey, name, type), by=c("column"="name")) %>% 
        filter(!(type %in% c("_uuid", "enumerator_id")) &
                   !str_starts(column, "_other$") &
                   !str_detect(column, "_specify$"))
    # convert character columns to factor and add enum.id
    data <- data[, all_of(cols$column)] %>% 
        mutate_if(is.character, factor) %>% 
        arrange(!!sym(col_enum)) %>%
        mutate(enum.id=as.numeric(!!sym(col_enum)), .after=!!sym(col_enum))
    
    # calculate similarity (for enumerators who completed at least 5 surveys)
    res <- data %>% split(data[[col_admin]]) %>% 
        lapply(function(gov){
            df <- gov %>% 
                group_by(enum.id) %>% 
                mutate(n=n()) %>% 
                filter(n>=5) %>% 
                ungroup() %>% 
                select_if(function(x) any(!is.na(x)))
            
            if (length(unique(df$enum.id)) > 1){
                # calculate gower distance
                gower_dist <- daisy(select(df, -c(!!sym(col_enum), enum.id)), 
                                    metric = "gower", warnBin = F, warnAsym = F, warnConst = F)
                # gower_mat <- as.matrix(gower_dist)
                # calculate silhouette
                si <- silhouette(df$enum.id, gower_dist)
                res.si <- summary(si)
                # create output
                r <- data.frame(enum.id=as.numeric(names(res.si$clus.avg.widths)), si=res.si$clus.avg.widths) %>% 
                    left_join(distinct(select(df, !!sym(col_admin), !!sym(col_enum), enum.id)), by="enum.id") %>% 
                    left_join(group_by(df, enum.id) %>% summarise(num.surveys=n(), .groups="drop_last"), by="enum.id") %>% 
                    select(!!sym(col_admin), !!sym(col_enum), num.surveys, si) %>% 
                    arrange(-si)
                return(r)
            }
        })
    do.call(rbind, res)
}



# combine the log but keep the change type and already filled values --------

create_combined_log_keep_change_type <- function(list_of_log,
                                                 dataset_name = "checked_dataset") {
    ## log must be a list
    
    if (is.data.frame(list_of_log) | is.character(list_of_log)) {
        stop(glue::glue("list_of_log must be a list which should contain the logs."))
    }
    
    ## look for dataset name
    if (!is.null(dataset_name)) {
        if (!dataset_name %in% names(list_of_log)) {
            stop(glue::glue(dataset_name, " can not be found in the list_of_log."))
        }
    }
    
    if (is.null(dataset_name) & "checked_dataset" %in% names(list_of_log)) {
        warning(glue::glue("You have a checked_dataset element in the list_of_log even though you have set dataset_name to NULL. Please check the parameter."))
    }
    
    if (is.null(dataset_name) & !"checked_dataset" %in% names(list_of_log)) {
        message(glue::glue("No dataset name is provided. Assuming that the dataset does not exist in the list_of_log."))
    }
    
    
    
    output <- list()
    
    if (!is.null(dataset_name)) {
        output[["checked_dataset"]] <- list_of_log[[dataset_name]]
    }
    
    list_of_log_only <- list_of_log[names(list_of_log)[!names(list_of_log) %in% dataset_name]]
    
    
    list_of_log_only <- list_of_log_only %>%
        purrr::map(.f = ~ dplyr::mutate(., dplyr::across(
            .cols = tidyselect::everything(),
            .fns = ~ format(., scientific = F, justify = "none", trim = T)
        )))
    
    print(names(list_of_log) |> glue::glue_collapse(", ") %>% glue::glue("List of element to combine- ", .))
    
    output[["cleaning_log"]] <- dplyr::bind_rows(list_of_log_only)# |>
    # dplyr::mutate(
    #   change_type = NA_character_,
    #   new_value = NA_character_
    # )
    
    if(is.null(output[["cleaning_log"]][["check_binding"]])) {
        output[["cleaning_log"]] <- output[["cleaning_log"]] |>
            dplyr::mutate(
                check_binding = paste(question, uuid, sep = " ~/~ ")
            )
    } else {
        output[["cleaning_log"]] <- output[["cleaning_log"]] |>
            dplyr::mutate(
                check_binding = dplyr::case_when(is.na(check_binding) ~ paste(question, uuid, sep = " ~/~ "),
                                                 TRUE ~  check_binding)
            )
        
    }
    
    output
}


#' Add new select multiple choices to the data
#'
#' @param input_df_tool_data Specify the data frame for the raw data
#' @param input_df_filled_cl Specify the data frame for the filled cleaning log
#' @param input_df_choices Specify the data frame for the choices in the choices sheet of the tool
#'
#' @return An updated data frame of the data with added columns for new choices
#' @export
#'
#' @examples
cts_add_new_sm_choices_to_data <- function(input_df_tool_data, input_df_filled_cl, input_df_choices, input_sm_seperator = "/") {
    # gather choice options based on unique choices list
    df_grouped_choices<- input_df_choices %>%
        group_by(list_name) %>%
        summarise(choice_options = paste(name, collapse = " : "))
    # regexes
    sm_question_regex <- paste0("\\w+\\",input_sm_seperator,"+\\w+")
    sm_int_choice_regex <- paste0("\\w+\\",input_sm_seperator)
    sm_int_question_regex <- paste0("\\",input_sm_seperator,"+\\w+")
    
    # get new name and choice pairs to add to the choices sheet
    new_vars_sm <- input_df_filled_cl %>%
        filter(str_detect(string = question, pattern = sm_question_regex)) %>%
        filter(!str_detect(string = question, pattern = "other$"), change_type %in% c("change_response")) %>%
        mutate(int.new_value = str_replace_all(string = question, pattern = sm_int_choice_regex, replacement = ""),
               int.question = str_replace_all(string = question, pattern = sm_int_question_regex, replacement = "")) %>% 
        left_join(df_survey, by = c("int.question" = "name")) %>%
        filter(str_detect(string = type, pattern = "select_one|select one|select_multiple|select multiple")) %>%
        separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop") %>%
        left_join(df_grouped_choices, by = "list_name") %>%
        filter(!str_detect(string = choice_options, pattern = int.new_value)) %>%
        select(question) %>%
        group_by(question) %>% 
        summarise(n = n())
    
    # handle when a question had not been answered
    df_add_columns_to_data <- input_df_tool_data %>% 
        butteR:::mutate_batch(nm = new_vars_sm$question, value = NA_character_ ) # %>% 
    
    # parent questions for select multiple
    col_changes_parent_vars_sm <- new_vars_sm %>% 
        mutate(question = str_replace_all(string = question, pattern = paste0(input_sm_seperator,".+"), replacement = "")) %>% 
        pull(question) %>% 
        unique()
    
    df_handle_sm_data <- df_add_columns_to_data
    
    for (cur_sm_col in col_changes_parent_vars_sm) {
        df_updated_data <- df_handle_sm_data %>% 
            mutate(
                across(contains(paste0(cur_sm_col, input_sm_seperator)), .fns = ~ifelse(!is.na(!!sym(cur_sm_col)) & is.na(.) , 0, .)),
                across(contains(paste0(cur_sm_col, input_sm_seperator)), .fns = ~ifelse(is.na(!!sym(cur_sm_col)), NA_integer_, .))
            )
        df_handle_sm_data <- df_updated_data
    }
    
    df_data_with_added_cols <- df_handle_sm_data
}


#' Update select multiple parent columns
#'
#' @param input_df_cleaning_step_data The output data frame from the cleaning step
#' @param input_sm_seperator The seperator for select multiple
#'
#' @return A list that contains an updated data and the extra log to add to the original log used for cleaning 
#' @export
#'
#' @examples
cts_update_sm_parent_cols <- function(input_df_cleaning_step_data, input_sm_seperator = "/") {
    
    # check existance of sm columns
    if(!str_detect(string = colnames(input_df_cleaning_step_data), pattern = paste0("\\",input_sm_seperator))){
        stop("check that there are select multiple columns and the sm seperator ")
    }
    
    # parent column names
    sm_parent_cols <- input_df_cleaning_step_data %>% 
        select(contains("/")) %>% 
        colnames() %>% 
        str_replace_all(pattern = "’", replacement = "") %>% 
        str_replace_all(pattern = "\\/+\\w+", replacement = "") %>% 
        unique()
    
    
    # update the sm parent columns using changes made during cleaning ---------
    
    # initialise data to be updated
    df_handle_parent_qn_data <- input_df_cleaning_step_data
    
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
    
    # extract updated data
    df_updated_parent_cols <- df_handle_parent_qn_data
    
    # generate extra log ------------------------------------------------------
    
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
    
    updated_sm_parent_data <- list("updated_sm_parents" = df_updated_parent_cols %>% 
                                       select(-matches("^int.|^check.")),
                                   "extra_log_sm_parents" = df_log_parent_sm_cols_changes)
    
}