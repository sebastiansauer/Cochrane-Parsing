compare_human_machine_extractions <- function() {
  

  
  reviewer_selected = config$reviewer
  write_to_disk = config$write_to_disk
  discrepancies_only = config$discrepancies_only
  verbose = config$verbose
  
  flog.info("compare_human_machine_extractions", name = "fun_log")
  flog.info("Starting fun compare_human_machine_extractions.")
  
  if (!exists("config")) {
    flog.info("Config.yaml has been read.")
    config <<- read_yaml("config.yaml")
  }
  
  ########## manual extractions:

  # Define output path:
  output_path <- glue("{here()}/output/comparison/{reviewer_selected}")
  
  if (verbose) print(paste("Assuming this output path:" , output_path, "\n"))
  flog.trace(paste("Assuming this output path:" , output_path))
  
  # Here's the path where the human (manual) extractions come from:
  extractions_manual_path <- glue("manual-extractions/{reviewer_selected}/{config$human_extractions_file}{reviewer_selected}.xlsx")
  
  if (verbose) print(paste("Assuming this path for manual extraction data:" , extractions_manual_path, "\n"))
  flog.info(paste("Assuming this path for manual extraction data:" , extractions_manual_path))
  
  
  if (!file.exists(extractions_manual_path)) {
    flog.error("File with manual extractions not found!")
    stop("File for `extractions_manual_path` not found!") 
  }
   
  else message("File found.")
  
  flog.trace("Read human extraction data.")
  extractions_manual <-
    read_xlsx(path = extractions_manual_path,
              skip = 1) %>%   # invalid header row
    slice(-c(1,2))  %>% # invalued header rows
    rename(reviewer = `...1`)

  flog.trace("Cleaning names.")
  extractions_manual2 <-
    extractions_manual %>% 
    clean_names()
  
  extractions_manual3 <-
    extractions_manual2
  
  names(extractions_manual3)[3] <- "SoF_table_number"
  names(extractions_manual3)[4] <- "is_outdated"
  names(extractions_manual3)[5] <- "is_withdrawn"
  names(extractions_manual3)[7] <- "GRADE_used"
  names(extractions_manual3)[22] <- "first_high_qual_outcome"
  
  if (verbose) print("Now starting recoding...\n")
  flog.trace("Now starting recoding.")
  
  flog.trace("Recoding values in columns.")
  extractions_manual3a <-
    extractions_manual3 %>% 
    mutate(across(.cols = c(3:7),
                  .fns = tolower)) %>% 
    mutate(is_outdated = ifelse(is_outdated == "yes", TRUE, FALSE),
           is_withdrawn = ifelse(is_withdrawn == "yes", TRUE, FALSE),
           GRADE_used = ifelse(GRADE_used == "yes", TRUE, FALSE)) %>% 
    group_by(url) %>% 
    mutate(SoF_table_number = row_number()) %>% 
    ungroup() %>% 
    mutate(has_high_quality_outcomes = ifelse(
      str_count(str_squish(first_high_qual_outcome)) > 0, TRUE, FALSE)) %>% 
    mutate(has_high_quality_outcomes = replace_na(has_high_quality_outcomes, FALSE)) %>% 
    mutate(first_high_qual_outcome = replace_na(first_high_qual_outcome, 
                                                "no high quality outcome"))
  
  
  if (verbose) print("Now sanitzing review url ...\n")
  flog.trace("Now sanitzing review url.")
  
  extractions_manual3b <- 
    extractions_manual3a %>% 
    mutate(cochrane_id = sanitize_review_url(url))
  
  
  flog.trace("Removeing parantheses, b/c unmatches parantheses caused problems.")
  if (verbose) print("Now removing parentheses.")
    extractions_manual3c <- 
    extractions_manual3b %>% 
    mutate(first_high_qual_outcome = str_remove_all(
      string = first_high_qual_outcome,
      pattern = "[()]+"))
  
  cols_to_be_checked <-
    c("is_outdated",
      "is_withdrawn",   
      "GRADE_used",
      "first_high_qual_outcome",
      "has_high_quality_outcomes"
    )
  
  id_cols_manual <-
    c("reviewer", "title", "url", "cochrane_id", "SoF_table_number")
  
  if (verbose) print("Now selecting relevant columns.")
  flog.trace("Now selecting relevant columns.")
  extractions_manual4 <-
    extractions_manual3c %>% 
    select(any_of(c(id_cols_manual, cols_to_be_checked)))
  
  if (verbose) print("Now filtering rows to selected reviewer.\n")
  flog.trace("Now filtering rows to selected reviewer.")
  extractions_manual5 <-
    extractions_manual4 %>% 
    filter(tolower(reviewer) == reviewer_selected)
  
  
  ########## machine extractions:
  
  
  extractions_machine_path <- glue( "output/{reviewer_selected}/{config$machine_extractions_file}{reviewer_selected}.xlsx")
  
  if (verbose) print(paste("Assuming this path for machine extracted data: ",
                     extractions_machine_path, "\n"))
  flog.info(paste("Assuming this path for machine extracted data: ",
                  extractions_machine_path))
  
   if (!file.exists(extractions_machine_path)) {
    flog.error("File does not exist!")
    stop("File does not exist!") 
   }
    else message("File found.")
  
  extractions_machine <- 
    read_xlsx(extractions_machine_path)
  
  flog.trace("Excel file with machine extracted data has been read.")
  if (verbose) print("Excel file with machine extracted data has been read.\n")
  
 
  if (str_detect(names(extractions_machine)[9], "^doi\\.\\..*\\d$")) {
    if (verbose) print("Column `doi` has an incorrect name, something starting with `doi..`. 
                       I'm correcting, but results my by flawed.")
    flog.warn("Column `doi` has an incorrect name, something starting with `doi..`. 
                       I'm correcting, but results my by flawed.")
    
    names(extractions_machine)[9] <- "doi"
  }

  
  extractions_machine2 <-
    extractions_machine %>% 
    mutate(is_outdated = !is_most_recent_version,
           url = doi,
           cochrane_id = sanitize_review_url(doi)) 
  
  flog.trace("Computing variables: GRADE_used, has_high_quality_outcomes, first_high_qual_outcome\n")
  extractions_machine3 <-
    extractions_machine2 %>% 
    group_by(doi, SoF_table_number) %>% 
    mutate(GRADE_used = str_detect(GRADE, "⊕|⊝")) %>% 
    mutate(GRADE_used = ifelse(is.na(GRADE_used), FALSE, TRUE )) %>% 
    mutate(GRADE_used = ifelse(any(GRADE_used == TRUE), TRUE, FALSE)) %>% 
    mutate(is_high_quality_outcome = str_detect(tolower(GRADE), "high")) %>% 
    mutate(has_high_quality_outcomes = case_when(
      any(is_high_quality_outcome) ~ TRUE,
      TRUE ~ FALSE)) %>% 
    mutate(first_high_qual_outcome = 
             ifelse(is_high_quality_outcome == TRUE,
                    Outcomes, "no high quality outcome")) %>% 
    ungroup()
  
  flog.trace("`SoF_table_number` should not be of type `character`.")
  if (is.character(extractions_machine3$SoF_table_number)) 
    extractions_machine3$SoF_table_number <- as.integer(extractions_machine3$SoF_table_number)
  
  extractions_machine3a <- extractions_machine3
  
  if (verbose) print("Now selecting columns.")
  flog.trace("Now selecting columns.")
  extractions_machine4 <-
    extractions_machine3a %>% 
    select(cochrane_id, 
           SoF_table_number, 
           is_paywalled, 
           warnings,
           is_high_quality_outcome,
           any_of(cols_to_be_checked))
  
  if (verbose) print("Now removing parentheses.")
  flog.trace("Now removing parentheses.")
  # remove parantheses, because some unmatched parantheses caused problems:
  extractions_machine4a <- 
    extractions_machine4 %>% 
    mutate(first_high_qual_outcome = str_remove_all(
      string = first_high_qual_outcome,
      pattern = "[()]+"
    ))
  
 
  
  if (verbose) print("Now starting merging...\n")
  flog.info("Merging human and machine results.")
  
  extractions_merged <- 
    extractions_manual5 %>% 
    left_join(y = extractions_machine4a, 
              by = c("cochrane_id", "SoF_table_number"))
  
  if (verbose) print("Now comparing human with machine extractions.")
  flog.info("Now comparing human with machine extractions.")
  extractions_merged2 <-
    extractions_merged %>% 
    mutate(identical_outdated = is_outdated.x == is_outdated.y,
           identical_withdrawn = is_withdrawn.x == is_withdrawn.y,
           identical_GRADE_used = GRADE_used.x == GRADE_used.y) %>% 
    rowwise() %>% 
    mutate(identical_has_high_quality_outcomes = 
             has_high_quality_outcomes.x == has_high_quality_outcomes.y) %>% 
    mutate(identical_first_high_qual_outcome = 
             case_when(
               first_high_qual_outcome.x == first_high_qual_outcome.y ~ TRUE,
               str_detect(first_high_qual_outcome.x, first_high_qual_outcome.y) ~ TRUE,
               str_detect(first_high_qual_outcome.y, first_high_qual_outcome.x) ~ TRUE,
               TRUE ~ FALSE
             )) %>% 
    ungroup()
  
  flog.info("Now starting testing for equality human/machine... \n")
  if (verbose) print("Now starting testing for equality human/machine... \n")
  extractions_merged3 <-
    extractions_merged2 %>% 
    filter(identical_outdated == FALSE |
             identical_withdrawn == FALSE |
             identical_GRADE_used == FALSE |
             identical_has_high_quality_outcomes == FALSE |
             identical_has_high_quality_outcomes == FALSE)
  
  if (verbose) print(paste0("Dimensions (row/cols) of Excel files with discrepancies: ", str_c(dim(extractions_merged), 
                                                                                                  collapse = " ")))
   
  
  if (write_to_disk) {
    
    if (!dir.exists(output_path)) {
      flog.warn("Output path does not exist, now creating it.")
      flog.warn(paste0("Output path: ", output_path))
      dir.create(output_path)
    }
    
    flog.info(glue("{output_path}/comparison_human_machine_{reviewer_selected}.xlsx"))
    path_ext_merged2 <- glue("{output_path}/comparison_human_machine_{reviewer_selected}.xlsx")
    if (verbose) print(paste("Assuming this output path for complete output file: ", path_ext_merged2, "\n"))
    
    write_xlsx(extractions_merged2,
               path = path_ext_merged2)
    
    
    path_ext_merged3 <-
      glue("{output_path}/comparison_human_machine_problems_only_{reviewer_selected}.xlsx")
    if (verbose) print(paste("Assuming this output path for file with discrepancies only: ", path_ext_merged3, "\n"))
    flog.info(glue("{output_path}/comparison_human_machine_problems_only_{reviewer_selected}.xlsx"))
    
    write_xlsx(extractions_merged3,
               path = path_ext_merged3)
    
    if (verbose) print("Files have been written to disk.")
    flog.info("Files have been written to disk.")
  }
  
  
  if (discrepancies_only) output <- extractions_merged3
  if (!discrepancies_only) output <- extractions_merged2
  
  
  return(output)
}
