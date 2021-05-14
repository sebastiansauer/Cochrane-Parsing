
# get_summary_table_metadata ----------------------------------------------




get_summary_table_metadata <- function(safe_page_content, 
                                       table_number = 1,
                                       verbose = TRUE) {
  
  
  writeLines("Start parsing SoF Table for metadata.\n")
  
  # run only if at least one such tables exists:
  nr_summaryOfFindingsTable <- get_nr_of_summary_tables(safe_page_content, 
                                                        verbose = TRUE)
  
  

  
  paste0("Number of summary tables detected: ", nr_summaryOfFindingsTable, "\n")
  
  
  # stop if strange stuff happens:
  if (table_number > nr_summaryOfFindingsTable) {
    
    print("This table does not exist! Aborting.\n")
    
    output <- 
      tibble(metadata = NA)
    
    return(output)
    
  }
  
  
  
  # otherwise, start normal work:
  # get raw summary of findings table:
  summaryOfFindingsTable <- 
    safe_page_content %>% 
    html_nodes(".summaryOfFindings") %>% 
    html_nodes("table") %>% 
    .[[table_number]] %>% 
    html_table(fill = TRUE)
  
  
  main_comparison_of_review <-
    summaryOfFindingsTable %>% 
    select(1) %>% 
    filter(row_number() == 1) %>% 
    pull()
  
  main_comparison_population <- 
    summaryOfFindingsTable %>% 
    select(1) %>% 
    slice(2) %>% 
    pull() %>% 
    str_extract("Patient or population.+Setting") %>% 
    str_remove_all(": |Patient or population|Setting")
  
  main_comparison_setting <-
    summaryOfFindingsTable %>% 
    select(1) %>% 
    slice(2) %>% 
    pull() %>% 
    str_extract("Setting.+Intervention") %>% 
    str_remove_all(": |Setting|Intervention")
  
  main_comparison_comparison_type <-
    summaryOfFindingsTable %>% 
    select(1) %>% 
    slice(2) %>% 
    pull() %>% 
    str_extract("Comparison.+") %>% 
    str_remove_all(":|: |Comparison") %>% 
    str_trim()
  
  
  output <-
    tibble(
      main_comparison_of_review = main_comparison_of_review,
      main_comparison_population = main_comparison_population,
      main_comparison_setting = main_comparison_setting,
      main_comparison_comparsion_type = main_comparison_comparison_type
    )
  
  if (verbose) print(output)
  
  
  
  return(output)
  
}







# get_summary_table -------------------------------------------------------



# page_content <- review$page_content


get_summary_table <- function(page_content, 
                              table_number = 1, 
                              drop_unused_cols = TRUE,
                              # set to TRUE if only the first outcome should be parsed:
                              first_outcome_only = FALSE,
                              verbose = TRUE) {
  
  
  warnings_summary_table <- NA
  
  
  
  # stop if critical warning has been raised earlier on:
  if (any(warning_df$critical == TRUE)) {
    
    writeLines("Stopping reading the summary table, as critical warning has been raised earlier on")
    writeLines("Returning empty SoF table")
    output <- create_empty_df(names_vec = c(get_summarytab_colnames(), get_summarytab_metadata_colnames()))
    
    output$warning <- str_c(warning_df$type, collapse = " | ")
                              
    return(output)
    
  }
  
  
  # run only if at least one such tables exists:
  
  nr_summaryOfFindingsTable <- get_nr_of_summary_tables(page_content)
  
  
  # if no summary table exists, report it, and stop:
  if ((table_number > nr_summaryOfFindingsTable) | 
      (nr_summaryOfFindingsTable == 0)) {
    
    # output <- 
    #   create_empty_df(names_vec = get_summarytab_colnames())
    warning_df <<-
      warning_df %>% 
      bind_rows(raise_warning(type = "No SoF table detected",
                              critical = FALSE))
    
    
    
    output <- create_empty_df(names_vec = get_summarytab_colnames())
    
    #output$Comments <- "No summary table detected, hence no outcomes reported"
    
    print("As no summary findings tables were detected, I'm stopping the collection of summary tables.")
    
    
    
    
  } else { # otherwise, go on:
    
    
    
    SoF_table_metadata <- get_summary_table_metadata(page_content = page_content,
                                                     table_number = table_number,
                                                     verbose = verbose)
    
    
    # get raw summary of findings table:
    summaryOfFindingsTable <- 
      page_content %>% 
      html_nodes(".summaryOfFindings") %>% 
      html_nodes("table") %>% 
      .[[table_number]] %>% 
      html_table(fill = TRUE)
    
    

    
    # find column in which the outcome variables are mentioned:
    col_Outcomes <- 
      summaryOfFindingsTable %>% 
      map(~ str_detect(., pattern = "^Outcome[s]*|Outcome[s]$")) %>% 
      map_lgl(~ any(. == TRUE)) %>% 
      which()
    
    # take first columns if there are more than one relevant column:
    if (length(col_Outcomes) > 1) {
      warning_df <<- 
        warning_df %>% 
        bind_rows(raise_warning(type = "too many columns at `col_Outcomes`",
                                critical = FALSE))
      
      col_Outcomes <- col_Outcomes[1]
      
      writeLines(glue::glue("Too many columns at `col_Outcomes`. Taking the first one. Might by wrong!\n"))
      
      #return(output)
    }
    
    if (length(col_Outcomes) == 0) {
      warning_df <<- 
        warning_df %>% 
        bind_rows(raise_warning(type = "no columns at `col_Outcomes`",
                                critical = FALSE))
      
      col_Outcomes <- "NO_COL_OUTCOME"
      
    }
    
    names(summaryOfFindingsTable)[col_Outcomes] <- "Outcomes"
    
    
    
    # add id column:
    summaryOfFindingsTable <- 
      summaryOfFindingsTable %>% 
      mutate(id_measure = row_number()) %>% 
      select(id_measure, everything())
    
    # delete non-data rows:
    header_rows <- 
      summaryOfFindingsTable %>% 
      filter(str_detect(str_to_lower(Outcomes), "^outcomes?$")) %>% 
      pull(id_measure) %>% 
      max()
    
    # define header rows:
    # we'll need this table further down below:
    summaryOfFindingsTable <-
      summaryOfFindingsTable %>% 
      mutate(header_row = row_number() <= header_rows) 
    
    # find column with GRADE rating:
    # "Quality of the evidence (GRADE)"
    col_GRADE <-
      summaryOfFindingsTable %>% 
      filter(header_row == TRUE) %>% 
      map(~ str_detect(., pattern = "Quality of the evidence (GRADE)$|(GRADE)")) %>% 
      map_lgl(~ any(. == TRUE)) %>% 
      which() %>% 
      names()
    
    if (verbose) writeLines("Col_GRADE found.")
    
    
    
    # delete unneeded header rows:
    summaryOfFindingsTable2 <-
      summaryOfFindingsTable %>%
      filter(id_measure > header_rows)
    
    
    # delete footer:
    identical_cells_across_cols <- 
      map2_lgl(.x = summaryOfFindingsTable2$Outcomes,
               .y = summaryOfFindingsTable2$X2,
               .f = identical)
    
    # delete rows with constant values:
    summaryOfFindingsTable3 <-
      summaryOfFindingsTable2 %>% 
      filter(!identical_cells_across_cols)  
    
    
    # 2nd attempt to find GRADE cols.
    # find columns where GRADES are shown:
    # take the columns with the most hits
     
    if (length(col_GRADE) != 1) {
    col_GRADE <- 
      summaryOfFindingsTable3 %>% 
      map( ~ str_detect(.x, pattern = "⊕|⊝")) %>% 
      map_dfr( ~ sum(. == TRUE))  %>%
      pivot_longer(everything()) %>% 
      filter(value == max(value)) %>% 
      select(name) %>% 
      pull()
    
    # if there are STILL multiple such cols, take the first one and raise error:
    if (length(col_GRADE) > 1) {
      warning_df <<-
        warning_df %>% 
        bind_rows(raise_warning(type = "Multiple cols found at `col_GRADES`. Taking the first one. Might be wrong!"))
      
      col_GRADE <- col_grades[1]
    }
    }
    
    
    
    # rename:
    summaryOfFindingsTable4 <-
      summaryOfFindingsTable3 %>% 
      rename(GRADE := {col_GRADE}) 
    
    
    # find the effect statistic used:
    effect_statistic <- "HR|SMD|RR|OR|MD|[M|m]ean.+[S|s]core"
    
    effect_statistic_per_outcome <- 
      summaryOfFindingsTable4 %>% 
      map_dfc(~ tibble(col = str_extract(., 
                                         pattern = effect_statistic))) %>% 
      pmap_chr(.f = paste) %>% 
      map_chr(~ str_remove_all(., pattern = "NA | NA"))
    
    
    # make sure the lengths are identical:
    if (length(effect_statistic_per_outcome) < nrow(summaryOfFindingsTable4)) {
      effect_statistic_per_outcome <- c(effect_statistic_per_outcome, 
                                        nrow(summaryOfFindingsTable4) - length(effect_statistic_per_outcome))
    }
    if (length(effect_statistic_per_outcome) > nrow(summaryOfFindingsTable4)) {
      effect_statistic_per_outcome <- effect_statistic_per_outcome[1:nrow(summaryOfFindingsTable4)]
    }
    
    
    
    # BUG???
    summaryOfFindingsTable5 <-
      summaryOfFindingsTable4 %>% 
      mutate(effect_statistic = effect_statistic_per_outcome)
    
    
    # find column where number of participants and number of studies are noted for each outcome:
    n_of_trials_string2 <- "([Ss]tudies)|[Pp]articipants"
    
    
    
    col_participants_studies <- 
      summaryOfFindingsTable %>%  # as defined above
      filter(header_row == TRUE) %>% 
      map( ~ str_detect(., pattern = n_of_trials_string2)) %>% 
      map_lgl( ~ any(. == TRUE)) %>% 
      keep(isTRUE) %>% 
      names()
    
    
    
    # take first columns if there are more than one relevant column:
    if (length(col_participants_studies) > 1) {
      warning_df <<- 
        warning_df %>% 
        bind_rows(raise_warning(type = "too many columns at `col_participants_studies`",
                                critical = FALSE))
      
      # output <- create_empty_df(names_vec = get_all_colnames())
      # output$doi <- review_url
      # output$warning <- warning_df$type
      # 
      col_participants_studies <- col_participants_studies[1]
      
      writeLines(glue::glue("Too many columns at `col_participants_studies`. Taking the first one. Might by wrong!\n"))
      
    }
    
    if (length(col_participants_studies) == 0) {
      warning_df <<- 
        warning_df %>% 
        bind_rows(raise_warning(type = "no columns found at `col_participants_studies`",
                                critical = FALSE))
      
      summaryOfFindingsTable6 <-
        summaryOfFindingsTable5 %>% 
        mutate(n_participants_studies = NA)  
    } else {  # if everything's ok:
      
      summaryOfFindingsTable6 <-
        summaryOfFindingsTable5 %>% 
        rename(n_participants_studies := {col_participants_studies}) 
    }
    
    
    summaryOfFindingsTable6 <-
      summaryOfFindingsTable6 %>% 
      mutate(n_participants = parse_n_subj_n_studies(n_participants_studies,
                                                     return = "subjects"),
             n_studies = parse_n_subj_n_studies(n_participants_studies,
                                                return = "studies"))
    
    
    
    # find column with effect sizes and CI per outcome:
    #rel_eff_CI_str2 <- "Relative effect[s]*[[[:space:]]*\\(95%[[:space:]]*CI\\)]*"
    rel_eff_CI_str <- "([Rr]elative [Ee]ffect.*95% CI)|95% CI  [^absolute]"
    # NOTE: ABSOLUTE effects are ANTI matched. Only RELATIVE effects!
    
    
    col_rel_eff_CI <- 
      summaryOfFindingsTable %>% 
      filter(header_row == TRUE) %>% 
      map( ~ str_detect(., pattern = rel_eff_CI_str)) %>% 
      map_lgl( ~ any(. == TRUE)) %>% 
      keep(isTRUE) %>% 
      names()
    
    
    if (length(col_rel_eff_CI) > 1) {
      warning_df <<- 
        warning_df %>% 
        bind_rows(raise_warning(type = "too many columns at `col_rel_eff_CI`",
                                critical = FALSE))
      
      # output <- create_empty_df(names_vec = get_all_colnames())
      # output$doi <- review_url
      # output$warning <- warning_df$type
      # 
      col_rel_eff_CI <- col_rel_eff_CI[1]
      
      writeLines(glue::glue("Too many columns at `col_rel_eff_CI`. Taking the first one. Might by wrong!\n"))
      
    }
    
    
    if (length(col_rel_eff_CI) == 0)  {
      col_rel_eff_CI <- "NO_RELATIV_EFFECTS_REPORTED"
      summaryOfFindingsTable6 <- 
        summaryOfFindingsTable6 %>% 
        mutate(NO_RELATIV_EFFECTS_REPORTED = NA)
    }
    
    summaryOfFindingsTable7 <- 
      summaryOfFindingsTable6 %>% 
      rename(relative_effect_95CI := {col_rel_eff_CI}) %>% 
      mutate(CI_lower = str_extract(relative_effect_95CI,
                                    "\\(\\d+\\.*\\d+")) %>% 
      mutate(CI_lower = str_remove(CI_lower, "\\(")) %>% 
      mutate(CI_upper = str_extract(relative_effect_95CI,
                                    "\\d+\\.*\\d+\\)")) %>% 
      mutate(CI_upper = str_remove(CI_upper, "\\)")) %>% 
      mutate(effect_size = str_extract(relative_effect_95CI, "[:alpha:]{1,4}\\s*\\d*\\.?\\d*"))
    
    
    # check if there's a comment column for each outcome:
    col_comments <- "Comments"
    
    
    col_comments <-
      summaryOfFindingsTable %>%
      filter(header_row == TRUE) %>% 
      map(~ str_detect(., pattern = col_comments)) %>%
      map_lgl( ~ any(. == TRUE)) %>%
      keep(isTRUE) %>%
      names()
    
    if (length(col_comments) > 1) {
      warning_df <<- 
        warning_df %>% 
        bind_rows(raise_warning(type = "too many columns at `col_comments`",
                                critical = FALSE))
      
      # output <- create_empty_df(names_vec = get_all_colnames())
      # output$doi <- review_url
      # output$warning <- warning_df$type
      # 
      col_comments <- v[1]
      
      writeLines(glue::glue("Too many columns at `col_comments`. Taking the first one. Might by wrong!\n"))
      
    }
    
    if (length(col_comments) == 0) col_comments <- NA
    
    
    # there's not always a column with comments,
    # so we need to be careful:
    if (!(col_comments %in% c(NA, "", " "))) {
      summaryOfFindingsTable7 <-
        summaryOfFindingsTable7 %>%
        rename(Comments := {col_comments})
    }
    
    #re-number if column:
    summaryOfFindingsTable7 <- 
      summaryOfFindingsTable7 %>% 
      mutate(id_measure = row_number()) %>% 
      select(id_measure, everything(), -header_row)
    
    
    # add SoF table metadata:
    summaryOfFindingsTable8 <-
      summaryOfFindingsTable7 %>% 
      bind_cols(SoF_table_metadata)
    
    
    # xxx
    # 
    # define `not-%in%`:
    `%nin%` <- Negate(`%in%`)
    
    # compute significance:
    summaryOfFindingsTable8 <-
    summaryOfFindingsTable8 %>%
      mutate(effect_statistic = str_squish(effect_statistic),
             is_significant = case_when(
               (str_detect(effect_statistic, "RR|OR|HR")) & (CI_lower > 1) ~ "is_significant",
               (str_detect(effect_statistic, "RR|OR|HR")) & (CI_upper < 1) ~ "is_significant",
               (effect_statistic == "SMD") & (CI_lower > 0) ~ "is_significant",
               (effect_statistic == "SMD") & (CI_upper < 0) ~ "is_significant",
               (effect_statistic %nin% str_to_upper(c("OR", "RR", "SMD", "HR"))) ~ "unknown",
               TRUE ~ "not-significant"
             ))

    output <- summaryOfFindingsTable8
    
    if (first_outcome_only) {
      output <- slice(output, 1)
    }
    
    if (drop_unused_cols) {
      output <- 
        output %>% 
        select(-starts_with("X"))
    }
    
    
    
  }
  
  
  if (verbose) writeLines("Finished parsing SoF Table.\n")
  
  return(output)
  
  }





