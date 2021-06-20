
get_summary_table <- function(page_content, 
                              table_number = 1) {
  
  
  drop_unused_cols <- config$drop_unused_cols
  first_outcome_only = config$first_outcome_only
  verbose = config$verbose
  
  
  if (verbose) writeLines("Start parsing SoF Table.\n")
  flog.info("Start parsing SoF Table.\n")
  
  #warnings_summary_table <- NA
  
  if (!exists("warning_df")) raise_warning(type = "Init")
  
  # stop if critical warning has been raised earlier on:
  # if (any(warning_df$critical == TRUE)) {
  #   
  #   writeLines("Stopping reading the summary table, as critical warning has been raised earlier on")
  #   writeLines("Returning empty SoF table")
  #   output <- create_empty_df(names_vec = c(get_summarytab_colnames(), get_summarytab_metadata_colnames()))
  #   
  #   output$warning <- str_c(warning_df$type, collapse = " | ")
  #                             
  #   return(output)
  #   
  # }
  
  
  # run only if at least one such tables exists:
  
  
  nr_summaryOfFindingsTable <- get_nr_of_summary_tables(page_content)
  
  
  # if no summary table exists, report it, and stop:
  if ((table_number > nr_summaryOfFindingsTable) || 
      (nr_summaryOfFindingsTable == 0)) {
    
    # output <- 
    #   create_empty_df(names_vec = get_summarytab_colnames())
    flog.warn("No SoF table detected.")
    raise_warning(type = "No SoF table detected",
                              critical = FALSE)
    output <- create_empty_df(names_vec = get_summarytab_colnames())
    print("As no summary findings tables were detected, I'm stopping the collection of summary tables.")

  } else { # otherwise, go on:
    
    
    
    SoF_table_metadata <- get_summary_table_metadata(page_content = page_content,
                                                     table_number = table_number,
                                                     verbose = verbose)
    
    flog.trace("Parse soF from html.")
    # get raw summary of findings table:
    summaryOfFindingsTable <- 
      page_content %>% 
      html_nodes(".summaryOfFindings") %>% 
      html_nodes("table") %>% 
      .[[table_number]] %>% 
      html_table(fill = TRUE)
    
    

    flog.trace("Find outcome columns.")
    # find column in which the outcome variables are mentioned:
    col_Outcomes <- 
      summaryOfFindingsTable %>% 
      map(~ str_detect(., pattern = "^Outcome[s]*|Outcome[s]$")) %>% 
      map_lgl(~ any(. == TRUE)) %>% 
      which()
    
    # take first columns if there are more than one relevant column:
    if (length(col_Outcomes) > 1) {
      flog.warn("Too many columns at col_Outcomes.")
     raise_warning(type = "too many columns at `col_Outcomes`",
                                critical = FALSE)
      
      col_Outcomes <- col_Outcomes[1]
      
      writeLines(glue::glue("Too many columns at `col_Outcomes`. Taking the first one. Might by wrong!\n"))
      
      #return(output)
    }
    
    if (length(col_Outcomes) == 0) {
      flog.warn("No columns at col_Outcomes.")
      raise_warning(type = "no columns at `col_Outcomes`",
                                critical = FALSE)
      
      col_Outcomes <- "NO_COL_OUTCOME"
      
    }
    
    names(summaryOfFindingsTable)[col_Outcomes] <- "Outcomes"
    
    
    flog.trace("Add id column to sof extractions.")
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
    flog.trace("Identify header rows from SoF table.")
    summaryOfFindingsTable <-
      summaryOfFindingsTable %>% 
      mutate(header_row = row_number() <= header_rows) 
    
    # find column with GRADE rating:
    # "Quality of the evidence (GRADE)"
    flog.trace("Find GRADE column.")
    col_GRADE <-
      summaryOfFindingsTable %>% 
      filter(header_row == TRUE) %>% 
      map(~ str_detect(., pattern = "Quality of the evidence (GRADE)$|(GRADE)")) %>% 
      map_lgl(~ any(. == TRUE)) %>% 
      which() %>% 
      names()
    
    if (verbose) writeLines("Col_GRADE found.")
    
    

    # delete footer:
    identical_cells_across_cols <- 
      map2_lgl(.x = summaryOfFindingsTable$Outcomes,
               .y = summaryOfFindingsTable$X2,
               .f = identical)
    
    flog.trace("Delete footer rows from SoF table.")
    # delete rows with constant values:
    summaryOfFindingsTable2 <-
      summaryOfFindingsTable %>% 
      filter(!identical_cells_across_cols)  
    
    # delete unneeded header rows:
    flog.trace("Delete unneeded header rows.")
    summaryOfFindingsTable3 <-
      summaryOfFindingsTable2 %>%
      group_by(header_row) %>% 
      mutate(header_row_nr = row_number()) %>% 
      ungroup() %>% 
      filter(header_row == FALSE | header_row_nr == 1) %>% 
      select(-header_row_nr)
    
    
      #filter(id_measure <= header_rows)
    
    
    
    
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
      flog.debug("More than one GRADE column was found. Taking the first one.")
     raise_warning(type = "Multiple cols found at `col_GRADES`. Taking the first one. Might be wrong!")
      
      col_GRADE <- col_grades[1]
    }
    }
    
    
    
    # rename:
    summaryOfFindingsTable4 <-
      summaryOfFindingsTable3 %>% 
      rename(GRADE := {col_GRADE}) 
    
    
    # find the effect statistic used:
    flog.trace("Searching the effect statistic.")
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
    
    
    flog.trace("Identifying SoF column with number of studies and participants.")
    col_participants_studies <- 
      summaryOfFindingsTable3 %>%  # as defined above
      filter(header_row == TRUE) %>% 
      #filter(!identical_cells_across_cols) %>%   
      map( ~ str_detect(., pattern = n_of_trials_string2)) %>% 
      map_lgl( ~ any(. == TRUE)) %>% 
      keep(isTRUE) %>% 
      names()
    
    
    
    # take first columns if there are more than one relevant column:
    if (length(col_participants_studies) > 1) {
      flog.warn("Too many columns at `col_participants_studies`. Taking the first one.")
     raise_warning(type = "too many columns at `col_participants_studies`",
                                critical = FALSE)
      
      # output <- create_empty_df(names_vec = get_all_colnames())
      # output$doi <- review_url
      # output$warning <- warning_df$type
      # 
      col_participants_studies <- col_participants_studies[1]
      
      writeLines(glue::glue("Too many columns at `col_participants_studies`. Taking the first one. Might by wrong!\n"))
      
    }
    
    if (length(col_participants_studies) == 0) {
      flog.warn("No columns found at `col_participants_studies`")
      raise_warning(type = "no columns found at `col_participants_studies`",
                                critical = FALSE)
      
      summaryOfFindingsTable6 <-
        summaryOfFindingsTable5 %>% 
        mutate(n_participants_studies = NA)  
    } else {  # if everything's ok, proceed:
      
      summaryOfFindingsTable6 <-
        summaryOfFindingsTable5 %>% 
        rename(n_participants_studies := {col_participants_studies}) 
    }
    
    flog.trace("Separating number of studies from number of participants in SoF table.")
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
    
    
    flog.trace("Identifying relative effect confidence interval.")
    col_rel_eff_CI <- 
      summaryOfFindingsTable %>% 
      filter(header_row == TRUE) %>% 
      map( ~ str_detect(., pattern = rel_eff_CI_str)) %>% 
      map_lgl( ~ any(. == TRUE)) %>% 
      keep(isTRUE) %>% 
      names()
    
    
    if (length(col_rel_eff_CI) > 1) {
      flog.warn("too many columns at `col_rel_eff_CI`")
     raise_warning(type = "too many columns at `col_rel_eff_CI`",
                                critical = FALSE)
      
      # output <- create_empty_df(names_vec = get_all_colnames())
      # output$doi <- review_url
      # output$warning <- warning_df$type
      # 
      col_rel_eff_CI <- col_rel_eff_CI[1]
      
      writeLines(glue::glue("Too many columns at `col_rel_eff_CI`. Taking the first one. Might by wrong!\n"))
      
    }
    
    
    if (length(col_rel_eff_CI) == 0)  {
      flog.infor("No relative effects identified.")
      col_rel_eff_CI <- "NO_RELATIV_EFFECTS_REPORTED"
      summaryOfFindingsTable6 <- 
        summaryOfFindingsTable6 %>% 
        mutate(NO_RELATIV_EFFECTS_REPORTED = NA)
    }
    
    flog.trace("Identifying relative effects, part 2.")
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
    
    flog.trace("Check if there is a comemnts column.")
    col_comments <-
      summaryOfFindingsTable %>%
      filter(header_row == TRUE) %>% 
      map(~ str_detect(., pattern = col_comments)) %>%
      map_lgl( ~ any(. == TRUE)) %>%
      keep(isTRUE) %>%
      names()
    
    if (length(col_comments) > 1) {
      flog.warn("too many columns at `col_comments`")
     raise_warning(type = "too many columns at `col_comments`",
                                critical = FALSE)
      
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
    
    flog.trace("Remove header rows from the actual data rows.")
    # remove add rows that are headers actually:
    summaryOfFindingsTable7a <- 
      summaryOfFindingsTable7 %>% 
      filter(Outcomes != "Outcomes")
    
    
    
    flog.trace("Renumber id column.")
    #re-number id column:
    summaryOfFindingsTable7b <- 
      summaryOfFindingsTable7a %>% 
      mutate(id_measure = row_number()) %>% 
      select(id_measure, everything(), -header_row)
    
    
    flog.trace("Add metadata.")
    # add SoF table metadata:
    summaryOfFindingsTable8 <-
      summaryOfFindingsTable7b %>% 
      bind_cols(SoF_table_metadata)
    
    
    # xxx
    # 
    # define `not-%in%`:
    `%nin%` <- Negate(`%in%`)
    
    
    flog.trace("Compute statistical significance.")
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
      flog.info("Only first output is kept.")
    }
    
    if (drop_unused_cols) {
      flog.info("Unused columns were dropped.")
      output <- 
        output %>% 
        select(-starts_with("X"))
    }
    
    
    
  }
  
  if (verbose) writeLines(paste0("SoF data dimensions: ",  dim(output)))
  flog.info(paste0("SoF data dimensions: ",  dim(output)))
  writeLines("\n")
  if (verbose) writeLines("Finished parsing SoF Table.\n")
  
  return(output)
  flog.info("Finished parsing the SoF table.")
  
  }





