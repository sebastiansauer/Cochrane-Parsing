

get_summary_table_metadata <- function(page_content, 
                                       table_number = 1,
                                       verbose = TRUE) {
  
  
  flog.info("get_summary_table_metadata", name = "funlog")
  if (verbose) writeLines("Start parsing SoF Table for metadata.\n")
  flog.info("Start parsing SoF Table for metadata.\n")
  # run only if at least one such tables exists:
  nr_summaryOfFindingsTable <- get_nr_of_summary_tables(page_content, 
                                                        verbose = TRUE)
  
  
  
  
  paste0("Number of summary tables detected: ", nr_summaryOfFindingsTable, "\n")
  flog.info(paste0("Number of summary tables detected: ", nr_summaryOfFindingsTable))
  
  # stop if strange stuff happens:
  if (table_number > nr_summaryOfFindingsTable) {
    
    print("This table does not exist! Aborting.\n")
    flog.eror("This table does not exist! Aborting")
    output <- 
      tibble(metadata = NA)
    
    return(output)
    
  }
  
  
  
  # otherwise, start normal work:
  # get raw summary of findings table:
  flog.trace("Read SoF Table for metadata extraction.")
  summaryOfFindingsTable <- 
    page_content %>% 
    html_nodes(".summaryOfFindings") %>% 
    html_nodes("table") %>% 
    .[[table_number]] %>% 
    html_table(fill = TRUE)
  
  
  flog.trace("Get main comparison of review.")
  main_comparison_of_review <-
    summaryOfFindingsTable %>% 
    select(1) %>% 
    filter(row_number() == 1) %>% 
    pull()
  
  
  flog.trace("Get main_comparison_population.")
  main_comparison_population <- 
    summaryOfFindingsTable %>% 
    select(1) %>% 
    slice(2) %>% 
    pull() %>% 
    str_extract("Patient or population.+Setting") %>% 
    str_remove_all(": |Patient or population|Setting")
  
  
  flog.trace("Get main_comparison_setting.")
  main_comparison_setting <-
    summaryOfFindingsTable %>% 
    select(1) %>% 
    slice(2) %>% 
    pull() %>% 
    str_extract("Setting.+Intervention") %>% 
    str_remove_all(": |Setting|Intervention")
  
  
  flog.trace("Get main_comparison_comparison_type.")
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
  
  print("SoF metadata dim:\n")
  print(dim(output))
  print("Finished parsing SoF metadata.\n")
  flog.info("Finished parsing SoF metadata.")
  
  return(output)
  
}

