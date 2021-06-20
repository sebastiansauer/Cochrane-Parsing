

get_nr_of_summary_tables <- function(my_page_content, 
                                     table_number = NULL, # which table should be extracted?
                                     verbose = TRUE) # should an empty table be returned? if so, what's the name of the only column?
{
  
  
  
  flog.info("Starting counting the number of SoF Tables.")
  flog.info("get_nr_of_summary_tables", name = "funlog")
  
  # check how many tables exist:
  nr_Table <- 
    my_page_content %>% 
    html_nodes("table") %>% 
    length()
  
  
  flog.trace("Check if if a section 'summary of results' exist.")
  # check if a section "summary of results" exist
  summary_sections_exists <- 
    my_page_content %>% 
    html_nodes(".section-collapse-title") %>% 
    html_text() %>% 
    str_trim() %>% 
    str_detect("Summary of findings") %>% 
    any()
  
  
  flog.trace("Get number of summary tables.")
  nr_summary_tables <- 
    my_page_content %>% 
    html_nodes(".summaryOfFindings") %>% 
    html_nodes(".table") %>% 
    length()
  
  # alternative way, likely less precise:
  nr_summary_tables2 <- 
    my_page_content %>% 
    html_nodes("table") %>% 
    html_nodes(".table-label") %>% 
    html_text() %>% 
    str_detect("Summary of findings") %>% sum()
  
  # warn if there are no summary tables:
  if (summary_sections_exists == FALSE) {
    writeLines("No (zero) summary sections detected!")
    flog.warn("No (zero) summary sections detected!")
    
    raise_warning(type = "No (zero) summary sections detected",
                  critical = FALSE)
  }
  else {
    if (verbose) writeLines(paste0("Number of summary tables detected: ", nr_summary_tables))
    flog.info(paste0("Number of summary tables detected: ", nr_summary_tables))
  }
  
  # warn if user queries for a table number that does not exist:
  if (!is.null(table_number)) {
    if (table_number > nr_summaryOfFindingsTable) {
      
      writeLines(paste("This table does not exist! Aborting.\n"))
      flog.error("This table does not exist! Aborting.")
    }
    
  }
  
  if (summary_sections_exists == FALSE & nr_summary_tables != 0)
    stop("summary_sections_exists == FALSE & nr_summary_tables != 0")
  
  
  return(nr_summary_tables)
}













