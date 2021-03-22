

# parse parts ------------------------------------------------------------





parse_review_parts <- function(
  review_url,
  reviewer = "?",
  overwrite = TRUE,
  final_table = TRUE,  # should the results be converted from list to df?
  verbose = TRUE, ...) {
  
  

  # check if output CSV file exists, and if it should not be overwritten, skip the parsing:
  if (reviewer != "?") output_dir <- glue("output/{reviewer}/{review_url}")
  output_file_exists <- check_if_review_file_exists(review_url,
                                                    output_dir = output_dir)
  if (output_file_exists & !overwrite) {
    
    output <- create_empty_df(names_vec = get_all_colnames())
    output$doi <- review_url
    output$warnings <- "Output file already exists"
    
    writeLines(glue::glue("Output file already exists. Skipping."))
    
    return(output)
    
  }
  
  
  # else, start normal work:
  init_new_review()
  
  # initialize emptye output df:
  output <- create_empty_df(names_vec = get_all_colnames())
  
  if (verbose) cat(paste0("**Starting to parse the review with this doi: ", review_url, "**\n"))
  
  # if review should not be taken vom from rds file, but read from html page:
  # read html page, must be sanitized! (see function for that):
  safe_page_content <- safely_read_html(review_url)  
  
  
  
  # on error, stop:
  if (!is.null(safe_page_content$error)) {
    
    warning_df <<-
      warning_df %>% 
      bind_rows(
        raise_warning(type = safe_page_content$error$message,
                      critical = TRUE))
    
    output <- create_empty_df(names_vec = get_all_colnames())
    #output$warnings <- safe_page_content$error$message
    output$doi <- review_url
    output$warnings <- str_c(warning_df$type, collapse =  " - ")
    
    writeLines("Error 404 on reading full review page. Stopping this review.\n")
    
    return(output)
    
    
  } else {  # else parse regularly:
    
    # close url:
    # if (verbose) writeLines("Closing Url.\n")
    # url <- url(review_url, "rb")
    # close(url)
    
    page_content <- safe_page_content$result
    
    # parse info page, must be sanitized! (see function for that):
    info_page <- get_review_info_page(review_url)
    
    
 
    
    # read metadata:
    metadata <- get_review_metadata(page_content,
                                    reviewer = reviewer)
    
    # read abstract:
    abstract <- get_abstract(page_content)
    
    # check if there' a critical warning, in which case we stop parsing:
    if (any(warning_df$critical == TRUE)) {
      
      
      writeLines(glue::glue("STOPPING parsing. Critical warning has been raised: {str_c(warning_df$type, collapse = ' | ')}"))
      
      final_table <- concat_tables(
        info_page = info_page,
        page_content = page_content,
        summarytable = NA,
        metadata_review =  metadata,
        abstract_review = abstract
        #metadata_summaryTable = NA
      )
      
      output <- review$final_table
      
      return(output)
      
    } else {
      # continue regular parsing:
      
      summaryTable_count <- get_nr_of_summary_tables(page_content)
      
      # possibly_get_summary_table <- possibly(get_summary_table,
      #                                        otherwise = stop_parsing_return_empty_df(
      #                                          review_url = review_url,
      #                                          error_message = "Error in `get_summary_table`"
      #                                        ))
      
      summarytable1 <- get_summary_table(page_content)
      
      #review$summarytable1 <- possibly_get_summary_table(review$page_content)
      #review$summaryTable_metadata <- get_summary_table_metadata(review$page_content)
      
      
      #undebug(concat_tables)
      final_table <- concat_tables(
        info_page = info_page,
        page_content = page_content,
        summarytable = summarytable1,
        metadata_review =  metadata,
        abstract_review = abstract
        #metadata_summaryTable = review$summaryTable_metadata
      )
      
      
      output <- final_table
      
      if (verbose) {
        print(output)
        writeLines("\n")
        writeLines(paste0("Review has been parsed.\n"))
      }
    }
  }
  
  return(output)
  
}


