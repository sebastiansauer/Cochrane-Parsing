

# parse parts ------------------------------------------------------------





parse_review_parts <- function(
  review_url,
  reviewer = "?",
  overwrite = TRUE,
  final_table = TRUE,  # should the results be converted from list to df?
  verbose = TRUE, ...) {
  
  
  
  parse_individual_parts <- function(review_url) {
    
    
    # else, start normal work:
    init_new_review()
    
    
    
    
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
      
      output <- 
        info_page
      
      # read metadata:
      
      metadata <- get_review_metadata(page_content,
                                      reviewer = reviewer)
      
      output <-
        output %>% 
        bind_cols(metadata)
      
      # read abstract:
      abstract <- get_abstract(page_content)
      
      output <-
        output %>% 
        bind_cols(abstract)
      
      # check if there' a critical warning, in which case we stop parsing:
      # if (any(warning_df$critical == TRUE)) {
      #   
      #   
      #   writeLines(glue::glue("STOPPING parsing. Critical warning has been raised: {str_c(warning_df$type, collapse = ' | ')}"))
      #   
      #   final_table <- concat_tables(
      #     info_page = info_page,
      #     page_content = page_content,
      #     summarytable = create_empty_df(names_vec = get_summarytab_colnames()),
      #     metadata_review =  metadata,
      #     abstract_review = abstract
      #     #metadata_summaryTable = NA
      #   )
      #   
      #   output <- final_table
      #   
      #   return(output)
      #   
      #   
      # } 
      # continue regular parsing:
      
      safely_get_nr_of_summary <- safely(get_nr_of_summary_tables)
      safe_summaryTable_count <- safely_get_nr_of_summary(page_content)
      
      # on error:
      if (!is.null(safe_summaryTable_count$error)) {
        
        summaryTable_count <- 0
        summarytable <- create_empty_df(names_vec = get_summarytab_colnames())
        
        warning_df <<-
          warning_df  %>% 
          bind_cols(raise_warning(type = "Zero summary tables detected,
                                  critical = FALSE"))
        
        return(output)
        
        
      }  # no error in get_nr_of_summary, then read summary table:
      
      
      summaryTable_count <- safe_summaryTable_count$result
      #safe_get_summary_table <- safely(get_summary_table)
      #safe_summarytable1 <- safe_get_summary_table(page_content)
      
      summarytable <- 
        1:summaryTable_count %>% 
        map_dfr(~ get_summary_table(page_content = page_content,
                                     table_number = .,),
                                    .id = "SoF_table_number")
      
      
      
      
      # on error:
      #if (!is.null(summarytable$error)) {
      #   summarytable <- create_empty_df(names_vec = get_summarytab_colnames())
      #   
      #   warning_df <<-
      #     warning_df  %>% 
      #     bind_cols(raise_warning(type = "Zero summary tables detected,
      #                             critical = FALSE"))
      #   
      # 
      # } else summarytable <- safe_summarytable1$result
        
        output <- 
          output %>% 
          bind_cols(summarytable)
        
      }
      
      return(output)
      
    }  # end of fun parse_individual_parts
  
  
 
  
  
  
  # initialize emptye output df:
  

  # check if output CSV file exists, and if it should not be overwritten, skip the parsing:
  if (reviewer != "?") {output_dir <- glue("output/{reviewer}")
  } else {output_dir <- glue("output")}
  
  output_file_exists <- check_if_review_file_exists(review_url,
                                                    output_dir = output_dir)
  if (output_file_exists & !overwrite) {
    
    output <- create_empty_df(names_vec = get_all_colnames())
    output$doi <- review_url
    output$warnings <- "Output file already exists"
    
    writeLines(glue::glue("Output file already exists. Skipping."))
    
    return(output)
    
  }
  

  safely_parse_individual_parts <- safely(parse_individual_parts)
  safe_output <- safely_parse_individual_parts(review_url = review_url) 
  
  if (!is.null(safe_output$error)) { 
    
    output <- create_empty_df(names_vec = get_all_colnames())
    warning_df <<-
      warning_df %>% 
      bind_rows(raise_warning(type = "parse_parts failed.",
                              critical = FALSE))
  } else {
    
    output <- safe_output$result
  }


 
  
      # #undebug(concat_tables)
      # final_table <- concat_tables(
      #   info_page = info_page,
      #   page_content = page_content,
      #   summarytable = summarytable1,
      #   metadata_review =  metadata,
      #   abstract_review = abstract
      #   #metadata_summaryTable = review$summaryTable_metadata
      # )
      
      
      # output <- final_table
      
      if (verbose) {
        print(output)
        writeLines("\n")
        writeLines(paste0("Review has been parsed.\n"))
      }
    

  
  return(output)

}


