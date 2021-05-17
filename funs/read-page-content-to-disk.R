


# read html page to RDS file ----------------------------------------------




read_page_content_to_disk <- function(review_url, 
                                      output_dir = "output",
                                      overwrite = FALSE,
                                      verbose = TRUE){
  
  # this function reads the thml of a review (and its info page) and
  # stores the resulting html structure to disk.
  # The use of this function is to save time: Accessing a web page takes time but not error-prone.
  # Therefore, it's useful to separate the mere reading from the (bug-prone) processing of the content.
  
  
  output <- list()
  
  
  writeLines("Reading html of review page.\n")
  
  # read html page, must be sanitized! (see function for that):
  safely_read_html <- safely(read_html)
  safe_page_content <- safely_read_html(review_url,
                                        verbose = verbose)  
  
  url <- url(review_url, "rb")
  close(url)
  
  if (!is.null(safe_page_content$error)) {
    
   raise_warning(type = "Could not read review html file",
                      critical = TRUE)
    
    
  }
  
  
  output[["review_page"]] <- safe_page_content
  
  
  
  writeLines("Reading html of info review page.\n")
  
  # read html page, must be sanitized! (see function for that):
  # get infopage page content:
  infopage_url <- glue::glue("{review_url}/information") %>%
    str_remove("/full")
  
  # parse info page, must be sanitized! (see function for that):
  safely_page_content_info_page <- safely_read_html(infopage_url,
                                                    verbose = verbose)
  
  url <- url(infopage_url, "rb")
  close(url)
  
  if (!is.null(safely_page_content_info_page$error)) {
    
    warning_df <<-
      warning_df %>% 
      bind_rows(raise_warning(type = "Could not read review html info page",
                              critical = TRUE))
  }
  
  output[["info_page"]] <- safely_page_content_info_page
  
  
  output[["output_dir"]] <- output_dir
  
  
  # writing to disk:
  writeLines("Now writing page content file to disk.\n")
  
  output_file_exists <- check_if_review_file_exists(review_url,
                                                    file_type = "rds")
  
  # check if we should overwrite it, otherwise stop (if output  file already exists):
  if (output_file_exists & !overwrite) {
    writeLines(glue::glue("Output file exists. NOT overwriting.\n"))
  } else {
    
    file_path <- glue::glue("{output_dir}/{review_url}.rdata") %>% 
      sanitize_review_url() 
    
    file_path <- file_path[1]
    
    write_rds(x = output, 
              file = file_path)
    writeLines(glue::glue("Results have been saved to file: {file_path}\n"))
  }
  
  
  
}


