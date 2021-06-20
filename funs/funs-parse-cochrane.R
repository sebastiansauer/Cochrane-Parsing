

write_parsed_review_to_csv_file <- function(review_url,
                                        review){
  
  
  reviewer = config$reviewer
  output_dir = config$output_path
  overwrite = config$overwrite
  
  
  
  writeLines("Now writing file to disk.\n")
  flog.info("Now writing file to disk.\n")
  
  if (reviewer != "?") output_dir <- glue("output/{reviewer}")
  
  
  output_file_exists <- check_if_review_file_exists(review_url)
  
  # check if we should overwrite it, otherwise stop (if output  file already exists):
  if (output_file_exists & !overwrite) {
    writeLines(glue::glue("Output file exists. NOT overwriting.\n"))
  } else {
    
    file_path <- glue::glue("{output_dir}/{review_url}.csv") %>% 
      sanitize_review_url() 
    
    file_path <- file_path[1]
    
    write_csv(x = review, 
              file = file_path)
    flog.info(glue::glue("Results have been saved to file: {file_path}\n"))
    writeLines(glue::glue("Results have been saved to file: {file_path}\n"))
  }
  
  return(review)
  
}



