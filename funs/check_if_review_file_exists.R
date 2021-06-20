
check_if_review_file_exists <- function(review_url,
                                        file_type = config$file_type) {
  
  output_dir = config$output
  verbose = config$verbose
 
  #flog.info(current_fun_name, name = "funlog")
  flog.info("Checking if review file already exists in output directory.")
  
  if (length(output_dir) == 0) stop("Please specify output directory.")
  
  
  review_url_sanitized <- sanitize_review_url(review_url)
  
  
  # use doi to check whether output file exists
  # BUG!!!
  file_path <- glue::glue("{output_dir}/{review_url_sanitized}.{file_type}") 
  
  file_path <- file_path[1]
  
  writeLines(glue::glue("Checking if output file already exists: {file_path}\n"))
  
  
  
  # check if output file exists:
  output_file_exists <- file.exists(file_path)
  
  if (verbose & output_file_exists) writeLines(glue::glue("Output file exists: {file_path}\n"))
  
  if (verbose & !output_file_exists) writeLines(glue::glue("Output file does NOT exist: {file_path}\n"))
  
  
  flog.info("Having finalized checking for output file.")
  return(output_file_exists)
  
}

