
parse_review_parts <- function(
  review_url_cochrane, ...) {
  
  flog.info("parse_review_parts", name = "funlog")
  flog.info("Starting __`parse_review_parts`__")
 
  
  reviewer = config$reviewer
  overwrite = config$overwrite
  # final_table = TRUE,  # should the results be converted from list to df?
  verbose = config$verbose
  
 
  # check if output CSV file exists, and if it should not be overwritten, skip the parsing:
  if (reviewer != "?") {
    output_dir <- glue("output/{config$reviewer}")
  } else {output_dir <- glue("output")}
  
  flog.trace(paste0("Output dir is: ", output_dir))
  
  output_file_exists <- check_if_review_file_exists(review_url_cochrane)
  if (output_file_exists & !overwrite) {
    
    output <- create_empty_df(names_vec = get_all_colnames())
    output$doi <- review_url_cochrane
    output$warnings <- "Output file already exists"
    flog.warn("Output file already exists. Overwrite=FALSE")
    
    writeLines(glue::glue("Output file already exists. Skipping."))
    return(output)
  }  # enf of fun
  

  
  # main part: parsing individual parts of the review

  safely_parse_individual_parts <- safely(parse_individual_parts)
  safe_output <- 
    safely_parse_individual_parts(
      review_url_cochrane
      ) 
  
  
  
  if (!is.null(safe_output$error)) { 
    if (verbose) {
      print("Error on 'safe_output':")
      print(safe_output$error)
    }
    flog.error(paste0("`safely_parse_individual_parts` yielded this error: ",
                      safe_output$error))
    
    output <- create_empty_df(names_vec = get_all_colnames())
    output$doi <- review_url_cochrane
    
    raise_warning(type = "parse_parts failed!")
    flog.error("Parse parts failed! Position: `safe_output$error`")
    
  } else {  # if no error:
    
    output <- safe_output$result


  flog.trace("Elimine duplicate rows.")
  output <- 
  output %>% 
    relocate(doi, everything()) %>% 
    select(-id_measure) %>% 
    distinct() %>% 
    mutate(id_measure = row_number()) 
 
  }
  
      if (verbose) {
        print(output)
        writeLines("\n")
        writeLines(paste0("Review has been parsed.\n"))
      }
    
  flog.info("__Review has been parsed.__")
  
  return(output)

}


