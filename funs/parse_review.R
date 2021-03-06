
parse_review <- function(review_url) {
  
  
  #config <- read_yaml("config.yaml")
  
  verbose = config$verbose
  output_dir_to_write = paste0(config$output_path,"/",config$reviewer)
  reviewer  = config$reviewer
  sound = config$sound
  overwrite_file = config$overwrite
  

  flog.info(paste0("Starting with review url: ", review_url))
  flog.info("parse_review", name = "funlog")
  
  # Report the number of the review currently processed
  # relative to the list of dois provided
  
  if (!exists("dois_to_be_parsed")) dois_to_be_parsed <- init_dois_to_be_extracted()
  
  pos_current_review_in_dois_list <- which(review_url == dois_to_be_parsed)
  
  writeLines(paste0("____Review number____", 
                    pos_current_review_in_dois_list,
                    "____ of ", length(dois_to_be_parsed),"."))
  flog.info(paste0("____Currently processing review at position ", pos_current_review_in_dois_list,
                   " of ", length(dois_to_be_parsed), "____"))
  
  
  #flog.info(paste0("Starting with review number: ", count_reviews))
  #if (verbose) writeLines(glue::glue("______Now starting with review number ((( {count_reviews} )))______\n"))
  if (verbose) writeLines(glue::glue("______Now review with url ((( {review_url} )))______\n"))
  
  
  review_url_cochrane <- build_cochrane_url_from_doi(review_url)
  
  #be polite:
  #bow_result <- bow(url = review_url_cochrane,
  #                  user_agent = "Sebastian Sauer - sebastiansauer1@gmail.com")
  # not yet fully implemented!
  
  # parse all parts
  review_parsed_parts <- parse_review_parts(review_url_cochrane)
  
  # add warnings:
  review_parsed_parts <-  
    review_parsed_parts %>% 
    mutate(warnings = str_c(warning_df$type, collapse = " | "))
  
  
  write_parsed_review_to_csv_file(review_url = review_url_cochrane,
                                  review = review_parsed_parts)
  
  
  

  flog.info("Review has been parsed.")
  writeLines("Finalizing. Warnings:\n")
  print(warning_df)
  
  return(review_parsed_parts)
  
  if (sound) system("say Ich habe fertig!")
  
}

