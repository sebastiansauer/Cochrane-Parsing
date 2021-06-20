
parse_dois <- function(dois_of_selected_reviewer) {
  

  flog.info("Starting extracting reviews.")
  extracted_reviews <- 
    dois_of_selected_reviewer %>% 
    map_dfr(parse_review, 
            .id = "id_review")
  
  
  output_filename <- paste0(config$output_path,"/",config$reviewer,
                            "/", config$machine_extractions_file,
                            "_",config$reviewer)
  
  flog.trace("Writing xlsx filewith extracted reviews.")
  writexl::write_xlsx(extracted_reviews, 
                      path = paste0(output_filename,".xlsx"))
  
  flog.trace("Writing csv file with extracted reviews.")
  
  write_csv(extracted_reviews,
            file = paste0(output_filename, ".csv"))
  
  return(extracted_reviews)
  
}



