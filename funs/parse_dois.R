parse_dois <- function(dois_of_selected_reviewer) {

  flog.info("Starting extracting reviews.")
  flog.info("parse_dois", name = "funlog")
  
  extracted_reviews <- 
    dois_of_selected_reviewer %>% 
    map_dfr(parse_review,  # that's the main function of the whole shebang!!!
            .id = "id_review")
  
  output_path <- paste0(config$output_path,"/",config$reviewer)
  
  flog.info(paste0("Output path: ", output_path))
  
  output_filename <- paste0(output_path,
                            "/", config$machine_extractions_file,
                            "_",config$reviewer)

  # at times there can be two (or even more) underscores, but only 1 is needed:
  output_filename <- str_replace_all(output_filename, "_{2,}", "_")
  
  flog.info(paste0("Output_filename: ", output_filename))
  
  flog.trace(paste0("(Relative) Output path/file name is: ", output_filename))
  
  if (!dir.exists(output_path)) {
    flog.info(paste0("Creating directory as to output_path and reviewer: ", output_path))
    dir.create(output_path)
    if (verbose) print(paste0("Creating directory as to output_path and reviewer: ",
                              output_path)) 
  }
  
  flog.trace("Writing xlsx filewith extracted reviews.")
  writexl::write_xlsx(extracted_reviews, 
                      path = paste0(output_filename,".xlsx"))
  
  flog.trace("Writing csv file with extracted reviews.")
  
  write_csv(extracted_reviews,
            file = paste0(output_filename, ".csv"))
  
  return(extracted_reviews)
  
}



