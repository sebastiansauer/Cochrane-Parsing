



# Helper functions --------------------------------------------------------



create_empty_df <- function(names_vec) {
  
  flog.info("create_empty_df", name = "funlog")
  
  df <- data.frame(matrix(ncol = length(names_vec), nrow = 1)) 
  colnames(df) <- names_vec
  
  return(df)
}



get_review_metadata_colnames <- function() {
  
  flog.info("get_review_metadata_colnames", name = "funlog")
  
  review_metadata_colnames <- c(
    "title",
    "doi",
    "authors",
    "publish_type",
    "is_most_recent_version",
    "url_most_most_version",
    "summaryTable_count",
    "GRADE_somewhere_in_the_text",
    "is_paywalled"
  )
  
  return(review_metadata_colnames)
  
}


get_summarytab_colnames <- function() {

  flog.info("get_summarytab_colnames", name = "funlog")
  
  summarytab1_colnames <- c(
    "id_measure",
    "Outcomes",
    "X2",
    "X3",
    "relative_effect_95CI",
    "n_participants_studies",
    "n_participants",
    "n_studies",
    "GRADE",
    "Comments",
    "effect_statistic",
    "CI_lower",
    "CI_upper",
    "effect_size",
    "is_significant"
  )
  
  return(summarytab1_colnames)
  
}


get_summarytab_metadata_colnames <- function() {
  
  flog.info("get_summarytab_metadata_colnames", name = "funlog")
  
  summarytab_metadata_colnames <- c(
    "main_comparison_of_review",
    "main_comparison_population",
    "main_comparison_setting",
    "main_comparison_comparsion_type"
  )
  
  return(summarytab_metadata_colnames)  
  
}

get_infopage_colnames <- function() {
 
   flog.info("get_infopage_colnames", name = "funlog")

  infopage_colnames <- c(
    "publication_date",
    "review_type",
    "review_group",
    "review_mesh_keywords"
  )
  
  return(infopage_colnames)  
  
}





get_all_colnames <- function(output_file = "automatic",  # if first, take first output file in "output" folder
                             verbose = FALSE, ...) {
  
  flog.info("get_all_colnames", name = "funlog")
  
  
  if (output_file == "automatic") {
    cat("Searching for get-all-columns.csv.\n")
    if (file.exists("output/get-all-columns.csv")) 
      {output_file_df <- read_csv(glue::glue("output/get-all-columns.csv"),
                                 col_types = cols(),
                                 ...) 
      } else print("get-all-columns.csv not found!\n")
  
  # if (output_file == "first") {
  #   first_file_found <- dir("output/", pattern = "^\\d.*csv")[1]
  #   output_file_df <- read_csv(glue::glue("output/{first_file_found}"),
  #                              col_types = cols(),
  #                              ...)
  } else {
    if (!file.exists(output_file)) stop("File not found!")
    output_file_df <- read_csv(output_file, 
                               col_types = cols(),
                               ...)
  }
  
  
  output <- names(output_file_df)
  
  if (verbose) print(output)
  
  return(output)
}







stop_parsing_return_empty_df <- function(review_url,
                                         error_message = "no message",
                                         is_critical = TRUE) {
 
  flog.info("stop_parsing_return_empty_df", name = "funlog")  
  raise_warning(type = error_message,
                critical = is_critical)
  
  output <- create_empty_df(names_vec = get_all_colnames())
  output$doi <- review_url
  
  writeLines(glue::glue("Stop parsing: {warning_df$type}\n"))
  
  return(output)
  
}












