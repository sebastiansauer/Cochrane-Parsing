init_dois_to_be_extracted <- function(){
  
  
  flog.trace("Reading dois file.")
  flog.info("init_dois_to_be_extracted", name = "funlog")
  

  flog.trace("Reading `config.yaml`.")
  config <- read_yaml("config.yaml")
  
  verbose <- config$verbose
  if (verbose) print("`config.yaml` has been red.")
  
  if (verbose) print("Reading dois file.")
  
  dois_list <- readxl::read_xlsx(config$dois_file)
  
  reviewer_list <-
    dois_list %>% 
    count(reviewer) %>% 
    pull(reviewer) %>% 
    tolower()
  
  flog.trace(paste0("This reviewer list was found in `dois_file: ", reviewer_list))
  
  if (!(tolower(config$reviewer) %in% reviewer_list)) {
    stop("Reviewer of config file not find in dois data file.")
    flog.error("Reviewer of config file not find in dois data file.")
  }
    
  
  if (!dir.exists(config$output_path)) {
    flog.trace("Creating directory as to output_path.")
    dir.create(config$output_path)
    if (verbose) print("Creating directory as to output_path.")    
  }
  
  if (verbose) writeLines(paste0("Selected reviewer is: ",config$reviewer))
  flog.trace(paste0("Selected reviewer is: ", config$reviewer))
  flog.trace("Filtering dois for selected reviewer.")
  if (verbose) print("Filtering dois for selected reviewer.")    
 
  dois_list_selected_reviewer <- 
    dois_list %>% 
    dplyr::filter(tolower(reviewer) == config$reviewer)
  

  
  if (is.null(config$start_at_doi)) config$start_at_doi <- 1
  if (is.null(config$end_at_doi)) config$end_at_doi <- nrow(dois_list_selected_reviewer)
  
   dois_list_selected_reviewer <- 
      dois_list_selected_reviewer %>% 
      dplyr::slice(config$start_at_doi:config$end_at_doi)
 
  dois_of_selected_reviewer <-   
    dois_list_selected_reviewer %>% 
    dplyr::pull(url)

  flog.trace(paste0("Number of dois to be extracted is: ", 
                    length(dois_of_selected_reviewer)))
  
  flog.trace(paste0("First doi to be extracted is: ", 
                    dois_of_selected_reviewer[[1]]))

  if (verbose) writeLines(paste0("First doi to be extracted is: ", 
                                 dois_of_selected_reviewer[[1]]))
  if (verbose) writeLines(paste0("Number of dois to be extracted is: ", 
                                 length(dois_of_selected_reviewer)))
  
  
  
  return(dois_of_selected_reviewer)
  
}