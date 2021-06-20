init <- function() {  
  

  flog.appender(appender.file('parse-dois.log'))
  flog.info("Starting init.")
  flog.trace("libs loaded.")
  
  flog.appender(appender.file("funs-called.log"), 
                name = "funlog")
  flog.info("init", name = "funlog")
  
  
  flog.trace("Reading config.caml")
  stopifnot(file.exists("config.yaml"))
  flog.info("Object 'config' defined as global variable")
  config <<- read_yaml("config.yaml")
  flog.threshold(config$flog_threshold)
  if (config$rm_old_logfile) {
    system("rm parse-dois.log")
    flog.info("Removing existing log file. Starting fresh log file.")
  }
  
  flog.trace("Reading dois file.")
  dois_list <- readxl::read_xlsx(config$dois_file)
  
  if (!dir.exists(config$output_path)) {
    flog.trace("Creating directory as to output_path")
    dir.create(config$output_path)
  }
  
  
  flog.trace("Filtering dois for selected reviewer.")
  dois_of_selected_reviewer <- 
    dois_list %>% 
    dplyr::filter(tolower(reviewer) == config$reviewer) %>% 
    dplyr::slice(config$start_at_doi:config$end_at_doi) %>% 
    dplyr::pull(url)
  
  flog.trace(paste0("Selected reviewer is: ",config$reviewer))
  flog.trace(paste0("First doi to be extracted is: ", 
                    dois_of_selected_reviewer[[1]]))
  flog.trace(paste0("Number of dois to be extracted is: ", 
                    length(dois_of_selected_reviewer)))
  return(dois_of_selected_reviewer)
  
  
}
