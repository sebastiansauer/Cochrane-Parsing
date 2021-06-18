
parse_dois <- function() {
  
  library(tidyverse)  # data wrangling
  library(rvest)  # web scraping
  library(xml2)  # web scraping
  library(stringr)  # string manipulation
  #library(printr)  # print dfs as tables
  library(glue)  # glueing
  library(rcrossref)  # citation count
  #library(conflicted)
  library(glue)
  #library(polite)
  library(tidytext)
  library(readxl)
  #library(magrittr)
  library(yaml)
  library(futile.logger)
  
  flog.appender(appender.file('parse-dois.log'))
  flog.debug("libs loaded.")
  flog.threshold(DEBUG)
  
  flog.debug("Sourcing funs.")
  source("funs/helper-funs.R")
  source("funs/funs-parse-cochrane.R")
  source("funs/get_summary_table.R")
  source("funs/parse-review-parts.R")
  source("funs/parse_review.R")
  
  flog.debug("Funs sourced.")
  
  
  flog.debug("Reading config.caml")
  stopifnot(file.exists("config.yaml"))
  config <- read_yaml("config.yaml")
  
  flog.debug("Reading dois file.")
  dois_list <- readxl::read_xlsx(config$dois_file)
  
  if (!dir.exists(config$output_path)) {
    flog.debug("Creating directory as to output_path")
    dir.create(config$output_path)
  }
  
  
  flog.debug("Filtering dois for selected reviewer.")
  dois_of_selected_reviewer <- 
    dois_list %>% 
    dplyr::filter(tolower(reviewer) == config$reviewer) %>% 
    dplyr::slice(config$start_at_doi:config$end_at_doi) %>% 
    dplyr::pull(url)
  
  
  count_reviews <- 1
  
  flog.debug("Extracting reviews.")
  extracted_reviews <- 
    dois_of_selected_reviewer %>% 
    map_dfr(parse_review, 
            .id = "id_review")
  
  
  output_filename <- paste0(config$output_path,"/",config$reviewer,
                            "/", config$machine_extractions_file,
                            "_",config$reviewer)
  
  flog.debug("Writing xlsx filewith extracted reviews.")
  writexl::write_xlsx(extracted_reviews, 
                      path = paste0(output_filename,".xlsx"))
  
  flog.debug("Writing csv file with extracted reviews.")
  
  write_csv(extracted_reviews,
            file = paste0(output_filename, ".csv"))
  
  return(extracted_reviews)
  
}



