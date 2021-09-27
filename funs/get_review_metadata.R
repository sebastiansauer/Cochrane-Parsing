


get_review_metadata <- function(page_content){
  
  verbose <- config$verbose
  
  if (verbose) writeLines("Start parsing the review meta data\n")
  flog.info("Start parsing the review meta data.")
  
  # initialize problems
  has_no_warnings <- TRUE
  
  # parse title:
  title_publication <- page_content %>% 
    html_nodes(".publication-title") %>% 
    html_text()
  
  
  # get review_doi:
  review_doi <- get_review_url_from_pagecontent(page_content)
  
  # get authors:
  authors <- 
    page_content %>% 
    html_nodes(".author") %>% 
    html_text() %>% 
    as.list() %>% 
    discard(.p = str_detect(., "declaration")) %>% 
    paste(collapse = ", ")
  
  
  # publish type:
  publish_type <-
    page_content %>% 
    html_nodes(".publish-type") %>% 
    html_text()
  
  
  # check how many summary tables exist:
  summaryTable_count <- get_nr_of_summary_tables(page_content, verbose = FALSE)
  
  if (str_detect(publish_type, "Diagnostic")) {
    #output <- stop_parsing_return_empty_df(review_url = review_doi,
    error_message <- glue::glue("Publish type is {publish_type}")
    flog.error("Publish type is not 'review'.")
    
    raise_warning(type = error_message)
    
  }  
  
  flog.trace("check if its the most recent version.")
  chr_is_most_recent_version <-
    page_content %>% 
    html_nodes(".newest-version-banner") %>% 
    html_text() %>% 
    str_remove_all(pattern = "^[[:blank:]]+|[[:blank:]]+$") 
  
  
  flog.trace("check if the review has been withdrawn.")
  is_withdrawn <- NA
  
  version_warning <-
    page_content %>% 
    html_nodes(".version-warning") %>% 
    html_text()
  
  
  if (length(version_warning) > 0)  {  # review is withdrawn:
    
    # if there is a version warning, stop parsing:
    
    # warning_df <<- 
    #   warning_df %>% 
    #   bind_rows(raise_warning(type = version_warning,
    #                           critical = FALSE))
    
    raise_warning(type = version_warning)
    
    flog.warn("Version warning: Withdrawn?")
    writeLines(glue::glue("Version warning: {warning_df$type}\n"))
    
  } else { # reivew is not withdran
    is_withdrawn <- FALSE
  }
  
  
  
  # init variables for checking if we have the most recent version:
  is_most_recent_version <- NA
  url_most_most_version <- NA
  
  # if there's no string saying that the review is outdated, 
  # we have the most recent version:
  if (length(chr_is_most_recent_version) == 0) {
    is_most_recent_version <- TRUE
  } else {
    if ((
      str_detect(chr_is_most_recent_version,
                 "not the most recent")
    ))  {
      is_most_recent_version <- FALSE 
      
      # warning_df <<- 
      #   warning_df %>% 
      #   bind_rows(raise_warning(type = "outdated_version",
      #                           critical = FALSE))
      # 
      raise_warning(type = "outdated_version")
      flog.warn("Outdated review.")
      
      # get url to most recent version:
      url_most_most_version <- 
        page_content %>% 
        html_nodes(".version-button") %>% 
        html_nodes("a") %>% 
        html_attr("href") %>% 
        str_c("https://www.cochranelibrary.com", .)
    } else { 
      is_most_recent_version <- TRUE
    }
  }
  
  
  # check whether the string "GRADE" appears somewhere in the text:
  GRADE_somewhere_in_the_text <- 
    page_content %>% 
    html_node("body") %>% 
    html_text() %>% 
    str_detect(c("GRADE"))
  
  
  # check if paywalled
  is_paywalled <- NA
  is_paywalled <-
    page_content %>% 
    html_nodes(".unlock") %>% 
    html_text() %>% 
    str_to_lower() %>% 
    str_trim() %>% 
    str_detect("unlock .* full review") %>% 
    magrittr::extract(1)
  
  if (is.na(is_paywalled)) is_paywalled <- FALSE
  
  if (is_paywalled){
    raise_warning(type = "is paywalled")
    flog.warn("This review is paywalled.")
  }
  
  citation_count <- get_number_of_citations(review_doi)
  
  
  output <- tibble(
    doi = review_doi,
    title = title_publication,
    authors = authors,
    publish_type = publish_type,
    is_withdrawn = is_withdrawn,
    is_most_recent_version = is_most_recent_version,
    url_most_most_version = url_most_most_version,
    summaryTable_count = summaryTable_count,
    GRADE_somewhere_in_the_text = GRADE_somewhere_in_the_text,
    is_paywalled = is_paywalled,
    citation_count = citation_count
    #warning = str_c(warning_df$type, collapse = " - ")
  )
  
  if (verbose) print(output)
  
  flog.info("Returning metadata.")
  return(output)
}

