

# get_nr_SoF_tables -------------------------------------------------------




# check-if-review-file-exists ---------------------------------------------


check_if_review_file_exists <- function(review_url, 
                                        output_dir = "output",
                                        file_type = "csv",
                                        verbose = TRUE) {
  
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
  
  
  return(output_file_exists)
  
}






# get-review-info-page -----------------------------------------------------------


get_review_info_page <- function(review_url, verbose = TRUE) {
  
  if (verbose) writeLines("Now reading info page.\n")
  
  infopage_url <- glue::glue("{review_url}/information") %>% 
    str_remove("/full") %>% 
    str_squish()
  
  
  safe_page_content_info_page <- safely_read_html(infopage_url) 
  
  # on error, stop:
  if (!is.null(safe_page_content_info_page$error)) {
    
    raise_warning(type = safe_page_content_info_page$error$message,
                      critical = TRUE)
    
    output <- create_empty_df(names_vec = get_infopage_colnames())
    output$doi <- review_url
    #output$warnings <- safe_page_content_info_page$error$message
    
    writeLines("Error 404 on reading info review page.\n")
    
    return(output)
    
  
    } else { # go on normally with parsing:
  
  # if (verbose) writeLines("Now closing the connection (info page).")
  # url <- url(infopage_url, "rb")
  # close(url)
  
  page_content_info_page <- safe_page_content_info_page$result
  
  info_publication <- 
    page_content_info_page %>% 
    html_nodes("#information") %>% 
    html_text()
  
  
  publication_date <-
    info_publication %>% 
    str_extract("Version published:\\s+\\d+\\s+\\w+\\s+\\d{4}")  %>% 
    str_remove("Version published:\\s+") %>% 
    str_squish()
  
  if (length(publication_date) == 0)
    publication_date <- NA  
  
  
  review_type <-
    info_publication %>% 
    str_extract("Type:\\s+\\w+") %>% 
    str_remove("Type:") %>% 
    str_squish()
  
  if (length(review_type) == 0)
    review_type <- NA
  
  
  review_group <-
    info_publication %>% 
    str_extract("Cochrane\\s+Editorial\\s+Group:\\s+.+Copyright?") %>% 
    str_remove("Cochrane\\s+Editorial\\s+Group:") %>% 
    str_remove_all("Copyright:*") %>% 
    str_squish
  
  if (length(review_group) == 0)
    review_group <- NA
  
  
  review_mesh_keywords <- 
    page_content_info_page %>% 
    html_nodes("#keywords") %>% 
    html_text()
  
  if (length(review_mesh_keywords) == 0)
    review_mesh_keywords <- NA
  
  
  output <- tibble(
    publication_date = publication_date,
    review_type = review_type,
    review_group = review_group,
    review_mesh_keywords = review_mesh_keywords
  )
  
  if (verbose) writeLines("Finished parsing the info page.\n")
  
  return(output)
    }
  
  
}







# get-review-metadata -----------------------------------------------------




get_review_metadata <- function(page_content,
                                verbose = TRUE){
  
  if (verbose) writeLines("Start parsing the review meta data\n")
  
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


  raise_warning(type = error_message)

}  
  
  # check if its the most recent version:
  chr_is_most_recent_version <-
    page_content %>% 
    html_nodes(".newest-version-banner") %>% 
    html_text() %>% 
    str_remove_all(pattern = "^[[:blank:]]+|[[:blank:]]+$") 
  
  
  # check if the review has been withdrawn:
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
  
  if (is_paywalled)
    
    # warning_df <<- 
    # warning_df %>% 
    # bind_rows(raise_warning(type = "is paywalled",
    #                         critical = FALSE))
  
    raise_warning(type = "is paywalled")
  
  citation_count <- get_number_of_citations(review_doi)
  
  
  output <- tibble(title = title_publication,
                 doi = review_doi,
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
  
  return(output)
  }




# get_abstract ------------------------------------------------------------



get_abstract <- function(page_content, verbose = TRUE) {
  
  if (verbose) writeLines("Now reading abstract.\n")
  

  # parse structured abstract (in parts):
  abstract <- page_content %>%
    html_node("body") %>% 
    xml_find_all("//section[contains(@class, 'abstract')]") %>% 
    rvest::html_text() 
  
  # background section:
  background_raw <- str_extract(abstract, "Background.+Objectives")
  background <- 
    background_raw %>% str_remove_all("Background|Objectives")
  
  # objectives section:
  objectives_raw <- str_extract(abstract, "Objectives.+Search methods")
  objectives <- 
    objectives_raw %>% str_remove_all("Objectives|Search methods")
  
  # search method section:
  search_methods_raw <- str_extract(abstract, "Search methods.+Selection criteria")
  search_methods <-
    search_methods_raw %>% str_remove_all("Search methods|Selection criteria")
  
  # selection criteria:
  selection_criteria_raw <- str_extract(abstract, "Search methods.+Data collection and analysis")
  selection_criteria <- 
    selection_criteria_raw %>% str_remove_all("Search methods|Data collection and analysis")
  
  # data collection and analysis section:
  data_coll_analysis_raw <- str_extract(abstract, "Data collection and analysis.+Main results")
  data_coll_analysis <-
    data_coll_analysis_raw %>% str_remove_all("Data collection and analysis|Main results")
  
  # meain results section:
  main_results_raw <- str_extract(abstract, "Main results.+Authors' conclusions")
  main_results <- 
    main_results_raw %>% str_remove_all("Main results|Authors' conclusions")
  
  # authors' conclusion section:
  conclusions_raw <- str_extract(abstract, "Authors' conclusions.+")
  conclusions <- 
    conclusions_raw %>% str_remove_all("Authors' conclusions")
  
  
  # add sentiment analysis of conclusion: 
  conclusion_sentiments <- get_conclusion_sentiment(conclusions)
  
  
  output <- tibble(
    background = background,
    objectives = objectives,
    search_methods = search_methods,
    selection_criteria = selection_criteria,
    data_coll_analysis = data_coll_analysis,
    main_results = main_results,
    conclusions = conclusions
  ) %>% 
    bind_cols(conclusion_sentiments)
  
  if (verbose) print(output)
  
  print("Abstract has been read.\n")
  
  return(output)
  
}














# write-parsed-review-to-file ---------------------------------------------



write_parsed_review_to_csv_file <- function(review_url,
                                        review,
                                        reviewer = "?",
                                        output_dir = "output",
                                        overwrite = TRUE){
  
  writeLines("Now writing file to disk.\n")
  
  if (reviewer != "?") output_dir <- glue("output/{reviewer}")
  
  
  output_file_exists <- check_if_review_file_exists(review_url,
                                                    output_dir = output_dir)
  
  # check if we should overwrite it, otherwise stop (if output  file already exists):
  if (output_file_exists & !overwrite) {
    writeLines(glue::glue("Output file exists. NOT overwriting.\n"))
  } else {
    
    file_path <- glue::glue("{output_dir}/{review_url}.csv") %>% 
      sanitize_review_url() 
    
    file_path <- file_path[1]
    
    write_csv(x = review, 
              file = file_path)
    writeLines(glue::glue("Results have been saved to file: {file_path}\n"))
  }
  
  return(review)
  
}






# final call --------------------------------------------------------------


#init()

parse_review <- function(review_url,
                         verbose = TRUE,
                         output_dir_to_write = "output",
                         reviewer  = "?",
                         sound = TRUE,
                         overwrite_file = TRUE) {
  
  if (!exists("count_reviews"))
    count_reviews <<- 1
  
  if (verbose) writeLines(glue::glue("______Now starting with review number ((( {count_reviews} )))______\n"))
  if (verbose) writeLines(glue::glue("______Now review with url ((( {review_url} )))______\n"))
  
  
  review_url_cochrane <- build_cochrane_url_from_doi(review_url)
  
  #be polite:
  #bow_result <- bow(url = review_url_cochrane,
  #                  user_agent = "Sebastian Sauer - sebastiansauer1@gmail.com")
  # not yet fully implemented!
  
  # parse all parts
  review_parsed_parts <- parse_review_parts(review_url_cochrane, 
                                            reviewer = reviewer,
                                            overwrite = overwrite_file)
  
  # add warnings:
  review_parsed_parts <-  
    review_parsed_parts %>% 
    mutate(warnings = str_c(warning_df$type, collapse = " | "))
  
  
  write_parsed_review_to_csv_file(review_url = review_url_cochrane,
                              review = review_parsed_parts,
                              output_dir = output_dir_to_write,
                              reviewer = reviewer,
                              overwrite = overwrite_file)
  
  if (!exists("count_reviews")) 
    count_reviews <- 0

  count_reviews <<- count_reviews + 1
  
  writeLines("Finalizing. Warnings:\n")
  print(warning_df)
  
  return(review_parsed_parts)
  
  if (sound) system("say Ich habe fertig!")
  
}

