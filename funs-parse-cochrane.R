library(tidyverse)  # data wrangling
library(rvest)  # web scraping
library(xml2)  # web scraping
library(stringr)  # string manipulation




# Helper functions --------------------------------------------------------



create_empty_df <- function(names_vec) {
  
  df <- data.frame(matrix(ncol = length(names_vec), nrow = 1)) 
  colnames(df) <- names_vec
  
  return(df)
}



get_review_metadata_colnames <- function() {
  
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
  
  summarytab_metadata_colnames <- c(
    "main_comparison_of_review",
    "main_comparison_population",
    "main_comparison_setting",
    "main_comparison_comparsion_type"
  )
  
  return(summarytab_metadata_colnames)  
  
}

get_infopage_colnames <- function() {
  
  infopage_colnames <- c(
    "publication_date",
    "review_type",
    "review_group",
    "review_mesh_keywords"
  )
  
  return(infopage_colnames)  
  
}





get_all_colnames <- function(output_file = "first",  # if first, take first output file in "output" folder
                             verbose = FALSE, ...) {
  
  if (output_file == "first") {
    first_file_found <- dir("output/", pattern = "^\\d.*csv")[1]
    output_file_df <- read_csv(glue::glue("output/{first_file_found}"),
                               col_types = cols(),
                               ...)
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




raise_warning <- function(type,
                          critical = c(FALSE, TRUE),
                          write_to_disk = TRUE){
  
  
  warning_df <-
    tibble(
      type = unlist(type),
      date = Sys.Date(),
      time = Sys.time(),
      critical = critical
    )
  
  if (write_to_disk) write_csv(warning_df,
                               file = "logging/warnings.csv")
  
  return(warning_df)
  
}




sanitize_review_url <- function(review_url,
                                verbose = FALSE) {
  
  delete_string <- "http[s]*://www.cochranelibrary.com/cdsr/doi/10.1002/"
  delete_string2 <- "http[s]*://[dx.]*doi.org/10.1002/"
  
  output <- str_remove(review_url,
                       delete_string) %>% 
    str_remove(delete_string2) %>% 
    str_remove("/full") %>%
    str_squish()
  
  if (verbose) print(output)
  
  return(output)
  
  
}




stop_parsing_return_empty_df <- function(review_url,
                                         error_message = "no message",
                                         is_critical = TRUE) {
  
  warning_df <<- 
    warning_df %>% 
    bind_rows(raise_warning(type = error_message,
                            critical = is_critical))
  
  output <- create_empty_df(names_vec = get_all_colnames())
  output$doi <- review_url
  
  writeLines(glue::glue("Stop parsing: {warning_df$type}\n"))
  
  return(output)
  
}





get_review_url_from_pagecontent <- function(page_content) {
  
  # get doi:
  review_doi <-
    page_content %>% 
    html_nodes(".doi-header") %>% 
    html_text() %>% 
    str_remove_all(pattern = " ")
  
  return(review_doi)
}




init_new_review <- function() {
  
  
  # initialize logging:
  warning_df <<- raise_warning(type = " ",
                               critical = FALSE,
                               write_to_disk = FALSE) 
  

  
  
}



build_cochrane_url_from_doi <- function(review_url) {
  
  # example OK: https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD008268.pub3/full
  
  # ex. NOT OK: http://dx.doi.org/10.1002/14651858.CD008268.pub3
  # 
  url_is_doi_type <- 
    review_url %>% 
    str_detect("^https?://(dx.)?doi.org/10.1002/")
  
  if (url_is_doi_type) {
    # get url stem:
    url_stem <- sanitize_review_url(review_url) 
    
    # rebuild it to www.cochranelibrary.com form:
    url_cochrane <- 
      paste0("https://www.cochranelibrary.com/cdsr/doi/10.1002/", url_stem, "/full")
    output <- url_cochrane %>% str_squish()
  } else {
    output <- review_url %>% 
      str_squish()
  }
  
  return(output)
  
}





# function to parse the number of subjects
# and the number of studies from one string/variable:
parse_n_subj_n_studies <- function(n_participants_studies,
                                   return = c("subjects", "studies")) {
  
  
  rct_string <- "\\(\\d+\\s*(RCT[s]?|stud\\w+)\\)"
  
  n_studies <- str_extract_all(n_participants_studies, 
                               rct_string) %>% 
    simplify() %>% 
    parse_number()
  
  
  n_participants <- str_remove_all(n_participants_studies,
                                   rct_string) %>% 
    simplify() %>% 
    parse_number()
  
  
  if (length(n_participants_studies) > length(n_participants)){
    n_participants <- c(n_participants, rep(NA, length(n_participants_studies) - length(n_participants)))
  }
  if (length(n_participants_studies) > length(n_studies)){
    n_studies <- c(n_studies, rep(NA, length(n_participants_studies) - length(n_studies)))
  }
  
  
  
  if (return == "subjects") return(n_participants)
  if (return == "studies") return(n_studies)
  
}





get_conclusion_sentiment <- function(conclusion, verbose = TRUE) {
  
  
  if (verbose) writeLines("Detecting emotionality of conclusions\n")
  
  
  # get sentiment dict:
  bing_sentiments <- get_sentiments("bing")
  
  # count pos and neg sentiment words:
  pos_neg_count <- 
    conclusion %>% 
    str_squish() %>% 
    tibble(text = .) %>% 
    unnest_tokens(word, text) %>% 
    inner_join(bing_sentiments) %>% 
    count(sentiment)
  
  
  emo_words_in_conclusion <- 
    conclusion %>% 
    str_squish() %>% 
    tibble(text = .) %>% 
    unnest_tokens(word, text) %>% 
    inner_join(bing_sentiments) %>% 
    summarise(emo_words = str_c(word, collapse = " - ")) %>% 
    pull(emo_words)
  
  output <- 
    tibble(
      pos_neg_ratio = pos_neg_count$n[2]/pos_neg_count$n[1],
      emo_count = sum(pos_neg_count$n),
      emo_words_in_conclusion = emo_words_in_conclusion
    )
  
  return(output)
  
}



safely_read_html <- safely(read_html)


get_number_of_citations <- function(review_url, 
                                    verbose = TRUE) {
  
  delete_string <- "http[s]*://www.cochranelibrary.com/cdsr/doi/"
  delete_string2 <- "http[s]*://[dx.]*doi.org/"
  
  sanitized_url <- str_remove(review_url,
                       delete_string) %>% 
    str_remove(delete_string2) %>% 
    str_remove("/full")
 
  
  count <- rcrossref::cr_citation_count(doi = sanitized_url)$count
  
  
  
  if (verbose) writeLines(glue::glue("This review has been cited {count} times."))
  
  return(count)
  
}



# get_nr_SoF_tables -------------------------------------------------------




get_nr_of_summary_tables <- function(page_content, 
                                     table_number = NULL, # which table should be extracted?
                                     verbose = TRUE) # should an empty table be returned? if so, what's the name of the only column?
{
  
  
  # stop if critical warning has been raised earlier on:
  # if (any(warning_df$critical == TRUE)) {
  #   
  #   writeLines("Stopping reading the summary table, as critical warning has been raised earlier on")
  #   
  #   output <- create_empty_df(names_vec = get_all_colnames())
  #   output$doi <- review_url
  #   output$warning <- warning_df$type
  #   
  #   return(output)
  #   
  # }
  
  
  
  # check how many tables exist:
  nr_Table <- 
    page_content %>% 
    html_nodes("table") %>% 
    length()
  
  
  
  # check if a section "summary of results" exist
  summary_sections_exists <- 
    page_content %>% 
    html_nodes(".section-collapse-title") %>% 
    html_text() %>% 
    str_trim() %>% 
    str_detect("Summary of findings") %>% 
    any()
  
  
  # get number of summary tables:
  nr_summary_tables <- 
    page_content %>% 
    html_nodes(".summaryOfFindings") %>% 
    html_nodes(".table") %>% 
    length()
  
  # alternative way, likely less precise:
  nr_summary_tables2 <- 
    page_content %>% 
    html_nodes("table") %>% 
    html_nodes(".table-label") %>% 
    html_text() %>% 
    str_detect("Summary of findings") %>% sum()
  
  # warn if there are no summary tables:
  if (summary_sections_exists == FALSE) {
    writeLines("No (zero) summary sections detected!")
    
    warning_df <<- 
      warning_df %>% 
      bind_rows(raise_warning(type = "No (zero) summary sections detected",
                              critical = FALSE))
  }
  else {
    if (verbose) writeLines(paste0("Number of summary tables detected: ", nr_summary_tables))
    
  }
  
  # warn if user queries for a table number that does not exist:
  if (!is.null(table_number)) {
    if (table_number > nr_summaryOfFindingsTable) {
      
      writeLines(paste("This table does not exist! Aborting.\n"))
    }
    
  }
  
  if (summary_sections_exists == FALSE & nr_summary_tables != 0)
    stop("summary_sections_exists == FALSE & nr_summary_tables != 0")
  
  
  return(nr_summary_tables)
}




# check-if-review-file-exists ---------------------------------------------


check_if_review_file_exists <- function(review_url, 
                                        output_dir = "output",
                                        file_type = "csv",
                                        verbose = TRUE) {
  
  if (length(output_dir) == 0) stop("Please specify output directory.")
  

  
   # use doi to check whether output file exists
  file_path <- glue::glue("{output_dir}/{review_url}.{file_type}") %>% 
    sanitize_review_url() 
  
  file_path <- file_path[1]
  
  writeLines(glue::glue("Checking if output file already exists: {file_path}\n"))
  

  
  # check if output file exists:
  output_file_exists <- file.exists(file_path)
  
  if (verbose & output_file_exists) writeLines(glue::glue("Output file exists: {file_path}\n"))

  if (verbose & !output_file_exists) writeLines(glue::glue("Output file does NOT exist: {file_path}\n"))
  
  
  return(output_file_exists)
  
}


# get-info-page -----------------------------------------------------------


get_review_info_page <- function(review_url, verbose = TRUE) {
  
  if (verbose) writeLines("Now reading info page.\n")
  
  infopage_url <- glue::glue("{review_url}/information") %>% 
    str_remove("/full") %>% 
    str_squish()
  
  
  safe_page_content_info_page <- safely_read_html(infopage_url) 
  
  # on error, stop:
  if (!is.null(safe_page_content_info_page$error)) {
    
    warning_df <<-
      warning_df %>% 
      bind_cols(
        raise_warning(type = safe_page_content_info_page$error$message,
                      critical = TRUE))
    
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
                                reviewer = "?",
                                verbose = TRUE){
  
  writeLines("Start parsing the review meta data\n")
  
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

# return(output)
    warning_df <<-
      warning_df %>% 
      bind_rows(raise_warning(type = error_message,
                          critical = TRUE))

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
  
  
  if (length(version_warning) > 0)  {
    
    # if there is a version warning, stop parsing:
    
    warning_df <<- 
      warning_df %>% 
      bind_rows(raise_warning(type = version_warning,
                              critical = FALSE))
    
    #output <- create_empty_df(names_vec = get_all_colnames())
    #output$doi <- review_url
    
    writeLines(glue::glue("Version warning: {warning_df$type}\n"))
    
    #return(output)
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
      
      warning_df <<- 
        warning_df %>% 
        bind_rows(raise_warning(type = "outdated_version",
                                critical = FALSE))
      
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
    
    warning_df <<- 
    warning_df %>% 
    bind_rows(raise_warning(type = "is paywalled",
                            critical = FALSE))
  
  
  citation_count <- get_number_of_citations(review_doi)
  
  
  output <- tibble(title = title_publication,
                 doi = review_doi,
                 reviewer = reviewer,
                 authors = authors,
                 publish_type = publish_type,
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
  
  return(output)
  
}






# concatenate tables -------------------------------------------


concat_tables <- function(page_content,
                          info_page,
                          metadata_review,
                          abstract_review,
                          #metadata_summaryTable,
                          stop_early = FALSE, 
                          summarytable,
                          verbose = TRUE,
                          drop_unused_cols = TRUE) {
  
  
  writeLines("Starting table concatenation.\n")
  
  # stop if critical warning has been raised earlier on:
  if (any(warning_df$critical == TRUE & stop_early)) {
    
    writeLines("Stopping concatenating, as critical warning has been raised earlier on")
    
    output <- create_empty_df(names_vec = get_all_colnames())
    output$doi <- review_url
    output$warnings = str_c(warning_df$type, collapse = " | ")
    
    return(output)
    
  }
  

  if (is.na(summarytable)) {
    if (verbose) {writeLines("No summarytable handed over.\n")}
    
    summarytable <- create_empty_df(names_vec = c(get_summarytab_colnames(),
                                                  get_summarytab_metadata_colnames()
                                    ))
  } 
  
  
  # concatenate summarytable and abstract as main df:
  output_table <-
    summarytable %>% 
    bind_cols(abstract_review)
  
  # add info page:
  output_table2 <- 
    output_table %>% 
    bind_cols(info_page)
  

  # add review metadata such as doi, title, author:
  output_table3 <-
    metadata_review %>%
    bind_cols(output_table2)
  
  
  
  # then, add summarytable metadata to main df:
  # output_table4 <- 
  #   metadata_summaryTable %>% 
  #   bind_cols(output_table3)
  
  
  # drop unused cols:
  if (drop_unused_cols) {
    output_table4 <- 
      output_table3 %>% 
      select(-starts_with("X"))
  }
  
  
  # add warnings
  output_table5 <-
    output_table4 %>% 
    mutate(warnings = str_c(warning_df$type, collapse = " | "))
  
  
  # sort columns:
  output_table6 <-
    output_table5 %>% 
    select(
      doi, 
      reviewer,
      publication_date,
      review_type,
      review_group,
      review_mesh_keywords,
      citation_count,
      
      title, 
      authors, 
      publish_type, 
      is_most_recent_version, 
      url_most_most_version,
      
      
      starts_with("main_comp"),
      
      background,
      objectives,
      search_methods,
      selection_criteria,
      data_coll_analysis,
      main_results,
      conclusions,
      pos_neg_ratio,
      emo_count,
      emo_words_in_conclusion,
      
      
      everything())
  
  output <- output_table6
  
  if (verbose) print(output)
  
  
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
                         output_dir = "output",
                         reviewer  = "?",
                         overwrite_file = TRUE) {
  
  if (verbose) writeLines(glue::glue("______Now starting with review number ((( {count_reviews} )))______\n"))
  if (verbose) writeLines(glue::glue("______Now review with url ((( {review_url} )))______\n"))
  
  
  review_url_cochrane <- build_cochrane_url_from_doi(review_url)
  
  #be polite:
  bow_result <- bow(url = review_url,
                    user_agent = "Sebastian Sauer - sebastiansauer1@gmail.com")

  
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
                              output_dir = output_dir,
                              reviewer = reviewer,
                              overwrite = overwrite_file)
  
  #if (!exists(x = count_reviews))

  count_reviews <<- count_reviews + 1
  
  writeLines("Finalizing. Warnings:\n")
  print(warning_df)
  
  return(review_parsed_parts)
  
  
}

