



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

