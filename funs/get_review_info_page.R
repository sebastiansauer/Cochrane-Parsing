
get_review_info_page <- function(review_url) {
  
  verbose <- config$verbose
  
  if (verbose) writeLines("Now reading info page.\n")
  flog.info("Now reading info page.")
  flog.info("get_review_info_page", name = "funlog")
  
  infopage_url <- glue::glue("{review_url}/information") %>% 
    str_remove("/full") %>% 
    str_squish()
  
  
  safe_page_content_info_page <- safely_read_html(infopage_url) 
  
  # on error, stop:
  if (!is.null(safe_page_content_info_page$error)) {
    
    flog.warn("Error on reading the info page of the review.")
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



