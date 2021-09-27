get_number_of_citations <- function(review_url, 
                                    verbose = TRUE) {
  
  flog.info("get_number_of_citations", name = "funlog")  
  
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
