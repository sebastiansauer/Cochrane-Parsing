
sanitize_review_url <- function(review_url) {
  
  flog.info("sanitize_review_url", name = "funlog")
  flog.trace("sanitizing review url")
  verbose <- config$verbose
  
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
