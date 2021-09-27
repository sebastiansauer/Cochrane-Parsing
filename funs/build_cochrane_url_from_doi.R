build_cochrane_url_from_doi <- function(review_url) {
  
  flog.info("build_cochrane_url_from_doi", name = "funlog")  
  
  # example OK: https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD008268.pub3/full
  
  # ex. NOT OK: http://dx.doi.org/10.1002/14651858.CD008268.pub3
  # 
  url_is_doi_type <- 
    review_url %>% 
    str_detect("^https?://(dx.)?doi.org/10.1002/")
  

  if (url_is_doi_type) {
    # get url stem:
    url_stem <- sanitize_review_url(review_url) }
  
  
  url_is_stem <-
    review_url %>% 
    str_detect("^\\d+\\.CD\\d+")
  
  if (url_is_stem) url_stem <- review_url
  
  if (url_is_doi_type | url_is_stem) {
    flog.trace("URL is of type doi or of type Cochrane Stem (eg., '14651858.CD001180.pub4')")
    # rebuild it to www.cochranelibrary.com form:
    url_cochrane <- 
      paste0("https://www.cochranelibrary.com/cdsr/doi/10.1002/", url_stem, "/full")
    output <- url_cochrane %>% str_squish()
    flog.trace(paste0("This url has now been build: ", url_cochrane))
    
  } else {
    output <- review_url %>% 
      str_squish()
  }
  
  return(output)
  
}
