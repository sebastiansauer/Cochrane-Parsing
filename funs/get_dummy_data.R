
get_dummy_data <- function(url = NA){
  
  flog.info("get_dummy_data", name = "funlog")
  
  mindfulness_url <- "https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD012791.pub2/full"  # 2 sof tables
  if (is.na(url)) url <- mindfulness_url
  
  
  output <- list(
    url = url,
    sanitized_review_url = sanitize_review_url(url)
    )
  
}
  
