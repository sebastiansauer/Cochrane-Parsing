is_valid_cochrane_url <- function(url){
  
  flog.info("`check_if_valid_cochrane_url()`", name = "funlog")
  
  # example of valide Cochrane url: https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD001180.pub4/full
  # example of URL stem: 14651858.CD001180
  # 
  cochrane_valid_pattern <-
    "^https://www.cochranelibrary.com/cdsr/doi/10.1002/\\d+\\.CD\\d+(\\.pub.)?/full$"
   
  is_valid_cochrane_url <- 
    str_detect(url, pattern = cochrane_valid_pattern)
  
  return(is_valid_cochrane_url)
  
}
