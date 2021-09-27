


get_review_url_from_pagecontent <- function(page_content) {
  
  flog.info("get_review_url_from_pagecontent", name = "funlog")  
  
  # get doi:
  review_doi <-
    page_content %>% 
    html_nodes(".doi-header") %>% 
    html_text() %>% 
    str_remove_all(pattern = " ")
  
  return(review_doi)
}


