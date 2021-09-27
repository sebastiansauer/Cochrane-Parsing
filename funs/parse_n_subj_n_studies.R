



# function to parse the number of subjects
# and the number of studies from one string/variable:
parse_n_subj_n_studies <- function(n_participants_studies,
                                   return = c("subjects", "studies")) {
  
  flog.info("parse_n_subj_n_studies", name = "funlog")  
  
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





