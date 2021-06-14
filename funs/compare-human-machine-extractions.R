compare_human_machine_extractions <- function(human_extractions_path,
                                              machine_extractions_path,
                                              reviewer_name = "?",
                                              output_path = "",
                                              discrepancies_only = FALSE,
                                              verbose = TRUE
                                              ) {
  
  library(tidyverse)  # data wrangling
  library(rvest)  # web scraping
  library(xml2)  # web scraping
  
  library(printr)  # print dfs as tables
  library(glue)  # glueing
  library(rcrossref)  # citation count
  #library(conflicted)  # detect package confligcs
  library(readxl)  # import excel data
  library(janitor)  # clean data 
  library(here)  # relative file paths
  library(writexl)  # write to xslx
  
  
  source("funs/helper-funs.R")
  source("funs/funs-parse-cochrane.R")
  source("funs/get_summary_table.R")
  source("funs/parse-review-parts.R")
  
  
  extractions_manual <-
    read_xlsx(path = human_extractions_path,
              skip = 1) %>%   # invalid header row
    slice(-c(1,2))  %>% # invalued header rows
    rename(reviewer = `...1`)
  
  
  extractions_manual2 <-
    extractions_manual %>% 
    clean_names()
  
  extractions_manual3 <-
    extractions_manual2
  
  names(extractions_manual3)[3] <- "SoF_table_number"
  names(extractions_manual3)[4] <- "is_outdated"
  names(extractions_manual3)[5] <- "is_withdrawn"
  names(extractions_manual3)[7] <- "GRADE_used"
  names(extractions_manual3)[22] <- "first_high_qual_outcome"
  
  extractions_manual3a <-
    extractions_manual3 %>% 
    mutate(across(.cols = c(3:7),
                  .fns = tolower)) %>% 
    mutate(is_outdated = ifelse(is_outdated == "yes", TRUE, FALSE),
           is_withdrawn = ifelse(is_withdrawn == "yes", TRUE, FALSE),
           GRADE_used = ifelse(GRADE_used == "yes", TRUE, FALSE)) %>% 
    group_by(url) %>% 
    mutate(SoF_table_number = row_number()) %>% 
    ungroup() %>% 
    mutate(has_high_quality_outcome = ifelse(
      str_count(str_squish(first_high_qual_outcome)) > 0, TRUE, FALSE)) %>% 
    mutate(has_high_quality_outcome = replace_na(has_high_quality_outcome, FALSE)) %>% 
    mutate(first_high_qual_outcome = 
             replace_na(first_high_qual_outcome, "no high quality outcome"))
  
  
  extractions_manual3b <- 
    extractions_manual3a %>% 
    mutate(cochrane_id = sanitize_review_url(url))
  
  
  cols_to_be_checked <-
    c("is_outdated",
      "is_withdrawn",   
      "GRADE_used",
      "first_high_qual_outcome",
      "has_high_quality_outcome"
    )
  
  id_cols_manual <-
    c("reviewer", "title", "cochrane_id", "SoF_table_number")
  
  
  extractions_manual4 <-
    extractions_manual3b %>% 
    select(any_of(c(id_cols_manual, cols_to_be_checked)))
  
  extractions_manual5 <-
    extractions_manual4 %>% 
    filter(tolower(reviewer) == reviewer_name)
  
  
  
  # MACHINE data
  
  extractions_machine <- 
    read_xlsx(machine_extractions_path)
  
  extractions_machine2 <-
    extractions_machine %>% 
    mutate(is_outdated = !is_most_recent_version,
           url = doi,
           cochrane_id = sanitize_review_url(doi)) 
  
  
  extractions_machine3 <-
    extractions_machine2 %>% 
    group_by(doi, SoF_table_number) %>% 
    mutate(GRADE_used = str_detect(GRADE, "⊕|⊝")) %>% 
    mutate(GRADE_used = ifelse(is.na(GRADE_used), FALSE, TRUE )) %>% 
    mutate(GRADE_used = ifelse(any(GRADE_used == TRUE), TRUE, FALSE)) %>% 
    ungroup() %>% 
    mutate(has_high_quality_outcome = str_detect(tolower(GRADE), "high")) %>% 
    mutate(has_high_quality_outcome = ifelse(
      is.na(has_high_quality_outcome), FALSE, has_high_quality_outcome)) %>% 
    mutate(first_high_qual_outcome = ifelse(has_high_quality_outcome == TRUE,
                                            Outcomes, "no high quality outcome")) %>% 
    ungroup()
  
  
  extractions_machine4 <-
    extractions_machine3 %>% 
    select(cochrane_id, SoF_table_number,
           any_of(cols_to_be_checked))
  
  
  extractions_merged <- 
    extractions_manual5 %>% 
    left_join(y = extractions_machine4, 
              by = c("cochrane_id", "SoF_table_number"))
  
  
  # SEE HERE WHICH CHECKS WERE PERFORMED:
  
  extractions_merged2 <-
    extractions_merged %>% 
    mutate(identical_outdated = is_outdated.x == is_outdated.y,
           identical_withdrawn = is_withdrawn.x == is_withdrawn.y,
           identical_GRADE_used = GRADE_used.x == GRADE_used.y) %>% 
    mutate(identical_has_high_quality_outcome = 
             identical(has_high_quality_outcome.x, has_high_quality_outcome.y)) %>% 
    mutate(identical_first_high_qual_outcome = 
             case_when(
               identical(first_high_qual_outcome.x, first_high_qual_outcome.y) ~ TRUE,
               str_detect(first_high_qual_outcome.x, first_high_qual_outcome.y) ~ TRUE,
               str_detect(first_high_qual_outcome.y, first_high_qual_outcome.x) ~ TRUE,
               TRUE ~ FALSE
             ))
  
  extractions_merged3 <-
    extractions_merged2 %>% 
    filter(identical_outdated == FALSE |
             identical_withdrawn == FALSE |
             identical_GRADE_used == FALSE |
             identical_has_high_quality_outcome == FALSE |
             identical_has_high_quality_outcome == FALSE)
  
  
  output <- extractions_merged2
  
  if (discrepancies_only) output <- extractions_merged3
  
  if (write_to_disk) 
    write_xlsx(output,
               path = output_path)
  
  return(output)
}