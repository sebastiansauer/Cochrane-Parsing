compare_human_machine_extractions <- function(reviewer_selected = "?",
                                              write_to_disk = TRUE,
                                              discrepancies_only = TRUE,
                                              verbose = TRUE
                                              ) {
  
  library(tidyverse)  # data wrangling
  library(rvest)  # web scraping
  library(xml2)  # web scraping
  
  library(printr)  # print dfs as tables
  library(glue)  # glueing
  library(rcrossref)  # citation count
  #library(conflicted)  # detect package conflicts
  library(readxl)  # import excel data
  library(janitor)  # clean data 
  library(here)  # relative file paths
  library(writexl)  # write to xslx
  
  

  
  ########## manual extractions:

  # Define output path:
  output_path <- glue("{here()}/output/comparison/{reviewer_selected}")
  
  if (verbose) print(paste("Assuming this output path:" , output_path, "\n"))
  
  # Here's the path where the human (manual) extractions come from:
  extractions_manual_path <- glue("manual-extractions/{reviewer_selected}/InEffective Cochrane Reviews Final_{reviewer_selected}.xlsx")
  
  if (verbose) print(paste("Assuming this path for manual extraction data:" , extractions_manual_path, "\n"))
  
  
  if (!file.exists(extractions_manual_path)) 
    stop("File not found!") 
  else message("File found.")
  
  # Read human extraction data:
  extractions_manual <-
    read_xlsx(path = extractions_manual_path,
              skip = 1) %>%   # invalid header row
    slice(-c(1,2))  %>% # invalued header rows
    rename(reviewer = `...1`)

  # clean names:
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
  
  if (verbose) print("Now starting recoding...\n")
  
  # recode:
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
    mutate(has_high_quality_outcomes = ifelse(
      str_count(str_squish(first_high_qual_outcome)) > 0, TRUE, FALSE)) %>% 
    mutate(has_high_quality_outcomes = replace_na(has_high_quality_outcomes, FALSE)) %>% 
    mutate(first_high_qual_outcome = replace_na(first_high_qual_outcome, 
                                                "no high quality outcome"))
  
  
  if (verbose) print("Not sanitzing review url ...\n")
  
  extractions_manual3b <- 
    extractions_manual3a %>% 
    mutate(cochrane_id = sanitize_review_url(url))
  
  
  # remove parantheses, b/c unmatches parantheses caused problems:
  if (verbose) print("Now removing parentheses.")
    extractions_manual3c <- 
    extractions_manual3b %>% 
    mutate(first_high_qual_outcome = str_remove_all(
      string = first_high_qual_outcome,
      pattern = "[()]+"))
  
  cols_to_be_checked <-
    c("is_outdated",
      "is_withdrawn",   
      "GRADE_used",
      "first_high_qual_outcome",
      "has_high_quality_outcomes"
    )
  
  id_cols_manual <-
    c("reviewer", "title", "url", "cochrane_id", "SoF_table_number")
  
  if (verbose) print("Now selecting relevant columns.")
  extractions_manual4 <-
    extractions_manual3c %>% 
    select(any_of(c(id_cols_manual, cols_to_be_checked)))
  
  if (verbose) print("Now filtering rows to selected reviewer.\n")
  extractions_manual5 <-
    extractions_manual4 %>% 
    filter(tolower(reviewer) == reviewer_selected)
  
  ########## machine extractions:
  
  
  extractions_machine_path <- glue( "output/{reviewer_selected}/reviews_output_machine_{reviewer_selected}.xlsx")
  
  if (verbose) print(paste("Assuming this path for machine extracted data",
                     extractions_machine_path, "\n"))
  
   if (!file.exists(extractions_machine_path)) 
     stop("File does not exist!") else message("File found.")
  
  extractions_machine <- 
    read_xlsx(extractions_machine_path)
  
  if (verbose) print("Excel file with machine extracted data has been read.\n")
  
  extractions_machine2 <-
    extractions_machine %>% 
    mutate(is_outdated = !is_most_recent_version,
           url = doi,
           cochrane_id = sanitize_review_url(doi)) 
  
  print("Computing variables: GRADE_used, has_high_quality_outcomes, first_high_qual_outcome\n")
  extractions_machine3 <-
    extractions_machine2 %>% 
    group_by(doi, SoF_table_number) %>% 
    mutate(GRADE_used = str_detect(GRADE, "⊕|⊝")) %>% 
    mutate(GRADE_used = ifelse(is.na(GRADE_used), FALSE, TRUE )) %>% 
    mutate(GRADE_used = ifelse(any(GRADE_used == TRUE), TRUE, FALSE)) %>% 
    mutate(is_high_quality_outcome = str_detect(tolower(GRADE), "high")) %>% 
    mutate(has_high_quality_outcomes = case_when(
      any(is_high_quality_outcome) ~ TRUE,
      TRUE ~ FALSE)) %>% 
    mutate(first_high_qual_outcome = 
             ifelse(is_high_quality_outcome == TRUE,
                    Outcomes, "no high quality outcome")) %>% 
    ungroup()
  
  # `SoF_table_number` should not be of type `character`:
  if (is.character(extractions_machine3$SoF_table_number)) 
    extractions_machine3$SoF_table_number <- as.integer(extractions_machine3$SoF_table_number)
  
  extractions_machine3a <- extractions_machine3
  
  if (verbose) print("Now selecting columns.")
  extractions_machine4 <-
    extractions_machine3a %>% 
    select(cochrane_id, 
           SoF_table_number, 
           is_paywalled, 
           warnings,
           is_high_quality_outcome,
           any_of(cols_to_be_checked))
  
  if (verbose) print("Now removing parentheses.")
  # remove parantheses, because some unmatched parantheses caused problems:
  extractions_machine4a <- 
    extractions_machine4 %>% 
    mutate(first_high_qual_outcome = str_remove_all(
      string = first_high_qual_outcome,
      pattern = "[()]+"
    ))
  
 
  
  if (verbose) print("Now starting merging...\n")
  
  extractions_merged <- 
    extractions_manual5 %>% 
    left_join(y = extractions_machine4a, 
              by = c("cochrane_id", "SoF_table_number"))
  
  if (verbose) print("Now comparing human with machine extractions.")
  extractions_merged2 <-
    extractions_merged %>% 
    mutate(identical_outdated = is_outdated.x == is_outdated.y,
           identical_withdrawn = is_withdrawn.x == is_withdrawn.y,
           identical_GRADE_used = GRADE_used.x == GRADE_used.y) %>% 
    rowwise() %>% 
    mutate(identical_has_high_quality_outcomes = 
             has_high_quality_outcomes.x == has_high_quality_outcomes.y) %>% 
    mutate(identical_first_high_qual_outcome = 
             case_when(
               first_high_qual_outcome.x == first_high_qual_outcome.y ~ TRUE,
               str_detect(first_high_qual_outcome.x, first_high_qual_outcome.y) ~ TRUE,
               str_detect(first_high_qual_outcome.y, first_high_qual_outcome.x) ~ TRUE,
               TRUE ~ FALSE
             )) %>% 
    ungroup()
  
  if (verbose) print("Now starting testing for equality human/machine... \n")
  extractions_merged3 <-
    extractions_merged2 %>% 
    filter(identical_outdated == FALSE |
             identical_withdrawn == FALSE |
             identical_GRADE_used == FALSE |
             identical_has_high_quality_outcomes == FALSE |
             identical_has_high_quality_outcomes == FALSE)
  
  if (verbose) print(paste0("Dimensions (row/cols) of Excel files with discrepancies: ", str_c(dim(extractions_merged), 
                                                                                                  collapse = " ")))
  
  if (write_to_disk) {
    path_ext_merged2 <- glue("{output_path}/comparison_human_machine_{reviewer_selected}.xlsx")
    if (verbose) print(paste("Assuming this output path for complete output file: ", path_ext_merged2, "\n"))
    
    write_xlsx(extractions_merged2,
               path = path_ext_merged2)
    
    
    path_ext_merged3 <-
      glue("{output_path}/comparison_human_machine_problems_only_{reviewer_selected}.xlsx")
    if (verbose) print(paste("Assuming this output path for file with discrepancies only: ", path_ext_merged3, "\n"))
    
    write_xlsx(extractions_merged3,
               path = path_ext_merged3)
    
    if (verbose) print("Files have been written to disk.")
  }
  
  
  if (discrepancies_only) output <- extractions_merged3
  if (!discrepancies_only) output <- extractions_merged2
  
  
  return(output)
}
