library(tidyverse)  # data wrangling
library(rvest)  # web scraping
library(xml2)  # web scraping
library(stringr)  # string manipulation




# Helper functions --------------------------------------------------------



create_empty_df <- function(names_vec) {
  
  df <- data.frame(matrix(ncol = length(names_vec), nrow = 1)) 
  colnames(df) <- names_vec
  
  return(df)
}



get_review_metadata_colnames <- function() {
  
  review_metadata_colnames <- c(
    "title",
    "doi",
    "authors",
    "publish_type",
    "is_most_recent_version",
    "url_most_most_version",
    "summaryTable_count",
    "GRADE_somewhere_in_the_text",
    "is_paywalled"
  )
  
  return(review_metadata_colnames)
  
}


get_summarytab_colnames <- function() {
  
  summarytab1_colnames <- c(
  "id_measure",
  "Outcomes",
  "X2",
  "X3",
  "relative_effect_95CI",
  "n_participants_studies",
  "n_participants",
  "n_studies",
  "GRADE",
  "Comments",
  "effect_statistic",
  "CI_lower",
  "CI_upper",
  "effect_size"
  )
  
  return(summarytab1_colnames)
  
}


get_summarytab_metadata_colnames <- function() {
  
  summarytab_metadata_colnames <- c(
  "main_comparison_of_review",
  "main_comparison_population",
  "main_comparison_setting",
  "main_comparison_comparsion_type"
  )

  return(summarytab_metadata_colnames)  
  
}




get_all_colnames <- function(output_file = "first",  # if first, take first output file in "output" folder
                             verbose = FALSE, ...) {
  
  if (output_file == "first") {
    first_file_found <- dir("output/", pattern = "^\\d.*csv")[1]
    output_file_df <- read_csv(glue::glue("output/{first_file_found}"),
                               col_types = cols(),
                               ...)
  } else {
  if (!file.exists(output_file)) stop("File not found!")
  output_file_df <- read_csv(output_file, 
                             col_types = cols(),
                             ...)
  }
  
  
  output <- names(output_file_df)
  
  if (verbose) print(output)
  
  return(output)
}




raise_warning <- function(type,
                          critical = c(FALSE, TRUE),
                          write_to_disk = TRUE){
  

  warning_df <-
    tibble(
      type = type,
      date = Sys.Date(),
      time = Sys.time(),
      critical = critical
    )
  
  if (write_to_disk) write_csv(warning_df,
                               file = "logging/warnings.csv")
  
  return(warning_df)
  
}




sanitize_review_url <- function(review_url,
                                verbose = FALSE) {
  
  delete_string <- "http[s]*://www.cochranelibrary.com/cdsr/doi/10.1002/"
  delete_string2 <- "http[s]*://[dx.]*doi.org/10.1002/"
  
  output <- str_remove(review_url,
                       delete_string) %>% 
    str_remove(delete_string2) %>% 
    str_remove("/full")
  
  if (verbose) print(output)
  
  return(output)
  
  
}




stop_parsing_return_empty_df <- function(review_url,
                                         error_message = "no message",
                                         is_critical = TRUE) {
  
  warning_df <<- raise_warning(type = error_message,
                               critical = is_critical)
  
  output <- create_empty_df(names_vec = get_all_colnames())
  output$doi <- review_url
  output$warning <- warning_df$type
  
  writeLines(glue::glue("Stop parsing: {warning_df$type}\n"))
  
  return(output)
  
}





get_review_url_from_pagecontent <- function(page_content) {
  
  # get doi:
  review_doi <-
    page_content %>% 
    html_nodes(".doi-header") %>% 
    html_text() %>% 
    str_remove_all(pattern = " ")
  
  return(review_doi)
}




init <- function() {
  
  
  # initialize logging:
  warning_df <<- raise_warning(type = "NO warnings",
                               critical = FALSE,
                               write_to_disk = FALSE) 
  
  # init count:
  i <<- 1
  
  
}



build_cochrane_url_from_doi <- function(review_url) {
  
  # example OK: https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD008268.pub3/full
  
  # ex. NOT OK: http://dx.doi.org/10.1002/14651858.CD008268.pub3
  # 
  url_is_doi_type <- 
  review_url %>% 
    str_detect("^https?://(dx.)?doi.org/10.1002/")
  
  if (url_is_doi_type) {
  # get url stem:
  url_stem <- sanitize_review_url(review_url) 
  
  # rebuild it to www.cochranelibrary.com form:
  url_cochrane <- 
    paste0("https://www.cochranelibrary.com/cdsr/doi/10.1002/", url_stem, "/full")
  output <- url_cochrane
  } else {
    output <- review_url
  }
  
  return(output)
  
}








# check-if-review-file-exists ---------------------------------------------


check_if_review_file_exists <- function(review_url, 
                                        output_dir = "output",
                                        verbose = TRUE) {
  
  if (length(output_dir) == 0) stop("Please specify output directory.")
  
  # use doi to check whether output file exists
  file_path <- glue::glue("{output_dir}/{review_url}.csv") %>% 
    sanitize_review_url() 
  
  file_path <- file_path[1]
  
  # check if output file exists:
  output_file_exists <- file.exists(file_path)
  
  if (verbose) writeLines(glue::glue("Output file exists: {file_path}\n"))
  
  return(output_file_exists)
  
}


# get-info-page -----------------------------------------------------------


get_review_info_page <- function(review_url, verbose = TRUE) {
  
  
  infopage_url <- glue::glue("{review_url}/information") %>% 
    str_remove("/full")
  
  page_content <- read_html(infopage_url)  
  
  info_publication <- 
    page_content %>% 
    html_nodes("#information") %>% 
    html_text()
  
  
  publication_date <-
    info_publication %>% 
    str_extract("Version published:\\s+\\d+\\s+\\w+\\s+\\d{4}") %>% 
    str_remove("Version published:\\s+") %>% 
    str_squish()
  
  
  review_type <-
    info_publication %>% 
    str_extract("Type:\\s+\\w+") %>% 
    str_remove("Type:") %>% 
    str_squish()
  
  
  review_group <-
    info_publication %>% 
    str_extract("Cochrane\\s+Editorial\\s+Group:\\s+.+Copyright?") %>% 
    str_remove("Cochrane\\s+Editorial\\s+Group:") %>% 
    str_remove_all("Copyright:*") %>% 
    str_squish

  review_mesh_keywords <- 
  page_content %>% 
    html_nodes("#keywords") %>% 
    html_text()
   
    
  output <- tibble(
    publication_date = publication_date,
    review_type = review_type,
    review_group = review_group,
    review_mesh_keywords = review_mesh_keywords)
  
  return(output)
    
  
}







# get-review-metadata -----------------------------------------------------




get_review_metadata <- function(page_content, verbose = TRUE){
  
  
  # initialize problems
  has_no_warnings <- TRUE
  
  # parse title:
  title_publication <- page_content %>% 
    html_nodes(".publication-title") %>% 
    html_text()
  
  
  # get review_doi:
  review_doi <- get_review_url_from_pagecontent(page_content)
  
  # get authors:
  authors <- 
    page_content %>% 
    html_nodes(".author") %>% 
    html_text() %>% 
    as.list() %>% 
    discard(.p = str_detect(., "declaration")) %>% 
    paste(collapse = ", ")
  
  
  # publish type:
  publish_type <-
    page_content %>% 
    html_nodes(".publish-type") %>% 
    html_text()
  
  # XXX
  if (str_detect(publish_type, "Diagnostic")) {
    output <- stop_parsing_return_empty_df(review_url = review_doi,
                                           error_message = glue::glue("Publish type is {publish_type}"))
    
    return(output)
  }  
  
  # check if its the most recent version:
  chr_is_most_recent_version <-
    page_content %>% 
    html_nodes(".newest-version-banner") %>% 
    html_text() %>% 
    str_remove_all(pattern = "^[[:blank:]]+|[[:blank:]]+$") 
  
  
  # check if the review has been withdrawn:
  is_withdrawn <- NA
  
  version_warning <-
    page_content %>% 
    html_nodes(".version-warning") %>% 
    html_text()
  

  if (length(version_warning) > 0)  {
   
      # if there is a version warning, stop parsing:
      
      warning_df <<- raise_warning(type = version_warning,
                                   critical = TRUE)
      output <- create_empty_df(names_vec = get_all_colnames())
      output$doi <- review_url
      output$warning <- warning_df$type
      
      writeLines(glue::glue("Stop parsing: {warning_df$type}\n"))
      
      return(output)
    }
  
  
  
  # init variables for checking if we have the most recent version:
  is_most_recent_version <- NA
  url_most_most_version <- NA
  
  # if there's no string saying that the review is outdated, 
  # we have the most recent version:
  if (length(chr_is_most_recent_version) == 0) {
    is_most_recent_version <- TRUE
  } else {
    if ((
      str_detect(chr_is_most_recent_version,
                 "not the most recent")
    ))  {
      is_most_recent_version <- FALSE 
      
      # get url to most recent version:
      url_most_most_version <- 
        page_content %>% 
        html_nodes(".version-button") %>% 
        html_nodes("a") %>% 
        html_attr("href") %>% 
        str_c("https://www.cochranelibrary.com", .)
    } else { 
      is_most_recent_version <- TRUE
    }
  }
  
  # check how many summary tables exist:
  summaryTable_count <- get_nr_of_summary_tables(page_content, verbose = FALSE)
  
  
  # check whether the string "GRADE" appears somewhere in the text:
  GRADE_somewhere_in_the_text <- 
  page_content %>% 
    html_node("body") %>% 
    html_text() %>% 
    str_detect(c("GRADE"))
  
  
  # check if paywalled
  is_paywalled <- NA
  is_paywalled <-
    page_content %>% 
    html_nodes(".unlock") %>% 
    html_text() %>% 
    str_to_lower() %>% 
    str_trim() %>% 
    str_detect("unlock .* full review") %>% 
    magrittr::extract(1)
  
  if (is.na(is_paywalled)) is_paywalled <- FALSE
  
  
  output <- list(title = title_publication,
                 doi = review_doi,
                 authors = authors,
                 publish_type = publish_type,
                 is_most_recent_version = is_most_recent_version,
                 url_most_most_version = url_most_most_version,
                 summaryTable_count = summaryTable_count,
                 GRADE_somewhere_in_the_text = GRADE_somewhere_in_the_text,
                 is_paywalled = is_paywalled,
                 warning = warning_df$type
                 )
  
  if (verbose) print(output)
  
  return(output)
}




# get_abstract ------------------------------------------------------------



get_abstract <- function(page_content, verbose = TRUE) {
  
  
  # stop if critical warning has been raised earlier on:
  if (warning_df$critical == TRUE) {
    
    writeLines("Stopping reading the summary table, as critical warning has been raised earlier on")
    
    output <- create_empty_df(names_vec = get_all_colnames())
    output$doi <- review_url
    output$warning <- warning_df$type
    
    return(output)
    
  }
  
  
  
  
  # parse structured abstract (in parts):
  abstract <- page_content %>%
    html_node("body") %>% 
    xml_find_all("//section[contains(@class, 'abstract')]") %>% 
    rvest::html_text() 
  
  # background section:
  background_raw <- str_extract(abstract, "Background.+Objectives")
  background <- 
    background_raw %>% str_remove_all("Background|Objectives")
  
  # objectives section:
  objectives_raw <- str_extract(abstract, "Objectives.+Search methods")
  objectives <- 
    objectives_raw %>% str_remove_all("Objectives|Search methods")
  
  # search method section:
  search_methods_raw <- str_extract(abstract, "Search methods.+Selection criteria")
  search_methods <-
    search_methods_raw %>% str_remove_all("Search methods|Selection criteria")
  
  # selection criteria:
  selection_criteria_raw <- str_extract(abstract, "Search methods.+Data collection and analysis")
  selection_criteria <- 
    selection_criteria_raw %>% str_remove_all("Search methods|Data collection and analysis")
  
  # data collection and analysis section:
  data_coll_analysis_raw <- str_extract(abstract, "Data collection and analysis.+Main results")
  data_coll_analysis <-
    data_coll_analysis_raw %>% str_remove_all("Data collection and analysis|Main results")
  
  # meain results section:
  main_results_raw <- str_extract(abstract, "Main results.+Authors' conclusions")
  main_results <- 
    main_results_raw %>% str_remove_all("Main results|Authors' conclusions")
  
  # authors' conclusion section:
  conclusions_raw <- str_extract(abstract, "Authors' conclusions.+")
  conclusions <- 
    conclusions_raw %>% str_remove_all("Authors' conclusions")
  
  
  output <- list(
    background = background,
    objectives = objectives,
    search_methods = search_methods,
    selection_criteria = selection_criteria,
    data_coll_analysis = data_coll_analysis,
    main_results = main_results,
    conclusions = conclusions
  )
  
  if (verbose) print(output)
  
  return(output)
  
}










# get_nr_of_summary_tables ------------------------------------------------



get_nr_of_summary_tables <- function(page_content, 
                                     table_number = NULL, # which table should be extracted?
                                     verbose = TRUE) # should an empty table be returned? if so, what's the name of the only column?
  {
  
  
  # stop if critical warning has been raised earlier on:
  if (warning_df$critical == TRUE) {
    
    writeLines("Stopping reading the summary table, as critical warning has been raised earlier on")
    
    output <- create_empty_df(names_vec = get_all_colnames())
    output$doi <- review_url
    output$warning <- warning_df$type
    
    return(output)
    
  }
  
  
  
   # check how many tables exist:
  nr_Table <- 
    page_content %>% 
    html_nodes("table") %>% 
    length()
  

  
  # check if a section "summary of results" exist
  summary_sections_exists <- 
    page_content %>% 
    html_nodes(".section-collapse-title") %>% 
    html_text() %>% 
    str_trim() %>% 
    str_detect("Summary of findings") %>% 
    any()
  
  
  # get number of summary tables:
  nr_summary_tables <- 
    page_content %>% 
    html_nodes(".summaryOfFindings") %>% 
    html_nodes(".table") %>% 
    length()
  
  # alternative way, likely less precise:
  nr_summary_tables2 <- 
    page_content %>% 
    html_nodes("table") %>% 
    html_nodes(".table-label") %>% 
    html_text() %>% 
    str_detect("Summary of findings") %>% sum()

  # warn if there are no summary tables:
  if (summary_sections_exists == FALSE) 
    writeLines("No (zero) summary section detected!")
  else {
    if (verbose) writeLines(paste0("Number of summary tables detected: ", nr_summary_tables))
    
  }
  
  # warn if user queries for a table number that does not exist:
  if (!is.null(table_number)) {
    if (table_number > nr_summaryOfFindingsTable) {
      
      writeLines(paste("This table does not exist! Aborting.\n"))
    }
    
  }
  
  if (summary_sections_exists == FALSE & nr_summary_tables != 0)
    stop("summary_sections_exists == FALSE & nr_summary_tables != 0")
  
  
  return(nr_summary_tables)
}





# get_summary_table -------------------------------------------------------



# page_content <- review$page_content


get_summary_table <- function(page_content, 
                              table_number = 1, 
                              # set to TRUE if only the first outcome should be parsed:
                              first_outcome_only = FALSE,
                              verbose = TRUE) {
  
  # stop if critical warning has been raised earlier on:
  if (warning_df$critical == TRUE) {
    
    writeLines("Stopping reading the summary table, as critical warning has been raised earlier on")
    
    output <- create_empty_df(names_vec = get_all_colnames())
    output$doi <- review_url
    output$warning <- warning_df$type
    
    return(output)
    
  }
  
  
  # run only if at least one such tables exists:
  
  nr_summaryOfFindingsTable <- get_nr_of_summary_tables(page_content)
  

  # if no summary table exists, report it, and stop:
  if ((table_number > nr_summaryOfFindingsTable) | 
      (nr_summaryOfFindingsTable == 0)) {
    
    output <- 
      create_empty_df(names_vec = get_summarytab_colnames())
    
    output$Comments <- "No summary table detected, hence no outcomes reported"
    
    print("As no summary findings tables were detected, I'm stopping the collection of summary tables.")
    return(output)
    
  } # otherwise, go on:
  
  # old version, likely to be buggy in some situations:
  summaryOfFindingsTable2 <- 
    page_content %>% 
    html_nodes("table") %>% 
    .[[table_number]] %>% 
    html_table(fill = TRUE)
  
  
  # get raw summary of findings table:
  summaryOfFindingsTable <- 
    page_content %>% 
    html_nodes(".summaryOfFindings") %>% 
    html_nodes("table") %>% 
    .[[table_number]] %>% 
    html_table(fill = TRUE)
  
  
  # find column in which the outcome variables are mentioned:
  col_Outcomes <- 
    summaryOfFindingsTable %>% 
    map(~ str_detect(., pattern = "^Outcome[s]*$")) %>% 
    map_lgl(~ any(. == TRUE)) %>% 
    which()
  
  names(summaryOfFindingsTable)[col_Outcomes] <- "Outcomes"
  
  # add id column:
  summaryOfFindingsTable <- 
    summaryOfFindingsTable %>% 
    mutate(id_measure = row_number()) %>% 
    select(id_measure, everything())
  
  # delete non-rata rows:
  header_rows <- 
    summaryOfFindingsTable %>% 
    filter(str_detect(str_to_lower(Outcomes), "^outcomes?$")) %>% 
    pull(id_measure) %>% 
    max()
  
  # we'll need this table further down below:
  summaryOfFindingsTable <-
    summaryOfFindingsTable %>% 
    mutate(header_row = row_number() <= header_rows) 
  
  summaryOfFindingsTable2 <-
    summaryOfFindingsTable %>%
    filter(id_measure > header_rows)
  
  
  # delete footer:
  identical_cells_across_cols <- 
    map2_lgl(.x = summaryOfFindingsTable2$Outcomes,
             .y = summaryOfFindingsTable2$X2,
             .f = identical)
  
  summaryOfFindingsTable3 <-
    summaryOfFindingsTable2 %>% 
    filter(!identical_cells_across_cols)  
  
  
  # find columns where GRADES are shown:
  col_GRADES <- 
    summaryOfFindingsTable3 %>% 
    map( ~ str_detect(.x, pattern = "⊕|⊝")) %>% 
    map_lgl( ~ any(. == TRUE)) %>% 
    keep(.p = . == TRUE) %>% 
    names()
  
  summaryOfFindingsTable4 <-
    summaryOfFindingsTable3 %>% 
    rename(GRADE := {col_GRADES}) 
  
  
  # find the effect statistic used:
  effect_statistic <- "SMD|RR|OR|MD|[M|m]ean.+[S|s]core"
  
  effect_statistic_per_outcome <- 
    summaryOfFindingsTable4 %>% 
    map_dfc(~ tibble(col = str_extract(., 
                                       pattern = effect_statistic))) %>% 
    pmap_chr(.f = paste) %>% 
    map_chr(~ str_remove_all(., pattern = "NA | NA"))
  
  summaryOfFindingsTable5 <-
    summaryOfFindingsTable4 %>% 
    mutate(effect_statistic = effect_statistic_per_outcome[1],)
  
  
  # find column where number of participants and number of studies are noted for each outcome:
  #n_of_trials_string <- "of [Pp]articipants[[:space:]]*\\([Ss]tudies\\)|"
  n_of_trials_string2 <- "([Ss]tudies)|[Pp]articipants"

  

  col_participants_studies <- 
    summaryOfFindingsTable %>%  # as defined above
    filter(header_row == TRUE) %>% 
    map( ~ str_detect(., pattern = n_of_trials_string2)) %>% 
    map_lgl( ~ any(. == TRUE)) %>% 
    keep(isTRUE) %>% 
    names()
  
  # stop parsing if column is not uniquely defined:
  if (length(col_participants_studies) > 1) {
    warning_df <<- raise_warning(type = "too many columns at `col_participants_studies`",
                             critical = TRUE)
    output <- create_empty_df(names_vec = get_all_colnames())
    output$doi <- review_url
    output$warning <- warning_df$type
    
    writeLines(glue::glue("Stop parsing: {warning_df$type}\n"))
    
    return(output)
  }
  
  
  # function to parse the number of subjects
  # and the number of studies from one string/variable:
  parse_n_subj_n_studies <- function(n_participants_studies,
                                     return = c("subjects", "studies")) {
    
    
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
  
  
  summaryOfFindingsTable6 <-
    summaryOfFindingsTable5 %>% 
    rename(n_participants_studies := {col_participants_studies})  
  
  summaryOfFindingsTable6 <-
    summaryOfFindingsTable6 %>% 
    mutate(n_participants = parse_n_subj_n_studies(n_participants_studies,
                                                   return = "subjects"),
           n_studies = parse_n_subj_n_studies(n_participants_studies,
                                              return = "studies"))
    # separate(col = n_participants_studies, sep = "\\(",
    #          into = c("n_participants", "n_studies"),
    #          remove = FALSE)
  
  
  # find column with effect sizes and CI per outcome:
  #rel_eff_CI_str2 <- "Relative effect[s]*[[[:space:]]*\\(95%[[:space:]]*CI\\)]*"
  rel_eff_CI_str <- "([Rr]elative [Ee]ffect.*95% CI)|95% CI  [^absolute]"
  # NOTE: ABSOLUTE effects are ANTI matched. Only RELATIVE effects!

    
  col_rel_eff_CI <- 
    summaryOfFindingsTable %>% 
    filter(header_row == TRUE) %>% 
    map( ~ str_detect(., pattern = rel_eff_CI_str)) %>% 
    map_lgl( ~ any(. == TRUE)) %>% 
    keep(isTRUE) %>% 
    names()
  
  
  # if the column is not uniquely defined, stop parsing:
  if (length(col_rel_eff_CI) > 1) {
    warning_df <<- raise_warning(type = "too many columns at `col_rel_eff_CI`",
                                 critical = TRUE)
    output <- create_empty_df(names_vec = get_all_colnames())
    output$doi <- review_url
    output$warning <- warning_df$type
    
    writeLines(glue::glue("Stop parsing: {warning_df$type}\n"))
    
    return(output)
  }
  
  if (length(col_rel_eff_CI) == 0)  {
    col_rel_eff_CI <- "NO_RELATIV_EFFECTS_REPORTED"
    summaryOfFindingsTable6 <- 
      summaryOfFindingsTable6 %>% 
      mutate(NO_RELATIV_EFFECTS_REPORTED = NA)
    }
  
  
  
  summaryOfFindingsTable7 <- 
    summaryOfFindingsTable6 %>% 
    rename(relative_effect_95CI := {col_rel_eff_CI}) %>% 
    mutate(CI_lower = str_extract(relative_effect_95CI,
                                  "\\(\\d+\\.*\\d+")) %>% 
    mutate(CI_lower = str_remove(CI_lower, "\\(")) %>% 
    mutate(CI_upper = str_extract(relative_effect_95CI,
                                  "\\d+\\.*\\d+\\)")) %>% 
    mutate(CI_upper = str_remove(CI_upper, "\\)")) %>% 
    mutate(effect_size = str_extract(relative_effect_95CI, "[:alpha:]{1,4}\\s*\\d*\\.?\\d*"))
  
  
  # check if there's a comment column for each outcome:
  col_comments <- "Comments"
  
  # XXX
  col_comments <-
    summaryOfFindingsTable %>%
    filter(header_row == TRUE) %>% 
    map(~ str_detect(., pattern = col_comments)) %>%
    map_lgl( ~ any(. == TRUE)) %>%
    keep(isTRUE) %>%
    names()
  
  if (length(col_comments) == 0) col_comments <- NA
  
  # there's not always a column with comments,
  # so we need to be careful:
  if (!(col_comments %in% c(NA, "", " "))) {
    summaryOfFindingsTable7 <-
      summaryOfFindingsTable7 %>%
      rename(Comments := {col_comments})
  }
  
  
  #re-number if column:
  summaryOfFindingsTable7 <- 
    summaryOfFindingsTable7 %>% 
    mutate(id_measure = row_number()) %>% 
    select(id_measure, everything(), -header_row)
  
  
  output <- summaryOfFindingsTable7
  
  if (first_outcome_only) {
    output <- slice(output, 1)
  }
  
  return(output)
  
}





# get_summary_table_metadata ----------------------------------------------




get_summary_table_metadata <- function(page_content, 
                                       table_number = 1,
                                       verbose = TRUE) {
  
  # stop if critical warning has been raised earlier on:
  if (warning_df$critical == TRUE) {
    
    writeLines("Stopping reading the summary table, as critical warning has been raised earlier on")
    
    output <- create_empty_df(names_vec = get_all_colnames())
    output$doi <- review_url
    output$warning <- warning_df$type
    
    return(output)
    
  }
  
  
  
  
  
  # run only if at least one such tables exists:
  
  nr_summaryOfFindingsTable <- get_nr_of_summary_tables(page_content, verbose = FALSE)
  
  if (nr_summaryOfFindingsTable == 0) {
    print("No (zero) summary tables detected! This function cannot report results. \n")
    output <- stop_parsing_return_empty_df(review_url = 
                                             get_review_url_from_pagecontent(page_content),
                                           error_message = "No summary tables detected.")
    return(output)
  }  
    
  else {
    paste0("Number of summary tables detected: ", nr_summaryOfFindingsTable, "\n")
    
  }
  
  if (table_number > nr_summaryOfFindingsTable) {
    
    print("This table does not exist! Aborting.\n")
    
    output <- 
      tibble(metadata = NA)
    
    return(output)
    
  }
  
  # old version, likely to be buggy in some situations:
  summaryOfFindingsTable2 <- 
    page_content %>% 
    html_nodes("table") %>% 
    .[[table_number]] %>% 
    html_table(fill = TRUE)
  
  
  # get raw summary of findings table:
  summaryOfFindingsTable <- 
    page_content %>% 
    html_nodes(".summaryOfFindings") %>% 
    html_nodes("table") %>% 
    .[[table_number]] %>% 
    html_table(fill = TRUE)
  
  
  main_comparison_of_review <-
    summaryOfFindingsTable %>% 
    select(1) %>% 
    filter(row_number() == 1) %>% 
    pull()
  
  main_comparison_population <- 
    summaryOfFindingsTable %>% 
    select(1) %>% 
    slice(2) %>% 
    pull() %>% 
    str_extract("Patient or population.+Setting") %>% 
    str_remove_all(": |Patient or population|Setting")
  
  main_comparison_setting <-
    summaryOfFindingsTable %>% 
    select(1) %>% 
    slice(2) %>% 
    pull() %>% 
    str_extract("Setting.+Intervention") %>% 
    str_remove_all(": |Setting|Intervention")
  
  main_comparison_comparison_type <-
    summaryOfFindingsTable %>% 
    select(1) %>% 
    slice(2) %>% 
    pull() %>% 
    str_extract("Comparison.+") %>% 
    str_remove_all(":|: |Comparison") %>% 
    str_trim()
  
  
  output <-
    list(
      main_comparison_of_review = main_comparison_of_review,
      main_comparison_population = main_comparison_population,
      main_comparison_setting = main_comparison_setting,
      main_comparison_comparsion_type = main_comparison_comparison_type
    )
  
  if (verbose) print(output)
  
  return(output)
  
}




# concatenate tables -------------------------------------------


concat_tables <- function(page_content,
                          info_page,
                          metadata_review,
                          abstract_review,
                          metadata_summaryTable,
                          summarytable,
                          verbose = TRUE,
                          drop_unused_cols = TRUE) {
  
  
  # stop if critical warning has been raised earlier on:
  if (warning_df$critical == TRUE) {
    
    writeLines("Stopping reading the summary table, as critical warning has been raised earlier on")
    
    output <- create_empty_df(names_vec = get_all_colnames())
    output$doi <- review_url
    output$warning <- warning_df$type
    
    return(output)
    
  }
  
  
  

  # reformat abstract list to df:
  abstract_df <-
    abstract_review %>% 
    map_df(1)
  
  # reformat metadata list to df:
  metadata_review_df <- 
    metadata_review %>% 
    map_df(1)
 
  
  # check if there is (at least) one summary table:
  summary_table_count <- get_nr_of_summary_tables(page_content) 
  
  stopifnot(length(summary_table_count) != 0)  # stop if the object is not well defined
  
  
  if (summary_table_count > 0L) {
    # reformat list to df:
    metadata_summaryTable_df <-
      metadata_summaryTable %>%
      map_df(1)

  }
  
  
  # create empty df, if there are no summary tables:
  if (summary_table_count == 0L) {
    
    summarytable <- create_empty_df(names = get_summarytab_colnames())
    metadata_summaryTable_df <- create_empty_df(names = get_summarytab_metadata_colnames())
    
  }
  
  
  # concatenate summarytable and abstract as main df:
  output_table <-
    summarytable %>% 
    bind_cols(abstract_df)
  
  # add info page:
  output_table2 <- 
    output_table %>% 
    bind_cols(info_page)
    
  
  
  # add review metadata:
  output_table3 <-    
    metadata_review_df %>% 
    bind_cols(output_table2)
  

  
  # then, add summarytable metadata to main df:
  output_table4 <- 
    metadata_summaryTable_df %>% 
    bind_cols(output_table3)
  
  
  # drop unused cols:
  if (drop_unused_cols) {
    output_table5 <- 
      output_table4 %>% 
      select(-starts_with("X"))
  }
  
  
  # sort columns:
  output_table6 <-
    output_table5 %>% 
    select(doi, 
           
           publication_date,
           review_type,
           review_group,
           review_mesh_keywords,
           
           title, 
           authors, 
           publish_type, 
           is_most_recent_version, 
           url_most_most_version,
           starts_with("main_comp"),
           background,
           objectives,
           search_methods,
           selection_criteria,
           data_coll_analysis,
           main_results,
           conclusions,
           everything())
  
  output <- output_table6
  
  if (verbose) print(output)
  
  
  return(output)
  
}





# parse_review ------------------------------------------------------------




parse_review_parts <- function(
  review_url,
  overwrite = TRUE,
  final_table = TRUE,  # should the results be converted from list to df?
  verbose = TRUE, ...) {
  
  
  # check if output file exists, and if so, skip the parsing:
  output_file_exists <- check_if_review_file_exists(review_url)
  
  if (output_file_exists & !overwrite) {
    
    output <- create_empty_df(names_vec = get_all_colnames())
    output$doi <- review_url
    output$warning <- "Output file already exists"
    
    writeLines(glue::glue("Output file already exists. Skipping."))
    
    return(output)
    
  }
  
  
  # else, start normal work:
  init()
  
  
 
  review <- list()
  
  if (verbose) cat(paste0("**Starting to parse the review with this doi: ", review_url, "**\n"))
  
  # parse info page:
  review$info_page <- get_review_info_page(review_url)
  
  # read html page:
  review$page_content <- read_html(review_url)  
  
  # read metadata:
  review$metadata <- get_review_metadata(review$page_content)
  
  # check if there' a critical warning, in which case we stop parsing:
  if (warning_df$critical == TRUE) {
    
    # stop parsing, document error:
    output <- create_empty_df(names_vec = get_all_colnames())
    output$doi <- review_url
    output$warning <- warning_df$type
    
    writeLines(glue::glue("STOPPING parsing. Critical warning has been raised: {warning_df$type}"))
    
    return(output)
    
  } else {
    # begin regular parsing:

  # parse abstract
  review$abstract <- get_abstract(review$page_content) %>% 
    map(str_trim)
  
  #undebug(get_summary_table)
  

  
  review$summaryTable_count <- get_nr_of_summary_tables(review$page_content)
  
  # possibly_get_summary_table <- possibly(get_summary_table,
  #                                        otherwise = stop_parsing_return_empty_df(
  #                                          review_url = review_url,
  #                                          error_message = "Error in `get_summary_table`"
  #                                        ))
  
  review$summarytable1 <- get_summary_table(review$page_content)
  
  #review$summarytable1 <- possibly_get_summary_table(review$page_content)
  review$summaryTable_metadata <- get_summary_table_metadata(review$page_content)
  
  
  #undebug(concat_tables)
  review$final_table <- concat_tables(
    info_page = review$info_page,
    page_content = review$page_content,
    summarytable = review$summarytable1,
    metadata_review =  review$metadata,
    abstract_review = review$abstract,
    metadata_summaryTable = review$summaryTable_metadata)
  
  output <- review
  
  if (final_table) output <- review$final_table
  
  
  
  if (verbose) {
    print(output)
    writeLines("\n")
    writeLines(paste0("Review has been parsed.\n"))
  }
  

  }
    
  
  return(output)
  
}


write_parsed_review_to_file <- function(review_url,
                                        review,
                                        output_dir = "output",
                                        overwrite = TRUE){
  
    
  output_file_exists <- check_if_review_file_exists(review_url)
    
    # check if we should overwrite it, otherwise stop (if output  file already exists):
    if (output_file_exists & !overwrite) {
      writeLines(glue::glue("Output file exists. NOT overwriting: {file_path}\n"))
    } else {
      write_csv(x = review, 
                file = file_path)
      writeLines(glue::glue("Results have been saved to file: {file_path}\n"))
    }
  
    return(review)
  
}



# check_if_output_file_exists <- function(review, output = "output") { 
#   
#   # check if output file exists:
#   file_path <- glue::glue("{output_dir}/{review$metadata$doi}.csv") %>% 
#     sanitize_review_url()
#   output_file_exists <- file.exists(file_path)
#  
#   }




# write_output_to_disk <- function(review, output = "output",
#                                  overwrite = TRUE) {
#   
#   output_file_exists <- check_if_output_file_exists(review, output)
#   
#   # check if we should overwrite it, otherwise stop (if output  file already exists):
#   if (output_file_exists == TRUE & overwrite == FALSE) {
#     writeLines(glue::glue("Output file exists. NOT overwriting: {file_path}\n"))
#     
#     # otherwise, start parsing
#   }
# }


parse_review <- function(review_url,
                         overwrite = TRUE) {
  
  review_url_cochrane <- build_cochrane_url_from_doi(review_url)
  
  review_parsed_parts <- parse_review_parts(review_url_cochrane, 
                                            overwrite = overwrite)
  write_parsed_review_to_file(review_url = review_url_cochrane,
                              review = review_parsed_parts,
                              overwrite = overwrite)
  
}

