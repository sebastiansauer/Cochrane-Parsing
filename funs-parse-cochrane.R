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




# get-review-metadata -----------------------------------------------------




get_review_metadata <- function(page_content, verbose = TRUE){
  
  # parse title:
  title_publication <- page_content %>% 
    html_nodes(".publication-title") %>% 
    html_text()
  
  
  # get doi:
  review_doi <-
    page_content %>% 
    html_nodes(".doi-header") %>% 
    html_text() %>% 
    str_remove_all(pattern = " ")
  
  
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
  
  
  
  # check if its the most recent version:
  chr_is_most_recent_version <-
    page_content %>% 
    html_nodes(".newest-version-banner") %>% 
    html_text() %>% 
    str_remove_all(pattern = "^[[:blank:]]+|[[:blank:]]+$") 
  
  
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
                 is_paywalled = is_paywalled
                 )
  
  if (verbose) print(output)
  
  return(output)
}




# get_abstract ------------------------------------------------------------



get_abstract <- function(page_content, verbose = TRUE) {
  
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
                              first_outcome_only = TRUE,
                              verbose = TRUE) {
  
  # run only if at least one such tables exists:
  
  nr_summaryOfFindingsTable <- get_nr_of_summary_tables(page_content)
  

  if ((table_number > nr_summaryOfFindingsTable) | 
      (nr_summaryOfFindingsTable == 0)) {
    
    output <- 
      tibble(Outcome = NA)
    
    print("As no summary findings tabls were detected, I'm stopping the collection of summary tables.")
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
    filter(str_detect(str_to_lower(Outcomes), "outcomes?$")) %>% 
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
  n_of_trials_string <- "of [Pp]articipants[[:space:]]*\\([Ss]tudies\\)|"
  n_of_trials_string2 <- "([Ss]tudies)|[Pp]articipants"

  

  col_participants_studies <- 
    summaryOfFindingsTable %>%  # as defined above
    filter(header_row == TRUE) %>% 
    map( ~ str_detect(., pattern = n_of_trials_string2)) %>% 
    map_lgl( ~ any(. == TRUE)) %>% 
    keep(isTRUE) %>% 
    names()
  
  summaryOfFindingsTable6 <-
    summaryOfFindingsTable5 %>% 
    rename(n_participants_studies := {col_participants_studies}) %>% 
    mutate(n_participants = str_extract(n_participants_studies, 
                                        "\\d+[[:blank:]]*[Pp]articipants"),
           n_studies = str_extract(n_participants_studies,
                                   "\\d+[[:blank:]]* [Ss]tud(y|ies)"))
    # separate(col = n_participants_studies, sep = "\\(",
    #          into = c("n_participants", "n_studies"),
    #          remove = FALSE)
  
  
  # find column with effect sizes and CI per outcome:
  rel_eff_CI_str2 <- "Relative effect[s]*[[[:space:]]*\\(95%[[:space:]]*CI\\)]*"
  rel_eff_CI_str <- "\\(95%[[:space:]]*CI\\)]*"

    
  col_rel_eff_CI <- 
    summaryOfFindingsTable %>% 
    map( ~ str_detect(., pattern = rel_eff_CI_str)) %>% 
    map_lgl( ~ any(. == TRUE)) %>% 
    keep(isTRUE) %>% 
    names()
  
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
  
  col_comments <-
    summaryOfFindingsTable %>%
    map(~ str_detect(., pattern = col_comments)) %>%
    map_lgl( ~ any(. == TRUE)) %>%
    keep(isTRUE) %>%
    names
  
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
    select(id_measure, everything())
  
  
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
  
  # run only if at least one such tables exists:
  
  nr_summaryOfFindingsTable <- get_nr_of_summary_tables(page_content, verbose = FALSE)
  
  if (nr_summaryOfFindingsTable == 0) print("No (zero) summary tables detected! This function cannot run. \n")
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
                          summarytable,
                          metadata_review,
                          abstract_review,
                          metadata_summaryTable,
                          verbose = TRUE,
                          drop_unused_cols = TRUE) {
  
  

  
  

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
  summarytable2 <-
    summarytable %>% 
    bind_cols(abstract_df)
  
  
  # add review metadata:
  summarytable3 <-    
    metadata_review_df %>% 
    bind_cols(summarytable2)
  

  
  # then, add summarytable metadata to main df:
  summarytable4 <- 
    metadata_summaryTable_df %>% 
    bind_cols(summarytable3)
  
  
  # drop unused cols:
  if (drop_unused_cols) {
    summarytable4 <- 
      summarytable4 %>% 
      select(-starts_with("X"))
  }
  
  
  # sort columns:
  summarytable5 <-
    summarytable4 %>% 
    select(doi, title, authors, publish_type, 
           is_most_recent_version, url_most_most_version,
           starts_with("main_comp"),
           background,
           objectives,
           search_methods,
           selection_criteria,
           data_coll_analysis,
           main_results,
           conclusions,
           everything())
  
  output <- summarytable5
  
  if (verbose) print(output)
  
  
  return(output)
  
}





# parse_review ------------------------------------------------------------




parse_review <- function(
  review_url, 
  final_table = TRUE,  # should the results be converted from list to df?
  save_to_file = TRUE,  # should results be saved to disk?
  output_dir = "output",  # folder where to write output to
  overwrite = FALSE,  # if FALSE, existing results file will not be overwritten
  verbose = TRUE, ...) {
  
  
  # check if output file exists:
  file_path <- glue::glue("{output_dir}/{review_url}.csv") %>% 
    str_remove("http[s]*://[dx.]*doi.org/10.1002/")
  output_file_exists <- file.exists(file_path)
  
  # check if we should overwrite it, otherwise stop (if output  file already exists):
  if (output_file_exists == TRUE & overwrite == FALSE) {
    writeLines(glue::glue("Output file exists. NOT overwriting: {file_path}\n"))
    
    output <- create_empty_df(names_vec = get_all_colnames())
    output$doi <- review_url
  } else {
  review <- list()
  
  if (verbose) cat(paste0("**Starting to parse the review with this doi: ", review_url, "**\n"))
  
  review$page_content <- read_html(review_url)  
  review$metadata <- get_review_metadata(review$page_content)
  
  review$abstract <- get_abstract(review$page_content)
  
  #undebug(get_summary_table)
  review$summaryTable_count <- get_nr_of_summary_tables(review$page_content)
  review$summarytable1 <- get_summary_table(review$page_content)
  review$summaryTable_metadata <- get_summary_table_metadata(review$page_content)
  
  #undebug(concat_tables)
  review$final_table <- concat_tables(
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
  
  if (save_to_file) {
    if (length(output_dir) == 0) stop("Please specify output directory.")

    # use doi to check whether output file exists
    file_path <- glue::glue("{output_dir}/{review$metadata$doi}.csv") %>% 
      str_remove("http[s]*://[dx.]*doi.org/10.1002/")
    
    # check if output file exists:
    output_file_exists <- file.exists(file_path)
    
    # check if we should overwrite it, otherwise stop (if output  file already exists):
    if (output_file_exists == TRUE & overwrite == FALSE) {
      writeLines(glue::glue("Output file exists. NOT overwriting: {file_path}\n"))
    } else {
    write_csv(x = review[["final_table"]], 
              file = file_path, ...)
    writeLines(glue::glue("Results have been save to file: {file_path}\n"))
    }
  }
  }
  
  return(output)
  
}
