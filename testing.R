source("funs/helper-funs.R")
source("funs/funs-parse-cochrane.R")
source("funs/get_summary_table.R")
source("funs/parse-review-parts.R")


bad_url2 <- "https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD004123.pub4"

review_url <- bad_url2
#review_url <- reviews_ss1
review_url
review_url_cochrane <- build_cochrane_url_from_doi(review_url)
review_url_cochrane


safe_page_content <- safely_read_html(review_url_cochrane)  

safe_page_content$error


#page_content <- safe_page_content$result

# parse info page, must be sanitized! (see function for that):
info_page <- get_review_info_page(review_url_cochrane)

# we build up the output of the function step by step
# first step: add info page results to output
output <- 
  info_page



# read metadata:

metadata <- get_review_metadata(safe_page_content$result)

# now add the metadata results to the output object:
output <-
  output %>% 
  bind_cols(metadata)

# read abstract:
abstract <- get_abstract(safe_page_content$result)

# add the abstract results to the output object:
output <-
  output %>% 
  bind_cols(abstract)


safely_get_nr_of_summary <- safely(get_nr_of_summary_tables)
safe_summaryTable_count <- safely_get_nr_of_summary(safe_page_content$result)

# on error:
if (!is.null(safe_summaryTable_count$error)) {
  
  summaryTable_count <- 0
  summarytable <- create_empty_df(names_vec = get_summarytab_colnames())
  
raise_warning(type = "Zero summary tables detected,
                                  critical = FALSE")
  
  return(output)
  
  
}  # no error in get_nr_of_summary, then read summary table:


summaryTable_count <- safe_summaryTable_count$result
#safe_get_summary_table <- safely(get_summary_table)
#safe_summarytable1 <- safe_get_summary_table(page_content)
#
summarytable <- 
get_summary_table(page_content = safe_page_content$result,
                  table_number = 1)

summarytable <- 
  1:summaryTable_count %>% 
  map_dfr(~ get_summary_table(page_content = safe_page_content$result,
                              table_number = .),
          .id = "SoF_table_number")





# bind summary tables results to output object
output <- 
  output %>% 
  bind_cols(summarytable)

