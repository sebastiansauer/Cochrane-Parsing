# See config.yaml for specs:

source("funs/load-libs.R")

load_libs()

files_to_source <- list.files("funs/", pattern = ".R")
files_to_source_w_path <- paste0("funs/", files_to_source)
#files_to_source_w_path
sapply(files_to_source_w_path, source)

init_flog()

dois_to_be_parsed <- init_dois_to_be_extracted()
dois_to_be_parsed 


undebug(parse_dois)
reviews_ss <- parse_dois(dois_to_be_parsed[1])


# debug(parse_review)
# 
# parse_review(dois_to_be_parsed[[1]])
