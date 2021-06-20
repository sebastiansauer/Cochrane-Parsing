# See config.yaml for specs:

source("R/load-libs.R")


files_to_source <- list.files("funs/")
files_to_source_w_path <- paste0("funs/", files_to_source)
#files_to_source_w_path
sapply(files_to_source_w_path, source)

dois_to_be_parsed <- init()

flog.trace("Sourcing funs.")

reviews <- parse_dois(dois_to_be_parsed)
