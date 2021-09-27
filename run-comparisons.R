source("funs/load-libs.R")

load_libs()

files_to_source <- list.files("funs/", pattern = ".R")
files_to_source_w_path <- paste0("funs/", files_to_source)
#files_to_source_w_path
sapply(files_to_source_w_path, source)

init_flog()

source("funs/compare-human-machine-extractions.R")
debug(compare_human_machine_extractions)

out <- compare_human_machine_extractions()
