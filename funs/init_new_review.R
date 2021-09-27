init_new_review <- function() {
  
  flog.info("init_new_review", name = "funlog")  
  
  # initialize logging:
  if (exists("warning_df")) rm(warning_df, inherits = TRUE)
  raise_warning(type = "INIT",
                critical = FALSE,
                write_to_disk = FALSE) 
  
  
}