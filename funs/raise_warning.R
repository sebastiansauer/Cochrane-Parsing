

raise_warning <- function(type,
                          critical = FALSE,
                          write_to_disk = TRUE){
  
  flog.info("raise_warning", name = "funlog")
  
  if (!exists("warning_df"))
    warning_df <-
      tibble(
        type = "Init",
        date = Sys.Date(),
        time = Sys.time(),
        critical = FALSE
      )
  
  
  warning_df_new_row <-
    tibble(
      type = unlist(type),
      date = Sys.Date(),
      time = Sys.time(),
      critical = critical
    )
  
  warning_df <<-
    warning_df %>% 
    bind_rows(warning_df_new_row)
  
  
  
  if (write_to_disk) write_csv(warning_df,
                               file = "logging/warnings.csv")
  
  return(warning_df)
  
}


