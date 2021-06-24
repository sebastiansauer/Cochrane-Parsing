init_flog <- function() {  
  

  flog.appender(appender.file('parse-dois.log'))
  flog.info("Starting init.")
 
  
  flog.appender(appender.file("funs-called.log"), 
                name = "funlog")
  
  stopifnot(file.exists("config.yaml"))
  
  config <<- read_yaml("config.yaml")
  if (config$verbose) print("Config file has been read.")
  
  flog.threshold(config$flog_threshold)
  if (config$rm_old_logfile) {
    system("rm parse-dois.log")
    system("rm funs-called.log")
    flog.info("Removing existing log file. Starting fresh log file.")
  }
  
  flog.trace("Log files declared.")
  flog.info("init_flog", name = "funlog")
  flog.trace("Reading config.caml")
  flog.info("Object 'config' defined as global variable")
  
  if (config$verbose) print("Config files have been declared.")
  if (config$verbose) print("`init_flog` has been run.")  

  
}
