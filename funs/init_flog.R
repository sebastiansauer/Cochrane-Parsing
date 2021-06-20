init_flog <- function() {  
  

  flog.appender(appender.file('parse-dois.log'))
  flog.info("Starting init.")
  flog.trace("libs loaded.")
  
  flog.appender(appender.file("funs-called.log"), 
                name = "funlog")
  flog.info("init_flog", name = "funlog")
  
  
  flog.trace("Reading config.caml")
  stopifnot(file.exists("config.yaml"))
  flog.info("Object 'config' defined as global variable")
  
  config <<- read_yaml("config.yaml")
  
  flog.threshold(config$flog_threshold)
  if (config$rm_old_logfile) {
    system("rm parse-dois.log")
    system("rm funs-called.log")
    flog.info("Removing existing log file. Starting fresh log file.")
  }
  

  
}
