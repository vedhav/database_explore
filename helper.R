library(RMySQL)
library(dbx)
library(tidyverse)
library(glue)
library(shiny)
library(shinyWidgets)
library(DT)

default_host_ip <- "127.0.0.1"
default_host_user_name <- "root"
default_host_password <- "admin@123"
default_db_name <- "information_schema"
default_db_port <- 3306
default_databases <- c("information_schema", "mysql", "performance_schema", "phpmyadmin")


selectDbQuery <- function(query, params = NULL, hostUserName = default_host_user_name, hostPassword = default_host_password, hostIP = default_host_ip, dbPort = default_db_port, dbName = default_db_name) {
  if(query =="") return(tibble())
  conn <- dbxConnect(adapter = "mysql", user = hostUserName, password = hostPassword, host = hostIP, port = dbPort, dbname = dbName)
  result <- NULL
  if(!is.null(params)) {
    #adding support for '\' - extending dbx
    for(field in 1:length(params))
    {
      if(typeof(params[[field]]) == 'character') {
        params[[field]] <- gsub("[\\]", "\\\\\\\\", params[[field]])
      }
    }
    result <- suppressWarnings(dbxSelect(conn, query, params))
  } else {
    result <- suppressWarnings(dbxSelect(conn, query))
  }
  #To remove the unnecessary results that we get from executing a stored procedure
  while(RMySQL::dbMoreResults(conn)) {
    try(RMySQL::dbNextResult(conn))
  }
  on.exit(dbxDisconnect(conn))
  return(as_tibble(result))
}


