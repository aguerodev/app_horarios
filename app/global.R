library(conflicted)
library(shiny)
library(bslib)
library(DBI)
library(auth0)
library(mirai)
library(lgr)
library(dbplyr)

# configuracion para logs
#
logger <- get_logger_glue("app")
logger$set_threshold("debug")
log_file <- sprintf("logs/log_%s.txt", format(Sys.time(), "%Y-%m-%d_%H:%M:%S"))
logger$add_appender(AppenderFile$new(log_file), name = log_file)


daemons(1)
onStop(function() daemons(0))


everywhere({
  library(GA)
  library(dplyr)
  library(tidyr)
  library(stringr)
})


options(box.path = ".")
box::use(modules / mod_select)
box::use(modules / mod_table)
box::use(modules / mod_ga)
