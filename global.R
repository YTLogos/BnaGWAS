options(warn = -1)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(shinydisconnect)
library(DT)
library(future.apply)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(colourpicker)
library(Cairo)
library(openxlsx)
library(data.table)
library(gtools)
library(writexl)
library(qqman)
library(reshape2)
library(shinyvalidate)
source("script/mod_gwas_ui.R")
source("script/mod_gwas_server.R")
source("script/readNewData.R")
source("script/gwas_emmax_cov.R")
source("script/extraxt_gene.R")
source("script/manhattan_qq_plot.R")
source("script/mod_about.R")
source("script/mod_documentation.R")

load("./data/Ref.gene.info.RData")

footerTagList <- list(
  tags$footer(id = "myFooter",
              shiny::includeHTML("www/md/footer.html")
  )
)

## shinyvalidate-detect if trait name contain special characters
char_detect <- function(input){
  values <- c(" ","/","@","#","!","&","%")
  index <- str_detect(input,values)
  if('TRUE' %in% index) "The Trait Name CAN NOT contain special characters (/!\\% and whitespace etc)!"
}

## To warn user that the phenotype data have missing values
# 
# missing_value_print <- function(input){
#   missing_value <- sum(is.na(input))
#   if(missing_value>0) print(paste0("You phenotype dataset has ",missing_value," missing values, this may affect GWAS results!"))
# }
