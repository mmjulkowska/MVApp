# variables <- colnames(input$data)
library(shiny)
library(shinythemes)
library(plotly)
library(doBy)
library(reshape)
library(plotrix)
library(corrplot)
library(tidyverse)
library(multcompView)

# install the missing packages - this can last some time for the first time you run the App
list.of.packages <- c("ggplot2", "shinythemes", "plotly", "doBy", "reshape", "reshape2", "plotrix", "corrplot", "tiryverse", "FactoMineR", "devtools", "factoextra", "missMDA", "dplyr" )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

# Load all the libraries 
library("shiny")
library("shinythemes")
library("plotly")
library("doBy")
library("reshape")
library("plotrix")
library("corrplot")
library("tidyverse")
library("reshape2")
library("FactoMineR")
library("devtools")
library("factoextra")
library("missMDA")
library("dplyr")

