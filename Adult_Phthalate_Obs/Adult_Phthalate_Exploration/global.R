library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(dbplyr)
library(magrittr)
library(dplyr)
library(haven)

#load data
phth<-readRDS("Phthalate_Exploration.Rds")
