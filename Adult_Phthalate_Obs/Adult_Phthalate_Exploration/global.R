library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(dbplyr)
library(magrittr)
library(dplyr)
library(haven)

#load data
phth<-readRDS("C:/Users/unews/Dropbox/NSS/Demographic-Phthalate-Intake-/Adult_Phthalate_Obs/Adult_Phthalate_Exploration/Phthalate_Exploration.Rds")

factors<-c("Age", "Gender", "Race/Ethnicity", "Cancer?", "Thyroid Issues?", "Diabetes?")
factors1<-c("Blank", "Age", "Gender", "Race/Ethnicity", "Cancer?", "Thyroid Issues?", "Diabetes?")

phth1<-phth%>% select(37,65:71)
colnames(phth1)<-c("Age of Participant", "Total Pht", "Race/Ethnicity", "Cancer?", "Thyroid Issues?", "Diabetes?", "Age Class", "Gender")  

