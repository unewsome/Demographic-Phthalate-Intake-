library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(haven)

#load data
phth<-readRDS("Phthalate_Exploration.Rds")

factors<-c("Age_Class", "Gender", "Race_Ethnicity", "Cancer", "Thyroid_Issues", "Diabetes")
# factors1<-c("Blank", "AgeClass", "Gender", "Race/Ethnicity", "Cancer", "Thyroid Issues", "Diabetes?")

phth1<-phth%>% dplyr::select(37,65:71)
colnames(phth1)<-c("Age of Participant", "Total_Pht", "Race_Ethnicity", "Cancer", "Thyroid_Issues", "Diabetes", "Age_Class", "Gender")  

