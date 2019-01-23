library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(haven)

#load data
phth<-readRDS("Phthalate_Exploration.Rds")

factors<-c("AgeClass", "Gender", "RaceEthnicity", "Cancer", "ThyroidIssues", "Diabetes")
factors1<-c("Blank", "AgeClass", "Gender", "RaceEthnicity", "Cancer", "Thyroid Issues", "Diabetes?")

phth1<-phth%>% dplyr::select(37,65:71)
colnames(phth1)<-c("Age of Participant", "TotalPht", "RaceEthnicity", "Cancer", "ThyroidIssues", "Diabetes", "AgeClass", "Gender")  

