library(dplyr)
library(tidyverse)
library(DT)
library(shinythemes)
library(shinydashboard)
library(ggplot2)



#load data
phth<-readRDS("Phthalate_Exploration.Rds")

factors<-c("Age_Class", "Gender", "Race_Ethnicity", "Cancer", "Thyroid_Issues", "Diabetes")

phth1<-phth%>% dplyr::select(37,65:71)
colnames(phth1)<-c("Age of Participant", "Total_Pht", "Race_Ethnicity", "Cancer", "Thyroid_Issues", "Diabetes", "Age_Class", "Gender")

conditions <- c("18-29 yoa", "30-49 yoa", "50-65 yoa", "65 and up yoa", "Mexican American","Hispanic","Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian", "Non-Hispanic Multiracial", "Male", "Female") #"Positive diagnosis", "Negative diagnosis", "Unknown diagnosis", "Borderline diagnosis")

