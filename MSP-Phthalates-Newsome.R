library(tidyverse)
library(haven)
library(dplyr)

PTH0910<-read_xpt('C:/Users/unews/Dropbox/NSS/Demographic-Phthalate-Intake-/Data/PHTHTE_F_09-10.XPT')
PTH1516<-read_xpt('C:/Users/unews/Dropbox/NSS/Demographic-Phthalate-Intake-/Data/PHTHTE_I_15-16.XPT')
reyes1314<-read_csv('C:/Users/unews/Dropbox/NSS/Demographic-Phthalate-Intake-/Data/Reyes_EHP_Phthalates_US_MCR.csv')
reyes_def<-read_csv('C:/Users/unews/Dropbox/NSS/Demographic-Phthalate-Intake-/Data/Reyes_EHP_Phthalates_US_MCR_definitions.csv')
reyes_metabolites<-read_csv('C:/Users/unews/Dropbox/NSS/Demographic-Phthalate-Intake-/Data/Reyes_EHP_Phthalates_US_metabolites.csv')
BMX1516<-read_xpt('C:/Users/unews/Dropbox/NSS/Demographic-Phthalate-Intake-/Data/BMX_I_1516.XPT')
CBQ1516<-read_xpt('C:/Users/unews/Dropbox/NSS/Demographic-Phthalate-Intake-/Data/CBQ_I_1516.XPT')
DEMO1516<-read_xpt('C:/Users/unews/Dropbox/NSS/Demographic-Phthalate-Intake-/Data/DEMO_I_1516.XPT')
DIQ1516<-read_xpt('C:/Users/unews/Dropbox/NSS/Demographic-Phthalate-Intake-/Data/DIQ_I_1516.XPT')
INQ1516<-read_xpt('C:/Users/unews/Dropbox/NSS/Demographic-Phthalate-Intake-/Data/INQ_I_1516.XPT')
MCQ1516<-read_xpt('C:/Users/unews/Dropbox/NSS/Demographic-Phthalate-Intake-/Data/MCQ_I_1516.XPT')
SSMHHT1516<-read_xpt('C:/Users/unews/Dropbox/NSS/Demographic-Phthalate-Intake-/Data/SSMHHT_I_1516.XPT')


#Merge groups to find relative information.  Focus on 2015 - 2016 data and specific columns of each data frame

pht_demo<-merge(PTH1516,DEMO1516,all.x=FALSE)
pht_demo_bmi<-merge(pht_demo,BMX1516,all.x=FALSE)
pht_demo_bmi_cbq<-merge(pht_demo_bmi,CBQ1516,all.x=FALSE)
pht_demo_bmi_cbq_diq<-merge(pht_demo_bmi_cbq,DIQ1516,all.x=FALSE)
pht_demo_bmi_cbq_diq_inq<-merge(pht_demo_bmi_cbq_diq,INQ1516,all.x=FALSE)
pht_all<-merge(pht_demo_bmi_cbq_diq_inq,MCQ1516,all.x=FALSE)

pht_all%>%
  select()