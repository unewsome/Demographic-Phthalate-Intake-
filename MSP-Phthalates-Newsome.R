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

pht_all2<-pht_all%>%
  select(1:34,37,38,40,41,90,106,107,111,113,114,116:120,173,186,191,208,210,220,226,228,230,234,235,248,250,258)

total_pht<-pht_all2%>%
  mutate(total_pht=URXCNP+URXCOP+URXECP+URXMBP+URXMC1+URXMEP+URXMHP+URXMNP+URXMOH+URXMZP+URXMIB+URXHIBP+URXMCOH+URXMHBP+URXMHNC)

demo_race<-total_pht%>%
  group_by(RIDRETH1,RIDRETH3)%>%
  summarise(mean_pht=mean(total_pht,na.rm=TRUE))
  # mutate(Race_Ethncity=["Mexican American","Hispanic","Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian", "Non-Hispanic Multi-racial"])
  
demo_race$Race_Ethnicity<-c("Mexican American","Hispanic","Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian", "Non-Hispanic Multi-racial")


ggplot(demo_race, aes(x=Race_Ethnicity,y=mean_pht))+
  geom_col()+ xlab('Race/Ethnicity') +
  ylab('Mean Phthalate Metabolite in Urine (ng/mL') +
  ggtitle('           Comparison of Phthalate Metabolite Excretion by Race/Ethnicity') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


demo_race_age<-total_pht%>%
  group_by(RIDRETH1,RIDRETH3,RIAGENDR)%>%
  summarise(mean_pht=mean(total_pht,na.rm=TRUE))


demo_race_age$Race_Ethnicity_Gender<-c("Mexican American Male","Mexican American Female","Hispanic Male","Hispanic Female", "Non-Hispanic White Male", "Non-Hispanic White Female" , "Non-Hispanic Black Male",  "Non-Hispanic Black Female", "Non-Hispanic Asian Male","Non-Hispanic Asian Female", "Non-Hispanic Multi-racial Male", "Non-Hispanic Multi-racial Female")


ggplot(demo_race_age, aes(x=Race_Ethnicity_Gender,y=mean_pht, fill=RIAGENDR))+
  geom_col()+ xlab('Race/Ethnicity and Gender') +
  ylab('Mean Phthalate Metabolite in Urine (ng/mL') +
  ggtitle('           Comparison of Phthalate Metabolite Excretion by Race/Ethnicity and Gender') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

