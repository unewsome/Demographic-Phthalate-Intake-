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


demo_race_gen<-total_pht%>%
  group_by(RIDRETH1,RIDRETH3,RIAGENDR)%>%
  summarise(mean_pht=mean(total_pht,na.rm=TRUE))


demo_race_gen$Race_Ethnicity_Gender<-c("Mexican American Male","Mexican American Female","Hispanic Male","Hispanic Female", "Non-Hispanic White Male", "Non-Hispanic White Female" , "Non-Hispanic Black Male",  "Non-Hispanic Black Female", "Non-Hispanic Asian Male","Non-Hispanic Asian Female", "Non-Hispanic Multi-racial Male", "Non-Hispanic Multi-racial Female")


ggplot(demo_race_gen, aes(x=Race_Ethnicity_Gender,y=mean_pht, fill=RIAGENDR))+
  geom_col()+ xlab('Race/Ethnicity and Gender') +
  ylab('Mean Phthalate Metabolite in Urine (ng/mL') +
  ggtitle('           Comparison of Phthalate Metabolite Excretion by Race/Ethnicity and Gender') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

demo_gen<-total_pht%>%
  group_by(RIAGENDR)%>%
  summarise(mean_pht=mean(total_pht,na.rm=TRUE))


demo_gen$Gender<-c("Male", "Female")

ggplot(demo_gen, aes(x=Gender,y=mean_pht))+
  geom_col()+ xlab('Gender') +
  ylab('Mean Phthalate Metabolite in Urine (ng/mL') +
  ggtitle('           Comparison of Phthalate Metabolite Excretion by Gender') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Race_BMI<-total_pht%>%
  group_by(RIDRETH1,RIDRETH3)%>%
  summarise(mean_BMI=mean(BMXBMI,na.rm=TRUE))

Race_BMI$Race_Ethnicity<-c("Mexican American","Hispanic","Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian", "Non-Hispanic Multi-racial")

ggplot(Race_BMI, aes(x=Race_Ethnicity,y=mean_BMI))+
  geom_col()+ xlab('Race/Ethnicity and Gender') +
  ylab('Mean BMI (kg/m^2') +
  ggtitle('           Comparison of BMI by Race/Ethnicity and Gender') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Cancer<-total_pht%>%
  group_by(MCQ220)%>%
  summarise(mean_pht=mean(total_pht,na.rm=TRUE))

Cancer$Diagnosis<-c("Yes", "No", "Don't know", "Missing")

ggplot(Cancer, aes(x=Diagnosis,y=mean_pht))+
  geom_col()+xlab("Incidence of Cancer Diagnosis")+ylab('Mean Phthalate Metabolite in Urine (ng/mL')+
    ggtitle(      'Comparison of Phthalate Metabolite Excretion by Cancer Diagnosis')


Cancer_race<-total_pht%>%
  group_by(RIDRETH1,RIDRETH3,MCQ220)%>%
  summarise(mean_pht=mean(total_pht,na.rm=TRUE))%>%
  filter(MCQ220=="1")

Cancer_race$Race_Ethnicity<-c("Mexican American","Hispanic","Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian", "Non-Hispanic Multi-racial")

ggplot(Cancer_race, aes(x=Race_Ethnicity,y=mean_pht))+
         geom_col()+ xlab('Race/Ethnicity and Positive Cancer Diagnosis')+ylab('Mean Phthalate Metabolite in Urine (ng/mL)')+
         ggtitle('           Comparison of Phalate Metabolism by Race/Ethnicity and Positive Cancer Diagnosis') +
         theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Ratio of cancer diagnosis per population
# Cancer_ratio_demo<-total_pht%>%
#   group_by(RIDRETH1,RIDRETH3,MCQ220)%>%
#   mutate(Race_Ethnicity=if_else(RIDRETH3=="1","Mexican American",Race_Ethnicity),
#          Race_Ethnicity=if_else(RIDRETH3=="2", "Hispanic",Race_Ethnicity),
#          Race_Ethnicity=if_else(RIDRETH3=="3", "Non-Hispanic White", Race_Ethnicity),
#          Race_Ethnicity=if_else(RIDRETH3=="4", "Non-Hispanic Black",Race_Ethnicity),
#          Race_Ethnicity=if_else(RIDRETH1=="5"& RIDRETH3=="6","Non-Hispanic Asian",Race-Race_Ethnicity),
#          Race_Ethnicity=if_else(RIDRETH1=="5"&RIDRETH3=="7", "Non-Hispanic Multiracial",Race_Ethnicity))

Race_tally<-table(total_pht$RIDRETH3)
Race_tally

totalMA<-Race_tally[1] #636
totalH<-Race_tally[2] #404
totalW<-Race_tally[3] #942
totalAA<-Race_tally[4] #746
totalAs<-Race_tally[5] #321
totalMr<-Race_tally[6] #156

# Cancer_ratio_demo<-total_pht%>%
#   group_by(RIDRETH1,RIDRETH3,MCQ220)%>%
#   mutate(Race_Ethnicity = case_when(   #why is it not adding a column
#     RIDRETH1==1~"Mexican American",
#     RIDRETH1==2~"Hispanic",
#     RIDRETH1==3~"Non-Hispanic White",
#     RIDRETH1==4~"Non-Hispanic Black",
#     RIDRETH1==5&RIDRETH3==6~"Non-Hispanic Asian",
#     RIDRETH1==5&RIDRETH3==7~"Non-Hispanic Multiracial"))
  

# 
# Cancer_ratio_demo1<-Cancer_ratio_demo%>%
#   group_by(Race_Ethnicity, MCQ220)%>%
#   mutate(Cancer_Diagnosis = case_when(
#     MCQ220==1~"Yes",
#     MCQ220==2~"NO",
#     MCQ220==9~"Unknown",
#     MCQ220=="NA"~"Missing")) #Not inputting variable
# 



# Cancer_race_ratio<-Cancer_ratio_demo1%>%
#   group_by(Race_Ethnicity,Cancer_Diagnosis)%>%
#   summarise(mean_pht=mean(total_pht,na.rm = TRUE))%>%
#   mutate(total_demo=sum())
# 
# Cancer_ratio_demo1%>%
#     mutate(Cancer_Diagnosis = case_when(
#       MCQ220==1~1,
#       MCQ220==2~2,
#       MCQ220==9~3,
#       is.na(MCQ220)~3
#   ))
# 
# 
# Cancer_ratio_demo1%>%
#   group_by(Race_Ethnicity,Cancer_Diagnosis)%>%
#   summarise(countc=count(Cancer_Diagnosis))
 
total_pht%>%
    mutate(Race_Ethnicity = case_when(   
      RIDRETH1==1~"Mexican American",
      RIDRETH1==2~"Hispanic",
      RIDRETH1==3~"Non-Hispanic White",
      RIDRETH1==4~"Non-Hispanic Black",
      RIDRETH1==5&RIDRETH3==6~"Non-Hispanic Asian",
      RIDRETH1==5&RIDRETH3==7~"Non-Hispanic Multiracial"))%>%
  mutate(Cancer_Diagnosis = case_when(
          MCQ220==1~1,
          MCQ220==2~2,
          MCQ220==9~3,
          is.na(MCQ220)~3
      ))
