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
  select(1:34,36,37,38,40,41,90,106,107,111,113,114,116:120,173,186,191,208,210,220,226,228,230,234,235,248,250,258)%>%
  filter(RIDAGEYR>=18)

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

totalMA<-Race_tally[1] #324
totalH<-Race_tally[2] #235
totalW<-Race_tally[3] #563
totalAA<-Race_tally[4] #421
totalAs<-Race_tally[5] #205
totalMr<-Race_tally[6] #81

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
 
newpht<-total_pht%>%
    mutate(Race_Ethnicity = case_when(   
      RIDRETH1==1~"Mexican American",
      RIDRETH1==2~"Hispanic",
      RIDRETH1==3~"Non-Hispanic White",
      RIDRETH1==4~"Non-Hispanic Black",
      RIDRETH1==5&RIDRETH3==6~"Non-Hispanic Asian",
      RIDRETH1==5&RIDRETH3==7~"Non-Hispanic Multiracial"))%>%
    mutate(Cancer_Diagnosis = case_when(
          MCQ220==1~"Positive diagnosis",
          MCQ220==2~"Negative diagnosis",
          MCQ220==9~"Unknown diagnosis",
          is.na(MCQ220)~"Unknown diagnosis"
      ))%>%
    mutate(Thyroid_issues = case_when(
       MCQ160M==1~"Positive diagnosis",
       MCQ160M==2~"Negative diagnosis",
       MCQ160M==9~"Unknown diagnosis",
      is.na(MCQ220)~"Unknown diagnosis"))%>%
    mutate(Diabetes = case_when(
      DIQ010==1~"Positive diagnosis",
      DIQ010==2~"Negative diagnosis",
      DIQ010==3~"Borderline diagnosis",#borderline
      is.na(DIQ010)~"Unknown diagnosis"))%>%#don't know
    mutate(AgeClass=case_when(
      RIDAGEYR>=18 & RIDAGEYR<=29~"18-29 yoa",
      RIDAGEYR>=30 & RIDAGEYR<=49~"30-49 yoa",
      RIDAGEYR>=50 & RIDAGEYR<65~"50-65 yoa",
      RIDAGEYR>=65~"65 and up yoa",
      is.na(RIDAGEYR)~"Age Unknown"))%>%
    mutate(Gender=case_when(
      RIAGENDR==1~"Male",
      RIAGENDR==2~"Female"
    ))


saveRDS(newpht, file="C:/Users/unews/nss_data_science/Demographic-Phthalate-Intake-/Adult_Phthalate_Obs/Adult_Phthalate_Exploration/Phthalate_Exploration.Rds")

yesBlack<-table(newpht$Cancer_Diagnosis==1&newpht$Race_Ethnicity=="Non-Hispanic Black")
noBlack<-table(newpht$Cancer_Diagnosis==2&newpht$Race_Ethnicity=="Non-Hispanic Black")
idkBlack<-table(newpht$Cancer_Diagnosis==3&newpht$Race_Ethnicity=="Non-Hispanic Black")

yesMA<-table(newpht$Cancer_Diagnosis==1&newpht$Race_Ethnicity=="Mexican American")
noMA<-table(newpht$Cancer_Diagnosis==2&newpht$Race_Ethnicity=="Mexican American")
idkMA<-table(newpht$Cancer_Diagnosis==3&newpht$Race_Ethnicity=="Mexican American")

yesHS<-table(newpht$Cancer_Diagnosis==1&newpht$Race_Ethnicity=="Hispanic")
noHS<-table(newpht$Cancer_Diagnosis==2&newpht$Race_Ethnicity=="Hispanic")
idkHS<-table(newpht$Cancer_Diagnosis==3&newpht$Race_Ethnicity=="Hispanic")


yesW<-table(newpht$Cancer_Diagnosis==1&newpht$Race_Ethnicity=="Non-Hispanic White")
noW<-table(newpht$Cancer_Diagnosis==2&newpht$Race_Ethnicity=="Non-Hispanic White")
idkW<-table(newpht$Cancer_Diagnosis==3&newpht$Race_Ethnicity=="Non-Hispanic White")

yesAs<-table(newpht$Cancer_Diagnosis==1&newpht$Race_Ethnicity=="Non-Hispanic Asian")
noAs<-table(newpht$Cancer_Diagnosis==2&newpht$Race_Ethnicity=="Non-Hispanic Asian")
idkAs<-table(newpht$Cancer_Diagnosis==3&newpht$Race_Ethnicity=="Non-Hispanic Asian")

yesMr<-table(newpht$Cancer_Diagnosis==1&newpht$Race_Ethnicity=="Non-Hispanic Multiracial")
noMr<-table(newpht$Cancer_Diagnosis==2&newpht$Race_Ethnicity=="Non-Hispanic Multiracial")
idkMr<-table(newpht$Cancer_Diagnosis==3&newpht$Race_Ethnicity=="Non-Hispanic Multiracial")

yesBlack+noBlack+idkBlack
yesBlack/totalAA

yesW+noW+idkW
yesW/totalW

yesBlack/(yesBlack+noBlack)

yesW/(yesW+noW)

table(newpht$Thyroid_issues)

thyBlack<-table(newpht$Thyroid_issues==1&newpht$Race_Ethnicity=="Non-Hispanic Black")
nothyBlack<-table(newpht$Thyroid_issues==2&newpht$Race_Ethnicity=="Non-Hispanic Black")
idkthyBlack<-table(newpht$Thyroid_issues==3&newpht$Race_Ethnicity=="Non-Hispanic Black")

thyMA<-table(newpht$Thyroid_issues==1&newpht$Race_Ethnicity=="Mexican American")
nothyMA<-table(newpht$Thyroid_issues==2&newpht$Race_Ethnicity=="Mexican American")
idkthyMA<-table(newpht$Thyroid_issues==3&newpht$Race_Ethnicity=="Mexican American")

thyHS<-table(newpht$Thyroid_issues==1&newpht$Race_Ethnicity=="Hispanic")
nothyHS<-table(newpht$Thyroid_issues==2&newpht$Race_Ethnicity=="Hispanic")
idkthyHS<-table(newpht$Thyroid_issues==3&newpht$Race_Ethnicity=="Hispanic")


thyW<-table(newpht$Thyroid_issues==1&newpht$Race_Ethnicity=="Non-Hispanic White")
nothyW<-table(newpht$Thyroid_issues==2&newpht$Race_Ethnicity=="Non-Hispanic White")
idkthyW<-table(newpht$Thyroid_issues==3&newpht$Race_Ethnicity=="Non-Hispanic White")

thyAs<-table(newpht$Thyroid_issues==1&newpht$Race_Ethnicity=="Non-Hispanic Asian")
nothyAs<-table(newpht$Thyroid_issues==2&newpht$Race_Ethnicity=="Non-Hispanic Asian")
idkthyAs<-table(newpht$Thyroid_issues==3&newpht$Race_Ethnicity=="Non-Hispanic Asian")

thyMr<-table(newpht$Thyroid_issues==1&newpht$Race_Ethnicity=="Non-Hispanic Multiracial")
nothyMr<-table(newpht$Thyroid_issues==2&newpht$Race_Ethnicity=="Non-Hispanic Multiracial")
idkthyMr<-table(newpht$Thyroid_issues==3&newpht$Race_Ethnicity=="Non-Hispanic Multiracial")

thyBlack/totalAA
thyW/totalW

Thyroid<-newpht%>%
  group_by(Thyroid_issues)%>%
  summarise(mean_pht=mean(total_pht,na.rm=TRUE))

Thyroid$Diagnosis<-c("Yes", "No", "Unknown")

ggplot(Thyroid, aes(x=Diagnosis,y=mean_pht))+
  geom_col()+xlab("Incidence of Thyroid Problems")+ylab('Mean Phthalate Metabolite in Urine (ng/mL')+
  ggtitle(      'Comparison of Phthalate Metabolite Excretion by Recognition of Thyroid Problem')


Diabetes<-newpht%>%
  group_by(Diabetes)%>%
  summarise(mean_pht=mean(total_pht,na.rm=TRUE))

Diabetes$Diagnosis<-c("Yes", "No", "Borderline", "Unknown")

ggplot(Diabetes, aes(x=Diagnosis,y=mean_pht))+
  geom_col()+xlab("Incidence of Diabetes")+ylab('Mean Phthalate Metabolite in Urine (ng/mL')+
  ggtitle(      'Comparison of Phthalate Metabolite Excretion by Diagnosis of Diabetes')

AgeClass<-newpht%>%
  group_by(AgeClass)%>%
  summarise(mean_pht=mean(total_pht,na.rm=TRUE))

ggplot(AgeClass, aes(x=AgeClass,y=mean_pht))+geom_col()+xlab("Age")+ylab('Mean Phthalate Metabolite in Urine (ng/ml')+
  ggtitle(         'Comparison of Phthalate Metabolite Excretion by Age)')


Age_Gender<-newpht%>%
  group_by(RIAGENDR,AgeClass)%>%
  summarise(mean_pht=mean(total_pht,na.rm=TRUE))

ggplot(newpht, aes(newpht$total_pht))+geom_histogram()

a<-"Gender"

data1<-phth1%>%
    group_by(`Race/Ethnicity`)%>%
    summarise(mean_pht=mean(`Total Pht`,na.rm=TRUE))

data1$Diagnosis<-case_when (
    nrow(data1)==2 & colnames(data1[1])=="Gender"~c("Female","Male"),
    nrow(data1)==6 & colnames(data1[1])=="Race/Ethnicity"~c("Hispanic","Mexican American","Non-Hispanic Asian","Non-Hispanic Black","Non-Hispanic Multi-racial","Non-Hispanic White"),
    nrow(data1)==4 & colnames(data1[1])=="Age Class"~c("18-29 yoa","30-49 yoa", "50-65 yoa","65 and up yoa"),
    nrow(data1)==3 & colnames(data1[1])=="Cancer?"~c("Yes", "No", "Unknown"),
    nrow(data1)==3 & colnames(data1[1])=="Thyroid Issue?"~c("Yes", "No", "Unknown"),
    nrow(data1)==4 & colnames(data1[1])=="Diabetes?"~c("Yes", "No", "Borderline", "Unknown")
  )

diagnosis<-function(df){
  df$Diagnosis<-case_when(
    nrow(df)==2 & colnames(df[1])=="Gender"~c("Female","Male"),
    nrow(df)==6 & colnames(df[1])=="Race/Ethnicity"~c("Hispanic","Mexican American","Non-Hispanic Asian","Non-Hispanic Black","Non-Hispanic Multi-racial","Non-Hispanic White"),
    nrow(df)==4 & colnames(df[1])=="Age Class"~c("18-29 yoa","30-49 yoa", "50-65 yoa","65 and up yoa"),
    nrow(df)==3 & colnames(df[1])=="Cancer?"~c("Yes", "No", "Unknown"),
    nrow(df)==3 & colnames(df[1])=="Thyroid Issue?"~c("Yes", "No", "Unknown"),
    nrow(df)==4 & colnames(df[1])=="Diabetes?"~c("Yes", "No", "Borderline", "Unknown"))}

data1<-phth1%>%
  group_by(`Race/Ethnicity`)%>%
  summarise(mean_pht=mean(`Total Pht`,na.rm=TRUE))

data1$Diagnosis<-case_when (
  colnames(data1[1])=="Gender"~c("Female","Male"),
  colnames(data1[1])=="Race/Ethnicity"~c("Hispanic","Mexican American","Non-Hispanic Asian","Non-Hispanic Black","Non-Hispanic Multi-racial","Non-Hispanic White")
  # colnames(data1[1])=="Age Class"~c("18-29 yoa","30-49 yoa", "50-65 yoa","65 and up yoa"),
  # colnames(data1[1])=="Cancer?"~c("Yes", "No", "Unknown"),
  # colnames(data1[1])=="Thyroid Issue?"~c("Yes", "No", "Unknown"),
  # colnames(data1[1])=="Diabetes?"~c("Yes", "No", "Borderline", "Unknown")
)

  ggplot(data1,aes(x=Diagnosis, y=mean_pht))+
    geom_col()+xlab("Incidence of " )+ylab('Mean Phthalate Metabolite in Urine (ng/mL')+
    ggtitle(       "Comparison of Phthalate Metabolite Excretion by " )

  data<-phth1%>%
      group_by(`Age Class`)%>%
      summarise(mean_pht=mean(`Total Pht`,na.rm=TRUE))  
  
  ggplot(data,aes(x=`Age Class`, y=mean_pht))+
    geom_col()+xlab("Incidence of " ) +ylab('Mean Phthalate Metabolite in Urine (ng/mL')+
    ggtitle(       "Comparison of Phthalate Metabolite Excretion by " )
  
  
  
  data2<-data1%>%
    mutate(Diagnosis=case_when (
      colnames(data1[1])=="Gender"~c("Female","Male"),
      colnames(data1[1])=="Race/Ethnicity"~c("Hispanic","Mexican American","Non-Hispanic Asian","Non-Hispanic Black","Non-Hispanic Multi-racial","Non-Hispanic White"),
      colnames(data1[1])=="Age Class"~c("18-29 yoa","30-49 yoa", "50-65 yoa","65 and up yoa"),
      colnames(data1[1])=="Cancer?"~c("Yes", "No", "Unknown"),
      colnames(data1[1])=="Thyroid Issue?"~c("Yes", "No", "Unknown"),
      colnames(data1[1])=="Diabetes?"~c("Yes", "No", "Borderline", "Unknown")
    ))
  
  see <- phth1%>%
    filter(Race_Ethnicity=="Mexican American")
  
  
  t <- t.test(see$Total_Pht,phth1$Total_Pht, var.equal = FALSE)
  
  tstat <- t[["statistic"]]
  
  
  
  # 
  # data<-eventReactive(input$go, {phth1%>%
  #   group_by(input$f1,input$f2,input$f3)%>%
  #   summarise(mean_pht=mean(`Total Pht`,na.rm=TRUE)%>%
  #   mutate(Diagnosis= case_when (
  #     input$f1=="Gender" & input$f2=="Blank" & input$f3=="Blank"~c("Female","Male")
  #   input$f1=="Race/Ethnicity" & input$f2=="Blank" & input$f3=="Blank"~c("Hispanic","Mexican American","Non-Hispanic Asian","Non-Hispanic Black","Non-Hispanic Multi-racial","Non-Hispanic White"),
  #   input$f1=="Age" & input$f2=="Blank" & input$f3=="Blank"~Diagnosis,
  #   input$f1=="Cancer?" & input$f2=="Blank"& input$f3=="Blank"~c("Yes", "No", "Unknown"),
  #   input$f1=="Thyroid Issue?" & input$f2=="Blank" & input$f3=="Blank"~c("Yes", "No", "Unknown"),
  #   input$f1=="Diabetes?"& input$f2=="Blank" & input$f3=="Blank"~c("Yes", "No", "Borderline", "Unknown") 
  # ))
  
  # input$f1=="Gender"& input$f2=="Age" & input$f3=="Blank"~,
  # input$f1=="Gender"& input$f2=="Race/Ethnicity" & input$f3=="Blank",
  # input$f1=="Gender"& input$f2=="Cancer?" & input$f3=="Blank"~,
  # input$f1=="Gender"& input$f2=="Thyroid Issues?" & input$f3=="Blank"~,
  # input$f1=="Gender"& input$f2=="Diabetes?" & input$f3=="Blank",
  # input$f1=="Gender"& input$f2=="Age" & input$f3=="Blank"~c("Female 18-29 yoa", "Male 18-29 yoa", "Female 30-49 yoa", "Male 30-49 yoa", "Female 50-65 yoa", "Male 50-65 yoa","Female 65 and up", "Male 65 and up yoa"),
  # input$f1=="Gender"& input$f2=="Race/Ethnicity" & input$f3=="Blank"~c("Hispanic Male","Hispanic Female","Mexican American Female","Mexican American Male", "Non-Hispanic Asian Female","Non-Hispanic Asian Male","Non-Hispanic Black Female", "Non-Hispanic Black Male","Non-Hispanic Multi-racial Female", "Non-Hispanic Multi-racial Male","Non-Hispanic White Female", "Non-Hispanic White Male"),
  # input$f1=="Gender"& input$f2=="Cancer?" & input$f3=="Blank"~c("Female-Positive Diagnosis","Male-Positive Diagnosis", "Female-Negative Diagnosis", "Male-Negative Diagnosis","Female-Unknown Diagnosis","Male-Unknown Diagnosis"),
  # input$f1=="Gender"& input$f2=="Thyroid Issues?" & input$f3=="Blank"~c("Female-Positive Diagnosis","Male-Positive Diagnosis", "Female-Negative Diagnosis", "Male-Negative Diagnosis","Female-Unknown Diagnosis","Male-Unknown Diagnosis"),
  # input$f1=="Gender"& input$f2=="Diabetes" & input$f3=="Blank"~c("Female-Positive Diagnosis","Male-Positive Diagnosis", "Female-Negative Diagnosis", "Male-Negative Diagnosis","Female-Borderline Diagnosis","Male-Borderline Diagnosis", "Female-Unknown Diagnosis","Male-Unknown Diagnosis"),
  # input$f1=="Age"& input$f2=="Gender" & input$f3=="Blank"~c("Female 18-29 yoa", "Male 18-29 yoa", "Female 30-49 yoa", "Male 30-49 yoa", "Female 50-65 yoa", "Male 50-65 yoa","Female 65 and up", "Male 65 and up yoa"),
  # # input$f1=="Age"& input$f2=="Race/Ethnicity" & input$f3=="Blank"~,
  # input$f1=="Age"& input$f2=="Cancer?" & input$f3=="Blank"~c("Positive Diagnosis 18-29 yoa","Negative Diagnosis 18-29 yoa", "Unknown Diagnosis 18-29 yoa","Positive Diagnosis 30-49 yoa","Negative Diagnosis 30-49 yoa", "Unknown Diagnosis 30-49 yoa", "Positive Diagnosis 50-65 yoa","Negative Diagnosis 50-65 yoa", "Unknown Diagnosis 50-65 yoa","Positive Diagnosis 65 and up yoa","Negative Diagnosis 65 and up yoa", "Unknown Diagnosis 65 and up yoa"),
  # input$f1=="Age"& input$f2=="Thyroid Issues?" & input$f3=="Blank"~c("Positive Diagnosis 18-29 yoa","Negative Diagnosis 18-29 yoa", "Unknown Diagnosis 18-29 yoa","Positive Diagnosis 30-49 yoa","Negative Diagnosis 30-49 yoa", "Unknown Diagnosis 30-49 yoa", "Positive Diagnosis 50-65 yoa","Negative Diagnosis 50-65 yoa", "Unknown Diagnosis 50-65 yoa","Positive Diagnosis 65 and up yoa","Negative Diagnosis 65 and up yoa", "Unknown Diagnosis 65 and up yoa"),
  # input$f1=="Age"& input$f2=="Diabetes?" & input$f3=="Blank"~c("Positive Diagnosis 18-29 yoa","Negative Diagnosis 18-29 yoa","Borderline Diagnosis 18-29 yoa" ,"Unknown Diagnosis 18-29 yoa","Positive Diagnosis 30-49 yoa","Negative Diagnosis 30-49 yoa", "Borderline Diagnosis 30-49 yoa","Unknown Diagnosis 30-49 yoa", "Positive Diagnosis 50-65 yoa","Negative Diagnosis 50-65 yoa","Borderline Diagnosis 50-65 yoa", "Unknown Diagnosis 50-65 yoa","Positive Diagnosis 65 and up yoa","Negative Diagnosis 65 and up yoa", "Borderline diagnosis 65 and up yoa", "Unknown Diagnosis 65 and up yoa"))
  # # input$f1=="Gender"& input$f2=="Race/Ethnicity" & input$f3=="Blank" 
  
# data<-eventReactive(input$go, {phth1%>%
#     group_by(input$f1,input$f2,input$f3)%>%
#     summarise(mean_pht=mean(`Total Pht`,na.rm=TRUE))
#   mutate(Diagnosis= case_when (
#     input$f1=="Gender" & input$f2="Blank" & input$f3="Blank"~c("Female","Male"),
#     input$f1=="Race/Ethnicity" & input$f2="Blank" & input$f3="Blank"~c("Hispanic","Mexican American","Non-Hispanic Asian","Non-Hispanic Black","Non-Hispanic Multi-racial","Non-Hispanic White"),
#     input$f1=="Age" & input$f2="Blank" & input$f3="Blank"~Diagnosis,
#     input$f1=="Cancer?" & input$f2=="Blank"& input$f3=="Blank"~c("Yes", "No", "Unknown"),
#     input$f1=="Thyroid Issue?" & input$f2=="Blank" & input$f3=="Blank"~c("Yes", "No", "Unknown"),
#     input$f1=="Diabetes?"& input$f2=="Blank" & input$f3=="Blank"~c("Yes", "No", "Borderline", "Unknown")
#     input$f1=="Gender"& input$f2=="Age" & input$f3=="Blank"~
#       input$f1=="Gender"& input$f2=="Race/Ethnicity" & input$f3=="Blank"
#     input$f1=="Gender"& input$f2=="Cancer?" & input$f3=="Blank"~
#       input$f1=="Gender"& input$f2=="Thyroid Issues?" & input$f3=="Blank"~
#       input$f1=="Gender"& input$f2=="Diabetes?" & input$f3=="Blank"
#     input$f1=="Gender"& input$f2=="Age" & input$f3=="Blank"~c("Female 18-29 yoa", "Male 18-29 yoa", "Female 30-49 yoa", "Male 30-49 yoa", "Female 50-65 yoa", "Male 50-65 yoa","Female 65 and up", "Male 65 and up yoa"),
#     input$f1=="Gender"& input$f2=="Race/Ethnicity" & input$f3=="Blank"~c("Hispanic Male","Hispanic Female","Mexican American Female","Mexican American Male", "Non-Hispanic Asian Female","Non-Hispanic Asian Male","Non-Hispanic Black Female", "Non-Hispanic Black Male","Non-Hispanic Multi-racial Female", "Non-Hispanic Multi-racial Male","Non-Hispanic White Female", "Non-Hispanic White Male"),
#     input$f1=="Gender"& input$f2=="Cancer?" & input$f3=="Blank"~c("Female-Positive Diagnosis","Male-Positive Diagnosis", "Female-Negative Diagnosis", "Male-Negative Diagnosis","Female-Unknown Diagnosis","Male-Unknown Diagnosis"),
#     input$f1=="Gender"& input$f2=="Thyroid Issues?" & input$f3=="Blank"~c("Female-Positive Diagnosis","Male-Positive Diagnosis", "Female-Negative Diagnosis", "Male-Negative Diagnosis","Female-Unknown Diagnosis","Male-Unknown Diagnosis"),
#     input$f1=="Gender"& input$f2=="Diabetes" & input$f3=="Blank"~c("Female-Positive Diagnosis","Male-Positive Diagnosis", "Female-Negative Diagnosis", "Male-Negative Diagnosis","Female-Borderline Diagnosis","Male-Borderline Diagnosis", "Female-Unknown Diagnosis","Male-Unknown Diagnosis"),
#     input$f1=="Age"& input$f2=="Gender" & input$f3=="Blank"~c("Female 18-29 yoa", "Male 18-29 yoa", "Female 30-49 yoa", "Male 30-49 yoa", "Female 50-65 yoa", "Male 50-65 yoa","Female 65 and up", "Male 65 and up yoa")
#     input$f1=="Age"& input$f2=="Race/Ethnicity" & input$f3=="Blank"~
#       input$f1=="Age"& input$f2=="Cancer?" & input$f3=="Blank"~c("Positive 18-29 yoa",)
#     input$f1=="Age"& input$f2=="Thyroid Issues?" & input$f3=="Blank"~
#       input$f1=="Age"& input$f2=="Diabetes?" & input$f3=="Blank"~
#       input$f1=="Gender"& input$f2=="Race/Ethnicity" & input$f3=="Blank"
# 

  
  
  # 
  # data<-eventReactive(input$go, {phth1%>%
  #   group_by(input$f1,input$f2,input$f3)%>%
  #   summarise(mean_pht=mean(`Total Pht`,na.rm=TRUE)%>%
  #   mutate(Diagnosis= case_when (
  #     input$f1=="Gender" & input$f2=="Blank" & input$f3=="Blank"~c("Female","Male")
  #   input$f1=="Race/Ethnicity" & input$f2=="Blank" & input$f3=="Blank"~c("Hispanic","Mexican American","Non-Hispanic Asian","Non-Hispanic Black","Non-Hispanic Multi-racial","Non-Hispanic White"),
  #   input$f1=="Age" & input$f2=="Blank" & input$f3=="Blank"~Diagnosis,
  #   input$f1=="Cancer?" & input$f2=="Blank"& input$f3=="Blank"~c("Yes", "No", "Unknown"),
  #   input$f1=="Thyroid Issue?" & input$f2=="Blank" & input$f3=="Blank"~c("Yes", "No", "Unknown"),
  #   input$f1=="Diabetes?"& input$f2=="Blank" & input$f3=="Blank"~c("Yes", "No", "Borderline", "Unknown") 
  # ))
  
  # input$f1=="Gender"& input$f2=="Age" & input$f3=="Blank"~,
  # input$f1=="Gender"& input$f2=="Race/Ethnicity" & input$f3=="Blank",
  # input$f1=="Gender"& input$f2=="Cancer?" & input$f3=="Blank"~,
  # input$f1=="Gender"& input$f2=="Thyroid Issues?" & input$f3=="Blank"~,
  # input$f1=="Gender"& input$f2=="Diabetes?" & input$f3=="Blank",
  # input$f1=="Gender"& input$f2=="Age" & input$f3=="Blank"~c("Female 18-29 yoa", "Male 18-29 yoa", "Female 30-49 yoa", "Male 30-49 yoa", "Female 50-65 yoa", "Male 50-65 yoa","Female 65 and up", "Male 65 and up yoa"),
  # input$f1=="Gender"& input$f2=="Race/Ethnicity" & input$f3=="Blank"~c("Hispanic Male","Hispanic Female","Mexican American Female","Mexican American Male", "Non-Hispanic Asian Female","Non-Hispanic Asian Male","Non-Hispanic Black Female", "Non-Hispanic Black Male","Non-Hispanic Multi-racial Female", "Non-Hispanic Multi-racial Male","Non-Hispanic White Female", "Non-Hispanic White Male"),
  # input$f1=="Gender"& input$f2=="Cancer?" & input$f3=="Blank"~c("Female-Positive Diagnosis","Male-Positive Diagnosis", "Female-Negative Diagnosis", "Male-Negative Diagnosis","Female-Unknown Diagnosis","Male-Unknown Diagnosis"),
  # input$f1=="Gender"& input$f2=="Thyroid Issues?" & input$f3=="Blank"~c("Female-Positive Diagnosis","Male-Positive Diagnosis", "Female-Negative Diagnosis", "Male-Negative Diagnosis","Female-Unknown Diagnosis","Male-Unknown Diagnosis"),
  # input$f1=="Gender"& input$f2=="Diabetes" & input$f3=="Blank"~c("Female-Positive Diagnosis","Male-Positive Diagnosis", "Female-Negative Diagnosis", "Male-Negative Diagnosis","Female-Borderline Diagnosis","Male-Borderline Diagnosis", "Female-Unknown Diagnosis","Male-Unknown Diagnosis"),
  # input$f1=="Age"& input$f2=="Gender" & input$f3=="Blank"~c("Female 18-29 yoa", "Male 18-29 yoa", "Female 30-49 yoa", "Male 30-49 yoa", "Female 50-65 yoa", "Male 50-65 yoa","Female 65 and up", "Male 65 and up yoa"),
  # # input$f1=="Age"& input$f2=="Race/Ethnicity" & input$f3=="Blank"~,
  # input$f1=="Age"& input$f2=="Cancer?" & input$f3=="Blank"~c("Positive Diagnosis 18-29 yoa","Negative Diagnosis 18-29 yoa", "Unknown Diagnosis 18-29 yoa","Positive Diagnosis 30-49 yoa","Negative Diagnosis 30-49 yoa", "Unknown Diagnosis 30-49 yoa", "Positive Diagnosis 50-65 yoa","Negative Diagnosis 50-65 yoa", "Unknown Diagnosis 50-65 yoa","Positive Diagnosis 65 and up yoa","Negative Diagnosis 65 and up yoa", "Unknown Diagnosis 65 and up yoa"),
  # input$f1=="Age"& input$f2=="Thyroid Issues?" & input$f3=="Blank"~c("Positive Diagnosis 18-29 yoa","Negative Diagnosis 18-29 yoa", "Unknown Diagnosis 18-29 yoa","Positive Diagnosis 30-49 yoa","Negative Diagnosis 30-49 yoa", "Unknown Diagnosis 30-49 yoa", "Positive Diagnosis 50-65 yoa","Negative Diagnosis 50-65 yoa", "Unknown Diagnosis 50-65 yoa","Positive Diagnosis 65 and up yoa","Negative Diagnosis 65 and up yoa", "Unknown Diagnosis 65 and up yoa"),
  # input$f1=="Age"& input$f2=="Diabetes?" & input$f3=="Blank"~c("Positive Diagnosis 18-29 yoa","Negative Diagnosis 18-29 yoa","Borderline Diagnosis 18-29 yoa" ,"Unknown Diagnosis 18-29 yoa","Positive Diagnosis 30-49 yoa","Negative Diagnosis 30-49 yoa", "Borderline Diagnosis 30-49 yoa","Unknown Diagnosis 30-49 yoa", "Positive Diagnosis 50-65 yoa","Negative Diagnosis 50-65 yoa","Borderline Diagnosis 50-65 yoa", "Unknown Diagnosis 50-65 yoa","Positive Diagnosis 65 and up yoa","Negative Diagnosis 65 and up yoa", "Borderline diagnosis 65 and up yoa", "Unknown Diagnosis 65 and up yoa"))
  # # input$f1=="Gender"& input$f2=="Race/Ethnicity" & input$f3=="Blank"
#Statistical Significance
# See http://onlinestatbook.com/2/tests_of_means/difference_means.html