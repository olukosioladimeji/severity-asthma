#PMIM202J code Assessment/Olukosi Bashiru Oladimeji(2260092)

#install needed packages
install.packages('tidyverse')
install.packages('survival')
install.packages('survminer')
install.packages('rpart')
install.packages("ggplot2")
# important libraries
library(RODBC)
library(tcltk)
library(tidyverse)
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)

getlogin <- function(userName=''){
  wnd <- tktoplevel()
  user <- tclVar(userName)
  passvar <- tclVar('')
  
  tkgrid(tklabel(wnd,text='Username:'))
  passBox <- tkentry(wnd,textvariable = user)
  tkgrid(passBox)
  
  tkgrid(tklabel(wnd,text='Password:'))
  passBox <- tkentry(wnd,textvariable=passvar,show='*')
  tkgrid(passBox)
  
  # Hitting return will also submit password.
  tkbind(passBox, '<Return>', function() tkdestroy(wnd))
  
  # OK button.
  tkgrid(tkbutton(wnd,text='OK',command=function() tkdestroy(wnd)))
  
  # Wait for user to click OK.
  tkwait.window(wnd)
  
  password <- tclvalue(passvar)
  userName <- tclvalue(user)
  
  db <- odbcConnect('PR_SAIL', userName, password)
  return(db)
}

channel <- getlogin()



# getting out all asthma patient from the GP data from 2018-02-14 to
# 2022-02-14(4years study period)
ASTHMA_PATIENTS <- sqlQuery(channel,"
              SELECT gp.ALF_PE, gp.GNDR_CD as GENDER, gp.WOB,
              gp.EVENT_CD as ALL_ASTHMA_CODE,
              gp.EVENT_DT
              FROM SAIL1281V.WLGP_GP_EVENT_CLEANSED_20220201 gp
              WHERE gp.EVENT_CD LIKE 'H33%'  AND gp.EVENT_DT>='2018-02-14'
              ")
#converting week of birth to age in years
date_wob1<-as.Date(ASTHMA_PATIENTS$WOB)
last_date1<- as.Date('2022-02-14')
AGE_WOB1<- round(last_date1- date_wob1, 0)

#using past0 to get age in years,make gender binary and restrict to the age 
# limit between 35years and 90years 
#inclusively and defininig severity using H333.,H334.,H332.,H3120, H335.
ASTHMA_PATIENTS<- ASTHMA_PATIENTS%>%
  mutate(AGE=as.numeric(paste0(round(AGE_WOB1/365))))%>%
  mutate(GENDER=ifelse(GENDER==1,0,1))%>%
  filter(AGE>=35 & AGE<=90)%>%
  mutate(SEVERITY=ifelse(ALL_ASTHMA_CODE=='H333.'| ALL_ASTHMA_CODE=='H334.'|
                           ALL_ASTHMA_CODE=='H332.'|ALL_ASTHMA_CODE=='H3120'|
                           ALL_ASTHMA_CODE=='H335.',1,0))
  
ASTHMA_PATIENTS<-ASTHMA_PATIENTS[,c(-3)] # removing WOB column
#checking for outliers and remove them using max((ASTHMA_PATIENTS$EVENT_DT))
ASTHMA_PATIENTS<-filter(ASTHMA_PATIENTS,EVENT_DT!="2099-12-26")
ASTHMA_PATIENTS<-filter(ASTHMA_PATIENTS,EVENT_DT!="2045-01-01")
ASTHMA_PATIENTS<-filter(ASTHMA_PATIENTS,EVENT_DT!="2042-04-27")
max((ASTHMA_PATIENTS$EVENT_DT))

# getting out patient with type ii diabetes and join to asthma table on ALF_PE
P_A_DIABETES <- sqlQuery(channel,"
              SELECT gp.ALF_PE,
              gp.EVENT_CD as ALL_A_DIABETES
              FROM SAIL1281V.WLGP_GP_EVENT_CLEANSED_20220201 gp
              WHERE gp.EVENT_CD = 'C10F.' AND gp.EVENT_DT>='2018-02-14'
              ")

TABLE1<-left_join(ASTHMA_PATIENTS, P_A_DIABETES, by= "ALF_PE")

# to remove duplicated values and drop NA
TABLE1<-TABLE1%>%
  distinct(ALF_PE,.keep_all = TRUE)
TABLE1<-TABLE1%>%
  drop_na(ALF_PE, GENDER, AGE, SEVERITY, ALL_ASTHMA_CODE, EVENT_DT)


#getting out the smokers and left join with table1 on ALF_PE
P_A_SMOKERS <- sqlQuery(channel,"
              SELECT gp.ALF_PE,
              gp.EVENT_CD as ALL_A_SMOKERS
              FROM SAIL1281V.WLGP_GP_EVENT_CLEANSED_20220201 gp
              WHERE gp.EVENT_CD = '137R.' AND gp.EVENT_DT>='2018-02-14'
              ")

TABLE2<-left_join(TABLE1, P_A_SMOKERS, by= 'ALF_PE')

TABLE2<-TABLE2%>%
  distinct(ALF_PE,.keep_all = TRUE)



# getting out data for obesity and left join to table2 on ALF_PE
P_A_OBESITY <- sqlQuery(channel,"
              SELECT gp.ALF_PE,
              gp.EVENT_CD as ALL_A_OBESITY
              FROM SAIL1281V.WLGP_GP_EVENT_CLEANSED_20220201 gp
              WHERE gp.EVENT_CD = 'C380.' AND gp.EVENT_DT>='2018-02-14'
              ")


TABLE3<-left_join(TABLE2, P_A_OBESITY, by= 'ALF_PE')

TABLE3<-TABLE3%>%
  distinct(ALF_PE,.keep_all = TRUE)



#getting out data for hypertension and left join to table3
P_A_HYPERTENSION <- sqlQuery(channel,"
              SELECT gp.ALF_PE,
              gp.EVENT_CD as ALL_A_HYPERTENSION
              FROM SAIL1281V.WLGP_GP_EVENT_CLEANSED_20220201 gp
              WHERE gp.EVENT_CD = 'G2...' AND gp.EVENT_DT>='2018-02-14'
              ")


TABLE4<-left_join(TABLE3, P_A_HYPERTENSION, by= 'ALF_PE')
TABLE4<-TABLE4%>%
  distinct(ALF_PE,.keep_all = TRUE)



#getting out data for allergic rhinisitis and left join to table 4
P_A_ALLERGIC_RHINITIS <- sqlQuery(channel,"
              SELECT gp.ALF_PE,
              gp.EVENT_CD as ALL_A_ALLERGIC_RHINITIS
              FROM SAIL1281V.WLGP_GP_EVENT_CLEANSED_20220201 gp
              WHERE gp.EVENT_CD = 'H17..' AND gp.EVENT_DT>='2018-02-14'
              ")


TABLE5<-left_join(TABLE4, P_A_ALLERGIC_RHINITIS, by= 'ALF_PE')
TABLE5<-TABLE5%>%
  distinct(ALF_PE,.keep_all = TRUE)



# getting out data for corona virus and left join to table 5
P_A_COVID_STATUS <- sqlQuery(channel,"
              SELECT gp.ALF_PE,
              gp.EVENT_CD as ALL_A_COVID_STATUS
              FROM SAIL1281V.WLGP_GP_EVENT_CLEANSED_20220201 gp
              WHERE gp.EVENT_CD = 'A795.' AND gp.EVENT_DT>='2018-02-14'
              ")


TABLE6<-left_join(TABLE5,P_A_COVID_STATUS, by= 'ALF_PE')

TABLE6<-TABLE6%>%
  distinct(ALF_PE,.keep_all = TRUE)%>%
  mutate(A_DIAB_STATUS=ifelse(is.na(ALL_A_DIABETES),0,1))%>%
  mutate(A_SMOKING_STATUS=ifelse(is.na(ALL_A_SMOKERS),0,1))%>%
  mutate(A_HYPER_STATUS=ifelse(is.na(ALL_A_HYPERTENSION),0,1))%>%
  mutate(A_OBES_STATUS=ifelse(is.na(ALL_A_OBESITY),0,1))%>%
  mutate(A_ALLERGIC_RHINITIS_STATUS=
           ifelse(is.na(ALL_A_ALLERGIC_RHINITIS),0,1))%>%
  mutate(COVID_STATUS=ifelse(is.na(ALL_A_COVID_STATUS),0,1))

TABLE6 <-TABLE6[,c(-7,-8,-9,-10,-11,-12)]


# lets see the death data to asthma
ASTHMA_DEATH <- sqlQuery(channel,"
              SELECT wa.ALF_PE,wa.DOD as DEATH_DATE
              FROM SAIL1281V.WDSD_AR_PERS_20220307 wa
              ")

TABLE7<-left_join(TABLE6, ASTHMA_DEATH, by='ALF_PE')

TABLE7<-TABLE7%>%
  mutate(DEAD_ALIVE=ifelse(is.na(DEATH_DATE),0,1))

  
TABLE7 <-TABLE7[,c(-3,-13)]

#creating a survival plot survival)
last_date1<- as.Date('2022-02-14')
START_DATE<- as.Date('2018-02-14')
TABLE7<-TABLE7%>%
  group_by(ALF_PE)%>%
  mutate(TIME_DAYS=as.numeric(round(EVENT_DT-START_DATE,0) ,units = "days"))

#calculating time to events for asthma attack
TABLE7 <- TABLE7[,c(-1,-3)]

# saving my data into csv
write.csv(TABLE7,"clean_data.csv",row.names = FALSE)

# i will now divide the data into two pre-covid and post-covid
data<- read.csv("clean_data.csv")

head(data)
count1<-table(data$GENDER)
print(count1)# 19528 male and 32807 female

count2<-table(data$SEVERITY)
print(count2) #34499 not severe case of asthma and 17836 severe

count3<-table(data$A_DIAB_STATUS)
print(count3)# 2518 typeii diabetes case and 50316 not diabetic

count4<-table(data$A_SMOKING_STATUS)
print(count4)# 4489 smokers and 47846 non-smoker

count5<-table(data$A_HYPER_STATUS)
print(count5)#997 case of hypertension and 51338 not hypertensive

count6<-table(data$A_OBES_STATUS)
print(count6)# 737case of obesity and 51598 not obes

count7<-table(data$A_ALLERGIC_RHINITIS_STATUS)
print(count7)#790cases with allergic rhinitis and 51545 of non allergic rhinitis

count8<-table(data$COVID_STATUS)
print(count8)# 899 cases of covid infection and 51436 of non

#performing logistic regression on my data
install.packages('glmnet')
install.packages('MASS')
library(glmnet)
library(MASS)

# using ggplot to see histogram of the data table
data%>%
  #gather()%>%
  pivot_longer(cols=everything())%>%
  ggplot(aes(value)) +
  facet_wrap(~name,scale="free") + geom_histogram()
  
# logistic regression model before covid infection
model <-glm(data=data, formula = SEVERITY~GENDER+AGE+
              A_SMOKING_STATUS+A_DIAB_STATUS+A_HYPER_STATUS+
              A_OBES_STATUS+A_ALLERGIC_RHINITIS_STATUS, family = binomial)
summary(model)



# logistic regression model after covid infection
model_covid <-glm(data=data, formula = SEVERITY~GENDER+AGE+
                    A_SMOKING_STATUS+A_DIAB_STATUS+A_HYPER_STATUS+
                    A_OBES_STATUS+A_ALLERGIC_RHINITIS_STATUS+COVID_STATUS,
                  family = binomial)
summary(model_covid)


# decision tree


par(mfrow=c(1,1),xpd=NA)
AGE_DECTREE<- scale(data$AGE)
         
decision_tree<- rpart(data=data,SEVERITY~GENDER+AGE_DECTREE+
                        A_SMOKING_STATUS+A_DIAB_STATUS+A_HYPER_STATUS+
                        A_OBES_STATUS+A_ALLERGIC_RHINITIS_STATUS+
                        COVID_STATUS,
                      control = rpart.control(minsplit =5 ,cp=0.0004))
str(decision_tree)
print(decision_tree)
plot(decision_tree)
text(decision_tree)


# survival analysis
model <-survfit(Surv(TIME_DAYS,SEVERITY)~1,data)
print(model)

# we create a survival plot for male and female
my_table<-table(data$SEVERITY,data$GENDER)
row.names(my_table)<-c("SEVERITY=0","SEVERITY=1")
colnames(my_table)<-c("male","female")

#creating a survival plot with sex factor
model_gender <-survfit(Surv(TIME_DAYS,SEVERITY)~GENDER,data=data)
print(model_gender)

survival_plot<-ggsurvplot(model_gender,data = data, 
                          legend.labs=c("Male","Female"),
                          conf.int=TRUE,pval=TRUE,censor=FALSE, risk.table=TRUE,
                          risk.table.title="Risk Table",
                          xlab="Time",ylab="Survival Probability",
                          main=" Survival plot with Risk Table")
survival_plot


data_frame_gender<-broom::tidy(model_gender)
view(data_frame_gender)


ggplot(data_frame_gender,aes(time,estimate,color=strata))+
  #geom_step()+
  geom_smooth(method="gam",span=0.5)+
  scale_color_manual(values = c("red","blue"))


# survival different between male and female
survdiff(Surv(TIME_DAYS,SEVERITY)~GENDER,data=data)




# putting other columns of interest
model_gender_covid <-survfit(Surv(TIME_DAYS,SEVERITY)~GENDER+
                               COVID_STATUS,data=data)
survival_plot_all<-ggsurvplot(model_gender_covid,data = data,
                          conf.int=TRUE,pval=TRUE,censor=FALSE, risk.table=TRUE,
                          risk.table.title="Risk Table",xlab="Time",
                          ylab="Survival Probability",
                          main=" Survival plot with Risk Table")
survival_plot_all


data_frame_gender_covid<-broom::tidy(model_gender_covid)
view(data_frame_gender_covid)


ggplot(data_frame_gender_covid,aes(time,estimate,color=strata,risk.table=TRUE))+
  #geom_step()+
  geom_smooth(method="gam",span=0.5)+
  scale_color_manual(values = c("red","blue","green","yellow"))


#cox regression
cox_model<-coxph(Surv(TIME_DAYS,SEVERITY)~GENDER+AGE+
                   A_SMOKING_STATUS+A_DIAB_STATUS+A_HYPER_STATUS+
                    A_OBES_STATUS+A_ALLERGIC_RHINITIS_STATUS+COVID_STATUS,
                 data=data)

summary(cox_model)
















































last_date1<- as.Date('2022-02-14')
START_DATE<- as.Date('2018-02-14')
TABLE7<-TABLE7%>%
  group_by(ALF_PE)%>%
  mutate(TIME_DAYS=as.numeric(round(EVENT_DT-START_DATE,0) ,units = "days"))

TABLE#calculating time to events for asthma attack
7 <- TABLE7[,c(-1,-3)]

TABLE7<-TABLE7%>%
  mutate(STRATA=TYPE_INDICATOR)


