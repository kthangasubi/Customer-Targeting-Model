
library(fastDummies)
library(dplyr)
library(MASS)

set.seed(1)
cat("\014")  
rm(list=ls())

coil<-read.csv('C:/Users/kthan/Downloads/S23(1).csv')
str(coil)

converted<-coil %>% 
  mutate(
    across
    (
      starts_with('P'), ~case_when
      (
        .x == 0 ~ 0,
        .x == 1 ~ 25,
        .x == 2 ~ 75,
        .x == 3 ~ 150,
        .x == 4 ~ 350,
        .x == 5 ~ 750,
        .x == 6 ~ 3000,
        .x == 7 ~ 7500,
        .x == 8 ~ 15000,
        .x == 9 ~ 30000,
        TRUE ~ -99
      )
    )
  )

converted <- converted%>% relocate (MOSHOO, .after = SeqNum)

converted <- converted%>% relocate (MOSTYP, .after = SeqNum)
converted <- converted%>% relocate (MGEMLE, .after = SeqNum)
converted <- converted%>% relocate (MOSTYP, .after = SeqNum)
converted <- converted%>% relocate (MAANTH, .after = SeqNum)
converted <- converted%>% relocate (MGEMOM, .after = SeqNum)
converted<-coil
str(converted)

converted<-converted %>% 
  mutate(across(MGODPR:MSKC, ~case_when(
    .x == 0 ~ 0,
    .x == 1 ~ 5.5,
    .x == 2 ~ 17,
    .x == 3 ~ 30,
    .x == 4 ~ 43,
    .x == 5 ~ 56,
    .x == 6 ~ 69,
    .x == 7 ~ 82,
    .x == 8 ~ 94,
    .x == 9 ~ 100,
    TRUE ~ -99
  )))

head(converted)
table(converted$PWAPAR, coil$PWAPAR)

converted <- dummy_cols(converted, select_columns = c('MOSTYP','MOSHOO'),remove_selected_columns = TRUE)
train <- converted %>% dplyr::sample_frac(0.70)
test  <- dplyr::anti_join(converted, train, by = 'SeqNum')

train<-subset(train,select=-c(SeqNum))
test <-subset(test,select=-c(SeqNum))


fullModel = glm(Resp ~ ., family = 'binomial', data = train) # model with all variables
nullModel = glm(Resp ~ 1, family = 'binomial', data = train) # model with the intercept only

interim<-summary(stepAIC(nullModel, # start with a model containing no variables
                         direction = 'forward', # run forward selection
                         scope = list(upper = fullModel, # the maximum to consider is a model with all variables
                                      lower = nullModel), # the minimum to consider is a model with no variables
                         trace = 0)) # do not show the step-by-step process of model selection

coef<-data.frame(interim[['coefficients']])
final<-coef[coef$Pr...z..<0.05,]
print(final)

varnames<-rownames(final)
varnames<-varnames[2:length(varnames)]
print(varnames)
#Step 10 

finalmodel<-glm(Resp ~ MSKB2+PAANHA+MGEMOM+MOSTYP_39+MOSTYP_34+MOSTYP_23+MAUT2+MAUT2+MRELGE+MOSHOO_9, family = 'binomial', data = train)
ForMemo<-lm(Resp ~ MSKB2+PAANHA+MGEMOM+MOSTYP_39+MOSTYP_34+MOSTYP_23+MAUT2+MAUT2+MRELGE+MOSHOO_9, data = train)
summary(ForMemo)
test$pred<-predict(finalmodel,newdata=test,type="response")
test<-test[order(-test$pred),]
str(test)
test$one<-1
test$cumprospects<-cumsum(test$one)
test$cumresp    <-cumsum(test$Resp)
View(test)

Perf<-subset(test,select=c(pred,cumprospects,cumresp))
Perf$PctProspect<-Perf$cumprospects/nrow(Perf)
Perf$PctResp    <-Perf$cumresp/max(Perf$cumresp)
View(Perf)

cutpoint<-subset(Perf,PctProspect>0.745 & PctProspect<0.755)

View(cutpoint)
head(test)
tail(Perf)

#Change pathname
write.csv(Perf,"C:/Users/kthan/Downloads/Perf.csv")

#5% Marginal
Marginal<-Perf %>% filter(row_number()==82|row_number()==82*2|row_number()==82*3|row_number()==82*4|row_number()==82*5
  |row_number()==82*6|row_number()==82*7|row_number()==82*8|row_number()==82*9|row_number()==82*10 
  |row_number()==82*11|row_number()==82*12|row_number()==82*13|row_number()==82*14|row_number()==82*15
  |row_number()==82*16|row_number()==82*17|row_number()==82*18|row_number()==82*19|row_number()==82*20)
print(Marginal)

Perf$Lift<-Perf$PctResp-Perf$PctProspect
MaxLift<-Perf[Perf$Lift==max(Perf$Lift),]
MaxLift

#Open in Excel, if have time
fm<-lm(Resp ~ MSKB2+PAANHA+MGEMOM+MOSTYP_39+MOSTYP_34+MOSTYP_23+MAUT2+MAUT2+MRELGE+MOSHOO_9, data = train)
summary(fm)

