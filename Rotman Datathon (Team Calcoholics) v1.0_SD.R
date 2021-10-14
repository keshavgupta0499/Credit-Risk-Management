install.packages("car")
install.packages("ggplot2")
install.packages("hrbrthemes")
install.packages("gmodels")
install.packages("xgboost")
install.packages("Information")
install.packages("caret")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("caTools")

library(rpart.plot)
library(Matrix)
library(car)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(gmodels)
library(xgboost)
library(Information)
library(rpart)
library(caret)
library(caTools)
library(randomForest)

Master_data <- read.csv("C:/Users/sd23859/Desktop/Personal/Datathon/dataset_Fall 2021.csv")
View(Master_data)

#Create user defined variables of # months a user became n days delinquent from the origination date of the loan
# n days are taken as 30,60,90,120 and 180

Master_data$Days_30 <- round(as.numeric((as.Date(Master_data$F30_DTE) - as.Date(Master_data$ORIG_DTE))/30.42))
Master_data$Days_60 <- round(as.numeric((as.Date(Master_data$F60_DTE) - as.Date(Master_data$ORIG_DTE))/30.42))
Master_data$Days_90 <- round(as.numeric((as.Date(Master_data$F90_DTE) - as.Date(Master_data$ORIG_DTE))/30.42))
Master_data$Days_120 <- round(as.numeric((as.Date(Master_data$F120_DTE) - as.Date(Master_data$ORIG_DTE))/30.42))
Master_data$Days_180 <- round(as.numeric((as.Date(Master_data$F180_DTE) - as.Date(Master_data$ORIG_DTE))/30.42))

#Create a user defined variable of difference of months between when when a customer became 180 days delinquent to 30 days delinquent
Master_data$Days_30x180 <- Master_data$Days_180 - Master_data$Days_30

Master_data$Days_30x180[is.na(Master_data$Days_30x180)==TRUE]<- 0

#Create a categorical varibale for Type of Loan 
#1: Small scale; 2: Medium Scale; 3: Large Scale

Master_data<- Master_data %>% 
  mutate(Type_of_loan = case_when(orig_amt < mean(orig_amt)- sd(orig_amt) ~ 1,
                                  orig_amt > mean(orig_amt)+ sd(orig_amt) ~ 3,
                                  orig_amt > mean(orig_amt)- sd(orig_amt) & orig_amt < mean(orig_amt)+ sd(orig_amt)~ 2))

#Impute missing data with 0 
Master_data$CSCORE_C[is.na(Master_data$CSCORE_C)==TRUE]<- 0

#Aggregate the credit scores if there are more than 1 borrower
Master_data<- Master_data %>% 
  mutate(Aggregate_Credit_Score = case_when(CSCORE_C >0 ~ (CSCORE_B+CSCORE_C)/2,
                                            CSCORE_C==0 ~ CSCORE_B))

#Create a categorical variable for credit score
#1: Poor, 2: Moderate, 3: Good, 4: Excellent

Master_data<- Master_data %>% 
  mutate(Credit_Score_Type = case_when(Aggregate_Credit_Score >=720 ~ 4,
                                       Aggregate_Credit_Score < 720 & Aggregate_Credit_Score >= 690 ~ 3,
                                       Aggregate_Credit_Score < 690 & Aggregate_Credit_Score >= 630 ~ 2,
                                       Aggregate_Credit_Score < 630 ~ 1))

#Impute missing data with 0 
Master_data$NET_LOSS[is.na(Master_data$NET_LOSS)==TRUE]<- 0


#To identify people with high probability to be delinquent
#Create a "Good Decision" cut using the customers data who were delinquent
Master_data_cut1<- Master_data[Master_data$LAST_STAT=="D",]
Master_data_cut2<- Master_data_cut1$Days_30x180[Master_data_cut1$Days_30x180>0]
Good_decision_Cutoff <- median(Master_data_cut2)


#Good decision is the objective function, which is defined as a key parameter to reduce the deliquency rate

Master_data<- Master_data %>% 
  mutate(Good_decision = case_when(LAST_STAT=="P"~ 1,
                                   LAST_STAT=="S" | LAST_STAT=="R" | LAST_STAT=="D"~ 0,
                                   LAST_STAT=="F" & NET_LOSS <= 0 ~ 1,
                                   LAST_STAT=="F" & NET_LOSS > 0 ~ 0,
                                   LAST_STAT=="N" & NET_LOSS <= 0 ~ 1,
                                   LAST_STAT=="N" & NET_LOSS > 0 ~ 0,
                                   LAST_STAT=="T" & NET_LOSS <= 0 ~ 1,
                                   LAST_STAT=="T" & NET_LOSS > 0 ~ 0,
                                   LAST_STAT=="C" & Days_30x180 <= 0 ~ 1,
                                   LAST_STAT=="L" & Days_30x180 <= 0 ~ 1,
                                   LAST_STAT=="C" & is.na(Days_30x180)==TRUE  ~ 1,
                                   LAST_STAT=="C" & Days_30x180>0 & Days_30x180 > Good_decision_Cutoff ~ 1,
                                   LAST_STAT=="C" & Days_30x180>0 & Days_30x180 <= Good_decision_Cutoff ~ 0,
                                   LAST_STAT=="L" & Days_30x180>0 & Days_30x180 > Good_decision_Cutoff ~ 1,
                                   LAST_STAT=="L" & Days_30x180>0 & Days_30x180 <= Good_decision_Cutoff ~ 0))

#Create a user defined variable for first time home buyer flag
Master_data<- Master_data %>% 
  mutate(FTHB_FLG = case_when(FTHB_FLG =="Y" ~ 1,
                              FTHB_FLG =="N" ~ 0))

#Create a user defined variable for relocation flag
Master_data<- Master_data %>% 
  mutate(relo_flg = case_when(relo_flg =="Y" ~ 1,
                              relo_flg =="N" ~ 0))

#Create a user defined variable for purpose
#1: Cash out refinance, 2: Refinance, 3: Property, 4: Refinance (Not specified)
Master_data<- Master_data %>% 
  mutate(purpose = case_when(purpose =="C" ~ 1,
                             purpose =="R" ~ 2,
                             purpose =="P" ~ 3,
                             purpose =="U" ~ 4))

#Assuming people with blank value not to have any mortgage insurance 

Master_data$mi_pct[is.na(Master_data$mi_pct)==TRUE]<- 0

#Create a data frame with objective function and potential predictors
Final_attributes_list <- c("dti","ocltv","oltv","Type_of_loan","Credit_Score_Type","FTHB_FLG","mi_pct","relo_flg","purpose","Good_decision","msa")
Final_Master_data <- dplyr::select(Master_data, contains(Final_attributes_list))
View(Final_Master_data)

#removing oltv since highly dependent on ocltv

Final_Master_data_2<- (dplyr::select(Final_Master_data, -c("oltv")))
Final_Master_data_3<- na.omit(Final_Master_data_2)

View(Final_Master_data_3)

#Calcualte relative importance by XGBoost
Sparse.Matrix<- sparse.model.matrix(Good_decision~.-1, data = Final_Master_data_3)
head(Sparse.Matrix)
relative.importance<- xgboost(data=Sparse.Matrix,label = Final_Master_data_3$Good_decision, max.depth=5, eta=5, nthread=5, nrounds =  5)

final.importance <- xgb.importance(feature_names = Sparse.Matrix@Dimnames[[2]], model = relative.importance)
head(final.importance)

#Calcualte Information value gain and weight of evidence
Final_Master_data_4 <- Final_Master_data_3

Final_Master_data_4$dti<- as.factor(Final_Master_data_4$dti)
Final_Master_data_4$ocltv <- as.factor(Final_Master_data_4$ocltv)
Final_Master_data_4$Type_of_loan <- as.factor(Final_Master_data_4$Type_of_loan)
Final_Master_data_4$Credit_Score_Type<- as.factor(Final_Master_data_4$Credit_Score_Type)
Final_Master_data_4$FTHB_FLG <- as.factor(Final_Master_data_4$FTHB_FLG)
Final_Master_data_4$relo_flg<- as.factor(Final_Master_data_4$relo_flg)
Final_Master_data_4$purpose<- as.factor(Final_Master_data_4$purpose)

Final_Master_data_4$Good_decision<- as.numeric(Final_Master_data_4$Good_decision)

#Calculate information value
IV<- create_infotables(data=Final_Master_data_4, y="Good_decision", bins = 10, parallel = TRUE)
IV_value<- data.frame(IV$Summary)

#Calculate WOE
IV$Tables
IV_value