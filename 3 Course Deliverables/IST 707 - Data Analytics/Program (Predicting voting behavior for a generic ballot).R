# Lauren Lawless
# IST 707
# 11/25/19
# Final Project

#install.packages("caret")
library(caret)
#install.packages("rpart")
library(rpart)
#install.packages("rattle")
library(rattle)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("e1071")
library(e1071)
#install.packages("klaR")
library(klaR)
#install.packages("kernlab")
library(kernlab)
#install.packages("randomForest")
library(randomForest)
#install.packages("FSelectorRcpp")
library(FSelectorRcpp)


split.fun <- function(x, labs, digits, varlen, faclen)
{
  # replace commas with spaces (needed for strwrap)
  labs <- gsub(",", " ", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width = 22), collapse = "\n")
  }
  labs
}


df_og <- read.csv("~/Documents/Grad School Things/IST 707 - Data Analysis/Project/rstudio-export/AP_VOTECAST_2018_DATA_V3.csv")
set.seed(5)
randIndex <- sample(1:dim(df_og)[1])
cutpoint <- floor(2*dim(df_og)[1]/3)



# Qualitative Set
df_qual <- read.csv("~/Documents/Grad School Things/IST 707 - Data Analysis/Project/rstudio-export/Votecast Qual.csv")
train_qual <- df_qual[randIndex[1:cutpoint],]
test_qual <- df_qual[randIndex[(cutpoint+1):dim(df_qual)[1]],]
# Choose variables
#####
vars_qual <- c("RACE5HouseGen",
               # "MODE",
               # "State",
               # "LV",
               "LIKELYVOTER",
               # "TRACK",
               "ISSUES",
               # "FAVTRUMP",
               # "FAVREP",
               # "FAVDEM",
               # "SUPREMECOURT",
               # "GOVTDO",
               # "NatEcon",
               # "GETAHEAD",
               # "TRADENATIONALECON",
               # "TRADELOCALECON",
               # "ECONFAIRWEALTHY",
               # "ECONFAIRMIDDLE",
               # "ECONFAIRPOOR",
               # "TAXCUTS",
               # "HEALTHLAW",
               # "HEALTHGOV",
               # "IMMDEPORT",
               # "IMMBETTER",
               # "IMMWALL",
               # "RACEREL",
               # "CLIMATE",
               # "GUNPOLICY",
               # "ABORTION",
               # "METOOBELIEVE",
               # "METOODEFEND",
               # "PCSPEECH",
               # "MARIJUANA",
               # "OPIOID",
               # "SAFETERROR",
               # "SAFECRIME",
               # "SAFECYBER",
               # "RUSSIA",
               # "INTERFERENCE",
               # "Q2020VOTE",
               # "REPINTENTIONS",
               # "DEMINTENTIONS",
               "ATTENDANCE",
               # "MARRIED",
               "MARRIED2",
               "GUNOWNER",
               "UNION",
               "VET",
               "LGB",
               "TRANSGENDER",
               "BORNCITIZEN",
               # "Q2016VOTE2",
               "FIRSTTIME",
               # "Vote2014",
               # "DemViolence",
               # "RepViolence",
               "SEX",
               "AGE65",
               "RACETH5",
               "EDUC",
               "INCOME",
               "PARTY",
               # "IDEO",
               "RELIG",
               "BORNAGAIN",
               "SIZEPLACE")
#####
sml_qual <- df_qual[,vars_qual]
train_smlqual <- sml_qual[randIndex[1:cutpoint],]
test_smlqual <- sml_qual[randIndex[(cutpoint+1):dim(sml_qual)[1]],]



# Quantitative Set
df_quant <- read.csv("~/Documents/Grad School Things/IST 707 - Data Analysis/Project/rstudio-export/Votecast Quant.csv")
train_quant <- df_quant[randIndex[1:cutpoint],]
test_quant <- df_quant[randIndex[(cutpoint+1):dim(df_quant)[1]],]
# Choose variables
#####
vars_quant <- c("RACE5HouseGen",
                # "MODE_Web",
                # "State",
                # "LV",
                "LIKELYVOTER",
                # "TRACK",
                "ISSUES_Jobs",
                "ISSUES_Healthcare",
                "ISSUES_Immigration",
                "ISSUES_Taxes",
                "ISSUES_Abortion",
                "ISSUES_Guns",
                "ISSUES_Environment",
                "ISSUES_ForeignPolicy",
                "ISSUES_Terrorism",
                #"FAVTRUMP",
                #"FAVREP",
                #"FAVDEM",
                #"SUPREMECOURT",
                #"GOVTDO",
                #"NatEcon",
                #"GETAHEAD",
                #"TRADENATIONALECON",
                #"TRADELOCALECON",
                #"ECONFAIRWEALTHY",
                #"ECONFAIRMIDDLE",
                #"ECONFAIRPOOR",
                #"TAXCUTS",
                #"HEALTHLAW",
                #"HEALTHGOV",
                #"IMMDEPORT",
                #"IMMBETTER",
                #"IMMWALL",
                #"RACEREL",
                #"CLIMATE",
                #"GUNPOLICY",
                #"ABORTION",
                #"METOOBELIEVE",
                #"METOODEFEND",
                #"PCSPEECH",
                #"MARIJUANA",
                #"OPIOID",
                #"SAFETERROR",
                #"SAFECRIME",
                #"SAFECYBER",
                #"RUSSIA",
                #"INTERFERENCE",
                #"Q2020VOTE",
                #"REPINTENTIONS",
                #"DEMINTENTIONS",
                "ATTENDANCE",
                "MARRIED2",
                "GUNOWNER",
                "UNION",
                "VET",
                "LGB",
                "TRANSGENDER",
                "BORNCITIZEN",
                #"Q2016VOTE2",
                "FIRSTTIME",
                #"Vote2014",
                #"DemViolence",
                #"RepViolence",
                "SEX_Men",
                "Age18_24",
                "Age25_29",
                "Age30_39",
                "Age40_49",
                "Age50_64",
                "RACETH5_White",
                "RACETH5_AABlack",
                "RACETH5_LatHisp",
                "RACETH5_Asian",
                "EDUC_SomeCollege",
                "EDUC_CollegeGrad",
                "EDUC_PostGrad",
                "Income25_49",
                "Income50_74",
                "Income75_99",
                "Income100",
                "PARTY",
                # "IDEO",
                "RELIG_Chris",
                "RELIG_Cath",
                "RELIG_Jew",
                "RELIG_Mus",
                "RELIG_None",
                "SIZEPLACE")
#####
sml_quant <- df_quant[,vars_quant]
train_smlquant <- sml_quant[randIndex[1:cutpoint],]
test_smlquant <- sml_quant[randIndex[(cutpoint+1):dim(sml_quant)[1]],]



# Mixed Set (factor dependent; numeric independent)
df_mixed <- data.frame(df_qual[,1],df_quant[,2:dim(df_quant)[2]])
colnames(df_mixed)[1] <- "RACE5HouseGen"
train_mixed <- df_mixed[randIndex[1:cutpoint],]
test_mixed <- df_mixed[randIndex[(cutpoint+1):dim(df_mixed)[1]],]

sml_mixed <- df_mixed[,vars_quant]
train_smlmixed <- sml_mixed[randIndex[1:cutpoint],]
test_smlmixed <- sml_mixed[randIndex[(cutpoint+1):dim(sml_mixed)[1]],]




pred <- data.frame(test_qual$RACE5HouseGen, test_quant$RACE5HouseGen)



# Decision Tree
# Model 1 - All Qual
set.seed(10)
start <- Sys.time()
model_dt1 <- train(RACE5HouseGen~., data=train_qual, metric="Accuracy", method="rpart", tuneLength = 10, cp = 0, trControl=trainControl(method="cv", number=3))
stop <- Sys.time()
time_dt1 <- stop-start
model_dt1
model_dt1$finalModel
fancyRpartPlot(model_dt1$finalModel, main="Decision Tree Model 1", palettes=c("Blues","Greens","Reds"), type=4, cex=0.6, split.fun=split.fun)

pred$pred_dt1 <- predict(model_dt1, newdata=test_qual, type="raw")
confusionMatrix(pred$pred_dt1, pred$test_qual.RACE5HouseGen)

# Model 2 - Small Qual
set.seed(10)
start <- Sys.time()
model_dt2 <- train(RACE5HouseGen~., data=train_smlqual, metric="Accuracy", method="rpart", tuneLength = 8, cp = 0, trControl=trainControl(method="cv", number=3))
stop <- Sys.time()
time_dt2 <- stop-start
model_dt2
#model_dt2$finalModel
fancyRpartPlot(model_dt2$finalModel, main="Decision Tree Model 2", palettes=c("Blues","Greens","Reds"), type=4, cex=0.6, split.fun=split.fun)

pred$pred_dt2 <- predict(model_dt2, newdata=test_smlqual, type="raw")
confusionMatrix(pred$pred_dt2, pred$test_qual.RACE5HouseGen)

# Model 3 - Small Qual minus TRACK
set.seed(10)
start <- Sys.time()
model_dt3 <- train(RACE5HouseGen~., data=train_smlqual, metric="Accuracy", method="rpart", tuneLength = 8, cp = 0, trControl=trainControl(method="cv", number=3))
stop <- Sys.time()
time_dt3 <- stop-start
model_dt3
#model_dt3$finalModel
fancyRpartPlot(model_dt3$finalModel, main="Decision Tree Model 3", palettes=c("Blues","Greens","Reds"), type=4, cex=0.6, split.fun=split.fun)

pred$pred_dt3 <- predict(model_dt3, newdata=test_smlqual, type="raw")
confusionMatrix(pred$pred_dt3, pred$test_qual.RACE5HouseGen)



# Naive Bayes
# Model 4 - All Mixed
set.seed(10)
start <- Sys.time()
model_nb4 <- train(RACE5HouseGen~., data=train_mixed, method="nb", trControl=trainControl(method="cv", number=3), tuneGrid=expand.grid(usekernel=c(TRUE,FALSE), fL=0:2, adjust=0:2)) 
stop <- Sys.time()
time_nb4 <- stop-start
time_nb4
model_nb4

pred$pred_nb4 <- predict(model_nb4, newdata=test_mixed, type="raw")
confusionMatrix(pred$pred_nb4, pred$test_qual.RACE5HouseGen)

# Model 5 - Small Mixed
set.seed(10)
start <- Sys.time()
model_nb5 <- train(RACE5HouseGen~., data=train_smlmixed, method="nb", trControl=trainControl(method="cv", number=3), tuneGrid=expand.grid(usekernel=c(TRUE,FALSE), fL=0:2, adjust=0:2)) 
stop <- Sys.time()
time_nb5 <- stop-start
time_nb5
model_nb5

pred$pred_nb5 <- predict(model_nb5, newdata=test_smlmixed, type="raw")
confusionMatrix(pred$pred_nb5, pred$test_qual.RACE5HouseGen)



# kNN
# Model 6 - Default
set.seed(10)
start <- Sys.time()
model_knn6 <- train(RACE5HouseGen~., data=train_smlmixed, method="knn")
stop <- Sys.time()
time_knn6 <- stop-start
time_knn6
model_knn6

pred$pred_knn6 <- predict(model_knn6, newdata=test_smlmixed)
confusionMatrix(pred$pred_knn6, pred$test_qual.RACE5HouseGen)

# Model 7 - Tuning
set.seed(10)
start <- Sys.time()
model_knn7 <- train(RACE5HouseGen~., data=train_smlmixed, method="knn",
                    tuneGrid=data.frame(k=seq(0,25,1)),
                    trControl=trainControl(method="repeatedcv",
                                           number=10, repeats=3
                    )
)
stop <- Sys.time()
time_knn7 <- stop-start
time_knn7
model_knn7

pred$pred_knn7 <- predict(model_knn7, newdata=test_smlmixed)
confusionMatrix(pred$pred_knn7, pred$test_qual.RACE5HouseGen)



# SVM
# Model 8 - Linear
set.seed(10)
start <- Sys.time()
model_svm8 <- train(RACE5HouseGen~., data=train_smlmixed,
                    method="svmLinear",
                    preProcess=c("center", "scale"),
                    trControl=trainControl(method="boot", number=25),
                    tuneGrid=expand.grid(C=seq(2.5, 5, 0.5)
                    )
)
stop <- Sys.time()
time_svm8 <- stop-start
time_svm8
model_svm8
plot(model_svm8)

pred$pred_svm8 <- predict(model_svm8, newdata=test_smlmixed)
confusionMatrix(pred$pred_svm8, pred$test_qual.RACE5HouseGen)

# Model 9 - Nonlinear separability (radial)
set.seed(10)
start <- Sys.time()
model_svm9 <- train(RACE5HouseGen~., data=train_smlmixed,
                    method="svmRadial",
                    preProcess=c("center", "scale"),
                    trControl=trainControl(method="boot", number=25),
                    tuneGrid=expand.grid(sigma=seq(0.5, 2, 0.5),
                                         C=seq(0.5, 2, 0.5)
                    )
)
stop <- Sys.time()
time_svm9 <- stop-start
time_svm9
model_svm9
plot(model_svm9)

pred$pred_svm9 <- predict(model_svm9, newdata=test_smlmixed)
confusionMatrix(pred$pred_svm9, pred$test_qual.RACE5HouseGen)

# Model 10 - Nonlinear separability (polynomial)
set.seed(10)
start <- Sys.time()
model_svm10 <- train(RACE5HouseGen~., data=train_smlmixed,
                     method = "svmPoly",
                     preProcess=c("center", "scale"),
                     trControl=trainControl(method="boot", number=25),
                     tuneGrid=expand.grid(degree=seq(0, 2, 1),
                                          scale=seq(0.5, 2, 0.5),
                                          C=seq(0.5, 2, 0.5)
                     )
)
stop <- Sys.time()
time_svm10 <- stop-start
time_svm10
model_svm10
plot(model_svm10)

pred$pred_svm10 <- predict(model_svm10, newdata=test_smlmixed)
confusionMatrix(pred$pred_svm10, pred$test_qual.RACE5HouseGen)

# Model 11 - Nonlinear separability (polynomial)
set.seed(10)
start <- Sys.time()
model_svm11 <- train(RACE5HouseGen~., data=train_smlmixed,
                     method = "svmPoly",
                     preProcess=c("center", "scale"),
                     trControl=trainControl(method="boot", number=25),
                     tuneGrid=expand.grid(degree=seq(3, 4, 1),
                                          scale=seq(0.5, 2, 0.5),
                                          C=seq(0.5, 2, 0.5)
                     )
)
stop <- Sys.time()
time_svm11 <- stop-start
time_svm11
model_svm11
plot(model_svm11)

pred$pred_svm11 <- predict(model_svm11, newdata=test_smlmixed)
confusionMatrix(pred$pred_svm11, pred$test_qual.RACE5HouseGen)



# Random Forest
# Model 12
set.seed(10)
start <- Sys.time()
model_rf12 <- train(RACE5HouseGen~., data=train_smlqual, method="rf")
stop <- Sys.time()
time_rf12 <- stop-start
time_rf12
model_rf12$finalModel

pred$pred_rf12 <- predict(model_rf12, newdata=test_smlqual)
confusionMatrix(pred$pred_rf12, pred$test_qual.RACE5HouseGen)

varimp_rf <- varImp(model_rf12)
varimp_rf
plot(varimp_rf, main = "Variable Importance with Random Forest")



# Pred Results
pred_num <- pred[,-1]
for (i in 2:dim(pred_num)[2]){
  pred_num[,i] <- as.integer(ifelse(pred_num[,i]=="Republican",1,ifelse(pred_num[,i]=="Democrat",-1,0)))
}
str(pred_num)
vars_score <- c(3,4,7,8,9,11,13)
pred_num$score <- rowSums(pred_num[,vars_score])/(length(vars_score))
pred_num$ensemble <- round(pred_num$score)
confusionMatrix(as.factor(pred_num$ensemble), as.factor(pred_num$test_quant.RACE5HouseGen))
View(pred_num[which(pred_num$test_quant.RACE5HouseGen==0),])



# Gain Ratio
info_qual <- information_gain(RACE5HouseGen~., data=train_qual, type="gainratio")
info_smlqual <- information_gain(RACE5HouseGen~., data=train_smlqual, type="gainratio")
info_quant <- information_gain(RACE5HouseGen~., data=train_quant, type="gainratio")
info_smlquant <- information_gain(RACE5HouseGen~., data=train_smlquant, type="gainratio")

head(info_qual[order(-info_qual$importance),],20)
head(info_smlqual[order(-info_smlqual$importance),],20)
head(info_quant[order(-info_quant$importance),],20)
head(info_smlquant[order(-info_smlquant$importance),],20)

