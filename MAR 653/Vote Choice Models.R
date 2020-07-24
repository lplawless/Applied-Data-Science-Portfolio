# Estimate likelihood of supporting Abrams among all voters
# Lauren Lawless
# MAR 653
# February 26, 2020

library(readxl)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("DescTools")
library(DescTools)

# Function definition
get_hitrate <- function(thresh){
  test$pred <- ifelse(test$pred_p > thresh,1,0)
  correct <- sum(ifelse(test$vote18_abrams + test$pred == 1,0,1))/nrow(test)
  return(correct)
}

best_hitrate <- 0
best_tp <- 0
hitrates <- data.frame("threshold"=(1:100)/100, "hitrate"=-1)

#####
# Obtain and Scrub
Project_Analysis <- read_excel("Project Analysis.xlsx", sheet = "All")
PA_sml <- Project_Analysis[,-1:-3]
PA_sml <- PA_sml[,-2:-4]
train <- PA_sml[PA_sml$Train == 1,-1]
test <- PA_sml[PA_sml$Train == 0,-1]

#####
# Model 0 - all
# hr: 0.918452
# tp: 0.4865238
# Model on train
train_0 <- train[,c(-15,-19,-25,-30:-32,-36,-41,-48,-52)]
model_0 <- glm(vote18_abrams~., family=binomial(link='logit'), data=train_0)
summary(model_0)

# Predict on test
test$pred_p <- predict(model_0, newdata=test, type=c("response"))
#hist(test$pred_p)
#plot(test$pred_p,test$vote18_abrams)

# Find optimal prediction threshold
hitrates <- data.frame("threshold" = 1:100)
hitrates$threshold <- hitrates$threshold/100
for(i in 1:100) {
  hitrates$hitrate[i] <- get_hitrate(hitrates$threshold[i])
}
# plot(hitrates$threshold, hitrates$hitrate,
#      main="Optimal Prediction Threshold",
#      xlab="Threshold",
#      ylab="Hit Rate",
#      type="p")

thresh_optimal <- hitrates$threshold[which.max(hitrates$hitrate)]

# Report results
test$pred <- ifelse(test$pred_p > thresh_optimal,1,0)
confusion <- table(test$vote18_abrams, test$pred)
print(confusion)

hitrate <- (confusion[1,1]+confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])
tp <- (confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])

print(ifelse(hitrate > best_hitrate, paste("New best model by hit rate!"),
             ifelse(tp > best_tp, "New best model by true positives!","Not the best model :(")))
cat("Current hit rate:\t",hitrate,"\nBest hit rate:\t\t",best_hitrate,"\nCurrent tp:\t\t",tp,"\nBest tp:\t\t",best_tp,"\n")
best_hitrate <- ifelse(hitrate > best_hitrate || (tp > best_tp && hitrate == best_hitrate), hitrate, best_hitrate)
best_tp <- ifelse(hitrate > best_hitrate || (tp > best_tp && hitrate == best_hitrate), tp, best_tp)


# accuracy = (confusion[1,1]+confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])

#####
# Model 1 - drop firsttimevoter, sex_m
# hr: 0.918452
# tp: 0.4872149
# Model on train
train_1 <- train[,c(1:3,6:14,16:18,20:24,26:29,33:35,37:40,42:47,49:51,53)]
model_1 <- glm(vote18_abrams~., family=binomial(link='logit'), data=train_1)
summary(model_1)

# Predict on test
test$pred_p <- predict(model_1, newdata=test, type=c("response"))
#hist(test$pred_p)
#plot(test$pred_p,test$vote18_abrams)

# Find optimal prediction threshold
hitrates <- data.frame("threshold" = 1:100)
hitrates$threshold <- hitrates$threshold/100
for(i in 1:100) {
  hitrates$hitrate[i] <- get_hitrate(hitrates$threshold[i])
}
# plot(hitrates$threshold, hitrates$hitrate,
#      main="Optimal Prediction Threshold",
#      xlab="Threshold",
#      ylab="Hit Rate",
#      type="p")

thresh_optimal <- hitrates$threshold[which.max(hitrates$hitrate)]

# Report results
test$pred <- ifelse(test$pred_p > thresh_optimal,1,0)
confusion <- table(test$vote18_abrams, test$pred)
print(confusion)

hitrate <- (confusion[1,1]+confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])
tp <- (confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])

print(ifelse(hitrate > best_hitrate, paste("New best model by hit rate!"),
             ifelse(tp > best_tp, "New best model by true positives!","Not the best model :(")))
cat("Current hit rate:\t",hitrate,"\nBest hit rate:\t\t",best_hitrate,"\nCurrent tp:\t\t",tp,"\nBest tp:\t\t",best_tp,"\n")
best_hitrate <- ifelse(hitrate > best_hitrate || (tp > best_tp && hitrate == best_hitrate), hitrate, best_hitrate)
best_tp <- ifelse(hitrate > best_hitrate || (tp > best_tp && hitrate == best_hitrate), tp, best_tp)


#####
# Model 2 - drop firsttimevoter, sex_m, relig
# hr: 0.9177609
# tp: 0.4872149
# Model on train
train_2 <- train[,c(1:3,6:14,16:18,20:24,26:29,33:35,37:40,49:51,53)]
model_2 <- glm(vote18_abrams~., family=binomial(link='logit'), data=train_2)
summary(model_2)

# Predict on test
test$pred_p <- predict(model_2, newdata=test, type=c("response"))
#hist(test$pred_p)
#plot(test$pred_p,test$vote18_abrams)

# Find optimal prediction threshold
hitrates <- data.frame("threshold" = 1:100)
hitrates$threshold <- hitrates$threshold/100
for(i in 1:100) {
  hitrates$hitrate[i] <- get_hitrate(hitrates$threshold[i])
}
# plot(hitrates$threshold, hitrates$hitrate,
#      main="Optimal Prediction Threshold",
#      xlab="Threshold",
#      ylab="Hit Rate",
#      type="p")

thresh_optimal <- hitrates$threshold[which.max(hitrates$hitrate)]

# Report results
test$pred <- ifelse(test$pred_p > thresh_optimal,1,0)
confusion <- table(test$vote18_abrams, test$pred)
print(confusion)

hitrate <- (confusion[1,1]+confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])
tp <- (confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])

print(ifelse(hitrate > best_hitrate, paste("New best model by hit rate!"),
             ifelse(tp > best_tp, "New best model by true positives!","Not the best model :(")))
cat("Current hit rate:\t",hitrate,"\nBest hit rate:\t\t",best_hitrate,"\nCurrent tp:\t\t",tp,"\nBest tp:\t\t",best_tp,"\n")
best_hitrate <- ifelse(hitrate > best_hitrate || (tp > best_tp && hitrate == best_hitrate), hitrate, best_hitrate)
best_tp <- ifelse(hitrate > best_hitrate || (tp > best_tp && hitrate == best_hitrate), tp, best_tp)


#####
# Model 3 - drop firsttimevoter, sex_m, income
# hr: 0.918452
# tp: 0.4865238
# Model on train
train_3 <- train[,c(1:3,6:14,16:18,20:24,26:29,33:35,42:47,49:51,53)]
model_3 <- glm(vote18_abrams~., family=binomial(link='logit'), data=train_3)
summary(model_3)

# Predict on test
test$pred_p <- predict(model_3, newdata=test, type=c("response"))
#hist(test$pred_p)
#plot(test$pred_p,test$vote18_abrams)

# Find optimal prediction threshold
hitrates <- data.frame("threshold" = 1:100)
hitrates$threshold <- hitrates$threshold/100
for(i in 1:100) {
  hitrates$hitrate[i] <- get_hitrate(hitrates$threshold[i])
}
# plot(hitrates$threshold, hitrates$hitrate,
#      main="Optimal Prediction Threshold",
#      xlab="Threshold",
#      ylab="Hit Rate",
#      type="p")

thresh_optimal <- hitrates$threshold[which.max(hitrates$hitrate)]

# Report results
test$pred <- ifelse(test$pred_p > thresh_optimal,1,0)
confusion <- table(test$vote18_abrams, test$pred)
print(confusion)

hitrate <- (confusion[1,1]+confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])
tp <- (confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])

print(ifelse(hitrate > best_hitrate, paste("New best model by hit rate!"),
             ifelse(tp > best_tp, "New best model by true positives!","Not the best model :(")))
cat("Current hit rate:\t",hitrate,"\nBest hit rate:\t\t",best_hitrate,"\nCurrent tp:\t\t",tp,"\nBest tp:\t\t",best_tp,"\n")
best_hitrate <- ifelse(hitrate > best_hitrate || (tp > best_tp && hitrate == best_hitrate), hitrate, best_hitrate)
best_tp <- ifelse(hitrate > best_hitrate || (tp > best_tp && hitrate == best_hitrate), tp, best_tp)


#####
# Model 4 - drop firsttimevoter, sex_m, income, races other than White and Black
# hr: 0.9191431
# tp: 0.4872149
# Model on train
train_4 <- train[,c(1:3,6:14,16:18,20:24,26:27,33:35,42:47,49:51,53)]
model_4 <- glm(vote18_abrams~., family=binomial(link='logit'), data=train_4)
summary(model_4)
pR2_mcf <- PseudoR2(model_4,which="McFadden")
print(pR2_mcf)
pR2_mcfadj <- PseudoR2(model_4,which="McFaddenAdj")
print(pR2_mcfadj)
pR2_cs <- PseudoR2(model_4,which="CoxSnell")
print(pR2_cs)

# Predict on test
test$pred_p <- predict(model_4, newdata=test, type=c("response"))
hist(test$pred_p, main="Distribution of P_estimate", xlab="P(vote18_abrams = 1")

# Find optimal prediction threshold
hitrates <- data.frame("threshold" = 1:100)
hitrates$threshold <- hitrates$threshold/100
for(i in 1:100) {
  hitrates$hitrate[i] <- get_hitrate(hitrates$threshold[i])
}
plot(hitrates$threshold, hitrates$hitrate,
     main="Optimal Prediction Threshold",
     xlab="Threshold",
     ylab="Hit Rate",
     type="p")

thresh_optimal <- hitrates$threshold[which.max(hitrates$hitrate)]

plot(test$pred_p, test$vote18_abrams, main="Accuracy", ylab="vote18_abrams", xlab="P(vote18_abrams = 1)")
abline(v=thresh_optimal, col="blue")

# Report results
test$pred <- ifelse(test$pred_p > thresh_optimal,1,0)
confusion <- table(test$vote18_abrams, test$pred)
print(confusion)

hitrate <- (confusion[1,1]+confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])
tp <- (confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])


print(ifelse(hitrate > best_hitrate, paste("New best model by hit rate!"),
             ifelse(tp > best_tp, "New best model by true positives!","Not the best model :(")))
cat("Current hit rate:\t",hitrate,"\nBest hit rate:\t\t",best_hitrate,"\nCurrent tp:\t\t",tp,"\nBest tp:\t\t",best_tp,"\n")
best_hitrate <- ifelse(hitrate > best_hitrate || (tp > best_tp && hitrate == best_hitrate), hitrate, best_hitrate)
best_tp <- ifelse(hitrate > best_hitrate || (tp > best_tp && hitrate == best_hitrate), tp, best_tp)

tp = test[test$pred==1 & test$vote18_abrams==1,]
fp = test[test$pred==1 & test$vote18_abrams==0,]
fn = test[test$pred==0 & test$vote18_abrams==1,]
summary(tp)[4,]
summary(fp)[4,]
summary(fn)[4,]

#####
# Model 5
# Model on train
train_5 <- train[,c(1:3,6:14,16:18,20:24,26:27,33:35,42:47,49:51,53)]#
#train_5$wht_guns <- train_5$raceth_wht * train_5$issues_gunpolicy
#test$wht_guns <- test$raceth_wht * test$issues_gunpolicy
#train_5$wht_party <- train_5$raceth_wht * train_5$party
#test$wht_party <- test$raceth_wht * test$party
#train_5$guns_party <- train_5$issues_gunpolicy * train_5$party
#test$guns_party <- test$issues_gunpolicy * test$party
#train_5$imm_party <- train_5$issues_immigration * train_5$party
#test$imm_party <- test$issues_immigration * test$party
train_5$party_known <- train_5$party * train_5$votedecide18_iveknownallalong
test$party_known <- test$party * test$votedecide18_iveknownallalong

ggplot(test, aes(votedecide18_iveknownallalong, party, color=vote18_abrams)) +
  geom_jitter(width = 0.25, height = 0.25)
  
ggplot(test,aes(party_known,vote18_abrams, color=issues_healthcare)) +
  geom_jitter(width=0.25,height=0.25)

model_5 <- glm(vote18_abrams~., family=binomial(link='logit'), data=train_5)
summary(model_5)

# Predict on test
test$pred_p <- predict(model_5, newdata=test, type=c("response"))
hist(test$pred_p, main="Distribution of P_estimate", xlab="P(vote18_abrams = 1)")

# Find optimal prediction threshold
hitrates <- data.frame("threshold" = 1:100)
hitrates$threshold <- hitrates$threshold/100
for(i in 1:100) {
  hitrates$hitrate[i] <- get_hitrate(hitrates$threshold[i])
}
plot(hitrates$threshold, hitrates$hitrate,
     main="Optimal Prediction Threshold",
     xlab="Threshold",
     ylab="Hit Rate",
     type="p")

thresh_optimal <- hitrates$threshold[which.max(hitrates$hitrate)]

plot(test$pred_p, test$vote18_abrams, main="Accuracy", ylab="vote18_abrams", xlab="P(vote18_abrams = 1)")
abline(v=thresh_optimal, col="blue")

# Report results
test$pred <- ifelse(test$pred_p > thresh_optimal,1,0)
confusion <- table(test$vote18_abrams, test$pred)
print(confusion)

hitrate <- (confusion[1,1]+confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])
tp <- (confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])

print(ifelse(hitrate > best_hitrate, paste("New best model by hit rate!"),
             ifelse(tp > best_tp, "New best model by true positives!","Not the best model :(")))
cat("Current hit rate:\t",hitrate,"\nBest hit rate:\t\t",best_hitrate,"\nCurrent tp:\t\t",tp,"\nBest tp:\t\t",best_tp,"\n")
best_hitrate <- ifelse(hitrate > best_hitrate || (tp > best_tp && hitrate == best_hitrate), hitrate, best_hitrate)
best_tp <- ifelse(hitrate > best_hitrate || (tp > best_tp && hitrate == best_hitrate), tp, best_tp)

