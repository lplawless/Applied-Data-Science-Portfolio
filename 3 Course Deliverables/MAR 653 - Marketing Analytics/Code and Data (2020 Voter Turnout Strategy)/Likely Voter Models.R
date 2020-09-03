# Estimate likelihood of voting among Abrams supporters
# Lauren Lawless
# MAR 653
# February 26, 2020

library(readxl)
#install.packages("ggplot2")
library(ggplot2)

# Function definition
get_hitrate <- function(thresh){
  test$pred <- ifelse(test$pred_p > thresh,1,0)
  correct <- sum(ifelse(test$lv_abrams + test$pred == 1,0,1))/nrow(test)
  return(correct)
}

best_hitrate <- 0
best_tp <- 0
hitrates <- data.frame("threshold"=(1:100)/100, "hitrate"=-1)

#####
# Obtain and Scrub
Project_Analysis <- read_excel("Project Analysis.xlsx", sheet = "All")
PA_sml <- Project_Analysis[Project_Analysis$vote18_abrams == 1,-1:-3]
PA_sml <- PA_sml[,-2:-4]
PA_sml$lv_abrams <- (PA_sml$likelyvoter-1)*-1 * PA_sml$vote18_abrams
#sum(PA_sml$lv_abrams)
hist(PA_sml$lv_abrams)
table(PA_sml$lv_abrams, PA_sml$Train)

train <- PA_sml[PA_sml$Train == 1,c(-1,-3,-4,-54)]
test <- PA_sml[PA_sml$Train == 0,c(-1,-3,-4,-54)]

#####
# Model 0 - all
# hr: 0.9289544
# tp: 0.009383378
# Model on train
train_0 <- train[,c(-13,-17,-23,-28:-30,-34,-39,-46,-50)]
model_0 <- glm(lv_abrams~., family=binomial(link='logit'), data=train_0)
summary(model_0)

# Predict on test
test$pred_p <- predict(model_0, newdata=test, type=c("response"))
hist(test$pred_p)

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

plot(test$pred_p,test$lv_abrams)
abline(v=thresh_optimal, col="blue")

# Report results
test$pred <- ifelse(test$pred_p > thresh_optimal,1,0)
confusion <- table(test$lv_abrams, test$pred)
print(confusion)

hitrate <- (confusion[1,1]+confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])
tp <- (confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])

print(ifelse(hitrate > best_hitrate && tp >= best_tp, paste("New best model by hit rate!"),
             ifelse(tp > best_tp && hitrate >= best_hitrate, "New best model by true positives!","Not the best model :(")))
cat("Current hit rate:\t",hitrate,"\nBest hit rate:\t\t",best_hitrate,"\nCurrent tp:\t\t",tp,"\nBest tp:\t\t",best_tp,"\n")
best_hitrate <- ifelse(hitrate > best_hitrate || (tp > best_tp && hitrate == best_hitrate), hitrate, best_hitrate)
best_tp <- ifelse(tp > best_tp && hitrate == best_hitrate || hitrate == best_hitrate, tp, best_tp)


#####
# Model 1 - drop sizeplace
# hr: 0.9302949
# tp: 0.009383378
# Model on train
train_1 <- train[,c(-13,-17,-23,-28:-30,-34,-39,-46:-50)]
model_1 <- glm(lv_abrams~., family=binomial(link='logit'), data=train_1)
summary(model_1)

# Predict on test
test$pred_p <- predict(model_1, newdata=test, type=c("response"))
hist(test$pred_p)

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

plot(test$pred_p,test$lv_abrams)
abline(v=thresh_optimal, col="blue")

# Report results
test$pred <- ifelse(test$pred_p > thresh_optimal,1,0)
confusion <- table(test$lv_abrams, test$pred)
print(confusion)

hitrate <- (confusion[1,1]+confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])
tp <- (confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])

print(ifelse(hitrate > best_hitrate && tp >= best_tp, paste("New best model by hit rate!"),
             ifelse(tp > best_tp && hitrate >= best_hitrate, "New best model by true positives!","Not the best model :(")))
cat("Current hit rate:\t",hitrate,"\nBest hit rate:\t\t",best_hitrate,"\nCurrent tp:\t\t",tp,"\nBest tp:\t\t",best_tp,"\n")
best_hitrate <- ifelse(hitrate > best_hitrate || (tp > best_tp && hitrate == best_hitrate), hitrate, best_hitrate)
best_tp <- ifelse(tp > best_tp && hitrate == best_hitrate || hitrate == best_hitrate, tp, best_tp)


#####
# Model 2 - drop vote decide, relig
# hr: 0.9827229
# tp: 0.02902557
# Model on train
train_2 <- train[,c(-14:-18,-24,-29:-31,-35,-40:-47,-51)]
model_2 <- glm(lv_abrams~., family=binomial(link='logit'), data=train_2)
summary(model_2)

# Predict on test
test$pred_p <- predict(model_2, newdata=test, type=c("response"))
hist(test$pred_p)

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

plot(test$pred_p,test$lv_abrams)
abline(v=thresh_optimal, col="blue")

# Report results
test$pred <- ifelse(test$pred_p > thresh_optimal,1,0)
confusion <- table(test$lv_abrams, test$pred)
print(confusion)

hitrate <- (confusion[1,1]+confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])
tp <- (confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])

print(ifelse(hitrate > best_hitrate && tp >= best_tp, paste("New best model by hit rate!"),
             ifelse(tp > best_tp && hitrate >= best_hitrate, "New best model by true positives!","Not the best model :(")))
cat("Current hit rate:\t",hitrate,"\nBest hit rate:\t\t",best_hitrate,"\nCurrent tp:\t\t",tp,"\nBest tp:\t\t",best_tp,"\n")
best_hitrate <- ifelse(hitrate > best_hitrate || (tp > best_tp && hitrate == best_hitrate), hitrate, best_hitrate)
best_tp <- ifelse(tp > best_tp && hitrate == best_hitrate || hitrate == best_hitrate, tp, best_tp)


#####
# Model 3 - drop vote decide, relig
# hr: 0.983414
# tp: 0.02971666
# Model on train
train_3 <- train[,c(-14:-18,-24,-29:-31,-35,-40:-47,-51)]
model_3 <- glm(lv_abrams~., family=binomial(link='logit'), data=train_3)
summary(model_3)

# Predict on test
test$pred_p <- predict(model_3, newdata=test, type=c("response"))
hist(test$pred_p)

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

plot(test$pred_p,test$lv_abrams)
abline(v=thresh_optimal, col="blue")

# Report results
test$pred <- ifelse(test$pred_p > thresh_optimal,1,0)
confusion <- table(test$lv_abrams, test$pred)
print(confusion)

hitrate <- (confusion[1,1]+confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])
tp <- (confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])

print(ifelse(hitrate > best_hitrate && tp >= best_tp, paste("New best model by hit rate!"),
             ifelse(tp > best_tp && hitrate >= best_hitrate, "New best model by true positives!","Not the best model :(")))
cat("Current hit rate:\t",hitrate,"\nBest hit rate:\t\t",best_hitrate,"\nCurrent tp:\t\t",tp,"\nBest tp:\t\t",best_tp,"\n")
best_hitrate <- ifelse(hitrate > best_hitrate || (tp > best_tp && hitrate == best_hitrate), hitrate, best_hitrate)
best_tp <- ifelse(tp > best_tp && hitrate == best_hitrate || hitrate == best_hitrate, tp, best_tp)


#####
# Model 4 - drop vote decide, relig, income
# hr: 
# tp: 
# Model on train
train_4 <- train[,c(-15:-19,-25,-30:-32,-36:-40,-41:-48,-52)]
model_4 <- glm(lv_abrams~., family=binomial(link='logit'), data=train_4)
summary(model_4)

# Predict on test
test$pred_p <- predict(model_4, newdata=test, type=c("response"))
hist(test$pred_p)

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

plot(test$pred_p,test$lv_abrams)
abline(v=thresh_optimal, col="blue")

# Report results
test$pred <- ifelse(test$pred_p > thresh_optimal,1,0)
confusion <- table(test$lv_abrams, test$pred)
print(confusion)

hitrate <- (confusion[1,1]+confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])
tp <- (confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])

print(ifelse(hitrate > best_hitrate && tp >= best_tp, paste("New best model by hit rate!"),
             ifelse(tp > best_tp && hitrate >= best_hitrate, "New best model by true positives!","Not the best model :(")))
cat("Current hit rate:\t",hitrate,"\nBest hit rate:\t\t",best_hitrate,"\nCurrent tp:\t\t",tp,"\nBest tp:\t\t",best_tp,"\n")
best_hitrate <- ifelse(hitrate > best_hitrate || (tp > best_tp && hitrate == best_hitrate), hitrate, best_hitrate)
best_tp <- ifelse(tp > best_tp && hitrate == best_hitrate || hitrate == best_hitrate, tp, best_tp)
