
library(readr)
library(tidyr)
library(tidyverse) 
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(pROC)


read_and_summarise <- function() {
  df <- read.csv('/home/thzou/Projects/B.I./Customer_Churn/WA_Fn-UseC_-Telco-Customer-Churn.csv')
  print(summary(df))
  return(df)
}

check_missing <- function(df) {
  missing_data <- df %>% summarise_all(funs(sum(is.na(.))/n()))
  missing_data <- gather(missing_data, key = "variables", value = "percent_missing")
  ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
    geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+
    xlab('variables')+
    coord_flip()+ 
    theme_bw()
  
}


df <- read_and_summarise()
check_missing(df)

##VISUALIZATIONS##
#options(repr.plot.width = 6, repr.plot.height = 4)


cat ("Number of missing values :", sum(is.na(df))) #number of missing values
dfnew = na.omit(df)
cat ("\nNumber of missing values (after removing the NAs):", sum(is.na(dfnew))) #double check for missing values


dfnew <- dfnew[complete.cases(dfnew),]
dfnew$SeniorCitizen <- as.factor(ifelse(dfnew$SeniorCitizen==1, 'YES', 'NO'))

dfnew = subset(dfnew, select = -c(customerID)) #drop the customerID

options(repr.plot.width = 6, repr.plot.height = 4)
dfnew %>% 
  group_by(Churn) %>% 
  summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, -percent), percent), fill = Churn)+
  geom_col(fill = c("#FC4E07", "#E7B800"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =3)+ 
  theme_bw()+  
  xlab("Churn") + 
  ylab("Percent")+
  ggtitle("Churn Percent")

theme1 <- theme_bw()+ theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5),legend.position="none")
theme2 <- theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),legend.position="none")

#options(repr.plot.width = 14, repr.plot.height = 10)
plot_grid(ggplot(dfnew, aes(x=gender,fill=Churn))+ geom_bar()+ theme2, 
          ggplot(dfnew, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')+theme2,
          ggplot(dfnew, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')+theme2,
          ggplot(dfnew, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')+theme2,
          ggplot(dfnew, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+theme2,
          ggplot(dfnew, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+theme2+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 14)),
          align = "h")

plot_grid(ggplot(dfnew, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')+ theme2+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(dfnew, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill')+theme2+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(dfnew, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill')+theme2+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(dfnew, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill')+theme2+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(dfnew, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill')+theme2+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(dfnew, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+theme2+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")

plot_grid(ggplot(dfnew, aes(x=StreamingMovies,fill=Churn))+ 
            geom_bar(position = 'fill')+ theme2+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(dfnew, aes(x=Contract,fill=Churn))+ 
            geom_bar(position = 'fill')+theme2+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(dfnew, aes(x=PaperlessBilling,fill=Churn))+ 
            geom_bar(position = 'fill')+theme2+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(dfnew, aes(x=PaymentMethod,fill=Churn))+
            geom_bar(position = 'fill')+theme2+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")



ggplot(dfnew, aes(x=Churn, y=tenure, fill=Churn)) +
  geom_boxplot(alpha=0.4) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="red", fill="red") +
  theme(legend.position="right") +
  scale_fill_brewer(palette="Set2") +
  xlab(" ")




ggplot(dfnew, aes(x=Churn, y=MonthlyCharges, fill=Churn)) +
  geom_boxplot(alpha=0.4) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="red", fill="red") +
  theme(legend.position="right") +
  scale_fill_brewer(palette="Set1") +
  xlab(" ")




ggplot(dfnew, aes(x=Churn, y=TotalCharges, fill=Churn)) +
  geom_boxplot(alpha=0.4) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="red", fill="red") +
  theme(legend.position="right") +
  scale_fill_brewer(palette="Set3") +
  xlab(" ")


library(mlbench)
library(caret)
library(corrplot)

# calculate correlation matrix
correlationMatrix <- cor(dfnew[,c('tenure','MonthlyCharges','TotalCharges')])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)


dfnew <- data.frame(lapply(dfnew, function(x) {
  gsub("No internet service", "No", x)}))

dfnew <- data.frame(lapply(dfnew, function(x) {
  gsub("No phone service", "No", x)}))
corrplot(correlationMatrix, method="number")


num_columns <- c("tenure", "MonthlyCharges", "TotalCharges")
dfnew[num_columns] <- sapply(dfnew[num_columns], as.numeric)

telco_int <- dfnew[,c("tenure", "MonthlyCharges", "TotalCharges")]
telco_int <- data.frame(scale(telco_int))

dfnew <- mutate(dfnew, tenure_bin = tenure)

dfnew$tenure_bin[dfnew$tenure_bin >=0 & dfnew$tenure_bin <= 12] <- '0-1 year'
dfnew$tenure_bin[dfnew$tenure_bin > 12 & dfnew$tenure_bin <= 24] <- '1-2 years'
dfnew$tenure_bin[dfnew$tenure_bin > 24 & dfnew$tenure_bin <= 36] <- '2-3 years'
dfnew$tenure_bin[dfnew$tenure_bin > 36 & dfnew$tenure_bin <= 48] <- '3-4 years'
dfnew$tenure_bin[dfnew$tenure_bin > 48 & dfnew$tenure_bin <= 60] <- '4-5 years'
dfnew$tenure_bin[dfnew$tenure_bin > 60 & dfnew$tenure_bin <= 72] <- '5-6 years'

dfnew$tenure_bin <- as.factor(dfnew$tenure_bin)

ggplot(dfnew, aes(tenure_bin, fill = tenure_bin)) + geom_bar()+ theme1


telco_cat <- dfnew[,-c(5,18,19)]

#Creating Dummy Variables
dummy<- data.frame(sapply(telco_cat,function(x) data.frame(model.matrix(~x-1,data =telco_cat))[,-1]))


df_final <- cbind(telco_int,dummy)

col_names <- names(df_final[,-c(1:3)])
df_final[,col_names] <- lapply(df_final[,col_names] , factor)

#########FEATURE IMPORTANCE##########



###LVQ method

set.seed(6)
library(e1071)
library(mlbench)
library(caret)
library(doParallel)

cl <- makeCluster(4)
registerDoParallel(cl)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model_lvq <- train(Churn~., data=df_final, method="lvq", trControl=control)
# estimate variable importance
importance_lvq <- varImp(model_lvq, scale=FALSE)
stopCluster(cl)
# summarize importance
print(importance_lvq)
plot(importance_lvq)



###ADA method

set.seed(7)
cl <- makeCluster(4)
registerDoParallel(cl)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model_ada <- train(Churn~., data=df_final, method="ada", trControl=control)
# estimate variable importance
importance_ada <- varImp(model_ada, scale=FALSE)

stopCluster(cl)
# summarize importance
print(importance_ada)
# plot importance
plot(importance_ada)


###SVM Radial kernel method

set.seed(8)
cl <- makeCluster(4)
registerDoParallel(cl)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model_svm <- train(Churn~., data=df_final, method="svmRadial", trControl=control)
# estimate variable importance
importance_svm <- varImp(model_svm, scale=FALSE)
stopCluster(cl)
# summarize importance
print(importance_svm)
# plot importance
plot(importance_svm)



##########FEATURE SELECTION##########


### Recursive Feature Elimination ###

y = df_final$Churn
x = subset(df_final, select = -c(Churn)) 

# define the control using a random forest selection function
cl <- makeCluster(4)
registerDoParallel(cl)
control <- rfeControl(functions=rfFuncs, method="cv", number=10, repeats=3)
# run the RFE algorithm
results <- rfe(x, y, rfeControl=control)
stopCluster(cl)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))



### Logistic Regression ###


#Splitting the data
set.seed(123)
indices = sample.split(df_final$Churn, SplitRatio = 0.65)
train = df_final[indices,]
validation = df_final[!(indices),]

cl <- makeCluster(4)
registerDoParallel(cl)

#Build the first model using all variables
model_1 = glm(Churn ~ ., data = train, family = "binomial")
summary(model_1)

stopCluster(cl)



model_2 <- stepAIC(model_1, direction="both",trace = 0)
summary(model_2)

vif(model_2)




model_3 <-glm(formula = Churn ~ tenure + MonthlyCharges + SeniorCitizen + 
                InternetService.xFiber.optic + InternetService.xNo + 
                OnlineSecurity + OnlineBackup + TechSupport + StreamingTV + Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                PaymentMethod.xElectronic.check + tenure_bin.x1.2.years + 
                tenure_bin.x5.6.years, family = "binomial", data = train)

summary(model_3)

vif(model_3)





model_4 <-glm(formula = Churn ~ tenure + MonthlyCharges + SeniorCitizen + 
                InternetService.xFiber.optic + InternetService.xNo + OnlineSecurity + OnlineBackup + TechSupport + 
                Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                PaymentMethod.xElectronic.check + tenure_bin.x1.2.years + 
                tenure_bin.x5.6.years, family = "binomial", data = train)

summary(model_4)

vif(model_4)


### Model Evaluation ###

final_model <- model_4 #keep the final model


pred <- predict(final_model, type = "response", newdata = validation[,-24])
summary(pred)
validation$prob <- pred

# Using probability cutoff of 50%.

pred_churn <- factor(ifelse(pred >= 0.50, "Yes", "No"))
actual_churn <- factor(ifelse(validation$Churn==1,"Yes","No"))
table(actual_churn,pred_churn)

cutoff_churn <- factor(ifelse(pred >=0.50, "Yes", "No"))
conf_final <- confusionMatrix(cutoff_churn, actual_churn, positive = "Yes")
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]

round(accuracy,digits=2)
round(sensitivity,digits=2)
round(specificity,digits=2)


perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, actual_churn, positive = "Yes")
  accuray <- conf$overall[1]
  sensitivity <- conf$byClass[1]
  specificity <- conf$byClass[2]
  out <- t(as.matrix(c(sensitivity, specificity, accuray))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")  return(out)
}

summary(pred)
s = seq(0.01,0.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
abline(v = 0.32, col="red", lwd=1, lty=2)
axis(1, at = seq(0.1, 1, by = 0.1))


optimal_cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

cat("\n\nOptimal Cutoff : ", optimal_cutoff)

cutoff_churn <- factor(ifelse(pred >= optimal_cutoff, "Yes", "No"))
conf_final <- confusionMatrix(cutoff_churn, actual_churn, positive = "Yes")
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]

accuracy = round(accuracy,digits=2)
sensitivity = round(sensitivity,digits=2)
specificity = round(specificity,digits=2)



summary(final_model)

perf_metrics <- data_frame(accuracy,sensitivity,specificity)
perf_metrics
