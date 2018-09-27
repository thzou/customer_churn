
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