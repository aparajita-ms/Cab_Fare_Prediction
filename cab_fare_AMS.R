rm(list=ls())

#set working directory
setwd("S:/Aparajita/edwisor/proj2_cab")

#Loading Libraries
x = c("ggplot2","gplots","DMwR",'xgboost', "corrgram",'DataCombine','Metrics', "rpart", "randomForest", "scales")
lapply(x, require, character.only = TRUE)
rm(x)

#Load data
cab_data = read.csv("train_cab.csv", header = T) 


#Explolatory Data Anlysis

#Checking the structure of data
str(cab_data)

#Changing the datatypes
cab_data$fare_amount=as.numeric(as.character(cab_data$fare_amount))
cab_data$pickup_date = as.Date(as.character(cab_data$pickup_datetime))
cab_data$pickup_mnth = as.factor(format(cab_data$pickup_date,"%m"))
cab_data$pickup_day = as.factor(format(cab_data$pickup_date,"%u"))
cab_data$pickup_hour = as.factor(format(strptime(cab_data$pickup_datetime,"%Y-%m-%d %H:%M:%S"),"%H"))
cab_data=subset(cab_data,select = -c(pickup_date,pickup_datetime))

#Visualising categorical data
ggplot(cab_data, aes_string(x = cab_data$pickup_mnth)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("pickup_mnth") + ylab('Count') + 
  ggtitle("Count for each month") +  theme(text=element_text(size=15))

ggplot(cab_data, aes_string(x = cab_data$pickup_day)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("pickup_day") + ylab('Count') + 
  ggtitle("Count for each day of week") +  theme(text=element_text(size=15))

ggplot(cab_data, aes_string(x = cab_data$pickup_hour)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("pickup_hour") + ylab('Count')  +
  ggtitle("Count for each hour") +  theme(text=element_text(size=15))

ggplot(cab_data, aes_string(x = as.factor(cab_data$passenger_count))) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("passenger_count") + ylab('Count') + 
  ggtitle("Count for no of Passenger") +  theme(text=element_text(size=15))


#Initial Cleanup

# Checking the FAre Amount
cab_data = cab_data[cab_data$fare_amount>=0,]

# Checking the passenger_count
lis=list('1','2','3','4','5','6')
`%notin%` <- Negate(`%in%`)
cab_data$passenger_count[cab_data$passenger_count %notin% lis]<-NA
cab_data$passenger_count=as.factor(cab_data$passenger_count)

#checking rows with 0 coordinates
nrow(cab_data[which(cab_data$pickup_longitude == 0 ),])
nrow(cab_data[which(cab_data$pickup_latitude == 0 ),])
nrow(cab_data[which(cab_data$dropoff_longitude == 0 ),])
nrow(cab_data[which(cab_data$pickup_latitude == 0 ),])
#Deleting rows with 0 coordinates and latitude greater than 90
cab_data = cab_data[-which(cab_data$pickup_latitude > 90),]
cab_data = cab_data[-which(cab_data$pickup_longitude == 0),]
cab_data = cab_data[-which(cab_data$dropoff_longitude == 0),]

#Missing Values Analysis
missing_val = data.frame(apply(cab_data,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(cab_data)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]

#Deleting Rows with NA
cab_data <- na.omit(cab_data)


#outlier Analysis
numeric_index = sapply(cab_data,is.numeric) 
numeric_index
numeric_data = cab_data[,numeric_index]
cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "fare_amount "), data = subset(cab_data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="fare_amount ")+
           ggtitle(paste("Box plot for",cnames[i])))
}

gridExtra::grid.arrange(gn1,ncol=1)
gridExtra::grid.arrange(gn2,gn3,ncol=2)
gridExtra::grid.arrange(gn4,gn5,ncol=2)

# #Replace all outliers with NA

 for(i in cnames){
   val = cab_data[,i][cab_data[,i] %in% boxplot.stats(cab_data[,i])$out]
   print(length(val))
   cab_data[,i][cab_data[,i] %in% val] = NA
 }

#deleting outliers
cab_data <- na.omit(cab_data)

#Feature Engineering

my_dist <- function(long1, lat1, long2, lat2) {
  rad <- pi/180
  a1 <- lat1*rad
  a2 <- long1*rad
  b1 <- lat2*rad
  b2 <- long2*rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1)*cos(b1)*(sin(dlon/2))^2
  c <- 2*atan2(sqrt(a), sqrt(1 - a))
  R <- 6367
  d <- R*c
  return(d)
}

cab_data$Distance_KM = my_dist(cab_data$pickup_longitude,cab_data$pickup_latitude,cab_data$dropoff_longitude,cab_data$dropoff_latitude)

#deleting where distance is zero
cab_data = cab_data[-which(cab_data$Distance_KM == 0),]

#visualizing distance vs fare amount
ggplot(cab_data, aes(x=Distance_KM, y=fare_amount)) + 
  geom_point(size=2, shape=23)


# Correlation Plot 
numeric_index = sapply(cab_data,is.numeric)
corrgram(cab_data[,numeric_index], order = F,upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Rearranging the Columns
col_order <- c("pickup_longitude","pickup_latitude","dropoff_longitude","dropoff_latitude","passenger_count", "pickup_mnth", "pickup_day", "pickup_hour", "Distance_KM", "fare_amount")
cab_data <- cab_data[, col_order]

#Clean the environment
rmExcept(c("my_dist", "cab_data"))

#getting test and train data
train.index = sample(1:nrow(cab_data),0.8*nrow(cab_data))
train = cab_data[ train.index,]
validation  = cab_data[-train.index,]

#Building Model

#Linear Regression

#preparaing the data for linear regression
cab_temp=cab_data

#Normality check
hist(cab_temp$Distance_KM)
hist(cab_temp$pickup_latitude)
hist(cab_temp$pickup_longitude)
hist(cab_temp$dropoff_longitude)
hist(cab_temp$dropoff_latitude)

#Normalisation
cnames = c("pickup_longitude", "pickup_latitude",  "dropoff_longitude","dropoff_latitude", "Distance_KM" )

for(i in cnames){
  cab_temp[,i] = (cab_temp[,i] - min(cab_temp[,i]))/
    (max(cab_temp[,i] - min(cab_temp[,i])))
}
rm(cnames)

train_temp = cab_data[ train.index,]
validation_temp  = cab_data[-train.index,]

lm_model = lm(fare_amount ~., data = train_temp)
summary(lm_model)
predictions_LR = predict(lm_model, validation_temp[,-10])

#Decision Tree
DT_model = rpart(fare_amount ~ ., data = train, method = "anova")
predictions_DT = predict(DT_model, validation[,-10])

#Random Forest
RF_model = randomForest(fare_amount ~ ., train, importance = TRUE, ntree=200)
plot(RF_model)
predictions_RF = predict(RF_model, validation[,-10])

#XGBoost
#data prep for XGBoost
train_data_matrix = as.matrix(sapply(train[-10],as.numeric))
validation_data_matrix = as.matrix(sapply(validation[,-10],as.numeric))

xgboost_model = xgboost(data = train_data_matrix,label = train$fare_amount,nrounds =50, verbose = FALSE)
predictions_GB = predict(xgboost_model, validation_data_matrix)

#calculate RMSE
rmse(validation_temp[,10],  predictions_LR)
rmse(validation[,10], predictions_DT)
rmse(validation[,10], predictions_RF)
rmse(validation[,10], predictions_GB)

#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y)*100)
}

MAPE(validation_temp[,10],  predictions_LR)
MAPE(validation[,10], predictions_DT)
MAPE(validation[,10], predictions_RF)
MAPE(validation[,10], predictions_GB)

#selecting gradient boosting and training it on complete training data set

cab_data_matrix = as.matrix(sapply(cab_data[-10],as.numeric))

xgboost_Final = xgboost(data = cab_data_matrix,label = cab_data$fare_amount,nrounds =50,verbose = FALSE)


#Loading test data
test = read.csv("test.csv", header = T) 
df=test

#preparing the test data
#test$passenger_count=as.factor(test$passenger_count)
test$pickup_date = as.Date(as.character(test$pickup_datetime))
test$pickup_mnth = as.factor(format(test$pickup_date,"%m"))
test$pickup_day = as.factor(format(test$pickup_date,"%u"))
test$pickup_hour = as.factor(format(strptime(test$pickup_datetime,"%Y-%m-%d %H:%M:%S"),"%H"))
test=subset(test,select = -c(pickup_date,pickup_datetime))

test$Distance_KM = my_dist(test$pickup_longitude,test$pickup_latitude,test$dropoff_longitude,test$dropoff_latitude)

col_order <- c("pickup_longitude","pickup_latitude","dropoff_longitude","dropoff_latitude","passenger_count", "pickup_mnth", "pickup_day", "pickup_hour", "Distance_KM")
test<-test[, col_order]

test_matrix = as.matrix(sapply(test,as.numeric))

#predction
predictions_Final = predict(xgboost_Final, test_matrix)

#saving the output
results <- data.frame(df, Pred_Fare = predictions_Final)

write.csv(results, file = 'GB_op_R.csv', row.names = FALSE)

