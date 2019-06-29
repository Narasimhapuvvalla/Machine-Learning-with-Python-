library(readxl)
Data_Train <- read_excel("C:/Users/Narasimha/Desktop/Flight_Ticket_Participant_Datasets-20190305T100527Z-001/Flight_Ticket_Participant_Datasets/Data_Train.xlsx")
data=Data_Train
str(data)
data$Airline<-as.numeric(as.factor(data$Airline))
data$Date_of_Journey <-  as.Date(data$Date_of_Journey,"%d/%m/%Y")
data$year <- as.numeric(format(data$Date_of_Journey,'%Y'))
data$month <- as.numeric(format(data$Date_of_Journey,'%m'))
table(data$Source)
table(data$Destination)
data$Source<-as.numeric(as.factor(data$Source))
data$Destination<-as.numeric(as.factor(data$Destination))

#data$Route_1 <- substr(data$Route,  1,3)
#data$Route_2 <- substr(data$Route,  6,9)
#data$Route_3 <- substr(data$Route,  12,15)
#data$Route_4 <- substr(data$Route,  18,21)
#data$Route_5 <- substr(data$Route,  24,27)

library(stringr)
data$hours <- sapply(strsplit(data$Duration, "h"), "[", 1)
minutes<-sapply(strsplit(data$Duration, "h"), "[", 2)
data$minutes<-sapply(strsplit(minutes, "m"), "[", 1)

data$Additional_Info<-as.numeric(as.factor(data$Additional_Info))
data$Total_Stops_new= as.numeric(ifelse(data$Total_Stops=="1 stop",1,ifelse(data$Total_Stops=="2 stops",2,ifelse(data$Total_Stops=="3 stops",3,ifelse(data$Total_Stops=="4 stops",4,5)))))
data$Route_new=as.numeric(as.factor(data$Route)) 
data$minutes[is.na(data$minutes)]<-0
data$minutes=as.numeric(data$minutes)
data
data<-na.omit(data)

data$hours<-as.numeric(data$hours)
data$minutes<-as.numeric(data$minutes)

df=c()
for (i in 1:nrow(data)) {
  if(data$minutes[i]>=30){
    f=data$hours[i] + 1
    df=c(df,f)
  } else{
    g=data$hours[i] 
    df=c(df,g)
    
  }
}
data$hour_new=df

f_data=data
f_data$Route=NULL
f_data$Dep_Time=NULL
f_data$Arrival_Time=NULL
f_data$Duration=NULL
f_data$Total_Stops=NULL
f_data$Route_1=NULL
f_data$Route_2=NULL
f_data$Route_3=NULL
f_data$Route_4=NULL
f_data$Route_5=NULL
f_data$Date_of_Journey=NULL
f_data$minutes=NULL
f_data$hours=NULL
f_data$year=NULL
colSums(is.na(f_data))
f_data$hour_new[is.na(f_data$hour_new)]<-round(mean(f_data$hour_new,na.rm = TRUE))
rf
# Build Linear Regression 

linear_model<-lm(Price~.,data=f_data)
summary(linear_model)

# Random FOrest Model 

library(randomForest)
f_data=na.omit(f_data)
sum(is.na(f_data))
rf <- randomForest(data$Price ~ ., data = f_data, importance = TRUE, ntree=1000)
summary(rf)

which.min(rf$mse)

