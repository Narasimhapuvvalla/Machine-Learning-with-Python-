rm(list = ls())
getwd()

# setwd("<location of your dataset>")


library(readr)
library(readxl)
train=read_excel("C:\\Users\\Narasimha\\Desktop\\Data_Train.xlsx")
train=read_excel("E:\\Chinna\\Research and Study\\Datasets\\Flight\\Data_Train.xlsx")
install.packages(readtab)
test=read_excel("E:\\Chinna\\Research and Study\\Datasets\\Flight\\Test_set.xlsx")
head(test)
my_data <- read_excel("my_file.xlsx", sheet = "data")
# Specify sheet by its index

my_data <- read_excel("my_file.xlsx", sheet = 2)
read_excel("my-spreadsheet.xls", na = "NA")
install.packages("xlsx")
read.xlsx(file, sheetIndex, header=TRUE)
read.xlsx2(file, sheetIndex, header=TRUE)
# .txt files read from 
df <- read.table("<FileName>.txt", 
                 header = TRUE)
install.packages("data.table")
library(data.table)
?read.table

sum(is.na(train))
head(train)
tail(train);str(train);
table(train$Airline)
table(train$Date_of_Journey)
table(train$Source)
table(train$Destination)
table(train$Route)
table(train$Dep_Time)
table(train$Arrival_Time)
table(train$Duration)
table(train$Total_Stops)
table(train$Additional_Info)
table(train$Date_of_Journey)


# data convertions 
tar=train 
train$Additional_Info= as.numeric(ifelse(train$Additional_Info=="1 Long layover",1,ifelse(train$Additional_Info=="1 Short layover",2,ifelse(train$Additional_Info=="2 Long layover",3,ifelse(train$Additional_Info=="Business class",4,ifelse(train$Additional_Info=="Change airports",5,ifelse(train$Additional_Info=="In-flight meal not included",6,ifelse(train$Additional_Info=="No check-in baggage included",7,ifelse(train$Additional_Info=="No info",8,ifelse(train$Additional_Info=="No Info",8,9))))))))))
table(train$Additional_Info) 
train$Total_Stops= as.numeric(ifelse(train$Total_Stops=="1 stop",1,ifelse(train$Total_Stops=="2 stops",2,ifelse(train$Total_Stops=="3 stops",3,ifelse(train$Total_Stops=="4 stops",4,5)))))
table(train$Total_Stops) 
install.packages("lubridate")
library(lubridate)
# train$time.duration <- as.duration(train$Duration)
train$time.duration<-NULL
str(train)
train$Dep_Time=as.factor(train$Dep_Time)
train$Arrival_Time=as.factor(train$Arrival_Time)
train$Duration=as.factor(train$Duration)
train$Route=as.factor(train$Route)
train$Route=as.numeric(train$Route)
table(train$Route)
train$Source=as.numeric(as.factor(train$Source))
train$Destination=as.numeric(as.factor(train$Destination))
train$Airline=as.numeric(as.factor(train$Airline))

dummy=train[,-c(2)]
model=lm(dummy$Price~.,data = dummy)
summary(model)
library(stringr)
df=as.vector(str_split(dummy$Duration, "h"))
str(df)




################# Data Import #########################

# From mysql 
install.packages("RMySQL")
library(RMySQL)

library(RMySQL)
library(psych)
con <- dbConnect(MySQL(),
                 user = 'root',
                 password = 'Narasimha@123',
                 host = 'localhost',
                 dbname='sakila')
dbListTables(con)
df=dbGetQuery(con,"select * from customer")
df=dbGetQuery(con,"select * from customer LIMIT 5;")
dat=dbSendQuery(con,"select * from customer")
dbGetInfo(dat)
dbListFields(con,"customer")
dbGetStatement(dat)
dbWriteTable(con,"mtcars",mtcars)
dfg=dbListFields(con,"customer")
yourtable <- dbReadTable(con,"sometable")
df=dbSendQuery(con,"select * from actor")

head(df)
str(df)
dim(df)



dbListConnections(con)


dbWriteTable(conn = con, name = 'Test', value = as.data.frame(Thurstone))



################ Data Export ################### 

library(RMySQL)
library(psych)
con <- dbConnect(MySQL(),
                 user = 'root',
                 password = 'Narasimha@123',
                 host = 'localhost',
                 dbname='narasimha')
dbListTables(con)
customer=dbSendQuery(con,df)
customer=df
yourtable <- dbReadTable(con,"sometable")
# write it back
dbWriteTable(con,"new",customer,overwrite=FALSE)
dbWriteTable(con, value = data.frame, name = "MyTable", append = TRUE ) 

# watch out with the overwrite argument it does what it says :)
dbDisconnect(con)
CREATE TABLE newspaper_search_results.tbl_newspaper_search_results (
  id INT NOT NULL AUTO_INCREMENT,
  story_title VARCHAR(99) NULL,
  story_date_published DATETIME NULL,
  story_url VARCHAR(99) NULL,
  search_term_used VARCHAR(45) NULL,
  PRIMARY KEY (id));
dbWriteTable(con, "new",customer,overwrite=FALSE append = FALSE ) 

testTable=customer
dbWriteTable(con, "testTable", testTable)
You can overwrite an existing table like this:
test=customer
dbWriteTable(con, "customer", customer, overwrite=TRUE)
dbWriteTable(con, "test", test)
dbWriteTable(con, "test", customer, overwrite=TRUE)



##########################


install.packages("RODBC")
library(RODBC)



#######################################################################


data_p <- read.csv("C:\\Users\\Narasimha\\Desktop\\data_p.csv",stringsAsFactors = FALSE)
dim(data_p)

head(data_p)

########### Data Manipulations ######################

data_p$point.machine="A"
table(data_p$point.machine)

########### Firest devide Operations based on Cuurent consumption ################


table(data_p$)

# Fierst find peak time or max curent consumtion 

# motar on when peak time 
df=data_p$current.value
max(df)
str(data_p)
d=data_p$current.value==near(0)
library(dplyr)
data_p[ data_p$current.value <= 1 ,]
data_p$operation<-c(rep(1,each = 1, len = 44),rep(2,each = 1, len = 44),rep(3,each = 1, len = 44))
data_p$id=rep(1:132)
data_p$diff = data_p$current.value - lag(data_p$current.value, default = first(data_p$current.value))
d=data_p[data_p$operation==1,]
Peak_current_1=max(d$current.value)
    


















