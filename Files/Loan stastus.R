


library(readr)
library(dplyr)
install.packages("Hmisc")
install.packages("psych")
library(Hmisc)
library(psych)

# load the Accepted_Loan dataset

loan_accp <- read.csv("C:\\Users\\Narasimha\\Downloads\\LoanStats3a.csv")  
summary(loan_accp)
str(loan_accp)
describe(loan_accp)
describe.by(loan_accp)

#  most popular loan type 

loan_accp %>%
  group_by(loan_status) %>%
  summarize(count=n())

# load the Rejected_Loan dataset

loan_rej <- read.csv("C:\\Users\\Narasimha\\Downloads\\RejectStatsA.csv")  
summary(loan_rej)
str(loan_rej)
describe(loan_rej)
describe.by(loan_rej)

names(loan_rej)
names(loan_accp)
# rename the varibles based on accepted loans 

names(loan_rej)[names(loan_rej) == 'Employment.Length'] <- 'emp_length'
names(loan_rej)[names(loan_rej) == 'State'] <- 'addr_state'
names(loan_rej)[names(loan_rej) == 'Loan.Title'] <- 'emp_title'

# most popular loan type of customers

loan_accp %>%
  group_by(loan_status) %>%
  summarize(count=n())



dat=seq(1,585585,by=10000)
help("sapply")
help(lappl)
