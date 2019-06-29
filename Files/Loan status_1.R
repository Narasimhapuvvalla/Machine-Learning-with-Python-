


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


# good loan 

data=loan_accp
dummy=data[colSums(is.na(data))<=13000]
dummy=data
dim(dummy)
names(data)
dummy[,c('id','funded_amnt','funded_amnt_inv',
         'int_rate','sub_grade','emp_title','issue_d','disbursement_method','debt_settlement_flag','settlement_status','settlement_date')]=NULL
dummy[,c('zip_code','out_prncp','out_prncp_inv',
         'total_pymnt','total_pymnt_inv')]=NULL
dummy[,c('total_rec_prncp','total_rec_int', 'total_rec_late_fee',
         'recoveries', 'collection_recovery_fee', 'last_pymnt_d',
         'last_pymnt_amnt')]=NULL

dummy$desc=NULL
dim(dummy)
colSums(is.na(dummy))


dummy[,c("pymnt_plan","collections_12_mths_ex_med","initial_list_status","policy_code","chargeoff_within_12_mths","application_type","hardship_flag")]=NULL
dummy$term=as.numeric(substr(dummy$term,1,3))
dummy$emp_length= as.numeric(ifelse(dummy$emp_length=="< 1 year",0.5,ifelse(dummy$emp_length=="10+ years",15,ifelse(dummy$emp_length==" n/a ",NA,substr(dummy$emp_length,1,1)))))
dummy$next_pymnt_d<-NULL
dummy$revol_util=as.numeric(sub("%", "",dummy$revol_util,fixed=TRUE))
dummy[,c("last_credit_pull_d","earliest_cr_line")]=NULL
dummy[,c("title","addr_state")]=NULL
dummy$debt_settlement_flag_date=NULL
dummy$loan_status<-as.factor(dummy$loan_status)

dummy$grade= as.numeric(ifelse(dummy$grade=="A",1,ifelse(dummy$grade=="B",2,ifelse(dummy$grade=="C",3,
                                                                                   ifelse(dummy$grade=="D",4,ifelse(dummy$grade=="E",5,ifelse(dummy$grade=="F",6,7)))))))


for(i in unique(dummy$verification_status)){
  dummy[paste("verification_status", i, sep = ".")] <- 
    ifelse(dummy$verification_status == i, 1, 0)
}
dummy$verification_status = NULL
for(i in unique(dummy$purpose)){
  dummy[paste("purpose", i, sep = ".")] <- 
    ifelse(dummy$purpose == i, 1, 0)
}
dummy$purpose = NULL
for(i in unique(dummy$home_ownership)){
  dummy[paste("home_ownership", i, sep = ".")] <- 
    ifelse(dummy$home_ownership == i, 1, 0)
}
dummy$home_ownership = NULL
colSums(is.na(dummy))
str(dummy)
tem_data=dummy
tem_data$loan_status=NULL

for(i in 1:ncol(tem_data)){
  tem_data[,i][is.na(tem_data[,i])] = round(mean(tem_data[,i],na.rm = TRUE))
}

colSums(is.na(tem_data))
tem_data$loan_status=dummy$loan_status
colSums(is.na(dummy))
names(tem_data)
# important varibles 
loan_amnt,term,emp_length,grade,purpose,verification_status
