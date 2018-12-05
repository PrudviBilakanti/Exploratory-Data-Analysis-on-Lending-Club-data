library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
#install ggcorrplot you dont have any such package
install.packages("ggcorrplot")
library(ggcorrplot)

#set working directory

df <- read.csv('loan.csv',stringsAsFactors = F,na.strings=c(""," ","NA","n/a","N/A"))

# Lets subset the data for the output variable loan_status with value Charged off or Fully paid. 

df <- subset(df,df$loan_status == 'Charged Off' | df$loan_status == 'Fully Paid')

# Lets do some cleaning. 
# Lets clean columsn with Zero variance

df <- df[sapply(df, function(x) n_distinct(x,na.rm = T) > 1)]

# Lets get rid of columns which doesnt provide any insights about why user defaults

# Below Coumns doesnt have any values that differentiate between Charged off and Fully paid loans or these columns represent values after declaring a loan as charged off

# out_prncp,out_prncp_inv,next_pymnt_d,recoveries


df <- df[,!(names(df) %in% c('out_prncp','out_prncp_inv','next_pymnt_d','recoveries','id','member_id','url','desc','title','mths_since_last_record','mths_since_last_delinq'))]

# Bucket funded_amount into buckets

min(df$funded_amnt)
max(df$funded_amnt)
summary(df$revol_bal)
df$funded_amnt_range[between(df$funded_amnt,0,10000)] <- 'Low_fund'
df$funded_amnt_range[between(df$funded_amnt,10001,20000)] <- 'Medium_fund'
df$funded_amnt_range[df$funded_amnt > 20000] <- 'High_fund'

summary(df)
# Bucket funded_amount into buckets

min(df$funded_amnt_inv)

max(df$funded_amnt_inv)

df$funded_amnt_inv_range[between(df$funded_amnt_inv,0,10000)] <- 'Low_inv_fund'
df$funded_amnt_inv_range[between(df$funded_amnt_inv,10001,20000)] <- 'Medium_inv_fund'
df$funded_amnt_inv_range[df$funded_amnt_inv > 20000] <- 'High_inv_fund'

# Remove 'months' from term

df$term <-  gsub(" months","",df$term)

# Lets format int_rate column so that we can bucket into segments for further analysis.

df$int_rate <-  gsub("%","",df$int_rate)
df$int_rate <- as.double(df$int_rate)

max(df$int_rate)
min(df$int_rate)

df$int_rate_category[between(df$int_rate,0,10)] <- 'Low_interest'
df$int_rate_category[between(df$int_rate,11,20)] <- 'Medium_interest'
df$int_rate_category[df$int_rate > 20] <- 'High_interest'


# Lets also bucket installment into buckets
summary(df$installment)

df$installment_category[between(df$installment,0,450)] <- 'Low_installment'
df$installment_category[between(df$installment,451,900)] <- 'Medium_installment'
df$installment_category[df$installment > 900] <- 'High_intallment'

# bucket dti into low medium and high

df$dti_category[between(df$dti,0,10)] <- 'Low_dti'
df$dti_category[between(df$dti,11,20)] <- 'Medium_dti'
df$dti_category[df$dti > 20] <- 'High_dti'


# Format the revolving credit utilization revol_util. remove % from the value

df$revol_util <-  gsub("%","",df$revol_util)
df$revol_util <- as.double(df$revol_util)

#Lets also bucket the revol_util
summary(df$revol_util)

df$rev_util_rate[between(df$revol_util,0,25)] <- 'Low_revol_credit'
df$rev_util_rate[between(df$revol_util,26,50)] <- 'Average_revol_credit'
df$rev_util_rate[between(df$revol_util,51,75)] <- 'above_avg_revol_credit'
df$rev_util_rate[between(df$revol_util,76,100)] <- 'High_revol_credit'



# extracting the years (so the 0-1 year exp will be represented as 1, 10 or 10+ as 10)

df$emp_length <- str_extract(df$emp_length, pattern = '[0-9]+')

# round funded amount by investors to match it with other rows

df$funded_amnt_inv <- round(df$funded_amnt_inv,0)
df$annual_inc <- round(df$annual_inc,0)

df$issue_d <- as.Date(paste('1-',df$issue_d,sep = ''),format ='%d-%b-%y')


df$issued_month <- as.numeric(format(df$issue_d, "%m"))


df$earliest_cr_line <- as.Date(paste('1-',df$earliest_cr_line,sep = ''),format ='%d-%b-%y')

df$last_pymnt_d <-as.Date(paste('1-',df$last_pymnt_d,sep = ''),format ='%d-%b-%y')

df$issue_year <- format(df$issue_d,"%Y")



#sub-grade has number and alphabet associated so we can split to get better insights
#subgrade spitting
split_sub_grade <-  function(x){
  return(unlist(str_split(x,''))[2])
}
df$sub_grade_no <- as.numeric(unlist(lapply(df$sub_grade, split_sub_grade)))

#filling na revo_util with mean 
df$revol_util[is.na(df$revol_util)] <-  48.7



# Delete title column.

########################################################## UNI VARIATE ANALYSIS ###########################################################




#Funded amout by investor distribution 
ggplot(df,aes(x=funded_amnt_inv,fill=loan_status))+
  geom_histogram(bins = 50)+ggtitle('Funded amount by Investor Distribution')

#we can see that we have some outliers in distribution
mean(df$funded_amnt_inv)
ggplot(df, aes(x="",y=funded_amnt_inv))+
  geom_boxplot()+ggtitle('Funded amount by Investor Boxplot')
#We can identify outliers in boxplot easily 
summary(df$funded_amnt_inv)


#catogery distribution of term
ggplot(df,aes(x=term,fill=term))+
  geom_bar()+
  geom_text(aes(label=paste(round((..count..)/sum(..count..)*100,1),'%'),vjust=-0.3),stat="count")+
  ggtitle('Terms distribution')



# Plotting Interest rate distribution
ggplot(df, aes(x=int_rate))+
  geom_histogram(binwidth = 5)+ggtitle('Interest rate Distribution')
# We can see that interest rate is around 10,15 have high count
  

#Installment Distribution
ggplot(df, aes(x=installment))+
  geom_histogram(binwidth = 100)+ggtitle('Installment Distribution')
#Understanding outliers using boxplot
ggplot(df, aes(x="",y=installment))+
  geom_boxplot()+ggtitle('Installment Boxplot')
#central tendency are affected due to outliers here

summary(df$installment)

# Analyse Grade catogery using bar plot  
ggplot(df,aes(x=grade,fill=grade))+
  geom_bar()+
  geom_text(aes(label=paste(round((..count..)/sum(..count..)*100,1),'%'),vjust=-0.3),stat="count")+
  ggtitle('grade distribution')


#Sub Grade

ggplot(df,aes(x=sub_grade_no,fill=factor(sub_grade_no)))+
  geom_bar()+
geom_text(aes(label=paste(round((..count..)/sum(..count..)*100,1),'%'),vjust=-0.3),stat="count")+
  ggtitle('sub_grade_no  distribution')



# emp
df$emp_title <- tolower(df$emp_title)
df$emp_title <-  gsub("^\\s+|\\s+$", "", df$emp_title)


#exp length 
ggplot(df,aes(x=emp_length,fill=loan_status))+
  geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle('Employee Experience Bar plot')


table(df$home_ownership)
# MORTGAGE     NONE    OTHER      OWN     RENT 
# 17659        3       98     3058    18899
### 
ggplot(df,aes(x=home_ownership,fill=home_ownership))+
  geom_bar()+
  geom_text(aes(label=paste(round((..count..)/sum(..count..)*100,1),'%'),vjust=-0.3),stat="count")+
  ggtitle('Home Ownership  Bar plot')
# 14 of all egments are defaulted

# annual income box plot and histogram
#since values are very large log transformation would give better insight

ggplot(df,aes(x="",y=annual_inc))+
  geom_boxplot()+ coord_trans(y = "log10")+ggtitle('Log Transformed  annual income boxplot')
ggplot(df,aes(x=annual_inc))+
  geom_histogram(bins = 30)+ggtitle('Annual income Histogram')

summary(df$annual_inc)
#Outliers are affecting central tendency in this feature


# Verification Status

ggplot(df, aes(x=verification_status,fill=verification_status))+
  geom_bar()+
  geom_text(aes(label=paste(round((..count..)/sum(..count..)*100,1),'%'),vjust=-0.3),stat="count")+
  ggtitle('Verification Status  Bar plot')
#There are high people with not verified as status around 43

# loan status
ggplot(df, aes(x=loan_status,fill=loan_status))+
  geom_bar()+geom_text(aes(label=paste(round((..count..)/sum(..count..)*100,1),'%'),vjust=-0.3),stat="count")+
  ggtitle('Loan Status  Bar plot')
#Here we can see that bank can have losses due to 14.6% of individuals tend to default
# Charged Off     Current  Fully Paid 
# 0.14167737  0.02870307  0.82961956



# Purpose

ggplot(df, aes(x=purpose,fill=purpose))+
  geom_bar()+
  geom_text(aes(label=paste(round((..count..)/sum(..count..)*100,1),'%'),vjust=-0.3),stat="count")+
  ggtitle('Purpose Bar plot')+ theme(axis.title.x=element_blank(),
                                                  axis.text.x=element_blank(),
                                                  axis.ticks.x=element_blank())

# majority people opted loan to consolidate debts(46%), credit card (12%),
#home improvements(7.4), major purchase(5.5%) and small bussiness(4.6%)

#zip code commented plot for zip as its hard to infer
#ggplot(df,aes(x=zip_code,fill=loan_status))+
#  geom_bar()



# addr_state
ggplot(df,aes(x=addr_state,fill=addr_state))+
  geom_bar()+ theme(axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank())

sort(table(df$addr_state),decreasing = T)

#These are the top 5 states. most of the loans are from California region
# CA   NY   FL   TX   NJ
# 7099 3812 2866 2727 1850

# dti

ggplot(df, aes(x=dti))+
  geom_histogram(bins = 50)+ggtitle('DTI Distribution')
#we can see a sudden drop in count around 28 
ggplot(df, aes(x='',y=dti))+geom_boxplot()
  



# delinq_2yrs
ggplot(df,aes(x=factor(delinq_2yrs),fill=factor(delinq_2yrs)))+
  geom_bar()

sort(table(df$delinq_2yrs),decreasing = T)
# Majority of the poeple were not delin1 in last 2 years

# Pending formatting on Date column
table(df$issue_d)
ggplot(df,aes(x=issue_d))+geom_bar()+ggtitle('Issue date histogram')

#inq_last_6mths
table(df$inq_last_6mths,df$loan_status)

ggplot(df, aes(x=inq_last_6mths))+
  geom_bar()+ggtitle('inq_last_6mths Distribution')

# people with less inq in last 6 months are more 


#mths_since_last_delinq
#ggplot(df,aes(x=mths_since_last_delinq,fill=loan_status))+
#  geom_bar()
# mths_since_last_delinq dosent seem to effect loan status directly
#droped column due to high number of na


#mths_since_last_record
#ggplot(df,aes(x=mths_since_last_record,fill=loan_status))+
#  geom_bar()
#we can drop this column as it contains more number of na and is not directly affecting loan status

# open_acc Distribution
ggplot(df, aes(x=(open_acc),fill=loan_status))+
  geom_histogram(bins = 50)

# Most of the poeple have credit lines between 2 and 15

#pub_rec
ggplot(df,aes(x=factor(pub_rec)))+
  geom_bar()+ggtitle('Number of derogatory public records distribution')

# Majority of the people had zero derogatory public records



# revol_bal
ggplot(df, aes(x="",y=revol_bal))+
  geom_boxplot()+coord_trans(x = "log")+ggtitle('Transformed  annual income boxplot')

#we can see many outliers lets plot distribution plot
ggplot(df,aes(x=revol_bal))+
  geom_histogram(binwidth = 10000)
# Most of the poeple had the revol balance between 0 and 25000


# revol_util distribution
ggplot(df, aes(x="",y=revol_util))+
  geom_boxplot()
summary(df$revol_util)

# Most of the people utilised 50 % of their revlving credit
#after loking at summary and distribution we can impute na values with mean



# Total credit lines in borrowers accoun
ggplot(df,aes(x=total_acc))+
  geom_histogram(bins = 10)+ggtitle('total_acc distribution')

# There are more number of people with credit line between 10 to 40



# Outstanding Principle#
#ggplot(df, aes(x=out_prncp))+
#  geom_histogram()#
# This is expected as most of the users have cleared their loans, there will be zero pending principle amount
#we can remove feature




# Outstanding principle for the amount funded by investors#
#ggplot(df,aes(x=out_prncp_inv))+
# geom_histogram()
# Same trend with the amount that is funded by investors.  

#total_pymnt
ggplot(df,aes(x=total_pymnt,fill=loan_status))+
  geom_histogram(bins = 50)


#total payment distribution and its effect on loan status
ggplot(df,aes(x=total_pymnt,fill=loan_status))+
  geom_histogram(bins = 50)+ggtitle('Total payment and loan status Distribution')

##################### Continues variables Analysis #######

ggplot(df, aes(x=dti, fill = factor(loan_status))) +  
  geom_histogram(aes(y=..density..),bins = 30, position="identity", alpha=0.5,fill="white", color="black")+
  geom_density(alpha=0.6) +
  labs(title = "DTI vs Status",
       x = "DTI",
       y = "Status")

ggplot(df, aes(x=loan_amnt, fill = factor(loan_status))) +  
  geom_histogram(aes(y=..density..), bins = 30,position="identity", alpha=0.5)+
  geom_density(alpha=0.6) +
  labs(title = "Loan Amount vs Status",
       x = "Loan Amount",
       y = "Status")




######################################## BI variate Analysis and Multi Variate analysis #########################################################

#plotting term vs loanstatus
ggplot(df,aes(x=term,fill=loan_status))+geom_bar()+ggtitle('Term vs Loan status')

#multi variate on term and loan status
ggplot(df,aes(x=term,fill=loan_status))+geom_bar()+ggtitle('Term vs Loan status vs grade')+facet_grid(.~grade )
ggplot(df,aes(x=term,fill=loan_status))+geom_bar()+ggtitle('Term vs Loan status vs Employee experience')+facet_grid(.~emp_length )

#for 10 year experience 60 month has high default rate 
ggplot(df,aes(x=term,fill=loan_status))+geom_bar()+ggtitle('Term vs Loan status vs verification_status')+facet_grid(.~verification_status )
ggplot(df,aes(x=term,fill=loan_status))+geom_bar()+ggtitle('Term vs Loan status vs purpose')+facet_grid(.~purpose )

#debt consolidation has high amount of loan default in both terms
ggplot(df,aes(x=term,fill=loan_status))+geom_bar()+ggtitle('Term vs Loan status vs home_ownership')+facet_grid(.~home_ownership )
ggplot(df,aes(x=term,fill=loan_status))+geom_bar()+ggtitle('Term vs Loan status vs dti_category')+facet_grid(.~dti_category )
ggplot(df,aes(x=term,fill=loan_status))+geom_bar()+ggtitle('Term vs Loan status vs installment_category')+facet_grid(.~installment_category )

#low installments has high defaulters
ggplot(df,aes(x=term,fill=loan_status))+geom_bar()+ggtitle('Term vs Loan status vs sub_grade_no')+facet_grid(.~sub_grade_no )


#plot home ownership vs loanstatus
ggplot(df,aes(x=home_ownership,fill=loan_status))+ggtitle('Home Ownership vs Loan Status')+geom_bar()

#mortgage and rent has high amount of defaulters
ggplot(df,aes(x=home_ownership,fill=loan_status))+geom_bar()+facet_grid(.~grade)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(df,aes(x=home_ownership,fill=loan_status))+ggtitle('Home Ownership vs Loan Status Vs Experiance')+geom_bar()+facet_grid(.~emp_length)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(df,aes(x=home_ownership,fill=loan_status))+geom_bar()+facet_grid(.~purpose)+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#mortgage,rent purpose has high default in debt consolidation
ggplot(df,aes(x=home_ownership,fill=loan_status))+geom_bar()+facet_grid(.~dti_category)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(df,aes(x=home_ownership,fill=loan_status))+geom_bar()+ggtitle('Home Ownership vs Loan Status vs installment catogery')+facet_grid(.~installment_category)+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#low installment and rent mortage are showing significant pattern
ggplot(df,aes(x=home_ownership,fill=loan_status))+geom_bar()+facet_grid(.~sub_grade_no)+theme(axis.text.x = element_text(angle = 90, hjust = 1))


#plot emp_length vs loan status and multi varient plots
ggplot(df,aes(x=emp_length,fill=loan_status))+geom_bar()
ggplot(df,aes(x=emp_length,fill=loan_status))+geom_bar()+facet_grid(.~grade)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(df,aes(x=emp_length,fill=loan_status))+geom_bar()+facet_grid(.~emp_length)+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#mortgage,rent purpose has high default in debt consolidation
ggplot(df,aes(x=emp_length,fill=loan_status))+geom_bar()+facet_grid(.~dti_category)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(df,aes(x=emp_length,fill=loan_status))+geom_bar()+facet_grid(.~installment_category)+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#low installment and rent mortage are showing significant pattern
ggplot(df,aes(x=emp_length,fill=loan_status))+geom_bar()+facet_grid(.~sub_grade_no)+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#plot grade vs loan status
ggplot(df,aes(x=loan_status,fill=loan_status))+geom_bar()+facet_grid(.~grade)+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#grade B,C,D has high amount of defaulters

ggplot(df,aes(x=grade,fill=loan_status))+geom_bar()+facet_grid(.~verification_status)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(df,aes(x=grade,fill=loan_status))+geom_bar()+facet_grid(.~emp_length)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(df,aes(x=grade,fill=loan_status))+geom_bar()+facet_grid(.~dti_category)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(df,aes(x=grade,fill=loan_status))+geom_bar()+facet_grid(.~df$home_ownership)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(df,aes(x=grade,fill=loan_status))+geom_bar()+facet_grid(.~installment_category)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(df,aes(x=grade,fill=loan_status))+geom_bar()+facet_grid(.~sub_grade_no)+theme(axis.text.x = element_text(angle = 90, hjust = 1))





#multi variate on term and loan status
ggplot(df,aes(x=purpose,fill=loan_status))+geom_bar()+facet_grid(.~grade )+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(df,aes(x=purpose,fill=loan_status))+geom_bar()+facet_grid(.~verification_status )+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(df,aes(x=purpose,fill=loan_status))+geom_bar()+facet_grid(.~dti_category )+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#medium dti has high rate of defaulters 
ggplot(df,aes(x=purpose,fill=loan_status))+geom_bar()+facet_grid(.~installment_category )+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#low installments has high defaults
ggplot(df,aes(x=purpose,fill=loan_status))+geom_bar()+facet_grid(.~sub_grade_no )+theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(df,aes(x=loan_status,fill=loan_status))+geom_bar()+facet_grid(.~installment_category )+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#low installments has high fully paid and high defaulters

ggplot(df,aes(x=loan_amnt,y=purpose , col=factor(loan_status)))+geom_point()+geom_jitter()

#we can see that debt considolation and small bussiness have good amount of defaulterss 
ggplot(df,aes(x=loan_amnt,y=purpose , col=loan_status))+geom_point()+ggtitle('Purpose vs Loan Status vs Loan amount vs Installment catogery')+geom_jitter()+facet_grid(.~installment_category )

#medium and low installments with dc and sb has high defaulters
ggplot(df,aes(x=loan_amnt,y=purpose , col=loan_status))+geom_point()+geom_jitter()+facet_grid(.~dti_category )


#lets look address 
ggplot(df,aes(x=addr_state,fill=loan_status))+geom_bar()+ggtitle('Address vs loan Status')
#CA and NY has high amount of 

#
ggplot(df,aes(factor(issued_month),fill=loan_status))+
  geom_bar(position = 'fill')




ggplot(df,aes(x=purpose,fill=loan_status))+
  geom_bar(position = 'fill')
#small business reltively high defaulters

ggplot(df,aes(x=grade,fill=loan_status))+
  geom_bar(position = 'fill')
#as grade increases charged off increases

ggplot(df,aes(x=sub_grade,fill=loan_status))+
  geom_bar(position = 'fill')
#f5 has highest amount of defaulters


ggplot(df,aes(x=home_ownership,fill=loan_status))+
  geom_bar(position = 'fill')
#none seems to haveno defaulters whihc is good from banks as then can confidently provide loans

ggplot(df,aes(x=zip_code,fill=loan_status))+
  geom_bar(position = 'fill')

############################################# scatter plot for continues variables#####  

## Loan amount - by grade and loan status
Grade_total <- df %>% select(grade,loan_status,loan_amnt) %>%
  group_by(grade,loan_status) %>% 
  summarise(loan_amnt = sum(loan_amnt))
  
ggplot(Grade_total, aes(x = grade, y = loan_amnt, col = factor(loan_status))) + geom_point(aes(size=loan_amnt)) + 
  labs(title = "Loan amount distribution among loan status and Grade",
       x = "grade",
       y = "Amount")


###  loan amount by purpose and loan status 
purpose_total <- df %>% select(purpose,loan_status,loan_amnt) %>%
  group_by(purpose,loan_status) %>% 
  summarise(loan_amnt = sum(loan_amnt))

ggplot(purpose_total, aes(x = purpose, y = loan_amnt, col = factor(loan_status))) +
  geom_point(aes(size=loan_amnt)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  labs(title = "Loan amount distribution among loan status and purpose",
       x = "purpose",
       y = "Amount")
##########################################################################

select_if(df, is.numeric) -> df_numeric

#correlation plot
ggcorrplot(round(cor(df_numeric,use = 'pairwise.complete.obs'),2), hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of variables", 
           ggtheme=theme_bw)+
  theme(plot.title = element_text(hjust = 0.5))

# Funded Amount buckets

df %>%
  group_by(funded_amnt_range,loan_status) %>%
  summarise(cnt = n()) %>%
  mutate(percent = paste0(round(100 * cnt/sum(cnt), 0), "%")) %>%
  ungroup() %>%
  as.data.frame() %>%
  ggplot(.,aes(x=funded_amnt_range,y=percent,fill=loan_status))+
  geom_bar(stat = 'identity')+
  labs(title='Defaulters ratio by the funded amount range', 
       x='Funded amount range',
       y='Percent',
       fill="Loan_Status")+
  geom_text(aes(y=(percent),label=(percent)),hjust = 0.5,vjust=1,size=3,
            position = position_stack(vjust = .5))+
  theme(plot.title = element_text(hjust = 0.5))



# funded amount by investor

df %>%
  group_by(funded_amnt_inv_range,loan_status) %>%
  summarise(cnt = n()) %>%
  mutate(percent = paste0(round(100 * cnt/sum(cnt), 0), "%")) %>%
  ungroup() %>%
  as.data.frame() %>%
  ggplot(.,aes(x=funded_amnt_inv_range,y=percent,fill=loan_status))+
  geom_bar(stat = 'identity')+
  labs(title='Defaulters ratio by the investors funded amount range', 
       x='Funded amount by investors range',
       y='Percent',
       fill="Loan_Status")+
  geom_text(aes(y=(percent),label=(percent)),hjust = 0.5,vjust=1,size=3,
            position = position_stack(vjust = .5))+
  theme(plot.title = element_text(hjust = 0.5))

# As the funded amount and funded amount by investors are nearly the same we clearly see that poeple
# with high amount of loans tend to default.

# Term and loan status
df %>%
  group_by(term,loan_status) %>%
  summarise(cnt = n()) %>%
  mutate(percent = paste0(round(100 * cnt/sum(cnt), 0), "%")) %>%
  ungroup() %>%
  as.data.frame() %>%
  ggplot(.,aes(x=term,y=percent,fill=loan_status))+
  geom_bar(stat = 'identity')+
  labs(title='Default ratio by loan term period', 
       x='Term',
       y='Percent',
       fill="Loan_Status")+
  geom_text(aes(y=(percent),label=(percent)),hjust = 0.5,vjust=1,size=3,
            position = position_stack(vjust = .5))+
  theme(plot.title = element_text(hjust = 0.5))

# *** Loans with higher term (60 month) tend to default more when compared to 36 months term.

# Int rate bucket

df %>%
  filter(int_rate_category != 'NA') %>%
  group_by(int_rate_category,loan_status) %>%
  summarise(cnt = n()) %>%
  mutate(percent = round(100 * cnt/sum(cnt), 0)) %>%
  ungroup() %>%
  as.data.frame() %>%
 ggplot(.,aes(x=int_rate_category,y=as.numeric(as.character(percent)),fill=loan_status))+
  geom_bar(stat = 'identity')+
  labs(title='Default ratio by loan term period', 
       x='Term',
       y='Percent',
       fill="Loan_Status")+
  geom_text(aes(label=paste(percent,'%')),hjust = 0.5,vjust=1,size=3,
            position = position_stack(vjust = .5))+
  theme(plot.title = element_text(hjust = 0.5))
# We can see that people with High interest rinterest rates default more
# installment_category

df %>%
  filter(installment_category != 'NA') %>%
  group_by(installment_category,loan_status) %>%
  summarise(cnt = n()) %>%
  mutate(percent = round(100 * cnt/sum(cnt), 0)) %>%
  ungroup() %>%
  as.data.frame() %>%
  ggplot(.,aes(x=installment_category,y=as.numeric(as.character(percent)),fill=loan_status))+
  geom_bar(stat = 'identity')+
  labs(title='Default ratio by loan installment category', 
       x='installment category',
       y='Percent',
       fill="Loan_Status")+
  geom_text(aes(label=paste(percent,'%')),hjust = 0.5,vjust=1,size=3,
            position = position_stack(vjust = .5))+
  theme(plot.title = element_text(hjust = 0.5))
# Even here we can see that people with high installment tend to default more.


# Grade
df %>%
  group_by(grade,loan_status) %>%
  summarise(cnt = n()) %>%
  mutate(percent = round(100 * cnt/sum(cnt), 0)) %>%
  ungroup() %>%
  as.data.frame() %>%
  ggplot(.,aes(x=grade,y=as.numeric(as.character(percent)),fill=loan_status))+
  geom_bar(stat = 'identity')+
  labs(title='Default ratio by Grade', 
       x='Grade',
       y='Percent',
       fill="Loan_Status")+
  geom_text(aes(label=paste(percent,'%')),hjust = 0.5,vjust=1,size=3,
            position = position_stack(vjust = .5))+
  theme(plot.title = element_text(hjust = 0.5))

# From the plot it is evident that the loan defaulters are high in the groups D to G


df %>%
  filter(emp_length != 'NA') %>%
  group_by(emp_length,loan_status) %>%
  summarise(cnt = n()) %>%
  mutate(percent = paste0(round(100 * cnt/sum(cnt), 0), "%")) %>%
  ungroup() %>%
  as.data.frame() 


df %>%
  filter(emp_length != 'NA') %>%
  group_by(emp_length,loan_status) %>%
  summarise(cnt = n()) %>%
  mutate(percent = round(100 * cnt/sum(cnt), 0)) %>%
  ungroup() %>%
  as.data.frame() %>%
  ggplot(.,aes(x=as.factor(emp_length),y=as.numeric(as.character(percent)),fill=loan_status))+
  geom_bar(stat = 'identity')+
  labs(title='Default ratio by Employent Length', 
       x='Employment Length',
       y='Percent',
       fill="Loan_Status")+
  geom_text(aes(label=(percent)),hjust = 0.5,vjust=1,size=3,
            position = position_stack(vjust = .5))+
  theme(plot.title = element_text(hjust = 0.5))


# The default ration is common across all the employment length.


# Home ownership

df %>%
  filter(home_ownership != 'NONE') %>%
  group_by(home_ownership,loan_status) %>%
  summarise(cnt = n()) %>%
  mutate(percent = round(100 * cnt/sum(cnt), 0)) %>%
  ungroup() %>%
  as.data.frame() %>%
  ggplot(.,aes(x=home_ownership,y=as.numeric(as.character(percent)),fill=loan_status))+
  geom_bar(stat = 'identity')+
  labs(title='Default ratio by home ownership status', 
       x='Home Ownership',
       y='Percent',
       fill="Loan_Status")+
  geom_text(aes(label=paste(percent,'%')),hjust = 0.5,vjust=1,size=3,
            position = position_stack(vjust = .5))+
  theme(plot.title = element_text(hjust = 0.5))

# Home ownership Rent and Mortgage to contribute to defaulters ratio.



# Verification Status
df %>%
  filter(verification_status != 'NA') %>%
  group_by(verification_status,loan_status) %>%
  summarise(cnt = n()) %>%
  mutate(percent = round(100 * cnt/sum(cnt), 0)) %>%
  ungroup() %>%
  as.data.frame() %>%
  ggplot(.,aes(x=verification_status,y=as.numeric(as.character(percent)),fill=loan_status))+
  geom_bar(stat = 'identity')+
  labs(title='Default ratio by LC verification of income source and others', 
       x='verification Status',
       y='Percent',
       fill="Loan_Status")+
  geom_text(aes(label=paste(percent,'%')),hjust = 0.5,vjust=1,size=3,
            position = position_stack(vjust = .5))+
  theme(plot.title = element_text(hjust = 0.5))


# In contrast to the general belief, loan defaults are higher in verfied pool.

# Purpose

df %>%
  filter(purpose != 'NA') %>%
  group_by(purpose,loan_status) %>%
  summarise(cnt = n()) %>%
  mutate(percent = round(100 * cnt/sum(cnt), 0)) %>%
  ungroup() %>%
  as.data.frame() %>%
  ggplot(.,aes(x=purpose,y=as.numeric(as.character(percent)),fill=loan_status))+
  geom_bar(stat = 'identity')+
  labs(title='Default ratio by purpose of the Loans', 
       x='Purpose of the Loan',
       y='Percent',
       fill="Loan_Status")+
  geom_text(aes(label=paste(percent,'%')),hjust = 0.5,vjust=1,size=3,
            position = position_stack(vjust = .5))+
  theme(plot.title = element_text(hjust = 0.5))
# People who took loans to setup small business tend to default ( 27% ) more,




# Debit to Income ratio
df %>%
  filter(dti_category != 'NA') %>%
  group_by(dti_category,loan_status) %>%
  summarise(cnt = n()) %>%
  mutate(percent = round(100 * cnt/sum(cnt), 0)) %>%
  ungroup() %>%
  as.data.frame() %>%
  ggplot(.,aes(x=dti_category,y=as.numeric(as.character(percent)),fill=loan_status))+
  geom_bar(stat = 'identity')+
  labs(title='Default ratio by Debit to income ration', 
       x='Debit to income ratio',
       y='Percent',
       fill="Loan_Status")+
  geom_text(aes(label=paste(percent,'%')),hjust = 0.5,vjust=1,size=3,
            position = position_stack(vjust = .5))+
  theme(plot.title = element_text(hjust = 0.5))
# People with high Debt to income ratio tend to default more, 17% of the people defaulted from the higher dt bucket.



# delinq_2yrs
df %>%
  filter(delinq_2yrs != 'NA') %>%
  group_by(delinq_2yrs,loan_status) %>%
  summarise(cnt = n()) %>%
  mutate(percent = round(100 * cnt/sum(cnt), 0)) %>%
  ungroup() %>%
  as.data.frame() %>%
  ggplot(.,aes(x=factor(delinq_2yrs),y=as.numeric(as.character(percent)),fill=loan_status))+
  geom_bar(stat = 'identity')+
  labs(title='Default ratio by delinq in last 2yrs', 
       x='Number of Delinq in last 2 years',
       y='Percent',
       fill="Loan_Status")+
  geom_text(aes(label=paste(percent,'%')),hjust = 0.5,vjust=1,size=3,
            position = position_stack(vjust = .5))+
  theme(plot.title = element_text(hjust = 0.5))
# We have less data with high delinq rate ni last 2 years. So we cannot firmly say that Defaulters ratio is high for the people who were delinq in last 2 years. 


#inq_last_6mths
df %>%
  filter(inq_last_6mths != 'NA') %>%
  group_by(inq_last_6mths,loan_status) %>%
  summarise(cnt = n()) %>%
  mutate(percent = round(100 * cnt/sum(cnt), 0)) %>%
  ungroup() %>%
  as.data.frame() %>%
  ggplot(.,aes(x=factor(inq_last_6mths),y=as.numeric(as.character(percent)),fill=loan_status))+
  geom_bar(stat = 'identity')+
  labs(title='Default ratio by number of inquiries in last 6 months', 
       x='Number of inquiries in last 6 months',
       y='Percent',
       fill="Loan_Status")+
  geom_text(aes(label=paste(percent,'%')),hjust = 0.5,vjust=1,size=3,
            position = position_stack(vjust = .5))+
  theme(plot.title = element_text(hjust = 0.5))


df %>%
  filter(inq_last_6mths != 'NA') %>%
  group_by(inq_last_6mths,loan_status) %>%
  summarise(cnt = n()) %>%
  mutate(percent = round(100 * cnt/sum(cnt), 0)) %>%
  ungroup() %>%
  as.data.frame() %>%
  filter(loan_status == 'Charged Off') %>%
  ggplot(.,aes(x=inq_last_6mths,y=percent,col=loan_status))+
  geom_point()+
  geom_smooth(method = 'loess')
# The Defaulters ratio is slightly high with high number of inquiries.


##################################END of EDA#############################

















