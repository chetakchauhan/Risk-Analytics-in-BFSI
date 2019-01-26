############################################################################################
# EDA assignment solution R Code for visualise the driving factors for loan defaults       #
# The Analysis is done univariate as well as Bi-Variate variables available in provided    #
# dataset. Graphs and charts are prepared for different analysis and to show analysis      #
# done for loan default reasons.                                                           #
# Please open this file in RStudio for better code readability                             #
#                                                                                          #
# Group Facilitator: Rakesh Pattanaik.                                                     #   
# Group Facilitator Roll No. - DDA1720036                                                  #
# Group Members: Rakesh Pattanaik, Raju Kumar, Vikrant Singh and Chetak Chauhan            #
############################################################################################

# Install Required Library ----
# Please install the below libraries before running the code.
# The packages need to be installed once and hence purposefully 
# kept commented after installing the packages once in order to avoid 
# reinstalling packages again and again.
# Please uncomment and install the packages if these packages 
# dont exist on your machine.
#install.packages("tm") # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
#install.packages("dplyr") #dplyr package
#install.packages("stringr")	#stringr package
#install.packages("ggplot2") #ggplot2 package
#install.packages("data.table") #data.table pakcage
#install.packages("tidyr") #tidyr package
#install.packages("corrplot") #corrplot package
#install.packages("scales") #scales package

# Import the required Library -----
# Please clear all the object and plots from the R-studio environment before running the code.
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("dplyr")
library("stringr")	
library("ggplot2")
library("data.table")
library("tidyr")
library("corrplot")
library("scales")

# Data Importing/Loading and Cleaning -----
# Set Working Directory.
# The working directory may change depending on the computer/machine  
# the code is running. 
# Please change the working directory as per your requirement and where
# the loan.csv data file (The provided data file in the assignment) is present.
setwd("/Users/hduser/Desktop/Upgrad/EDA/case_study/")

# Load the Dataset and making sure that strings are not treated as factors and 
# blanks are also treated as NA values. And removing duplicates.
# Below code may take 30-40 secs to load since the data file size is more than 30 Mb. 
loan_ds <- read.csv("loan.csv",header = TRUE, strip.white = TRUE, stringsAsFactors = FALSE,na.strings = c("", "NA"))

# Removing duplicate rows (if any). We have not found any duplicate rows in this DataSet.
# data frame with 0 columns and 39717 rows
loan_ds[-which(duplicated(loan_ds))]

# Discarding the Columns from the dataframewhich having mostly NA values 
# and dont have any value addition in the analysis.
# loan_ds_cln <- loan_ds %>% select(-c(annual_inc_joint:total_il_high_credit_limit))
# Creating a cloned data frame which is used for different insight creation.
loan_ds_cln <- loan_ds

# Trimming white spaces both sides in the emp_length column
loan_ds_cln$emp_length <- trimws(loan_ds_cln$emp_length,which = "both")

loan_ds_cln <- loan_ds %>% select(-c(annual_inc_joint:percent_bc_gt_75, tax_liens:total_il_high_credit_limit))

#adding 'emp_length_or' for tableau plotting.
loan_ds_cln <- loan_ds_cln %>% 
  mutate(emp_length_or=emp_length)

#writing data in to working directory for tableau plots to csv file. 
write.csv(loan_ds_cln, "cleaned_loan_data.csv",row.names = FALSE)

# Cleaning the emp_length column which has strings like year/years and Discarding symbols < 
loan_ds_cln$emp_length <- gsub('years|year|<|\\+', '', loan_ds_cln$emp_length)

# Creating a subset for Charged Off Entries.
loan_ds_charged_off <- filter(loan_ds_cln,loan_status=="Charged Off")

# Creating a subset for Fully Paid loan entries.
loan_ds_fully_paid <- filter(loan_ds_cln, loan_status=="Fully Paid")

# Creating a subset for Current loan entries.
current_loan <- filter(loan_ds,loan_status %in% c("Current"))
current_loan$loan_status <- factor(current_loan$loan_status)

# Combined subset for Fully Paid and Charged Off customers
loan_ds <- filter(loan_ds,loan_status %in% c("Fully Paid","Charged Off"))
loan_ds$loan_status <- factor(loan_ds$loan_status)

# Making Charged off customer as 1 and and Fully Paid customer as 0
# and creating a factor out of it.
loan_ds$loan_status <- ifelse(loan_ds$loan_status=="Charged Off",1,0)
loan_ds[sapply(loan_ds, is.character)] <- lapply(loan_ds[sapply(loan_ds, is.character)], 
                                           as.factor)
# Data Understanding -----
# Plotting Overall Distribution of loans
# The plot clearly shows the percentage of loans which are charged off and Fully Paid
loan_ds %>% ggplot(aes(x = as.factor(loan_ds$loan_status))) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  geom_text(aes(y = ((..count..) / sum(..count..)), label = scales::percent((..count..) /
                                                                              sum(..count..))),
            stat = "count",
            vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "Overall Distribution", y = "percent", x = "Overall Distribution") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )

# Deriving Year in which loan was given to customers
# Year ( YYYY) is derived from the issued date ( col. name issue_d)
loan_ds$issue_d <- paste("01-",loan_ds$issue_d,sep="")
loan_ds$issue_d <- as.Date(loan_ds$issue_d,"%d-%B-%y")
loan_ds$year <- as.factor(format(loan_ds$issue_d,"%Y"))

# Plotting the Year in which loan was given to customers
# The plot clearly shows 2011 is year year where most of the
# Loan was given and 2007 is being the least 
loan_ds %>% ggplot(aes(x = as.factor(loan_ds$year))) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  geom_text(aes(y = ((..count..) / sum(..count..)), label = scales::percent((..count..) /
                                                                              sum(..count..))),
            stat = "count",
            vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "Yearly Application Distribution loan types", y = "Percent", x = "Yearly Application Distribution loan types") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1)
  )

# Average overall default rate which comes around 14.6%
mean(loan_ds$loan_status) # 14.6%

loan_ds[sapply(loan_ds, is.character)] <- lapply(loan_ds[sapply(loan_ds, is.character)], as.factor)

# Let's see the correlation values of the numerical variables 
continous_var <- names(loan_ds)[sapply(loan_ds, class) != "factor"]

continous_data <- loan_ds[,(colnames(loan_ds) %in% continous_var)]

# Also, removing loan_status variable. it is a discrete variable
continous_data$loan_status <- NULL

# Also, removing date variable
continous_data$issue_d <- NULL

# Coreleation between loan amount and funded amount is approxiametely 98%
corr_amt <- cor(loan_ds$loan_amnt,loan_ds$funded_amnt)
corr_amt

# Coreleation between loan amount and annual income is approxiametely 26%
corr_cust <- cor(loan_ds$loan_amnt,loan_ds$annual_inc)
corr_cust

# Coreleation between loan amount and installment is approxiametely 93%
corr_ins <- cor(loan_ds$loan_amnt,loan_ds$installment)
corr_ins

# Plot the correlation matrix

correlations <- cor(continous_data[,c(3:8,19,20)], use="pairwise", method="spearman")
correlations
corrplot(correlations, method="number")



# Univariate Analysis -----
# Univariate Analysis with loan_status column
# Plotting charged off customers data in a histogram in order to depict for which 
# loan amount customer count is significant. From the chart its clear that for the
# loan amount ranging from 1000-10000 and above 20000 the charged off count is significant
# whcih to be looked out.
# Below line of code may throw a Warning messege since data points are more and hence for
# better visualization purpose plots in presentation created in tableau.
ggplot(loan_ds_charged_off, aes(x=loan_amnt)) + geom_bar(width = 500, position = "dodge")

# Univariate Analysis with Debt-To-Income Ratio (dtb) column
# Calculating therange for Debt-To-Income Ratio. 
# This will help in identifying the 68% range for defaulters.
# The mean and Standard Deviation sum is 20.5862
# and the mean and Standard Deviation deviation difference is 7.415047
mean(loan_ds_charged_off$dti, na.rm = FALSE)+sd(loan_ds_charged_off$dti, na.rm = FALSE)

mean(loan_ds_charged_off$dti, na.rm = FALSE)-sd(loan_ds_charged_off$dti, na.rm = FALSE)


# Plotting for the Debt-To-Income Ratio for defaulted loan entries. 
# This plot clearly identifying the DTI range for which defaulter are more.
# The plot shows the majority (68%) of defaults are in the DTI range 7.41-20.58
# Insight 1 - can be concluded as People who have a DTI ratio between 7.41-20.58 comprise 
# 68% of the defaulters. 
ggplot(loan_ds_charged_off, aes(dti))+geom_freqpoly(binwidth=4)

# Plotting Types of Products offered for the overall loan data set
# This plot clearly shows the percentage of loans given in different product types.
# Insight 2 - can be concluded as most of the loans given for debt consolidation
# where are renewable energy got the least amount of loan
loan_ds %>% ggplot(aes(x = as.factor(loan_ds$purpose))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "Types of Products offered", y = "Percent", x = "Types of Products offered")+theme(axis.text.y=element_blank(), axis.ticks=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 60, hjust = 1))


# BiVariate Analysis ------
# Plotting customers whom got Charged Off Plot according to loan term. i.e 36 months or 60 Months
# We can clearly see that loans raning between 2000 to 10000 and with a tenure of 36 months
# has more number of charged off cases. This is a interesting insight as part of our bi-variate analysis
# with loan_amount vs loan term
#Insight 3 - for loans up to 10000, the default is more for mid-term loan (36 months), 
#whereas for loan amount greater then 10000, the default is more for long-term loan (60 months).
ggplot(loan_ds_charged_off, aes(x=loan_amnt, fill=term)) + geom_histogram(binwidth = 5000,position = "dodge")

# Plotting the loan amount against the loan term. 
# This will help in identifying if there is any indication for default 
# based on the loan amount and the loan term.
# Insight 4 - for loans up to 25000, the loans are settled by 36 months, 
# for loans greater then 25000, the loans are settled in 60 months.
ggplot(loan_ds_fully_paid, aes(x=loan_amnt, fill=term))+geom_histogram(binwidth = 5000,position = "dodge")


# Plotting the loan amount against the employment length for defaulted loan entries. 
# This will help in identifying if there is any indication for default based on the loan amount 
# and the employment length.
# Insight 5 - People who have employment length >10 years are the most defaulters. 
ggplot(loan_ds_charged_off, aes(x=loan_amnt, fill=emp_length))+geom_histogram(binwidth = 5000,position = "stack")

# Plotting the loan amount against the loan grade for defaulted loan entries. 
# This will help in identifying if there is any indication for default based on 
# the loan amount and the grade.
# Insight 6 - Grade B loans incur the maximum defaults. 
ggplot(loan_ds_charged_off, aes(x=loan_amnt, fill=grade))+geom_histogram(binwidth = 5000)

# Plotting the loan amount against the employment length for defaulted loan entries. 
# This will help in identifying if there is any indication for default based on the loan amount 
# and the employment length.
# Insight 6a - People whose employment length is n/a i.e. not verified or not available 
#are the most defaulters. 
#This point should be taken seriously while verifying the customer employment 
#details before approving the loan.
ggplot(loan_ds_charged_off, aes(x=loan_amnt, fill=emp_length))+geom_histogram(binwidth = 5000,position = "stack") + facet_wrap(~term)

# Plotting the loan amount against the home ownership for defaulted loan entries. 
# This will help in identifying if there is any indication for default based on the 
# loan amount and the home ownership.
# Insight 7 - for loans <15000, people with rented houses are the maximum defaulters and 
# for loans >15000, people with mortgages are maximum defaulters.
ggplot(loan_ds_charged_off, aes(x=loan_amnt, fill=home_ownership))+geom_histogram(binwidth = 5000)

# Plotting the loan amount against the purpose for defaulted loan entries. 
# This will help in identifying if there is any indication for default based 
# on the loan amount and the loan purpose.
# Insight 8 - People who took loans for 'debt_consolidation' are the maximum defaulters.
ggplot(loan_ds_charged_off, aes(x=loan_amnt, fill=purpose))+geom_histogram(binwidth = 5000)

# Plotting for Public bankrupty records
# This plot clearly shows the loan_amount for which the public bankruptcy records are high
# Clearly for the loan amount ranging from 4000-10000 bankruptcy records are the maximum.
# Insight 9 - for the loan amount ranging from 4000-10000 customers seems to have maximum 
# bankruptcy records
ggplot(loan_ds_charged_off, aes(x=loan_amnt, fill=pub_rec_bankruptcies))+geom_histogram(binwidth = 5000)

# Plotting for verification_status records
# This plot clearly shows the loan_amount vs. verification_status
# such as Not Verified, Source Verified and Verified.
# Insight 10 -Clearly for the loan amount ranging from 4000-10000 the customers are not verified.
ggplot(loan_ds_charged_off,aes(x=loan_amnt,fill=verification_status))+geom_histogram(binwidth = 5000)

# Plotting for interest rate.
# 48% of People are given loan at intrest between 10-15% and they are 
# most likely to default and among these most of them belong to Grade B
# Insight 11 - 48% of People are given loan at intrest between 10-15%
# Below line of code may throw a Warning messege since data points are more and hence for
# better visualization purpose plots in presentation created in tableau.
loan_ds_charged_off$int_rate <-- as.numeric(sub("%","",loan_ds_charged_off$int_rate,fixed=TRUE))
ggplot(loan_ds_charged_off, aes(x=int_rate, fill=grade))+geom_bar(width = 0.5,Position="stack")

# Ploting the statewise (for all the states in the US present in the data set)charged off customers
# This plot clearly shows the charged off customers distributed over different states.
# CA being the state where number of customers are charged off are the highest.
# Insight 12 - CA being the state where number of customers are charged off are the highest.
ggplot(loan_ds_charged_off, aes(x=loan_amnt, fill=factor(addr_state))) + geom_histogram(binwidth = 5000) + facet_wrap(~term)

# Ploting the statewise Fully Paid customers
# This plot clearly shows the Fully Paid customers distributed over different states.
# CA being the state where number of customers are Fully Paid are the highest.
#Insight 12a - CA being the state where number of customers are charged off are the highest.
ggplot(loan_ds_charged_off, aes(x=loan_amnt, fill=addr_state )) + geom_histogram(binwidth = 5000) + facet_wrap(~term)

# Create Word Cloud -----
# A word cloud is being created from the description column in order to 
# Derive an insight for which topic is being chatted/talked the most.


# We have created a word cloud file since it was taking longer
# to execute the code with other approach (without creating the word cloud file)
# Delete the word cloud file it it exists already.
# Please change the working directory as per the requirement.
#setwd("/Users/hduser/Desktop/Upgrad/EDA/case_study/")
cld_file <- "word_cld.txt"
if (file.exists(cld_file)) file.remove(cld_file)

# Read the purpose column from the clean Data Frame
# Remove the unnecessary part from the text e.g stop words 
text <-
  loan_ds_cln %>% select(desc) %>% na.omit() %>%
  mutate(desc = gsub("Borrower added on [0-9]+/[0-9]+/[0-9]+ > ","",desc)) %>% write.csv(.,file="word_cld.txt", row.names = FALSE)

# Read the word cloud file created in the above path from the working directory.
filePath<-("word_cld.txt")
text <- readLines(filePath)
#str(text)

# Load the text of the word cloud file as a corpus
docs <- Corpus(VectorSource(text))

# Inspecting the content of Corpus created in the above step.
inspect(docs)

# Converting symbols such as /, @ and Pipe (|) to Spaces
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove our defined stop words
# Our stopwords defined as a character vector
docs <- tm_map(docs, removeWords, c("will", "just","want","purpose","year","make","good")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
docs <- tm_map(docs, stemDocument)

# Converting to to Term Document Matrix. 
# Below lines of code may take little logner to execute.(around a minute)
# and may require more memory to run. 
# Below code is tested and running perfectly on different machine with memory 8GB  and 16GB 
# with i7 quad core processor. Code may run longer if it run on a lower configuration machine.
# Recommended to run with similar configuration machine.
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Creating the word cloud.
# Below line of code may take little logner to execute.( around 30-40 seconds)
# Please wait till completion of below line of code to see the beautiful word cloud.
# since the corpous is huge.

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 10,
          max.words=400, random.order=FALSE, rot.per=0.55, 
          colors=brewer.pal(8, "Dark2"))

# To find Most frequent words  and their Frequencies
# at this point we will analyse what are the words most frequently 
# occuring with credit e.g. credit card loan, credit card debt credit card pay etc.

findFreqTerms(dtm, lowfreq = 4)
findAssocs(dtm, terms = "credit", corlimit = 0.3)
head(d, 10)

# Bar Plot showing most frequently occuring words 
# This plot clearly shows loan is being the most frequest used word
# and interest being the least frequent word
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


