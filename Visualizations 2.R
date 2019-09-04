
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(mice)
library(VIM)
library(pROC)
library(caret)
library(sqldf)


data=read.csv("PREMIUM_FUEL.csv", header = TRUE, na.strings = c("NA","","#NA"))

# check # of rows
ncol(data);
nrow(data);

# quickly preview data structure with HEAD(), STR(), and SUMMARY()
head(data,10)
str(data)
summary(data)

#check distribution of target variable
hist(data$TARGET);
summary(data$TARGET);


# DATA EXPLORATION: CATEGORICAL   


#1. Frequency table for gender
attach(data)
freq_tbl=table(CLIENT_GENDER)
head(freq_tbl)

# 1B. Proportion table
prop.table(freq_tbl)


# 2. cross-tab for 2 categorical features - Gender and Target
freq_xtab=xtabs(~CLIENT_GENDER+TARGET, data = data)
head(freq_xtab)

# 2B. cross-tab for 2 categorical features, this time with proportions
prop.table(freq_xtab)


# Using SQL and APPLY   

sqldf('select CLIENT_GENDER, avg("PARTNERS_SHOPPED") from data group by CLIENT_GENDER')

# can also do the same thing with tappl()
tapply(data$PARTNERS_SHOPPED, data$CLIENT_GENDER, FUN=mean, na.rm=TRUE)



# Can also use DPLYR
partner_shopped_by_gender <-
  data %>%
  group_by(CLIENT_GENDER) %>%
  summarise(mean_PARTNERS_SHOPPED = mean(PARTNERS_SHOPPED, na.rm=TRUE),fct_explicit_na(f, na_level = "(Missing)"))

head(partner_shopped_by_gender)


#  VISUALIZATIONS
attach(data)
freq_tbl=table(CLIENT_GENDER)

# 1A. barplot with absolute counts
barplot(freq_tbl)

# 1B. barplot with proportions
barplot(prop.table(freq_tbl))

# 2. create mosaic plot / pie chart for the different levels
mosaicplot(freq_tbl)
pie(freq_tbl)

# with many levels
mosaicplot(table(REWARD_HIST_SEGMENT))
pie(table(REWARD_HIST_SEGMENT))

# 3. Stacked bar-plot
freq_xtab=xtabs(~CLIENT_GENDER+TARGET)
head(freq_xtab)

# basic bar-plot will automatically be stacked
barplot(freq_xtab)
barplot(prop.table(freq_xtab,2), legend=colnames(freq_xtab))

# add labels, and colour
barplot(prop.table(freq_xtab,2),
        legend=rownames(freq_xtab),
        ylab="Target Variable", xlab="CLIENT_GENDER",
        main = "Looking for difference in Target Variable for different Gender Classes",
        col = c("red", "blue"))



#final full plot code
# sepertes the result by category and changes the y-axis origentation incrementes
barplot(freq_xtab, legend=rownames(freq_xtab), ylab="Target Variable", xlab="CLIENT_GENDER", 
        col=c("orange","deepskyblue1"), beside=T, las=1)



gender_vs_rwd_freq_xtab = table(CLIENT_GENDER,REWARD_HIST_SEGMENT)
gender_vs_rwd_pct_xtab = prop.table(gender_vs_rwd_freq_xtab,2)
barplot(gender_vs_rwd_pct_xtab,  col=c("orange","deepskyblue1"))


#  DATA EXPLORATION: NUMERIC VARIABLES 

# 1. Simple summary(data) is very useful for numeric
summary(data)

# 2 Hisograms
hist(data$NUM_FMLY_MEMBERS)
hist(log(data$CNT_VISITS_GAS_STATION))

# 3 What if we want to get histograms for all numeric variables? instead of 1 at a time
# Step 1: Get only numeric colums
list_of_numcols = sapply(data, is.numeric)
numcols = data[ , list_of_numcols]

str(data)
str(numcols)

# Step 2: Melt data
melt_data = melt(numcols, id.vars=c("ID"))
head(melt_data, 10)

# Step 3: Now suitable for a multiplot function
ggplot(data = melt_data, mapping = aes(x = value)) + 
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')

# MORE VISUALIZING
plot= ggplot(data, aes(REWARD_HIST_SEGMENT, ..count..)) + 
  geom_bar(aes(fill = CLIENT_GENDER), position = "stack")
plot

# Advantage -> changes the angle of the label
plot + theme(axis.text.x=element_text(angle = -90, hjust = 0))

#note stack just stacks up the raw counts, while fill scales all to % of total 
data2=data
levels(data2$REWARD_HIST_SEGMENT) = gsub(" ", "\n", levels(data2$REWARD_HIST_SEGMENT))
ggplot(data2, aes(REWARD_HIST_SEGMENT, ..count..)) + geom_bar(aes(fill = CLIENT_GENDER), position = "fill")
theme(axis.text.x = element_text(angle = 90, hjust = 1))



plot(data$TARGET, data$AVG_WKLY_SPND_ALL_PARTNERS)
# not useful

