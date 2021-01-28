require(tidyr)
require(dplyr)
require(fastDummies)
require(naniar)
require(RColorBrewer)
require(arulesViz)
require(shiny)
data <- read.csv('hr_analytics.csv', header = T,
                 na.strings='NULL')
dim(data) # dataset consists 14 columns & 54808 rows,
str(data) # structure of data,
table(sapply(data, class)) # 5 categorical and 9 integers,
summary(data) # missing values in region appear as values with no name '',
data[data==''] <- NA # changing '' to 'NA'
summary(data)
head(data)
vis_miss(data) # 4.4% of data for 'education' and 7.52% for 'previous_year_rating'
# are missing, since 'length_of_service' for all missing values in 
# 'previous_year_rating' is 1, it's obvious missing values are due to new employments
# hence I'll impute them with average ratings for each department.
# there is no clear pattern to investigate the reason behind availablity of missing
# values in 'education' column, and the proportion of them are not significant in the dataset
# hence I assume it is a good practice to drop the rows containing missing values.
data <- data %>% group_by(department) %>%
  mutate(previous_year_rating=ifelse(is.na(previous_year_rating),
                                     floor(mean(previous_year_rating,na.rm=TRUE))
                                     ,previous_year_rating)) # imputing missing values
unique(data$previous_year_rating) # checking imputation validity
dim(data)
data <- na.omit(data) #removing all rows containing missing values in education
dim(data)
unique(data$education)
vis_miss(data) # treating missing values validation
#data <- as.data.frame(lapply(data, function(x)gsub('\\s+','',x)))
summary(data)
# converting continues numerical variables to categories by bining them,
# Length of service:
breaks <- c(1, 5, 10, 15, 20, 25, 30, 37)
tags <- c('[1-5)', '[5-10)', '[10-15)', '[15-20)', '[20-25)', '[25-30)', '[30-37]' )
data$length_of_service_bin <- cut(data$length_of_service, include.lowest = T,
                                  breaks= breaks, 
                                  labels= tags, 
                                  right= F)
summary(data$length_of_service)
summary(data$length_of_service_bin)
# Age:
summary(data$age)
breaks <- c(20, 30, 40, 50, 60)
tags <- c('20-30', '30-40', '40-50', '50-60')
data$age_bin <- cut(data$age, include.lowest = T, breaks=breaks, labels = tags, 
                    right=F)
summary(data$age_bin)
# Average training score
summary(data$avg_training_score)
breaks <- c(39, 50, 70, 99)
tags <- c('below-50', '50-70', 'above-70')
data$avg_training_score_bin <- cut(data$avg_training_score, include.lowest = T, 
                                   breaks = breaks, labels = tags, right = F )
summary(data$avg_training_score_bin)
# Number of trainings
summary(data$no_of_trainings)
breaks <- c(1, 3, 7, 10)
tags <- c('less-than-3', '3-7', 'more-than-7')
data$no_of_trainings_bin <- cut(data$no_of_trainings, include.lowest = T, 
                                breaks = breaks, labels = tags, right = F)
summary(data$no_of_trainings_bin)
# Dropping continuous columns, as well as employee id
drops <- c("employee_id", "no_of_trainings", "age", "length_of_service", 
           "avg_training_score")
data <- data[, !(names(data) %in% drops)]
names(data)
# creating a subset of only those who got promoted
#data <- subset(data, is_promoted == 1)
#dim(data) # new dataset has got 13 columns and 4546 rows
#table(data$is_promoted)
# checking class of columns
table(sapply(data, class)) # yet we have 3 integers and 1 numeric column
data[] <- lapply(data, function(x) if(is.factor(x)) 
  factor(x) else x) #refactoring to remove the levels wich its char has been removed
data[] <- lapply(data, factor) # converting all variables to categorical
table(sapply(data, class)) # now we have 13 categorical variables
write.csv(data, 'clean.csv', row.names = F)
basket <- as(data, "transactions") # creating transaction prior working with apriori 
summary(basket) # top-5 frequent items in this transaction are, 'is_promoted=1', 
# 'no_of_trainings_bin=less-than-3', 'awards_won=0', 'KPIs_met..80.=1', and 'gender=m',
head(basket@itemInfo)
inspect(basket[1:10])
# item frequency histogram:
arules::itemFrequencyPlot(basket,topN=20,
                          col=brewer.pal(8,'Pastel2'),
                          main='Relative Item Frequency Plot'
                          ,type="relative",ylab="Item Frequency (Relative)")
freq_is <- apriori(basket, parameter = list(minlen=2, maxlen=6, target= 'frequent itemsets'
                                            ,conf=0.8, supp=0.001))
inspect(freq_is[1:10])
summary(freq_is)
rules_1 <- apriori(basket, parameter = list(minlen=2, maxlen=3, supp= 0.001, conf= 0.8),
                   appearance = list(rhs= c('is_promoted=1')))
inspect(rules_1)
rules_2 <- apriori(basket, parameter = list(minlen=4, maxlen=6, supp= 0.001, conf= 0.8),
                   appearance = list(rhs= c('is_promoted=1')))
inspect(rules_2[1:10])
plot(rules_2[1:20],method = "graph",
     control = list(type = "items")) # a bit confusing
plot(rules_2[1:20],
     method = "paracoord",
     control = list(reorder = TRUE)) # good one for inferential
plot(rules_2[1:10], method = "grouped") # good for small lists
plot(rules_2[1:10]@quality, col='red', pch=19) #confusing
# plot(rules_1[1:20], engine = "plotly") # doesn't work!!!!!!!!!!!!!!!!
#install.packages('shinythemes')
library(shinythemes)
ruleExplorer(rules_2[1:50])
# extracting more rules is required
rules_3 <- apriori(basket, parameter = list(minlen=5, maxlen=7, supp= 0.001, conf= 0.8),
                   appearance = list(rhs= c('is_promoted=1')))
inspect(rules_3[1:10])

plot(rules_3[1:20],method = "graph",
     control = list(type = "items"))
plot(rules_3[1:20],
     method = "paracoord",
     control = list(reorder = TRUE)) # good one for inferential
ruleExplorer(rules_3[1:50])
write(basket, file="basket.csv", format = c("basket", "single"),
      sep=" ", quote=TRUE)
# Preparing the dataset for SAS
data$ID <- seq.int(nrow(data))
df <- data
df$previous_year_rating <- mapvalues(df$previous_year_rating,
                                     from= c(1, 2, 3, 4, 5), to=c('pre_yr_rate_1', 'pre_yr_rate_2',
                                                                  'pre_yr_rate_3', 'pre_yr_rate_4',
                                                                  'pre_yr_rate_5'))
df$KPIs_met..80. <- mapvalues(df$KPIs_met..80.,
                                     from= c(1, 0), to=c('KPIs_met', 'KPIs_not_met'))
df$awards_won. <- mapvalues(df$awards_won.,
                              from= c(1, 0), to=c('won_awards', 'not_won_awards'))
df$is_promoted <- mapvalues(df$is_promoted,
                            from= c(1, 0), to=c('promoted', 'not_promoted'))
df$avg_training_score_bin <- mapvalues(df$avg_training_score_bin,
                            from= c('below-50', '50-70', 'above-70'
                                    ), to=c('train_score: below-50'
                                            , 'train_score: 50-70',
                                            'train_score: above-70'))
df$no_of_trainings_bin <- mapvalues(df$no_of_trainings_bin,
                                       from= c('less-than-3', '3-7', 'more-than-7'
                                       ), to=c('less-than-3_training'
                                               , '3-7_training',
                                               'more-than-7_training'))
long <- df %>%
  pivot_longer(!ID, names_to= 'cols', values_to= 'Transactions')
head(long)
write.csv(long, 'SAStransaction.csv', row.names = T)
