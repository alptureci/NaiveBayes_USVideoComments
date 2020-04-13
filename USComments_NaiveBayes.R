
#@Authors: Dmitrii Dunin, Alp Tureci

setwd("/Users/atureci/Documents/ITU/MachineLearning/FinalReport/")
library(e1071)
library(dplyr)
library(ngram)
library(tidytext)
library(klaR)
library(ROCR)
library(generics)
library(stringr)
library(ggplot2)
library(psych)


# Install package "ngram"
#install.packages("ngram")
#install.packages("psych")

#df <- data.frame(matrix(ncol = 4, nrow = 100))
#UScomments <- read.csv("~/Documents/ITU/MachineLearning/FinalReport/UScomments_clean_v2.csv")

#UScomments <- read.csv("~/Documents/ITU/MachineLearning/FinalReport/UScomments_clean_v2.csv", header = TRUE, sep = ",", quote = "\"'")
UScomments <- read.csv("~/Documents/ITU/MachineLearning/FinalReport/UScomments_clean_v2.csv", header = TRUE, sep = ",")


us_clean_replies <- UScomments[!is.na(as.numeric(as.character(UScomments$replies))), ]
us_clean_likes <- us_clean_replies[!is.na(as.numeric(as.character(us_clean_replies$likes))), ]
us_clean_comments <- us_clean_likes[!is.na(as.character(as.character(us_clean_likes$comment_text))), ]
us_clean_comments_v2 <- dplyr::filter(us_clean_comments, grepl("[a-zA-Z].", us_clean_comments$comment_text))


#TEST
#clean_comments[Reduce('&', lapply(clean_comments["likes"], function(x) !is.na(as.numeric(x)))),]
#us_clean_replies <- clean_comments[!is.na(as.numeric(as.character(clean_comments$replies))), ]
#is.numeric(" 2017")
#END TEST

clean_comments <- us_clean_comments_v2
summary(clean_comments)

clean_comments$likes <- as.numeric(as.character(clean_comments$likes))
summary(clean_comments)

clean_comments$replies <- as.numeric(as.character(clean_comments$replies))
summary(clean_comments)


#ng30 <- ngram(clean_comments$comment_text, n = 30)

text = paste(clean_comments$comment_text, collapse = " ")

result = tibble(text = text) %>% 
  unnest_tokens(word, text) %>%    # split words
  anti_join(stop_words) %>%    # take out "a", "an", "the", etc.
  anti_join(data.frame(word = "wow"))  %>% #in case we will want to exclude curse words
  #anti_join(profanity_filter
  filter(str_detect(word, "[a-z']$"))  %>%
  count(word, sort = TRUE)    # count occurrences
print(result, n=30)


# Get top 30 used words
top_words = result[1:30,]$word

comments_columns = clean_comments

# Append top 26 words as columns to dataframe
comments_columns[top_words] <- 0

# Count the number of occurrencies of each word for each comment
# Put the count in the column with the word name
for (i in 1: length(top_words)){
  comments_columns[result[i:i,]$word] <- str_count(comments_columns$comment_text, result[i:i,]$word)
}

# Add column "liked" with logical value of $likes column (0 = FALSE, > 0 = TRUE)
# NOT POPULAR - [< 10]
# POPULAR - [11 - 100]
# VERY POPULAR - [> 100]
# comments_columns["liked"] <- as.factor(as.logical(comments_columns$likes))

classified_columns <- mutate(comments_columns,
       popularity = ifelse(
         likes < 10,
         "NOT POPULAR",
         ifelse(likes %in% 11:100, "POPULAR",
                ifelse(likes > 100, "VERY POPULAR", "NOT POPULAR"))
       ))

# Filters the unnecessary firt 4 columns. 
# We are only interested in the occurance of the most commonly 
# used words whic are transformed into the columns
comments_columns_filtered = classified_columns[,5:length(classified_columns)]
str(comments_columns_filtered)
comments_columns_filtered$popularity <- as.factor(comments_columns_filtered$popularity)


#sample_comments <- sample(comments_columns_filtered, size = 5000)
sample_comments <- comments_columns_filtered[1:50000,]
sampled = comments_columns_filtered[sample(nrow(comments_columns_filtered), size = 50000),]


#PREDITION WITH E1071
# Using NaiveBayes on 30 attributes of top 30 word counts to predict "liked" column
bayes_result = naiveBayes(popularity~ ., data = comments_columns_filtered)
bayes_result
str(bayes_result)
summary(bayes_result)

#plot(bayes_result) #Error in xy.coords(x, y, xlabel, ylabel, log) : 
#'x' is a list, but does not have components 'x' and 'y'

#bayes_sample <- NaiveBayes(liked~ ., data = sample_comments)

#??predict.naiveBayes
modelPred <- predict(bayes_result, sample_comments)
str(modelPred)
summary(modelPred)

conf_matrix <- table(modelPred, sample_comments$popularity)
plot(conf_matrix)



#PREDICT WITH KLAR
bayes_result_klar = NaiveBayes(popularity~ ., data = comments_columns_filtered)
bayes_result_klar
str(bayes_result_klar)
summary(bayes_result_klar)
plot(bayes_result_klar) 

# KLAR PREDICTION
modelPred <- predict(bayes_result_klar, sampled)
str(modelPred)
summary(modelPred)
Err_Klar <- 1 - sum(modelPred$class == sampled$popularity)/length(sampled$popularity)























