rm(list = ls())

setwd("D:/Documents/Stat 628")
load("Fast_Food_Data.RData")
head(business)

business_uniq <- unique(business$business_id)
review_business<-unique(review$business_id)
B_L<-length(business_uniq)

business_uniq

#plotCIName(business$stars,business$name, alpha = 0.01/B_L)

if (!require("tidytext")) {
  install.packages("tidytext")
  stopifnot(require("tidytext"))
}

if (!require("text2vec")) {
  install.packages("text2vec")
  stopifnot(require("text2vec"))
}
if (!require("e1071")) {
  install.packages("e1071")
  stopifnot(require("e1071"))
}

 
if (!require("plyr")) {
  install.packages("plyr")
  stopifnot(require("plyr"))
}

if (!require("dplyr")) {
  install.packages("dplyr")
  stopifnot(require("dplyr"))
}

if (!require("ggplot2")) {
  install.packages("ggplot2")
  stopifnot(require("ggplot2"))
}

if (!require("textdata")) {
  install.packages("textdata")
  stopifnot(require("textdata"))
}

if (!require("tidyr")) {
  install.packages("tidyr")
  stopifnot(require("tidyr"))
}


if (!require("influential")) {
  install.packages("influential")
  stopifnot(require("influential"))
}

if (!require("ggraph")) {
  install.packages("ggraph")
  stopifnot(require("ggraph"))
}

if (!require("glmnet")) {
  install.packages("glmnet")
  stopifnot(require("glmnet"))
}

if (!require("caret")) {
  install.packages("caret")
  stopifnot(require("caret"))
}

if (!require("lubridate")) {
  install.packages("lubridate")
  stopifnot(require("lubridate"))
}

if (!require("selectiveInference")) {
  install.packages("selectiveInference")
  stopifnot(require("selectiveInference"))
}


review$text<-as.character(review$text)

length(business$name)


####graph most counted words in all reviews####

review %>%
  unnest_tokens(word, text,to_lower=TRUE) %>%
  filter(!word %in% stop_words$word) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head(20) %>%
  ggplot(aes(x = word,y = n)) +
  geom_bar(stat='identity',colour="white") +
  geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Word', y = 'Word Count', 
       title = 'Word Count-all stars') +
  coord_flip() + 
  theme_bw()

####graph most counted words in 5-star restaurants####
wordsf_5star<-review %>% filter(stars == 5) 
str(wordsf_5star)
 wordsf_5star %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head(20) %>%
  ggplot(aes(x = word,y = n)) +
  geom_bar(stat='identity',colour="white") +
  geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Word', y = 'Word Count', 
       title = 'Word Count-5 stars') +
  coord_flip() + 
  theme_bw()
#################################
 
 ####graph most counted words in 4-star restaurants####
 wordsf_4star<-review %>% filter(stars == 4) 
 wordsf_4star %>%
   unnest_tokens(word, text) %>%
   filter(!word %in% stop_words$word) %>%
   count(word,sort = TRUE) %>%
   ungroup() %>%
   mutate(word = factor(word, levels = rev(unique(word)))) %>%
   head(20) %>%
   ggplot(aes(x = word,y = n)) +
   geom_bar(stat='identity',colour="white") +
   geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
             hjust=0, vjust=.5, size = 4, colour = 'black',
             fontface = 'bold') +
   labs(x = 'Word', y = 'Word Count', 
        title = 'Word Count-4 stars') +
   coord_flip() + 
   theme_bw()
 #################################
 ####graph most counted words in 3-star restaurants####
 wordsf_3star<-review %>% filter(stars == 3) 
 
 wordsf_3star %>%
   unnest_tokens(word, text) %>%
   filter(!word %in% stop_words$word) %>%
   count(word,sort = TRUE) %>%
   ungroup() %>%
   mutate(word = factor(word, levels = rev(unique(word)))) %>%
   head(20) %>%
   ggplot(aes(x = word,y = n)) +
   geom_bar(stat='identity',colour="white") +
   geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
             hjust=0, vjust=.5, size = 4, colour = 'black',
             fontface = 'bold') +
   labs(x = 'Word', y = 'Word Count', 
        title = 'Word Count-3 stars') +
   coord_flip() + 
   theme_bw()
 #################################
 ####graph most counted words in 2-star restaurants####
 wordsf_2star<-review %>% filter(stars == 2) 
  wordsf_2star %>%
   unnest_tokens(word, text) %>%
   filter(!word %in% stop_words$word) %>%
   count(word,sort = TRUE) %>%
   ungroup() %>%
   mutate(word = factor(word, levels = rev(unique(word)))) %>%
   head(20) %>%
   ggplot(aes(x = word,y = n)) +
   geom_bar(stat='identity',colour="white") +
   geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
             hjust=0, vjust=.5, size = 4, colour = 'black',
             fontface = 'bold') +
   labs(x = 'Word', y = 'Word Count', 
        title = 'Word Count-2 stars') +
   coord_flip() + 
   theme_bw()
 #################################
 
 ####graph most counted words in 1-star restaurants####
 wordsf_1star<-review %>% filter(stars == 1) 
 str(wordsf_1star)
 wordsf_1star %>%
   unnest_tokens(word, text) %>%
   filter(!word %in% stop_words$word) %>%
   count(word,sort = TRUE) %>%
   ungroup() %>%
   mutate(word = factor(word, levels = rev(unique(word)))) %>%
   head(20) %>%
   ggplot(aes(x = word,y = n)) +
   geom_bar(stat='identity',colour="white") +
   geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
             hjust=0, vjust=.5, size = 4, colour = 'black',
             fontface = 'bold') +
   labs(x = 'Word', y = 'Word Count', 
        title = 'Word Count-1 star') +
   coord_flip() + 
   theme_bw()
 #################################
####Plot of mean and distribution
 ggplot(business, aes(x=stars)) +
   geom_histogram(binwidth=.5, colour="black", fill="white") +
   geom_vline(aes(xintercept=mean(stars, na.rm=T)),   # Ignore NA values for mean
              color="red", linetype="dashed", size=1)
 skewness(business$stars)
 sd(business$stars)
 ####################################
 ####statistical description####

 mean(business$stars)
 mean_stars<-tapply(review$stars, review$business_id, mean)
 review %>%
   group_by(business_id) %>%
   summarize(mean_star = mean(stars, na.rm = TRUE))
 
 business %>%
   group_by(state) %>%
   summarize(mean_star = mean(stars, na.rm = TRUE))
 
 min(ymd_hms(review$date))
 max(ymd_hms(review$date))






