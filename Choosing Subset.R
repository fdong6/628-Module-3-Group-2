rm(list = ls())

# library(tidytext)
# library(dplyr)


if (!require("stringr")) {
  install.packages("stringr")
  stopifnot(require("stringr"))
}


###### import raw data
#business_raw = read.csv("business_origin.csv", header = T)
#review_raw = read.csv("review_origin.csv", header = T)
#tip_raw = read.csv("tip_origin.csv", header = T)
#user_raw = read.csv("user_origin.csv", header = T)

business_raw = business
tip_raw = tip
user_raw = user
review_raw=review
###### narrow down
business_Madison = business_raw[which(business_raw$state=="WI" & business_raw$city=="Madison"),]
business_WI = business_raw[which(business_raw$state=="WI"),]
length(business_Madison)

#business = business_raw
business_id = as.character(business$business_id)

categories = as.character(business$categories)
categories = sapply(categories, function(x){     # split categories
  str_split(x, pattern = ", ")
})
t = table(unlist(categories))
sort(t, decreasing = T)[1:10]

# list = c("Fast Food", "Dry Cleaning & Laundry")
w = unlist(lapply(categories, function(x){
  all("Fast Food" %in% x)
}))

picked.business = business[w,]    # Fast Food subset: 1638 x 14
categories = as.character(picked.business$categories)
categories = sapply(categories, function(x){
  str_split(x, pattern = ", ")
})
t = table(unlist(categories))
sort(t, decreasing = T)[1:10]


business = picked.business
business_id = as.character(business$business_id)
business_uniq <- unique(business)
B_L<-length(business_uniq)
B_L
business_id_uniq <- unique(business_id)
B_L_id<-length(business_id_uniq)
B_L_id

### subset - review
review_w = unlist(sapply(review_raw$business_id, function(x){
  ifelse((x %in% business_id), T, F)
}))
review = review_raw[review_w,]     # 33262 x 9
user_id = as.character(review$user_id)

### subset - tip
tip_w = unlist(sapply(tip_raw$business_id, function(x){
  all(x %in% business_id)
}))
tip_user_w = unlist(sapply(tip_raw$user_id, function(x){
  all(x %in% user_id)
}))
tip = tip_raw[tip_w,]
dim(tip_raw[tip_w,])        # 5823 x 5
dim(tip_raw[tip_user_w,])   # 53941 x 5

### subset - user
user_w = unlist(sapply(user_raw$user_id, function(x){
  all(x %in% user_id)
}))
user = user_raw[user_w,]    # 21741 x 22
















###### analysis - professor - maybe python
plotWordStar <- function(stars,DTM,wordList,mfrow = c(4,4)) {
  par(mfrow = mfrow)
  col_DTM = colnames(DTM)
  for(i in 1:length(wordList)) {
    index = which(col_DTM == wordList[i])
    if(length(index) == 0) {
      warning(paste(wordList[i],"not detected"))
      next
    } 
    dtm_vec = as.numeric(DTM[,index])
    names(dtm_vec) = rownames(DTM)
    starsY = rep(0,5)
    for(j in 1:5) {
      # I've changed this code to scale by total number of stars. This
      # I think provides better resolution than before.
      element = dtm_vec[as.character(stars$uniqueID[which(stars$stars == j)])]
      starsY[j]  = sum(element > 0,na.rm=TRUE) / sum(stars$stars == j)
    }
    barplot(starsY,main=wordList[i],xlab="Stars",ylab="Word Freq")
  }  
}
Y = review_out %>% select(uniqueID,stars)