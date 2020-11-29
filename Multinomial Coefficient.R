rm(list = ls())

library(tidytext)
library(plyr)
library(dplyr)
library(ggplot2)
library(text2vec)
library(glmnet)
library(tm)

# setwd("E:/Class/628/Module 3 Data/")
load("Fast_Food_Data.RData")
# load("W:/UW_Class_2020_2021/628/Module3/Data/Fast_Food_Data.RData")

business_uniq <- unique(business$name)
(B_L<-length(business_uniq))
# business_uniq


# wordList<-c("bad","good","worst","best","awful","horrible","nice","excellent","fast","slow","average","wonderful","tasty",
#             "disgusting","terrific","amazing","dirty","fresh","love","friendly","cool","cheap","expensive","great","stale","quickly",
#             "delightful","clean")
# 
# name_Res<-c("Wendy's", "McDonald's","KFC","Five Guys","Sheetz","DP Dough","On Tap","Wings Over Madison")


length(business$name)


review_text<-as.character(review$text)
it <- itoken(review_text, preprocess_function = tolower, 
             tokenizer = word_tokenizer, chunks_number = 10, progessbar = F)
# using unigrams and bigrams here
vocab <- create_vocabulary(it, ngram = c(1L, 2L))
pruned_vocab = prune_vocabulary(vocab, term_count_min = 10, doc_proportion_max = 0.5,
                                doc_proportion_min = 0.001)

vectorizer = vocab_vectorizer(pruned_vocab)
dtm = create_dtm(it, vectorizer)
dim(dtm)

# model_tfidf = TfIdf$new()
# dtm_tfidf = model_tfidf$fit_transform(dtm)
# dim(dtm_tfidf)

# NFOLDS = 4
star = review$stars
star[which(star<3)] = 0
star[which(star>2)] = 1
star = as.factor(star)
# glmnet_classifier = glmnet(x = dtm, y = as.factor(review$stars), 
#                            family = 'binomial', type.multinomial = "grouped")

t1 = Sys.time()
cv.lasso = cv.glmnet(x = dtm, y = star, family = "binomial")
# cv.lasso_tfidf = cv.glmnet(x = dtm_tfidf, y = star, family = "binomial")

# Fit the final model on the training data
lasso.glm = glmnet(x = dtm, y = star, family = "binomial", lambda = cv.lasso$lambda.min)
# lasso.glm_tfidf = glmnet(x = dtm_tfidf, y = star, family = "binomial", lambda = cv.lasso_tfidf$lambda.min)

print(difftime(Sys.time(), t1, units = 'min'))


# Obtain the coefficients
tmp_coeffs = coef(lasso.glm, s = "lambda.min")
lasso_coefficients = data.frame(word = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1],
                                coef = tmp_coeffs@x)


#### Check significance
library(selectiveInference)
x = as.matrix(dtm)
rownames(x) = dtm@Dimnames[[1]]
colnames(x) = dtm@Dimnames[[2]]
xx = unique(x, MARGIN=2)
w = which(!(colnames(x) %in% colnames(xx)))
colnames(x)[w]    # deleted names

# refit the model
t1 = Sys.time()
cv.lasso.refit = cv.glmnet(x = dtm[,-w], y = star, family = "binomial")
lasso.glm.refit = glmnet(x = dtm[,-w], y = star, family = "binomial", lambda = cv.lasso.refit$lambda.min)
print(difftime(Sys.time(), t1, units = 'min'))

re_coeffs = coef(lasso.glm.refit, s = "lambda.min")
re_coefficients = data.frame(word = re_coeffs@Dimnames[[1]][re_coeffs@i + 1],
                             coef = re_coeffs@x)

xxx = as.matrix(dtm[,-w])
rownames(xxx) = dtm@Dimnames[[1]]
colnames(xxx) = dtm@Dimnames[[2]][-w]      # 33262 x 15161

# don't need to try this line
significance = fixedLassoInf(xxx, as.numeric(star) - 1, 
                             beta = coef(lasso.glm.refit, s = "lambda.min"), 
                             lambda = lasso.glm.refit$lambda, family = "binomial") # need memory


load("sig.RData")
pname = names(significance$vars)[significance$pv < 0.1] 

# check the influence
res = re_coefficients[re_coefficients$word %in% pname,]
res = res[order(res$coef),]
tail(res)
head(res)





# (s = glmnet_classifier$lambda[length(glmnet_classifier$lambda)])
# 
# summary(glmnet_classifier)
# 
# coeff = coef(glmnet_classifier, s=s)
# coefficient = coeff %>% lapply(as.matrix) %>%
#                         Reduce(cbind, x = .) %>%  t() # 5 x 43639
# 
# length(coeff$`1`)
# length(colnames(dtm))
# coeff$`1`[1]                           # category 1 - Intercept
# inf_1 = coeff$`1`[-1]-coeff$`1`[1]     
# colnames(dtm)[order(inf_1, decreasing = T)[c(1:50)]]   # positive words
# which(grepl("EEK", review_text, fixed = T))
# review_text[1537]
# colnames(dtm)[order(inf_1, decreasing = F)[c(1:50)]]   # negative words
# 
# 
# coeff[[2]]@x - coeff[[1]]@x
# 
# 
# rownames(cf.glmnet2) <- names(cf.glmnet)
# colnames(cf.glmnet2) <- colnames(cf.nnet)
# w = which(coeff$`1`!=0)
# coefficient = coeff$`1`[w]
# 
# plot(glmnet_classifier, xvar = "dev", label = TRUE)
# plot(glmnet_classifier, xvar = "lambda", label = TRUE, type.coef = "2norm")

