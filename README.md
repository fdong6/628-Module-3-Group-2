# 628-Module-3-Group 2
#
# Summary
  Our project focuses on restaurants serving fast food. We aim to provide advice to the restaurants based on our analysis in four categories: food, service, wating time, and sanitary conditions. Please refer to our report Executive Summary_Module 3.pdf for more details. Our presentation is saved in Module 3 Presentation_Group 2.mp4. 
#
# Codes 
This folder contains serveral R codes. Subset.R conducts data extraction and data cleaning, resulting in 628.Rdata.
module3_plots.R makes plots of the selected group of restaurants.
Multinomial Coefficient.R conducts NLP and Lasso regression and estimates p-value of estimated parameters from Lasso.
#
# Image
This folder includes plots of frequency of top 20 words appearing in all reviews, reviews with 1 star, 2 stars, 3 stars, 4, stars, and 5 stars, respectively. It also includes a histogram plot for review stars. In addtion, a table of regression estimates from Lasso is listed. This table only includes variables directly linked to the four targeted categories. Variables that are not useful to providing recommendations are excluded. Please note that the results of calculating the significance of parameters in Lasso are saved in sig.Rdata, which is too big to upload. 
#
# www
This folder contains app.R code for creating the Shiny app. It also contains several images used in the programing.

