## Load necessary libraries
library(dplyr)
library(tidyr)
library(reshape2)
library(reshape)
library(purrr)

## Set work directory
getwd()
setwd('C:/Users/Lasha/Desktop/Lasha/Education/`UW DS/II Semester/Advanced_R/ProjectMaterials/Project')

##########################################
####        Employment dataset        ####
##########################################
## Read employment data
data_e <- read.csv('data/employment_data.csv')
## Check and remove NA. In the dataset we have 'na',
## let's change that with NA and then remove it
data_e[data_e=="na"]<- NA
table(is.na(data_e))
sapply(data_e, function(x) sum(is.na(x)))
data_e <- drop_na(data_e)
## Transform factor variables into numeric
data_e <- data_e %>% modify_at(c(5:12), as.numeric)
## Check data
str(data_e)
## Save as .rds extension for Shiny
saveRDS(data_e, file = "data/employment_data.rds")
data_e <- readRDS("data/employment_data.rds")
str(data_e)


##########################################
####        Graduates dataset         ####
##########################################
## Read graduate data
data_g <- read.csv('data/graduates_by_institutions.csv')
head(data_g)
str(data_g)
## We can see that data contains each university in columns
## We can easily melt it to have university name in one column
data_g <- melt(data_g, id = c("year", "sex"))
str(data_g)
## Also, we have "-" special character in the values
## Let's replace that with "NA" and then remove from data
data_g[data_g=="-"]<- NA
## Check NAs
table(is.na(data_g))
sapply(data_g, function(x) sum(is.na(x)))
## Remove NA
data_g <- drop_na(data_g)
sapply(data_g, function(x) sum(is.na(x)))
## Transform value column from character into numeric variable
data_g <- data_g %>% modify_at(4, as.numeric)
str(data_g)
## Let's change the variable with "university" & value with "graduates"
colnames(data_g)[3:4] <- c("university", "graduates")
colnames(data_g)
## check a list of universities and number of graduates in 2018
check_university <- data_g %>%
  filter(year=="2018") %>% 
  group_by(university) %>% 
  tally(graduates)
head(check_university)
## Let's correctly name of the universities to make it easily readable
## Get the titles of universities
levels(data_g$university)
## Set new titles of universities
levels(data_g$university) <- c("National University of Singapore", "Nanyang Technological University",
                               "Singapore Management University", "Singapore Institute of Technology", 
                               "Singapore University of Technology and Design", 
                               "Singapore University of Social Sciences", "National Institute of Education",
                               "Singapore Politechnic", "Ngee Ann Polytechnic", "Temasek Polytechnic",
                               "Nanyang Polytechnic", "Republic Polytechnic", 
                               "Lasalle Diploma", "Lasalle Degree", "Nafa Diploma",
                               "Nafa Degree", "Institute of Technical Education")

levels(data_g$university)
## Save as .rds extension for Shiny
saveRDS(data_g, file = "data/graduates_by_institutions.rds")

