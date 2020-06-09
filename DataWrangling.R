## Load necessary libraries
library(dplyr)
library(tidyr)
library(reshape2)
library(reshape)
library(purrr)

## Set work directory
getwd()
setwd(
  'C:/Users/Lasha/Desktop/Lasha/Education/`UW DS/II Semester/Advanced_R/ProjectMaterials/Project'
)
setwd("/Users/z/Documents/Github repo/Adv_R_Project/R-Shiny-App-Graduate-Employment-Singapore")

##########################################
####        Employment dataset        ####
##########################################
## Read employment data
data_e <-
  read.csv('data/employment_data.csv', stringsAsFactors = FALSE)

## Check and remove NA. In the dataset we have 'na',
## let's change that with NA and then remove it
data_e[data_e == "na"] <- NA
table(is.na(data_e))
sapply(data_e, function(x)
  sum(is.na(x)))

# Singapore University of Technology and Design have empty School column
# we will use the information provided in the degree colum to fill the NAs
# Fill in missing school names with names in degree

SUTD <- data_e %>%
  filter(is.na(school)) %>%
  mutate(school = sub("\\).*", "", sub(".*\\(", "", .$degree))) %>%
  mutate(degree = gsub("\\s*\\([^\\)]+\\)", "", .$degree))
# Remove the original rows, and update them with the cleaned ones
data_e[data_e$university == "Singapore University of Technology and Design", ] <-
  NA
data_e <- rbind(data_e, SUTD)
data_e <- drop_na(data_e)

# since all schools in SMU are labeled with (4 year program)
# we will remove them using regex
SMU <- data_e %>%
  filter(university == "Singapore Management University") %>%
  mutate(school = gsub("\\s*\\([^\\)]+\\)", "", .$school))
# Remove the original rows, and update them with the cleaned ones
data_e[data_e$university == "Singapore Management University", ] <-
  NA
data_e <- rbind(data_e, SMU)
data_e <- drop_na(data_e)

# Convert college name College of Business (Nanyang Business School)
# to Nanyang Business School only
NBS <- data_e %>%
  filter(school == "College of Business (Nanyang Business School)") %>%
  mutate(school = sub("\\).*", "", sub(".*\\(", "", .$school)))
# Remove the original rows, and update them with the cleaned ones
data_e[data_e$school == "College of Business (Nanyang Business School)", ] <-
  NA
data_e <- rbind(data_e, NBS)
data_e <- drop_na(data_e)

# drop unused levels
data_e <- droplevels(data_e)

# Check school names
uni <- unique(data_e$university)
sch_list <- list()

for (u in 1:length(uni)) {
  sch <- unique(data_e[data_e$university == uni[u],]$school)
  sch_list[[u]] <- sch
  
}


# manually correct the school names:

# 1. NTU: Sports Science and Management & Sport Science and Management
data_e[data_e$school == "Sports Science and Management", "school"] <-
  "Sport Science and Management"


# 2. NUS:
data_e[data_e$school == "Faculty Of Dentistry", "school"] <-
  "Faculty of Dentistry"
data_e[data_e$school == "Faculty Of Engineering", "school"] <-
  "Faculty of Engineering"
data_e[data_e$school == "Multidisciplinary Programme", "school"] <-
  "Multidisciplinary Programmes"
data_e[data_e$school == "Multidisciplinary Program", "school"] <-
  "Multidisciplinary Programmes"
data_e[data_e$school == "Multi-Disciplinary Programme", "school"] <-
  "Multidisciplinary Programmes"
data_e[data_e$school == "YST Conservatory Of Music", "school"] <-
  "Yong Siew Toh Conservatory of Music"
data_e[data_e$school == "Yong Loo Lin School (Medicine)", "school"] <-
  "YLL School of Medicine"
data_e[data_e$school == "School of Design and Environment", "school"] <-
  "School of Design & Environment"

# 3. SIT

data_e[data_e$school == "Singapore Institute of Technology (SIT)", "school"] <-
  "Singapore Institute of Technology"
data_e[data_e$school == "Singapore Institute of Technology -Trinity College Dublin", "school"] <-
  "Trinity College Dublin"
data_e[data_e$school == "Singapore Institute of Technology -Trinity College Dublin / Trinity College Dublin", "school"] <-
  "Trinity College Dublin"
data_e[data_e$school == "SIT-Trinity College Dublin / Trinity College Dublin", "school"] <-
  "Trinity College Dublin"
data_e[data_e$school == "SIT-University of Glasgow", "school"] <-
  "University of Glasgow"
data_e[data_e$school == "Trinity College Dublin / Singapore Institute of Technology-Trinity College Dublin", "school"] <-
  "Trinity College Dublin"


# 4. SMU

data_e[data_e$university == "Singapore Management University",]$degree <-
  sub("[[:blank:]]\\([[:digit:]].*\\)", "", data_e[data_e$university == "Singapore Management University",]$degree)
data_e[data_e$university == "Singapore Management University",]$degree <-
  as.character(sub("\\)", "", sub("\\(", "", data_e[data_e$university == "Singapore Management University",]$degree)))

# update the levels again
data_e <- droplevels(data_e)

# check the na count again
sapply(data_e, function(x)
  sum(is.na(x)))

## Transform factor variables into numeric
data_e <- data_e %>% modify_at(c(5:12), as.numeric)
## Check data
str(data_e)
## Save as .rds extension for Shiny
saveRDS(data_e, file = "data/employment_data.rds")
data_e <- readRDS("data/employment_data.rds")
str(data_e)

# write the output
write.csv(data_e, "data/employment_data1.csv", row.names = FALSE)


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
data_g[data_g == "-"] <- NA
## Check NAs
table(is.na(data_g))
sapply(data_g, function(x)
  sum(is.na(x)))
## Remove NA
data_g <- drop_na(data_g)
sapply(data_g, function(x)
  sum(is.na(x)))
## Transform value column from character into numeric variable
data_g <- data_g %>% modify_at(4, as.numeric)
str(data_g)
## Let's change the variable with "university" & value with "graduates"
colnames(data_g)[3:4] <- c("university", "graduates")
colnames(data_g)
## check a list of universities and number of graduates in 2018
check_university <- data_g %>%
  filter(year == "2018") %>%
  group_by(university) %>%
  tally(graduates)
head(check_university)
## Let's correctly name of the universities to make it easily readable
## Get the titles of universities
levels(data_g$university)
## Set new titles of universities
levels(data_g$university) <-
  c(
    "National University of Singapore",
    "Nanyang Technological University",
    "Singapore Management University",
    "Singapore Institute of Technology",
    "Singapore University of Technology and Design",
    "Singapore University of Social Sciences",
    "National Institute of Education",
    "Singapore Politechnic",
    "Ngee Ann Polytechnic",
    "Temasek Polytechnic",
    "Nanyang Polytechnic",
    "Republic Polytechnic",
    "Lasalle Diploma",
    "Lasalle Degree",
    "Nafa Diploma",
    "Nafa Degree",
    "Institute of Technical Education"
  )

levels(data_g$university)
## Save as .rds extension for Shiny
saveRDS(data_g, file = "data/graduates_by_institutions.rds")
