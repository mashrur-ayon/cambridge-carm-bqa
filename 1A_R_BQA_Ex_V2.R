######################
##Task 1#########
#################
#Load the dataset

load("BQA_Lab.Rdata")

BQA_Lab_V2 <- BQA_Lab

save(BQA_Lab_V2, file = "BQA_Lab_V2.Rdata")

####################################
#Install and load necessary package#
####################################
#install.packages(c("......)) ---- if you want to install in group

install.packages("tidyverse")
install.packages("haven")
install.packages("labelled")
install.packages("summarytools")
install.packages("DescTools")
install.packages("janitor")
install.packages("forcats")
###################################
library(tidyverse)
#library(dplyr) 
library(haven) 
library(labelled)
library(summarytools)
library(DescTools)
library(janitor)
#library(forcats) 

##################################
##Load and view data#############
################################

data <- BQA_Lab

####################
##Sort data by age##
####################


data <- data %>% arrange(AGE)

View(data)

#####################
#Sort data by Marstat##
#######################

data <- data %>% arrange(MARSTAT)

View(data)

######################
#Sort Age by descending order
#############################

data <- data %>% arrange(desc(AGE))

View(data)


#################################
##Sorting by Multiple Variables##
#################################

data <- data %>% arrange(MARSTAT, AGE)

View(data)

###############################
#Change order of columns#######
###############################

data <- data %>% select(MARSTAT, AGE, everything())
View(data)

######################################################
####Group By Command#################################
#####################################################



data %>%
  group_by(MARSTAT) %>%
  summarise(avg_age = mean(AGE, na.rm = TRUE))

ungroup(data)

###################################################


data %>%
  group_by(MARSTAT) %>%
  summarise(avg_age = mean(AGE, na.rm = TRUE))%>% 
  ungroup()


####################################################

data %>%
  group_by(female) %>%
  summarise(avg_LifeSat = mean(LifeSat, na.rm = TRUE))%>% 
  ungroup()

###########################################################

##########################################################
###Collapsing Factor Variable#############################
##########################################################

#Check the existing label
levels(data$pplfair)
tabyl(data$pplfair) 

#First need to deal with missing data
data <- data %>%
  mutate(pplfair_recode = na_if(pplfair, "Refusal")) %>%
  mutate(pplfair_recode = na_if(pplfair_recode, "Don't know")) %>%
  mutate(pplfair_recode = na_if(pplfair_recode, "No answer")) %>%
  mutate(pplfair_recode = droplevels(pplfair_recode))   

########################################

#Recode the new variable
data <- data %>%
  mutate(pplfair_recode = pplfair_recode %>%
           fct_collapse(
             "Try to take advantage" = c("Most people try to take advantage of me", "1", "2", "3", "4"),
             "Neither" = "5",
             "Try to be fair" = c("6", "7", "8", "9", "Most people try to be fair")) )


#Label the new variable
attr(data$pplfair_recode, "label") <- "People try to be fair"

#Verify the changes
attributes(data$pplfair_recode)

tabyl(data$pplfair_recode)


####


#_____________________________________________________________________
#_____________________________________________________________________
#_practice 'wkvlorg' ----

#Check the existing label
levels(data$wkvlorg)

#First need to deal with missing data
data <- data %>%
  mutate(wkvlorg_recode = na_if(wkvlorg, "Refusal")) %>%
  mutate(wkvlorg_recode = na_if(wkvlorg_recode, "Don't know")) %>%
  mutate(wkvlorg_recode = na_if(wkvlorg_recode, "No answer")) %>%
  mutate(wkvlorg_recode = droplevels(wkvlorg_recode))


#Recode the new variable
data <- data %>%
  mutate(wkvlorg_recode = wkvlorg_recode %>%
           fct_collapse(
             "Never" = c("Never"),
             "Rarely" = c("Less often", "At least once every six months"),
             "Occasionally" = c("At least once every three months", "At least once a month"),
             "Frequently" = c("At least once a week")))


#Label the new variable
attr(data$wkvlorg_recode, "label") <- "Frequency of volunteering in past 12 months"             

#Verify the changes
attributes(data$wkvlorg_recode)

tabyl(data$wkvlorg_recode)

#1.6 RENAMING FACTOR LEVELS ####
#_____________________________________________________________________
#_____________________________________________________________________
#_example 'pplfair_recode' ----
#check levels
levels(data$pplfair_recode)

#Rename levels
data <- data %>%
  mutate(pplfair_recode = fct_recode(pplfair_recode,
                                     "Advantage" = "Try to take advantage",
                                     "Neutral" = "Neither",
                                     "Fair" = "Try to be fair"))
#Check new levels
levels(data$pplfair_recode)
#_____________________________________________________________________
#_____________________________________________________________________
#_practice 'economic_status2' ----
#Use code from FiAS to generate the variable
data <- data %>%
  mutate(economic_status2 = fct_recode(mnactic,
                                       "Unemployed" = "Unemployed, looking for job",
                                       "Unemployed" = "Unemployed, not looking for job",
                                       "Economically inactive" = "Permanently sick or disabled",
                                       "Economically inactive" = "Retired",
                                       "Economically inactive" = "Community or military service",
                                       "Economically inactive" = "Housework, looking after children, others",
                                       "Economically inactive" = "Other",
                                       NULL = "Not applicable",
                                       NULL = "Refusal",
                                       NULL = "Don't know",
                                       NULL = "No answer"
  ))


#check levels
levels(data$economic_status2)

#Rename levels
data <- data %>%
  mutate(economic_status2 = fct_recode(economic_status2,
                                       "Working" = "Paid work",
                                       "Student" = "Education",
                                       "Job seeker" = "Unemployed",
                                       "Not in labor force" = "Economically inactive"))


#check new levels
levels(data$economic_status2)


#1.8 USING LOGICAL OPERATORS AND A SINGLE VARIABLE TO CREATE A NEW VARIABLE ####
#_ifelse ----
#Create character variable
data <- data %>%
  mutate(adult = ifelse(AGE >= 18, "Adult", "Not Adult"))

#Create binary factor variable
data <- data %>% 
  mutate(adult = ifelse(AGE >= 18, "Adult", "Not Adult"), 
         adult = factor(adult, levels = c("Not Adult","Adult")))

#_case_when ----
#Create factor variable
data <- data %>%
  mutate(age_group = case_when(
    AGE < 18 ~ "Child",
    AGE >= 18 & AGE <= 64 ~ "Adult",
    AGE > 64 ~ "Senior",
    TRUE ~ NA_character_
  ) %>% factor(levels = c("Child", "Adult", "Senior")))

#_!is.na ----
#Using !is.na within another function 
data %>%
  filter(!is.na(AnnInc)) %>%  
  select(AGE, LifeSat ) %>% 
  descr()

#Using !is.na when creating new variables
data <- data %>%
  mutate(status = case_when(
    is.na(AGE) ~ "Missing Age",
    AGE >= 18 ~ "Adult",
    TRUE ~ "Child"
  ) %>% factor(levels = c("Child", "Adult", "Missing Age")))

#1.9 CREATING NEW VARIABLES USING MULTIPLE VARIABLES AND CONDITIONS ####

data <- data %>% 
  mutate(oldman = case_when( 
    is.na(gndr) | is.na(AGE) ~ NA_character_,  
    gndr == "Male" & AGE >= 70 ~ "Old Man",
    TRUE ~ "Not Old Man" ) %>%
      factor(levels = c("Old Man", "Not Old Man")))

data %>% 
  tabyl(oldman, gndr)

descr(data$AGE[data$oldman == "Old Man"])

descr(data$AGE[data$oldman == "Not Old Man"])

data %>%
  group_by(oldman) %>%
  summarise(
    avg_age = mean(AGE, na.rm = TRUE),
    min_age = min(AGE),
    max_age = max(AGE),
    n=n()
  ) %>%
  ungroup()

#1.10 PRACTICE EXERCISE ####
#_____________________________________________________________________
#____________________________________________________________________

# Create the 'happyparent' variable
data <- data %>%
  mutate(
    happyparent = case_when(
      # Handle missing values for either children or happiness
      chldhm %in% c("Not available") |
        happy %in% c("Refusal", "Don't know", "No answer") ~ NA_character_,
      
      # Happy parent: lives with children and reports happiness of 9 or 10
      chldhm == "Respondent lives with children at household grid" &
        (happy == "9" | happy == "Extremely happy") ~ "YES",
      
      # Not a happy parent: lives with children but does not report high happiness
      chldhm == "Respondent lives with children at household grid" &
        !(happy == "9" | happy == "Extremely happy") ~ "NO",
      
      # Does not live with children
      chldhm == "Does not" ~ "NO",
      
      TRUE ~ NA_character_  # Catch any other remaining missing values
    ) %>% factor(levels = c("NO", "YES"))
  )


#To check 4875 happy parents and 402 missing
data %>%
  tabyl(happyparent)

#To check respondents are correctly categorised based on whether they live with children
data %>%
  tabyl(happyparent, chldhm)

#To check respondents are correctly categorised based on their happiness level
data %>%
  tabyl(happy, happyparent)

