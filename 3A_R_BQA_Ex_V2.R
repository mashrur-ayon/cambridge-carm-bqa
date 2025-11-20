#************************************
#***** BQA - EXERCISE 3 *****
#************************************
#	***********************************************************************
#	***********************************************************************
#********************************************************************
#3.1 - SET UP SCRIPT FILE AND OPEN DATA ####
#********************************************************************
# Load required libraries
library(tidyverse)
library(haven) # For working with data files
library(labelled) #to work with labelled data
library(summarytools) #includes ctable which has certain useful table features
library(sjPlot) #nicely formatted output
library(stargazer) #nicely formatted output

# Set the working directory

setwd("~/GitHub/cambridge-carm-bqa")

#opens the newly created version of the data set
load("BQA_Lab_V2.RData")	 

#loads it into the dataframe called 'data' which makes it easier to call for all the rest of the commands
data<-BQA_Lab_V2


#	********************************************************************
#	********************************************************************
#********************************************************************
# 3.2 SCATTERPLOTS ####
#********************************************************************

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# _Simple scatterplots ----

data %>% 
  filter(complete.cases(AGE, GovSat)) %>% 
  ggplot(aes(x = AGE, y = GovSat)) + 
  geom_point() +
  ggtitle("Scatterplot of Government Satisfaction by Age")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#_Derived variables ----
#Check all variables to see that there are only 24 observations
data %>% 
  select( IMPRICH, IMPCREATE, LS, GETBY) %>% 
  dfSummary() %>% 
  view()

#___converting to numeric ----
#IMPRICH, IMPCREATE, LS and GETBY are stored as factors, convert to numeric

#Convert each variable
data <- data %>%
  mutate(IMPRICH2 = as.numeric(as.character(IMPRICH)))

data <- data %>%
  mutate(IMPCREATE2 = as.numeric(as.character(IMPCREATE)))

data <- data %>%
  mutate(LS2 = as.numeric(as.character(LS)))

data <- data %>%
  mutate(GETBY2 = as.numeric(as.character(GETBY)))


#___check conversion ----
#Check to see they have been correctly changed
data %>% 
  select(IMPRICH2, IMPCREATE2, LS2, GETBY2) %>% 
  dfSummary() %>% 
  view()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#_Derived Variable Scatterplots ----

#___no formatting ----
# Scatterplot of IMPRICH vs IMPCREATE, with no formatting
data %>% 
  filter(complete.cases(IMPRICH2, IMPCREATE2)) %>% 
  ggplot(aes(x = IMPRICH2, y = IMPCREATE2)) +
  geom_point() 

#___add labels/titles ----
# Scatterplot of IMPRICH vs IMPCREATE, with country labels and title
data %>% 
  filter(complete.cases(IMPRICH2, IMPCREATE2)) %>% 
  ggplot(aes(x = IMPRICH2, y = IMPCREATE2)) +
  geom_point() +
  geom_text(aes(label = COUNTRY)) +
  ggtitle("Scatterplot of Importance of Wealth vs Creativity") +
  scale_x_continuous(name = "Important to be rich - country means") +
  scale_y_continuous(name = "Important to be creative - country means") 

#___move labels ----
# Scatterplot of IMPRICH vs IMPCREATE, shift country labels up
data %>% 
  filter(complete.cases(IMPRICH2, IMPCREATE2)) %>% 
  ggplot(aes(x = IMPRICH2, y = IMPCREATE2)) +
  geom_point() +
  geom_text(aes(label = COUNTRY),  vjust = -1) +
  ggtitle("Scatterplot of Importance of Wealth vs Creativity") +
  scale_x_continuous(name = "Important to be rich - country means") +
  scale_y_continuous(name = "Important to be creative - country means") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#_Exercise ----
#___IMPRICH vs LS ----
#Scatterplot of IMPRICH vs LS, formatted 
data %>% 
  filter(complete.cases(IMPRICH2, LS2)) %>% 
  ggplot(aes(x = IMPRICH2, y = LS2)) +
  geom_point() +
  geom_text(aes(label = COUNTRY),  vjust = -1) +
  ggtitle("Scatterplot of Importance of Wealth vs Life Satisfaction") +
  scale_x_continuous(name = "Important to be rich - country means") +
  scale_y_continuous(name = "Life Satisfaction - country means") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#___IMPRICH vs GETBY ----
#Scatterplot of IMPRICH vs GETBY, formatted 
data %>% 
  filter(complete.cases(IMPRICH2, GETBY2)) %>% 
  ggplot(aes(x = IMPRICH2, y = GETBY2)) +
  geom_point() +
  geom_text(aes(label = COUNTRY),  vjust = -1) +
  ggtitle("Scatterplot of Importance of Wealth vs How easy it is to get by on income") +
  scale_x_continuous(name = "Important to be rich - country means") +
  scale_y_continuous(name = "How easily can you get by on income - country means") 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#___IMPCREATE vs LS ----
#Scatterplot of IMPCREATE vs LS, formatted 

data %>% 
  filter(complete.cases(IMPCREATE2, LS2)) %>% 
  ggplot(aes(x = IMPCREATE2, y = LS2)) +
  geom_point() +
  geom_text(aes(label = COUNTRY),  vjust = -1) +
  ggtitle("Scatterplot of Importance of Creativity vs Life Satisfaction") +
  scale_x_continuous(name = "Important to be creative - country means") +
  scale_y_continuous(name = "Life Satisfaction - country means") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#___IMPCREATE vs GETBY ----
#Scatterplot of IMPCREATE vs GETBY, formatted 
data %>% 
  filter(complete.cases(IMPCREATE2, GETBY2)) %>% 
  ggplot(aes(x = IMPCREATE2, y = GETBY2)) +
  geom_point() +
  geom_text(aes(label = COUNTRY),  vjust = -1) +
  ggtitle("Scatterplot of Importance of Creativity vs How easy it is to get by on income") +
  scale_x_continuous(name = "Important to be creative - country means") +
  scale_y_continuous(name = "How easily can you get by on income - country means") 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#___GETBY vs LS ----

#Scatterplot of GETBY vs LS, formatted 
data %>% 
  filter(complete.cases(GETBY2, LS2)) %>% 
  ggplot(aes(x = GETBY2, y = LS2)) +
  geom_point() +
  geom_text(aes(label = COUNTRY),  vjust = -1) +
  ggtitle("Scatterplot of How easy it is to get by on income vs Life Satisfaction") +
  scale_x_continuous(name = "How easy is it to get by on income - country means") +
  scale_y_continuous(name = "Life Satisfaction - country means") 

#	********************************************************************
#	********************************************************************
#********************************************************************
# 3.3 LINE OF BEST FIT - SCATTERPLOTS ####
#********************************************************************
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#_Add line of best fit ----
data %>% 
  filter(complete.cases(IMPRICH2, IMPCREATE2)) %>% 
  ggplot(aes(x = IMPRICH2, y = IMPCREATE2)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(aes(label = COUNTRY),  vjust = -1) +
  ggtitle("Scatterplot of Importance of Wealth vs Creativity") +
  scale_x_continuous(name = "Important to be rich - country means") +
  scale_y_continuous(name = "Important to be creative - country means") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#_Remove confidence interval ----
data %>% 
  filter(complete.cases(IMPRICH2, IMPCREATE2)) %>% 
  ggplot(aes(x = IMPRICH2, y = IMPCREATE2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = COUNTRY),  vjust = -1) +
  ggtitle("Scatterplot of Importance of Wealth vs Creativity") +
  scale_x_continuous(name = "Important to be rich - country means") +
  scale_y_continuous(name = "Important to be creative - country means") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#_Quadratic line of best fit ----
data %>% 
  filter(complete.cases(IMPRICH2, IMPCREATE2)) %>% 
  ggplot(aes(x = IMPRICH2, y = IMPCREATE2)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  geom_text(aes(label = COUNTRY),  vjust = -1) +
  ggtitle("Scatterplot of Importance of Wealth vs Creativity") +
  scale_x_continuous(name = "Important to be rich - country means") +
  scale_y_continuous(name = "Important to be creative - country means") 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Quadratic line of best fit, no conf. int.
data %>% 
  filter(complete.cases(IMPRICH2, IMPCREATE2)) %>% 
  ggplot(aes(x = IMPRICH2, y = IMPCREATE2)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  geom_text(aes(label = COUNTRY),  vjust = -1) +
  ggtitle("Scatterplot of Importance of Wealth vs Creativity") +
  scale_x_continuous(name = "Important to be rich - country means") +
  scale_y_continuous(name = "Important to be creative - country means") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#_Removing scatter points ----
data %>% 
  filter(complete.cases(IMPRICH2, IMPCREATE2)) %>% 
  ggplot(aes(x = IMPRICH2, y = IMPCREATE2)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  ggtitle("Scatterplot of Importance of Wealth vs Creativity") +
  scale_x_continuous(name = "Important to be rich - country means") +
  scale_y_continuous(name = "Important to be creative - country means")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Going back to GovSat and AGE
data %>% 
  filter(complete.cases(AGE, GovSat)) %>% 
  ggplot(aes(x = AGE, y = GovSat)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Scatterplot of Age vs Satisfaction with Government") +
  scale_x_continuous(name = "Age in Years") +
  scale_y_continuous(name = "Satisfaction with Government") 

#Might want to remove scatter
data %>% 
  filter(complete.cases(AGE, GovSat)) %>% 
  ggplot(aes(x = AGE, y = GovSat)) +
  geom_smooth(method = "lm") +
  ggtitle("Line of Best Fit of Age vs Satisfaction with Government") +
  scale_x_continuous(name = "Age in Years") +
  scale_y_continuous(name = "Satisfaction with Government") 

#ggplot automatically scales the axis, but we can override so the two graphs match
data %>% 
  filter(complete.cases(AGE, GovSat)) %>% 
  ggplot(aes(x = AGE, y = GovSat)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Scatterplot of Age vs Satisfaction with Government") +
  scale_x_continuous(name = "Age in Years", limits = c(15, 103)) +
  scale_y_continuous(name = "Satisfaction with Government", limits = c(4, 106)) 

data %>% 
  filter(complete.cases(AGE, GovSat)) %>% 
  ggplot(aes(x = AGE, y = GovSat)) +
  geom_smooth(method = "lm") +
  ggtitle("Line of Best Fit of Age vs Satisfaction with Government") +
  scale_x_continuous(name = "Age in Years", limits = c(15, 103)) +
  scale_y_continuous(name = "Satisfaction with Government", limits = c(4, 106))


#Focusing on lines of best fit we prefer the automatic range for axes
data %>% 
  filter(complete.cases(AGE, GovSat)) %>% 
  ggplot(aes(x = AGE, y = GovSat)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  ggtitle("Quadratic Line of Best Fit of Age vs Satisfaction with Government") +
  scale_x_continuous(name = "Age in Years") +
  scale_y_continuous(name = "Satisfaction with Government") 

#Both linear and quadratic on same graph
data %>% 
  filter(complete.cases(AGE, GovSat)) %>% 
  ggplot(aes(x = AGE, y = GovSat)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Linear and Quadratic Lines of Best Fit of Age vs Satisfaction with Government") +
  scale_x_continuous(name = "Age in Years") +
  scale_y_continuous(name = "Satisfaction with Government") 


#********************************************************************
# 3.4 CALCULATING SLOPE OF LINE OF BEST FIT ####
#********************************************************************
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Modify syntax for graph to add line of best fit
data %>% 
  filter(complete.cases(GETBY2, LS2)) %>% 
  ggplot(aes(x = GETBY2, y = LS2)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(aes(label = COUNTRY),  vjust = -1) +
  ggtitle("Scatterplot of How easy it is to get by on income vs Life Satisfaction") +
  scale_x_continuous(name = "How easy is it to get by on income - country means") +
  scale_y_continuous(name = "Life Satisfaction - country means") 

#Run a linear regression:
data %>%
  lm(LS2 ~ GETBY2, data = .) %>%
  {list(summary(.), confint(.))}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Try it yourself
data %>% 
  filter(complete.cases(IMPRICH2, IMPCREATE2)) %>% 
  ggplot(aes(x = IMPCREATE2, y = IMPRICH2)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(aes(label = COUNTRY),  vjust = -1) +
  ggtitle("Scatterplot of Importance of Wealth vs Creativity") +
  scale_x_continuous(name = "Importance of Creativity - country means") +
  scale_y_continuous(name = "Importance of wealth - country means") 

#Run a linear regression:
data %>%
  lm(IMPRICH2 ~ IMPCREATE2, data = .) %>%
  {list(summary(.), confint(.))}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Formatted output
#option 1 uses sjplot, doesn't display f-statistic
data %>%
  lm(LS2 ~ GETBY2, data = .) %>%
  tab_model()

#The command below gives more detailed output
data %>%
  lm(LS2 ~ GETBY2, data = .) %>%
  tab_model(
    show.se = TRUE,     # Show standard errors
    show.stat = TRUE,   # Show t-statistics
    show.p = TRUE,      # Show p-values
    show.r2 = TRUE     # Show R-squared
  )

#option 2 - uses stargazer, can display EIHTER CI or SE
data %>%
  lm(LS2 ~ GETBY2, data = .) %>%
  {stargazer(., 
             type = "text",       # Text output in console
             report = "vcstp*",   # Report coeffs, SEs, t-stats,  p-values
             digits = 3           # Set number of decimal places
  )}

data %>%
  lm(LS2 ~ GETBY2, data = .) %>%
  {stargazer(., 
             type = "text",       # Text output in console
             ci = TRUE,           #displays CI
             report = "vcstp*",   # Report coeffs, SEs, t-stats,  p-values
             digits = 3           # Set number of decimal places
  )}

