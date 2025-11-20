#************************************
#***** BQA - EXERCISE 4-7 *****
#************************************



#*********************************************************************
#4.1 - SET UP SCRIPT FILE AND OPEN DATA ####
#*********************************************************************
# Load required libraries
library(tidyverse)
library(haven) # For working with data files
library(labelled) #to work with labelled data
library(summarytools) #includes ctable which has certain useful table features
library(psych) #for correlation matrix
library(sjPlot) #formatted output from regressions 
library(emmeans) #for pairwise comparison of means after ANOVA
library(car) #for pairwise comparison after OLS
library(ggpubr) #for boxplots 

#these packages help with formatting of output:
library(broom)
library(gt)
library(stargazer)

# Set as working directory

setwd("~/GitHub/cambridge-carm-bqa")

#opens the newly created version of the data set
load("BQA_Lab_V2.RData")	 

#loads it into the dataframe called 'data' which makes it easier to call for all the rest of the commands
data<-BQA_Lab_V2


#	********************************************************************
#********************************************************************
# 4.2 PREPARING DATA ####
#********************************************************************
#Identify all variables relating to trust
look_for(data, "trust")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Examine trust variables using list
data %>% 
  select(ppltrst, trstprl, trstlgl, trstplc, trstplt, trstprt, trstep,   
         trstun) %>%
  dfSummary() %>%
  view()

#More efficient syntax to examine trust variables
data %>% 
  select(contains("trst")) %>% 
  dfSummary() %>% 
  view()

#too many levels to be displayed with default, so add argument to show more 
data %>% 
  select(contains("trst")) %>% 
  dfSummary(max.distinct.values = Inf) %>% 
  view()


#Need to recode the missing values, could do one variable at a time....
data <- data %>%
  mutate(pt_recode = na_if(ppltrst, "Refusal")) %>%
  mutate(pt_recode = na_if(pt_recode, "Don't know")) %>%
  mutate(pt_recode = na_if(pt_recode, "No answer")) %>%
  mutate(pt_recode = droplevels(pt_recode))   

#Use across() from dplyr to do all at once
#Calling these varname2 
data <- data %>%
  mutate(across(contains("trst"), ~ .x %>%
                  na_if("Refusal") %>%
                  na_if("Don't know") %>%
                  na_if("No answer") %>%
                  droplevels(), 
                .names = "{.col}2"))

#Convert new variables to numeric, don't need to generate another one as we still have the original variable, so can write over the _recode version
data <- data %>%
  mutate(pt_recode = as.numeric(pt_recode))

#Compare original, recoded but not numeric, recoded and numeric
data %>% 
  select(ppltrst, ppltrst2, pt_recode) %>% 
  dfSummary(max.distinct.values = Inf) %>% 
  view()

#Convert to numeric all at once
data <- data %>%
  mutate(across(contains("trst") & ends_with("2"), ~ as.numeric(.)))

#Check recoding
data %>% 
  select(contains("trst")) %>% 
  dfSummary(max.distinct.values = Inf) %>% 
  view()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Bonus - it is possible to convert multiple variables using a single command 
data <- data %>%
  mutate(across(c(IMPRICH, IMPCREATE, LS, GETBY), 
                ~ as.numeric(as.character(.)),
                .names = "{.col}2"))

#___check conversion 
#Check to see they have been correctly changed
data %>% 
  select(IMPRICH2, IMPCREATE2, LS2, GETBY2) %>% 
  dfSummary() %>% 
  view()


#********************************************************************
# 4.3 PRODUCING CORRELATION MATRIX ####
#********************************************************************

#psych package displays as 3 tables
# use = pairwise does each correlation using all available observations for that pair
# use = complete checks which observations have values for ALL variables, then does correlation tests only on that sample
#adjust=none because we do not want to adjust for multiple tests
#Correlation matrix with complete observations
data %>%
  select(contains("trst") & ends_with("2")) %>%
  corr.test(use = "complete", adjust = "none") 

#Pairwise correlation matrix
data %>%
  select(contains("trst") & ends_with("2")) %>%
  corr.test(use = "pairwise", adjust = "none") 

#For now we focus on pairwise
#sjplot displays nicely formatted table
data %>%
  select(contains("trst") & ends_with("2")) %>%
  tab_corr(triangle = "lower", 
           na.deletion = "pairwise", 
           show.p = TRUE, 
           p.numeric= TRUE,
           digits = 3)


#********************************************************************
# 4.4 CORRELATION COEFFICIENT AND EFFECT SIZE ####
#********************************************************************
#*Standardize the variables
#Govsat
data <- data %>%
  mutate(stdGovSat = scale(GovSat))

#progressive
data <- data %>%
  mutate(stdprogressive = scale(progressive))

#Correlation matrix unstandardised
data %>%
  select(GovSat, progressive) %>%
  cor(use = "complete.obs") 

#Correlation matrix standardised
data %>%
  select(stdGovSat, stdprogressive) %>%
  cor(use = "complete.obs") 

#Run a linear regression:
data %>%
  lm(GovSat ~ progressive, data = .) %>%
  summary()

#Run a linear regression for standardised values:
data %>%
  lm(stdGovSat ~ stdprogressive, data = .) %>%
  summary()

#*Standardize the variables omitting observations for which the other variable is missing
#*#Govsat
data <- data %>%
  mutate(
    mean_GovSat = mean(GovSat[!is.na(progressive)], na.rm = TRUE),
    sd_GovSat = sd(GovSat[!is.na(progressive)], na.rm = TRUE),
    stdGovSat2 = ifelse(
      !is.na(progressive), 
      (GovSat - mean_GovSat) / sd_GovSat, 
      NA
    )
  ) %>%
  select(-mean_GovSat, -sd_GovSat)  # Remove intermediate variables

#progressive
data <- data %>%
  mutate(
    mean_progressive = mean(progressive[!is.na(GovSat)], na.rm = TRUE),
    sd_progressive = sd(progressive[!is.na(GovSat)], na.rm = TRUE),
    stdprogressive2 = ifelse(
      !is.na(GovSat), 
      (progressive - mean_progressive) / sd_progressive, 
      NA
    )
  ) %>%
  select(-mean_progressive, -sd_progressive)  # Remove intermediate variables



#Correlation matrix standardised
data %>%
  select(stdGovSat2, stdprogressive2) %>%
  cor(use = "complete.obs") 

#Run a linear regression:
data %>%
  lm(stdGovSat2 ~ stdprogressive2, data = .) %>%
  summary()

#is displaying scientific notation, can change options to see without scientific notation 
# Temporarily set scipen to avoid scientific notation, scipen controls when R displays scientific notation, by setting it arbitrarily high, R will default to non-scientific notation
options(scipen = 999)

# Run the regression
data %>%
  lm(stdGovSat2 ~ stdprogressive2, data = .) %>%
  summary()

# You can reset scipen to default value
options(scipen = 0)


#*******************************************************************
#*******************************************************************
#*******************************************************************
#*******************************************************************
#*******************************************************************
#*******************************************************************




#*******************************************************************
# 5.1 ONE-SAMPLE T-TEST ####
#******************************************************************

# One-sample t-test comparing LifeSat to a mean of 59.5
#two-sided is default
t.test(data$LifeSat, mu = 59.5)

#can check one sided using alternative =
t.test(data$LifeSat, mu = 59.5, alternative =  "less")
t.test(data$LifeSat, mu = 59.5, alternative =  "greater")

#To display results in standard decimal format use scipen and then re-run commands
options(scipen = 999)

# One-sample t-test comparing LifeSat to a mean of 59.5
#two-sided is default
t.test(data$LifeSat, mu = 59.5)

#can check one sided using alternative =
t.test(data$LifeSat, mu = 59.5, alternative =  "less")
t.test(data$LifeSat, mu = 59.5, alternative =  "greater")


#*******************************************************************
# 5.2 TWO-SAMPLE T-TEST ####
#*******************************************************************
# Two-sample t-test for LifeSat by female
t.test(LifeSat ~ female, data = data,  var.equal = TRUE, na.action = na.omit)

#Graph to get visualisation 
data %>% 
  filter(!is.na(female)) %>% 
  ggboxplot(., x = "female", y = "LifeSat", color = "female",)


# Two-sample t-test for LifeSat by female, for Spain (ES)
data %>%
  filter(COUNTRY == "Spain") %>%
  t.test(LifeSat ~ female, data = ., var.equal = TRUE, na.action = na.omit)

# Two-sample t-test for LifeSat by female, for Great Britain (GB)
data %>%
  filter(COUNTRY == "Gr. Britain") %>%
  t.test(LifeSat ~ female, data = ., var.equal = TRUE, na.action = na.omit)

# Two-sample t-test for progressive by female
data %>%
  t.test(progressive ~ female, data = ., var.equal = TRUE, na.action = na.omit)


# Two-sample t-test for progressive by ed_d, grouped by country
data %>%
  group_by(COUNTRY) %>%
  do(tidy(t.test(progressive ~ ed_deg, data = ., var.equal = TRUE, na.action = na.omit))) %>% 
  print(n=Inf)

#output is  ugly, advanced coding gives us:

# Perform t-test, tidy the results, rename columns for better presentation, and format using gt
data %>%
  group_by(COUNTRY) %>%
  do(tidy(t.test(progressive ~ ed_deg, data = ., var.equal = TRUE, na.action = na.omit))) %>%
  rename(
    `Mean Difference` = estimate,
    `Group 1 Mean` = estimate1,
    `Group 2 Mean` = estimate2,
    `t-statistic` = statistic,
    `p-value` = p.value,
    `Degrees of Freedom` = parameter,
    `Conf Low` = conf.low,
    `Conf High` = conf.high
  ) %>%
  gt() %>%
  tab_header(title = "T-Test Results by Country") %>%
  fmt_number(columns = c(`Mean Difference`, `Group 1 Mean`, `Group 2 Mean`, `t-statistic`, `p-value`, `Conf Low`, `Conf High`), decimals = 3)

#*******************************************************************
#*******************************************************************
#*******************************************************************
#*******************************************************************
#*******************************************************************
#*******************************************************************
#*******************************************************************


# 6. ANOVA ####
#*******************************************************************

# Create nadults2 for nadults between 1 and 6
data <- data %>%
  mutate(nadults2 = ifelse(nadults >= 1 & nadults <= 6, nadults, NA))

# Calculate group means
data %>%
  filter(!is.na(nadults2)) %>% 
  group_by(nadults2) %>%
  summarise(
    Group_Mean = mean(LifeSat),
    Group_sd = sd(LifeSat),
    Count = n() )

#Visualise using boxplots
data %>% 
  filter(!is.na(nadults2)) %>% 
  ggboxplot(., x = "nadults2", y = "LifeSat", color = "nadults2",)


# ANOVA for LifeSat by nadults2
data %>%
  aov(LifeSat ~ as.factor(nadults2), data = .) %>%
  summary()


# Pairwise means for LifeSat over nadults2
data %>%
  aov(LifeSat ~ as.factor(nadults2), data = .) %>%
  emmeans(pairwise ~ nadults2, adjust = "none") %>%
  .$contrasts %>%
  summary(infer = c(TRUE, TRUE))  # Display both p-values and confidence intervals without adjustment

#*******************************************************************
#*******************************************************************
#*******************************************************************
#*******************************************************************
#*******************************************************************
#*******************************************************************

