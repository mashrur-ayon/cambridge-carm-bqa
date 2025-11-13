##########################
#### BQA Exercise 2 ######
##########################


##########################
##Load Required Library###
##########################


library(tidyverse)
library(haven) 
library(labelled) 
library(summarytools)
library(janitor)
library(sjPlot)


###################################
###Cross Tab Marstat and Lonely####
###################################

data %>% 
  tabyl(MARSTAT, LONELY)

#################################
##Fix Empty Levels#########
###########################
data <-data %>% 
  mutate(LONELY2 = droplevels(LONELY))

################################
####Rerunning##################
##############################

data %>% 
  tabyl(MARSTAT, LONELY2)

#####################################
##Omit Missing Values################
#####################################

data %>%
  filter(complete.cases(MARSTAT, LONELY2)) %>%  # Filter out rows with any NA values
  tabyl(MARSTAT, LONELY2)  # Create the cross-tab



############################################
##Adding Rows and Column###################
###########################################

data %>%
  filter(complete.cases(MARSTAT, LONELY2)) %>%  # Filter out rows with any NA values
  tabyl(MARSTAT, LONELY2) %>%   # Create the cross-tab
  adorn_totals(c("row", "col"))

#################################################
####Displaying Percentage Instead of Frequency###
#################################################

data %>%
  filter(complete.cases(MARSTAT, LONELY2)) %>%  # Filter out NA values
  tabyl(MARSTAT, LONELY2) %>%
  adorn_totals(c("row", "col")) %>% #Display row and column totals
  adorn_percentages("row") %>% # Display row percentages 
  adorn_pct_formatting()  # Optionally format the percentages nicely

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# _Display column percentages ----
data %>%
  filter(complete.cases(MARSTAT, LONELY2)) %>%  # Filter out NA values
  tabyl(MARSTAT, LONELY2) %>%
  adorn_totals(c("row", "col")) %>% #Display row and column totals
  adorn_percentages("col") %>% # Display column percentages 
  adorn_pct_formatting()  # Optionally format the percentages nicely


###############################################
###Display both freq and percentage############
###############################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#_Display BOTH frequencies AND percentages ----
data %>%
  filter(complete.cases(MARSTAT, LONELY2)) %>%  # Filter out NA values
  tabyl(MARSTAT, LONELY2) %>%
  adorn_totals(c("row", "col")) %>% #Display row and column totals
  adorn_percentages("row") %>% # Display row percentages 
  adorn_pct_formatting()  %>% # Optionally format the percentages nicely
  adorn_ns() #Display number of observations

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#_Using ctable() ----
#tabyl() works, but the code is getting quite verbose
#simplify coding using ctable() from summarytools package
ctable(data$MARSTAT, data$LONELY2,  
       totals = TRUE, #display both row and column totals
       useNA = "no",  #no longer have to filter out missing
       prop = "r",    #row percentages
       round.digits =2 ) #show proportions to 2 decimal places

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#_Change order of variables ----
# Cross-tabulations: see what happens when you change order of variables
ctable(data$LONELY2, data$MARSTAT,   
       totals = TRUE, #display both row and column totals
       useNA = "no",  #no longer have to filter out missing
       prop = "r",    #row percentages
       round.digits =2 ) #show proportions to 2 decimal places


#############################################
######Row Vs Column Percentage###############
#############################################

#_Preparing the variables ----
#Check both variables to see if any missing values need managing
data %>% 
  select(LRSCALE2, FREEHMS) %>% 
  dfSummary() %>% 
  view()

# Empty levels are being displayed for FREEHMS
#Generate FREEHMS2 which drops the empty levels from the factor
data <-data %>% 
  mutate(FREEHMS2 = droplevels(FREEHMS))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# _Row percentages ----
ctable(data$LRSCALE2, data$FREEHMS2,  
       totals = TRUE, 
       useNA = "no", 
       prop = "r", 
       round.digits = 2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# _Column percentages ----
ctable(data$LRSCALE2, data$FREEHMS2,  
       totals = TRUE, 
       useNA = "no", 
       prop = "c", 
       round.digits = 2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#_Total percentages ----
#  (this isn't commonly used but is an option)
ctable(data$LRSCALE2, data$FREEHMS2,  
       totals = TRUE, 
       useNA = "no", 
       prop = "t", 
       round.digits = 2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#_tab_xtab function ----
#tabyl() and ctable() won't display row and column percentages at the same time
#Use a different package - tab_xtab from sjPlot
#displays in the viewer not console

data %>%
  filter(complete.cases(LRSCALE2, FREEHMS2)) %>%
  { tab_xtab(.$LRSCALE2, .$FREEHMS2, 
             show.obs = TRUE, 
             show.row.prc = TRUE, 
             show.col.prc = TRUE) }

#Can add cell percentages as well
data %>%
  filter(complete.cases(LRSCALE2, FREEHMS2)) %>%
  { tab_xtab(.$LRSCALE2, .$FREEHMS2, 
             show.obs = TRUE, 
             show.row.prc = TRUE, 
             show.col.prc = TRUE,
             show.cell.prc = TRUE) }



##############################
#### Chi Square Test #########
##############################


ctable(data$MARSTAT, data$LONELY2,  
       totals = TRUE, #display both row and column totals
       useNA = "no",  #no longer have to filter out missing
       prop = "r",    #row percentages
       round.digits =2, #show proportions to 2 decimal places
       chisq = TRUE) #displays results from chi-square test


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#_Subsets of data ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ___age 70 and over ----
#For those age 70 and over
#can use pipes and filter() to look at subset of data

df70 <- data %>%
  filter(AGE >= 70) 

ctable(df70$MARSTAT, df70$LONELY2, 
       totals = TRUE, 
       useNA = "no", 
       prop = "r", 
       round.digits = 2,
       chisq = TRUE) 


#Nicely formatted output
ctable(df70$MARSTAT, df70$LONELY2, 
       totals = TRUE, 
       useNA = "no", 
       prop = "r", 
       round.digits = 2,
       chisq = TRUE)  %>%
  view()


###########################################
###Another option - tab_xtab from sjPlot###
###########################################
data %>%
  filter(AGE >= 70) %>%
  { tab_xtab(.$MARSTAT, .$LONELY2, 
             show.obs = TRUE, 
             show.row.prc = TRUE, 
             show.summary = TRUE) }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ___below age 22 ----
#For those below age 22
df22 <- data %>%
  filter(AGE < 22) 

ctable(df22$MARSTAT, df22$LONELY2, 
       totals = TRUE, 
       useNA = "no", 
       prop = "r", 
       round.digits = 2,
       chisq = TRUE)  

#Nicely formatted output
ctable(df22$MARSTAT, df22$LONELY2, 
       totals = TRUE, 
       useNA = "no", 
       prop = "r", 
       round.digits = 2,
       chisq = TRUE )  %>% 
  view()


#Another option - tab_xtab from sjPlot
data %>%
  filter(AGE <22) %>%
  { tab_xtab(.$MARSTAT, .$LONELY2, 
             show.obs = TRUE, 
             show.row.prc = TRUE, 
             show.summary = TRUE) }



###################################################
############## Grouping Data #####################
##################################################

#2.5 - GROUPING DATA - OPTIONAL ACTIVITY ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#_No grouping ----
#First examine for entire sample (same code as earlier)
ctable(data$LRSCALE2, data$FREEHMS2,  
       totals = TRUE, 
       useNA = "no", 
       prop = "r",    
       round.digits =2,
       chisq = TRUE ) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Grouping with ctable()
#Only ctable works with grouped data, layout of command is slightly different 
#to what you've seen before
#Output in console

with(data, 
     stby(data    = list(x = LRSCALE2, y = FREEHMS2), 
          INDICES = COUNTRY, 
          FUN     = ctable, 
          totals = TRUE, 
          useNA = "no", 
          prop = "r", 
          round.digits = 2,
          chisq = TRUE
     )) 

# _Displaying output in viewer 
with(data, 
     stby(data    = list(x = LRSCALE2, y = FREEHMS2), 
          INDICES = COUNTRY, 
          FUN     = ctable, 
          totals = TRUE, 
          useNA = "no", 
          prop = "r", 
          round.digits = 2,
          chisq = TRUE
     )) %>%
  view()



