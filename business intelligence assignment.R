library(readxl)
library(dplyr)    
library(broom)   
library(ggplot2)
library(skimr)
library(stargazer)
library(caret)
library(glmnet)

setwd("C:/Users/afari/Desktop/Data Driven management/Business Intelligence")

#import dataset
flexwork <- read_excel("WERSA2004.xlsx")

#create unique identifier column 
flexwork <- cbind(ID = 1:nrow(flexwork), flexwork) 

#Separate variables to be used analysis 
flexwork_sep <- flexwork %>%select(ID, 
                                   a3, 
                                   a4, 
                                   a5, 
                                   a6a, a6b, a6c, a6d, 
                                   a7a, a7b, a7c, a7d, a7e, 
                                   b1a, b1b, b1c, b1d, b1e, b1f, b1g, 
                                   b3a, b3b, b3c, 
                                   e1, 
                                   e9_1,e9_2,e9_3,e9_4,e9_5,e9_6,e9_7,e9_8,e9_9,e9_10,e9_11,e9_12,e9_13,
                                   e3, 
                                   e4_1, e4_2, e4_3,e4_4, e4_5,e4_6,
                                   e5)

#rename columns in new table 
colnames(flexwork_sep) <- c( "ID", "hours_worked_weekly", "overtime_hours_weekly",
                            "frequency_of_working_over_48_hours_weekly", "job_requires_hard_work", 
                            "little_time_to_complete_work", "job_secure", "work_worries_outside_work_hours",
                            "work_tasks", "work_pace", "work_style", "work_order", "start_end_work_time", "flexitime",
                            "jobsharing", "reduced_work_hours", "increased_work_hours", "remote_working", "shift_work",
                            "compressed_hours", "work_only_during_term_time", "paid_parental_leave","workplace_nursery",
                            "gender","word_processing_tasks", "send_recieve_email", "stock_related", "record_keeping",
                            "ordering_purchasing", "machinery_related", "data_entry", "data_analysis", "desktop_publishing",
                            "computer_aided_design", "programming_related", "other_tasks", "no_computer_related_tasks", "marital_status",
                            "no_dependent_children", "children_0_2", "children_3_4", "children_5_7", "children_8_11", "children_12_18",
                            "caregiver")


#checking for missing values and outliers
sum(is.na(flexwork_sep))
skim(flexwork_sep)


############################ data restriction for outcome variable #####################################
#check initial response levels in variable 
table(flexwork_sep$flexitime)

#multi-coded and not answered variables will be removed because it's not possible to 
#tell the exact selections for these observations

#remove multi-coded observations under flexi-time observation (335 observations removed) 
flexwork_sep <- flexwork_sep %>% filter(flexitime != "multi-coded (7)")
flexwork_sep <- flexwork_sep %>% filter(flexitime != "not answered (9)")


# yes - using flextime arrangements
# don't know, no, not answered  - not using flextime arrangements

#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( flexitime = ifelse( flexitime =="don't know (8)" | flexitime == "no" , "no flexitime", flexitime))

flexwork_sep <-flexwork_sep %>%
  mutate( flexitime = ifelse( flexitime =="yes", "using flexitime", flexitime))


################################################ jobsharing ######################################################
#check initial response levels in variable 
table(flexwork_sep$jobsharing)

#multi-coded and not answered variables will be removed because it's not possible to 
#tell the exact selections for these observations

#remove multicoded observations under jobsharing observation (210 observations removed) 
flexwork_sep <- flexwork_sep %>% filter(jobsharing != "multi-coded (7)")
flexwork_sep <- flexwork_sep %>% filter(jobsharing != "not answered (9)")

# yes - using jobshare
# don't know, no, not answered - jobshare

#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( jobsharing = ifelse( jobsharing =="don't know (8)" | jobsharing == "no" , "no jobsharing", jobsharing))

flexwork_sep <-flexwork_sep %>%
  mutate( jobsharing = ifelse( jobsharing =="yes", "using jobsharing", jobsharing))

##################################################################### reduced_work_hours ########################################################################
#check initial response levels in variable 
table(flexwork_sep$reduced_work_hours)

#multi-coded and not answered variables will be removed because it's not possible to 
#tell the exact selections for these observations

#remove multicoded observations under flexi-time observation (147 observations removed) 
flexwork_sep <- flexwork_sep %>% filter(reduced_work_hours != "multi-coded (7)")
flexwork_sep <- flexwork_sep %>% filter(reduced_work_hours != "not answered (9)")

# yes - workhours reduction
# don't know, no, not answered - no workhours reduction

#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( reduced_work_hours = ifelse( reduced_work_hours =="don't know (8)" | reduced_work_hours == "no" , "no workhours reduction", reduced_work_hours))

flexwork_sep <-flexwork_sep %>%
  mutate( reduced_work_hours = ifelse( reduced_work_hours =="yes", "workhours reduction", reduced_work_hours))

##################################################################### increased_work_hours ############################################
#check initial response levels in variable 
table(flexwork_sep$increased_work_hours)

#multi-coded variables will be removed because it's not possible to tell 
#the exact selections for these observations 

#remove multicoded observations under flexi-time observation (469 observations removed) 
flexwork_sep <- flexwork_sep %>% filter(increased_work_hours != "multi-coded (7)")
flexwork_sep <- flexwork_sep %>% filter(increased_work_hours != "not answered (9)")

# yes - workhours increase
# don't know, no, not answered - no workhours increase

#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( increased_work_hours = ifelse( increased_work_hours =="don't know (8)" | increased_work_hours == "no", "no workhours increase", increased_work_hours))

flexwork_sep <-flexwork_sep %>%
  mutate( increased_work_hours = ifelse( increased_work_hours =="yes", "workhours increase", increased_work_hours))

##################################################################### remote_working #########################################
#check initial response levels in variable 
table(flexwork_sep$remote_working)

#multicoded variables will be removed because it's notpossible to 
#tell the exact selections for these observations 

#remove multicoded observations under flexi-time observation (73 observations removed) 
flexwork_sep <- flexwork_sep %>% filter(remote_working != "multi-coded (7)")
flexwork_sep <- flexwork_sep %>% filter(remote_working != "not answered (9)")

# yes - work from home
# don't know, no, not answered - no workfrom home

#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( remote_working = ifelse( remote_working =="don't know (8)" | remote_working == "no" , "no remote working", remote_working))

flexwork_sep <-flexwork_sep %>%
  mutate( remote_working = ifelse( remote_working =="yes", "remote working", remote_working))


##################################################################### shift_work ###################################################################################
#check initial response levels in variable 
table(flexwork_sep$shift_work)

#multi-coded variables will be removed because it's not possible to 
#tell the exact selections for these observations 

#remove multicoded observations under flexi-time observation (57 observations removed) 
flexwork_sep <- flexwork_sep %>% filter(shift_work != "multi-coded (7)")
flexwork_sep <- flexwork_sep %>% filter(shift_work != "not answered (9)")

# yes - work from home
# don't know, no, not answered - no workfrom home

#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( shift_work = ifelse( shift_work =="don't know (8)" | shift_work == "no" , "no shift jobs", shift_work))

flexwork_sep <-flexwork_sep %>%
  mutate( shift_work = ifelse( shift_work =="yes", "shift jobs", shift_work))

##################################################################### compressed_hours #################################################################################
#check initial response levels in variable 
table(flexwork_sep$compressed_hours)

#multi-coded variables will be removed because it's not possible to tell 
#the exact selections for these observations 

#remove multicoded observations under flexi-time observation (57 observations removed) 
flexwork_sep <- flexwork_sep %>% filter(compressed_hours != "multi-coded (7)")
flexwork_sep <- flexwork_sep %>% filter(compressed_hours != "not answered (9)")

# yes - same hrs + short wk
# don't know, no, not answered- no same hrs + short wk

#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( compressed_hours = ifelse( compressed_hours =="don't know (8)" | compressed_hours == "no" , "no compressed hours", compressed_hours))

flexwork_sep <-flexwork_sep %>%
  mutate( compressed_hours = ifelse( compressed_hours =="yes", "compressed hours", compressed_hours))


############################################################################################################################################################################

#Join all outcome values
#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( availability_of_flexible_work = (ifelse( flexitime =="using flexitime" | jobsharing == "using jobsharing" | reduced_work_hours == "workhours reduction" | increased_work_hours == "workhours increase" | remote_working == "remote working" |
                                      shift_work == "shift jobs" |  compressed_hours == "compressed hours" , 1, 0)))

#count of each category
table(flexwork_sep$availability_of_flexible_work)

summary(flexwork_sep$availability_of_flexible_work)
#with a mean of 0.69 predicted probability using
#lm is unlikely to be greater/less than (0,1)
#the linear model will therefore be used

#################################################################################################################################################################
#############################################################################################################################################################
############################################################################################################################################################


#################################################################### work_only_during_term_time #############################################################################################

#check initial response levels in variable 
table(flexwork_sep$work_only_during_term_time)

#multicoded variables will be removed because it's notpossible to tell
#the exact selections for these observations 

#remove multicoded observations under flexi-time observation 535 observations removed) 
flexwork_sep <- flexwork_sep %>% filter(work_only_during_term_time != "multi-coded (7)")
flexwork_sep <- flexwork_sep %>% filter(work_only_during_term_time != "not answered (9)")

#yes - termtime work
# don't know, no, not answered - no termtime work

#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( work_only_during_term_time = ifelse( work_only_during_term_time =="don't know (8)" | work_only_during_term_time == "no" , "no term time work", work_only_during_term_time))

flexwork_sep <-flexwork_sep %>%
  mutate( work_only_during_term_time = ifelse( work_only_during_term_time =="yes", "termtime work", work_only_during_term_time))

############################################################paid_parental_leave #####################################################
#check initial response levels in variable 
table(flexwork_sep$paid_parental_leave)

#multicoded variables will be removed because it's notpossible to 
#tell the exact selections for these observations 

#remove multicoded observations under flexi-time observation (242 observations removed) 
flexwork_sep <- flexwork_sep %>% filter(paid_parental_leave != "multi-coded (7)")
flexwork_sep <- flexwork_sep %>% filter(paid_parental_leave != "not answered (9)")

# yes - paid parental leave
# don't know, no, not answered  - no paid parental leave

#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( paid_parental_leave = ifelse( paid_parental_leave =="don't know (8)" | paid_parental_leave == "no" , "no paid parental leave", paid_parental_leave))

flexwork_sep <-flexwork_sep %>%
  mutate( paid_parental_leave = ifelse( paid_parental_leave =="yes", "paid parental leave", paid_parental_leave))

##################################################################### workplace_nursery #####################################################
#check initial response levels in variable 
table(flexwork_sep$workplace_nursery)

#multicoded variables will be removed because it's not possible to
#tell the exact selections for these observations 

#remove multicoded observations under flexi-time observation (70 observations removed) 
flexwork_sep <- flexwork_sep %>% filter(workplace_nursery != "multi-coded (7)")
flexwork_sep <- flexwork_sep %>% filter(workplace_nursery != "not answered (9)")

# yes - workplce nursery
# don't know, no, not answered - no workplace nursery

#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( workplace_nursery = ifelse( workplace_nursery =="don't know (8)" | workplace_nursery == "no" , "no workplace nursery", workplace_nursery))

flexwork_sep <-flexwork_sep %>%
  mutate( workplace_nursery = ifelse( workplace_nursery =="yes", "workplace nursery", workplace_nursery))

##########################################################################################################
#Join all work and childcare related values
#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( childcare_support = (ifelse( work_only_during_term_time == "termtime work" | paid_parental_leave == "paid parental leave" |
                                         workplace_nursery == "workplace nursery..." , 1, 0)))


###################################################################################################################
##################################################################################################################
####################################################################################################################

##################################################################### gender #####################################################

#check initial response levels in variable 
table(flexwork_sep$gender)

#variable assess the probability of accessing flexwork initiatives based on gender resulting from the assumption
#that females have significantly higher child care duties

#remove no answer (106 observations)
#not answered- difficult to pin point your gender 
flexwork_sep <- flexwork_sep %>% filter(gender != "not answered (9)")

#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( gender = ifelse( gender == "female", 1, 0))

###################################################################################################################
##################################################################################################################
####################################################################################################################

##################################################################### caregiver #####################################################


#check initial response levels in variable 
table(flexwork_sep$caregiver)

#this variable assess the probability of a person to access flexwork initiatives 
#depending on whether or not they take care of sick relatives 

#remove no answer-( cannot tell whether or not they take care of a sick relative)(143 observations removed)
flexwork_sep <- flexwork_sep %>% filter(caregiver != "not answered (9)")

flexwork_sep <-flexwork_sep %>%
  mutate( caregiver = ifelse( caregiver == "no", "not a care giver", caregiver))

flexwork_sep <-flexwork_sep %>%
  mutate( caregiver = ifelse( caregiver == "yes, 0-4 hours a week" | caregiver == "yes, 5-9 hours a week" | caregiver == "yes, 20-34 hours a week" | caregiver == "yes, 35 or more hours a week" , "caregiver", caregiver))

flexwork_sep <-flexwork_sep %>%
  mutate(caregiver = ifelse( caregiver == "caregiver", 1, 0))

###################################################################################################################
##################################################################################################################
####################################################################################################################

##################################################################### no_dependent_children #####################################################

## children dependence####
table(flexwork_sep$no_dependent_children)

#i have dependent children - no tick
#i do not have dependent children - tick

#remove no answer
flexwork_sep <- flexwork_sep %>% filter(no_dependent_children != "not answered (9)")

#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( no_dependent_children = ifelse( no_dependent_children =="box not ticked", "children dependent", no_dependent_children))

flexwork_sep <-flexwork_sep %>%
  mutate( no_dependent_children = ifelse( no_dependent_children == "box ticked" ,"children not dependent", no_dependent_children))

##################################################################### children_0_2 ##############################################################################################################
table(flexwork_sep$children_0_2)

#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( children_0_2 = ifelse( children_0_2 =="box not ticked", "no children aged 0-2", children_0_2))

flexwork_sep <-flexwork_sep %>%
  mutate( children_0_2 = ifelse( children_0_2 == "box ticked" ,"children aged 0-2", children_0_2))

##################################################################### children_3_4 ##############################################################################################################
table(flexwork_sep$children_3_4)

#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( children_3_4 = ifelse( children_3_4 =="box not ticked", "no children aged 3-4", children_3_4))

flexwork_sep <-flexwork_sep %>%
  mutate( children_3_4 = ifelse( children_3_4 == "box ticked" ,"children aged 3-4", children_3_4))

##################################################################### children_5_7 ##############################################################################################################
table(flexwork_sep$children_5_7)

#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( children_5_7 = ifelse( children_5_7 =="box not ticked", "no children aged 5-7", children_5_7))

flexwork_sep <-flexwork_sep %>%
  mutate( children_5_7 = ifelse( children_5_7 == "box ticked" ,"children aged 5-7", children_5_7))

##################################################################### children_8_11 ##############################################################################################################
table(flexwork_sep$children_8_11)

#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( children_8_11 = ifelse( children_8_11 =="box not ticked", "no children aged 8-11", children_8_11))

flexwork_sep <-flexwork_sep %>%
  mutate( children_8_11 = ifelse( children_8_11 == "box ticked" ,"children aged 8-11", children_8_11))

##################################################################### children_12_18 ##############################################################################################################
table(flexwork_sep$children_12_18)

#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( children_12_18 = ifelse( children_12_18 =="box not ticked", "no children aged 12-18", children_12_18))

flexwork_sep <-flexwork_sep %>%
  mutate( children_12_18 = ifelse( children_12_18 == "box ticked" ,"children aged 12-18", children_12_18))

######################################################################################################

#group into dependent vs independent children 

#Join independent values
#recode 
#1 - for dependent children
#0 - for independent children

flexwork_sep <-flexwork_sep %>%
  mutate( independent_dependent_children = (ifelse( no_dependent_children  == "children dependent" |  children_0_2  == "children aged 0-2" |
                                     children_3_4  == "children aged 3-4" |  children_5_7  == "children aged 5-7"|
                                     children_8_11  == "children aged 8-11"|  children_12_18  == "children aged 12-18" , 1, 0)))


###################################################################################################################
##################################################################################################################
####################################################################################################################

##################################################################### frequency_of_working_over_48_hours_weekly ##############################################################################################################

##how often do you work over 48hrs##
table(flexwork_sep$frequency_of_working_over_48_hours_weekly)

#multicoded variables will be removed because it's notpossible to tell
#the exact selections for these observations 

#remove multicoded observations under flexi-time observation (87 observations removed) 
flexwork_sep <- flexwork_sep %>% filter(frequency_of_working_over_48_hours_weekly != "multi-coded (7)")
flexwork_sep <- flexwork_sep %>% filter(frequency_of_working_over_48_hours_weekly != "not answered (9)")


#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( frequency_of_working_over_48_hours_weekly = ifelse( frequency_of_working_over_48_hours_weekly == "never" , "nothing over 48hrs a week", frequency_of_working_over_48_hours_weekly))

flexwork_sep <-flexwork_sep %>%
  mutate( frequency_of_working_over_48_hours_weekly = ifelse(  frequency_of_working_over_48_hours_weekly == "every week" | frequency_of_working_over_48_hours_weekly == "less often than once a month" | frequency_of_working_over_48_hours_weekly == "once a month" |  frequency_of_working_over_48_hours_weekly == "two or three times a month" , "over 48hrs a week", frequency_of_working_over_48_hours_weekly))

flexwork_sep <-flexwork_sep %>%
  mutate( working_over_48hrs_weekly = ifelse(  frequency_of_working_over_48_hours_weekly == "over 48hrs a week" , 1, 0))



###################################################################################################################
##################################################################################################################
####################################################################################################################


##################################################################### job_requires_hard_work-work_worries_outside_work_hours ##############################################################################################################


table(flexwork_sep$job_requires_hard_work)
table(flexwork_sep$little_time_to_complete_work)
table(flexwork_sep$job_secure)
table(flexwork_sep$work_worries_outside_work_hours)


#agree/disagree with statement 
#remove multicoded observations under flexi-time observation (172 observations removed) 
flexwork_sep <- flexwork_sep %>% filter(job_requires_hard_work != "multi-coded (7)")
flexwork_sep <- flexwork_sep %>% filter(little_time_to_complete_work != "multi-coded (7)")
flexwork_sep <- flexwork_sep %>% filter(job_secure != "multi-coded (7)")
flexwork_sep <- flexwork_sep %>% filter(work_worries_outside_work_hours != "multi-coded (7)")

flexwork_sep <- flexwork_sep %>% filter(job_requires_hard_work != "not answered (9)")
flexwork_sep <- flexwork_sep %>% filter(little_time_to_complete_work != "not answered (9)")
flexwork_sep <- flexwork_sep %>% filter(job_secure != "not answered (9)")
flexwork_sep <- flexwork_sep %>% filter(work_worries_outside_work_hours != "not answered (9)")



#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( job_requires_hard_work = ifelse( job_requires_hard_work == "disagree" |  job_requires_hard_work == "don't know (8)" |  job_requires_hard_work == "neither agree nor disagree" |  job_requires_hard_work == "strongly disagree", "disagree", job_requires_hard_work))
flexwork_sep <-flexwork_sep %>%
  mutate( little_time_to_complete_work = ifelse( little_time_to_complete_work == "disagree" |  little_time_to_complete_work == "don't know (8)" |  little_time_to_complete_work == "neither agree nor disagree" |  little_time_to_complete_work == "strongly disagree", "disagree", little_time_to_complete_work))
flexwork_sep <-flexwork_sep %>%
  mutate( job_secure = ifelse( job_secure == "disagree" |  job_secure == "don't know (8)" |  job_secure == "neither agree nor disagree" |  job_secure == "strongly disagree", "disagree", job_secure))
flexwork_sep <-flexwork_sep %>%
  mutate( work_worries_outside_work_hours = ifelse( work_worries_outside_work_hours == "disagree" |  work_worries_outside_work_hours == "don't know (8)" |  work_worries_outside_work_hours == "neither agree nor disagree" |  work_worries_outside_work_hours == "strongly disagree", "disagree", work_worries_outside_work_hours))


flexwork_sep <-flexwork_sep %>%
  mutate( job_requires_hard_work = ifelse( job_requires_hard_work == "agree" | job_requires_hard_work == "strongly agree", "agree", job_requires_hard_work))
flexwork_sep <-flexwork_sep %>%
  mutate( little_time_to_complete_work = ifelse( little_time_to_complete_work == "agree" | little_time_to_complete_work == "strongly agree" , "agree", little_time_to_complete_work))
flexwork_sep <-flexwork_sep %>%
  mutate( job_secure = ifelse( job_secure == "agree" | job_secure == "strongly agree" , "agree", job_secure))
flexwork_sep <-flexwork_sep %>%
  mutate( work_worries_outside_work_hours = ifelse( work_worries_outside_work_hours == "agree" | work_worries_outside_work_hours == "strongly agree" , "agree", work_worries_outside_work_hours))

flexwork_sep <-flexwork_sep %>%
  mutate( does_your_job_require_hardwork = ifelse(  job_requires_hard_work == "agree" , 1, 0))

flexwork_sep <-flexwork_sep %>%
  mutate( completes_work_in_little_time = ifelse(  little_time_to_complete_work == "agree" , 1, 0))

flexwork_sep <-flexwork_sep %>%
  mutate( is_your_job_secure = ifelse(  job_secure == "agree" , 1, 0))

flexwork_sep <-flexwork_sep %>%
  mutate( Worrying_about_work = ifelse(  work_worries_outside_work_hours == "agree" , 1, 0))


###################################################################################################################
##################################################################################################################
####################################################################################################################

##################################################################### work_tasks-start_end_work_time ##############################################################################################################

table(flexwork_sep$work_tasks)
table(flexwork_sep$work_pace)
table(flexwork_sep$work_style)
table(flexwork_sep$work_order)
table(flexwork_sep$start_end_work_time)

##control over your work##
#remove multicoded observations under flexi-time observation (193 observations removed) 
flexwork_sep <- flexwork_sep %>% filter(work_tasks != "multi-coded (7)")
flexwork_sep <- flexwork_sep %>% filter(work_pace != "multi-coded (7)")
flexwork_sep <- flexwork_sep %>% filter(work_style != "multi-coded (7)")
flexwork_sep <- flexwork_sep %>% filter(work_order != "multi-coded (7)")
flexwork_sep <- flexwork_sep %>% filter(start_end_work_time != "multi-coded (7)")


flexwork_sep <- flexwork_sep %>% filter(work_tasks != "not answered (9)")
flexwork_sep <- flexwork_sep %>% filter(work_pace != "not answered (9)")
flexwork_sep <- flexwork_sep %>% filter(work_style != "not answered (9)")
flexwork_sep <- flexwork_sep %>% filter(work_order != "not answered (9)")
flexwork_sep <- flexwork_sep %>% filter(start_end_work_time != "not answered (9)")



#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( work_tasks = ifelse( work_tasks == "none" |  work_tasks == "don't know (8)", "no control", work_tasks))
flexwork_sep <-flexwork_sep %>%
  mutate( work_pace = ifelse( work_pace == "none" |  work_pace == "don't know (8)", "no control", work_pace))
flexwork_sep <-flexwork_sep %>%
  mutate( work_style = ifelse( work_style == "none" |  work_style == "don't know (8)", "no control", work_style))
flexwork_sep <-flexwork_sep %>%
  mutate( work_order = ifelse( work_order == "none" |  work_order == "don't know (8)", "no control", work_order))
flexwork_sep <-flexwork_sep %>%
  mutate( start_end_work_time = ifelse( start_end_work_time == "none" |  start_end_work_time == "don't know (8)", "no control", start_end_work_time))

flexwork_sep <-flexwork_sep %>%
  mutate( work_tasks = ifelse( work_tasks == "a little" | work_tasks == "a lot" | work_tasks == "some", "control", work_tasks))
flexwork_sep <-flexwork_sep %>%
  mutate( work_pace = ifelse( work_pace == "a little" | work_pace == "a lot" | work_pace == "some", "control", work_pace))
flexwork_sep <-flexwork_sep %>%
  mutate( work_style = ifelse( work_style == "a little" | work_style == "a lot" | work_style == "some", "control", work_style))
flexwork_sep <-flexwork_sep %>%
  mutate( work_order = ifelse( work_order == "a little" | work_order == "a lot" | work_order == "some", "control", work_order))
flexwork_sep <-flexwork_sep %>%
  mutate( start_end_work_time = ifelse( start_end_work_time == "a little" | start_end_work_time == "a lot" | start_end_work_time == "some", "control", start_end_work_time))


flexwork_sep <-flexwork_sep %>%
  mutate( control_over_work_tasks = ifelse(  work_tasks == "control" , 1, 0))
flexwork_sep <-flexwork_sep %>%
  mutate( control_over_work_pace = ifelse(  work_pace == "control" , 1, 0))
flexwork_sep <-flexwork_sep %>%
  mutate( control_over_work_style = ifelse(  work_style == "control" , 1, 0))
flexwork_sep <-flexwork_sep %>%
  mutate( control_over_order_of_tasks = ifelse(  work_order == "control" , 1, 0))
flexwork_sep <-flexwork_sep %>%
  mutate( control_over_work_start_end_times = ifelse(  start_end_work_time == "control" , 1, 0))









###################################################################################################################
##################################################################################################################
####################################################################################################################

##################################################################### word_processing_tasks - no_computer_related_tasks ##############################################################################################################


#remove multicoded observations under flexi-time observation (681) observations removed) 
flexwork_sep <- flexwork_sep %>% filter(word_processing_tasks != "not answered (99)")
flexwork_sep <- flexwork_sep %>% filter(send_recieve_email != "not answered (99)")
flexwork_sep <- flexwork_sep %>% filter(stock_related != "not answered (99)")
flexwork_sep <- flexwork_sep %>% filter(record_keeping != "not answered (99)")
flexwork_sep <- flexwork_sep %>% filter(ordering_purchasing != "not answered (99)")
flexwork_sep <- flexwork_sep %>% filter(machinery_related != "not answered (99)")
flexwork_sep <- flexwork_sep %>% filter(data_entry != "not answered (99)")
flexwork_sep <- flexwork_sep %>% filter(data_analysis != "not answered (99)")
flexwork_sep <- flexwork_sep %>% filter(desktop_publishing != "not answered (99)")
flexwork_sep <- flexwork_sep %>% filter(computer_aided_design != "not answered (99)")
flexwork_sep <- flexwork_sep %>% filter(programming_related != "not answered (99)")
flexwork_sep <- flexwork_sep %>% filter(other_tasks != "not answered (99)")
flexwork_sep <- flexwork_sep %>% filter(no_computer_related_tasks  != "not answered (99)")





#recode 
flexwork_sep <-flexwork_sep %>%
  mutate( word_process = ifelse( word_processing_tasks == "box ticked", 1, 0),
          email = ifelse( send_recieve_email == "box ticked", 1, 0),          
          stock = ifelse( stock_related == "box ticked", 1, 0),          
          records = ifelse( record_keeping == "box ticked", 1, 0),          
          ordering = ifelse( ordering_purchasing == "box ticked", 1, 0),          
          monitoring = ifelse( machinery_related == "box ticked", 1, 0),          
          data_entry = ifelse( data_entry == "box ticked", 1, 0),          
          data_analysis = ifelse( data_analysis == "box ticked", 1, 0),
          desk_publish = ifelse( desktop_publishing == "box ticked", 1, 0),
          computer_design = ifelse( computer_aided_design == "box ticked", 1, 0),
          programming = ifelse( programming_related == "box ticked", 1, 0),
          any_task = ifelse( other_tasks == "box ticked", 1, 0),
          no_computer = ifelse( no_computer_related_tasks == "box ticked", 1, 0),
  )


############################################################################################################################################################################
#########################################################################################################################################################################
######################################################################################################################################################################### 

####Descriptive statistics########

availability_of_flexible_workdesc <- flexwork_sep %>% summarise(average = mean(availability_of_flexible_work),
                    minimum = min(availability_of_flexible_work), maximum =max(availability_of_flexible_work),
                    S.D_ = sd(availability_of_flexible_work))


childcaresupportdesc <- flexwork_sep %>% summarise(average = mean(childcare_support),
                                                  minimum = min(childcare_support), maximum =max(childcare_support),
                                                  S.D_ = sd(childcare_support))


independent_dependent_childrendesc <- flexwork_sep %>% summarise(average = mean(independent_dependent_children),
                                                     minimum = min(independent_dependent_children), maximum =max(independent_dependent_children),
                                                     S.D_ = sd(independent_dependent_children))

working_over_48hrs_weeklydesc <- flexwork_sep %>% summarise(average = mean(working_over_48hrs_weekly),
                                            minimum = min(working_over_48hrs_weekly), maximum =max(working_over_48hrs_weekly),
                                            S.D_ = sd(working_over_48hrs_weekly))

## do you agree or disagree with this statement variable options  
does_your_job_require_hardworkdesc <- flexwork_sep %>% summarise(average = mean(does_your_job_require_hardwork),
                                                 minimum = min(does_your_job_require_hardwork), maximum =max(does_your_job_require_hardwork),
                                                 S.D_ = sd(does_your_job_require_hardwork))

completes_work_in_little_timedesc <- flexwork_sep %>% summarise(average = mean(completes_work_in_little_time),
                                                  minimum = min(completes_work_in_little_time), maximum =max(completes_work_in_little_time),
                                                  S.D_ = sd(completes_work_in_little_time))

is_your_job_securedesc <- flexwork_sep %>% summarise(average = mean(is_your_job_secure),
                                                  minimum = min(is_your_job_secure), maximum =max(is_your_job_secure),
                                                  S.D_ = sd(is_your_job_secure))

Worrying_about_workdesc <- flexwork_sep %>% summarise(average = mean(Worrying_about_work),
                                                  minimum = min(Worrying_about_work), maximum =max(Worrying_about_work),
                                                  S.D_ = sd(Worrying_about_work))

##how much control do you have over your work variable options 
control_over_work_tasksdesc <- flexwork_sep %>% summarise(average = mean(control_over_work_tasks),
                                                  minimum = min(control_over_work_tasks), maximum =max(control_over_work_tasks),
                                                  S.D_ = sd(control_over_work_tasks))


control_over_work_pacedesc <- flexwork_sep %>% summarise(average = mean(control_over_work_pace),
                                              minimum = min(control_over_work_pace), maximum =max(control_over_work_pace),
                                              S.D_ = sd(control_over_work_pace))

control_over_work_styledesc <- flexwork_sep %>% summarise(average = mean(control_over_work_style),
                                              minimum = min(control_over_work_style), maximum =max(control_over_work_style),
                                              S.D_ = sd(control_over_work_style))

control_over_order_of_tasksdesc <- flexwork_sep %>% summarise(average = mean(control_over_order_of_tasks),
                                              minimum = min(control_over_order_of_tasks), maximum =max(control_over_order_of_tasks),
                                              S.D_ = sd(control_over_order_of_tasks))

control_over_work_start_end_timesdesc <- flexwork_sep %>% summarise(average = mean(control_over_work_start_end_times),
                                              minimum = min(control_over_work_start_end_times), maximum =max(control_over_work_start_end_times),
                                              S.D_ = sd(control_over_work_start_end_times))



######type of work you do 
wordprocess <- flexwork_sep %>% summarise(average = mean(word_process),
                                                 minimum = min(word_process), maximum =max(word_process),
                                                 S.D_ = sd(word_process))

stock <- flexwork_sep %>% summarise(average = mean(stock),
                                           minimum = min(stock), maximum =max(stock),
                                           S.D_ = sd(stock))

records <- flexwork_sep %>% summarise(average = mean( records),
                                    minimum = min( records), maximum =max( records),
                                    S.D_ = sd( records))

ordering <- flexwork_sep %>% summarise(average = mean(ordering),
                                    minimum = min(ordering), maximum =max(ordering),
                                    S.D_ = sd(ordering))

monitoring <- flexwork_sep %>% summarise(average = mean( monitoring),
                                    minimum = min(stock), maximum =max(stock),
                                    S.D_ = sd(stock))

dataentry <- flexwork_sep %>% summarise(average = mean(data_entry),
                                    minimum = min(data_entry), maximum =max(data_entry),
                                    S.D_ = sd(data_entry))

dataanalysis <- flexwork_sep %>% summarise(average = mean( data_analysis),
                                         minimum = min( data_analysis), maximum =max( data_analysis),
                                         S.D_ = sd( data_analysis))

deskpublish <- flexwork_sep %>% summarise(average = mean( desk_publish),
                                            minimum = min( desk_publish), maximum =max( desk_publish),
                                            S.D_ = sd(desk_publish))

computerdesign <- flexwork_sep %>% summarise(average = mean(  computer_design),
                                           minimum = min(  computer_design), maximum =max(  computer_design),
                                           S.D_ = sd( computer_design))

programming <- flexwork_sep %>% summarise(average = mean(  programming),
                                              minimum = min(  programming), maximum =max(  programming),
                                              S.D_ = sd( programming))

anytask <- flexwork_sep %>% summarise(average = mean(  any_task),
                                          minimum = min(  any_task), maximum =max(  any_task),
                                          S.D_ = sd( any_task))
  
nocomputer <- flexwork_sep %>% summarise(average = mean(   no_computer),
                                       minimum = min(   no_computer), maximum =max(   no_computer),
                                       S.D_ = sd( no_computer))


###############################################################################################################################################################
##############################################################################################################################################################
#############################################################################################################################################################

###Linear regressions 


####main explanatory variable - control or not
###### Amount of influence you have over key job tasks 

availability_of_flexible_work_predA <- lm(availability_of_flexible_work ~  control_over_work_tasks + control_over_work_pace + control_over_work_style + control_over_order_of_tasks + control_over_work_start_end_times, data = flexwork_sep )
summary(availability_of_flexible_work_predA)

######other explanatory variables 
###### does employee have children and if they do, is it possible that women are more likely to flexwork arrangements because they are usually the ones with child care
###responsibilities?

availability_of_flexible_work_predB <- lm(availability_of_flexible_work ~ control_over_work_tasks + control_over_work_pace + control_over_work_style + control_over_order_of_tasks + control_over_work_start_end_times + childcare_support + gender,
                      data = flexwork_sep )
summary(availability_of_flexible_work_predB)

###### does employee have carer responsibilities and if they do, is it possible that they can access flex work arrangements due to this. 

availability_of_flexible_work_predC <- lm(availability_of_flexible_work ~ control_over_work_tasks + control_over_work_pace + control_over_work_style + control_over_order_of_tasks + control_over_work_start_end_times + childcare_support + gender + caregiver,
                     data = flexwork_sep )
summary(availability_of_flexible_work_predC)

###### if the employee uses majorly a computer for their most significant tasks is it possible for them to do these tasks remotely, as working from home is part of the
#outcome variable . 

availability_of_flexible_work_predD <- lm(availability_of_flexible_work ~ control_over_work_tasks + control_over_work_pace + control_over_work_style + control_over_order_of_tasks + control_over_work_start_end_times + childcare_support + gender + caregiver +
                      word_process + email + stock + records + ordering +monitoring + data_entry +data_analysis + desk_publish +computer_design + programming + any_task + no_computer + 
                      working_over_48hrs_weekly , data = flexwork_sep )
summary(availability_of_flexible_work_predD)

#### how long does employee work, if they work long hours are they likely to be offered flex work arrangements to improve work life balance? if employee feels as though they re unable to 
##do their work within the given time can they access flextime arrangements to increase time flexibility?
availability_of_flexible_work_predE <- lm(availability_of_flexible_work ~ control_over_work_tasks + control_over_work_pace + control_over_work_style + control_over_order_of_tasks + control_over_work_start_end_times + childcare_support + gender + caregiver +
                       word_process + email + stock + records + ordering +monitoring + data_entry +data_analysis + desk_publish +computer_design + programming + any_task + no_computer + 
                       working_over_48hrs_weekly + does_your_job_require_hardwork + completes_work_in_little_time + is_your_job_secure + Worrying_about_work + working_over_48hrs_weekly, data = flexwork_sep )
summary(availability_of_flexible_work_predE)

stargazer(availability_of_flexible_work_predA, availability_of_flexible_work_predB, availability_of_flexible_work_predC, availability_of_flexible_work_predD, availability_of_flexible_work_predE, type = "text", out = "C:/Users/afari/Desktop/Data Driven management/Business Intelligence/.docx")



################################################################################################################################################################
set.seed(12345)
train <- flexwork_sep %>% sample_frac(0.8)
test  <- anti_join(flexwork_sep, train, by = "ID")


#######################################################################################################################################################################                   

availability_of_flexible_work_predt <- lm(availability_of_flexible_work ~ childcare_support + 
                       gender +
                       caregiver +
                       working_over_48hrs_weekly +
                       control_over_work_tasks + control_over_work_pace + control_over_work_style + control_over_order_of_tasks + control_over_work_start_end_times +
                       does_your_job_require_hardwork + completes_work_in_little_time + is_your_job_secure + Worrying_about_work +
                       word_process + email + stock + records + ordering +monitoring + data_entry +data_analysis + desk_publish +computer_design + programming + any_task + no_computer, data = train )
summary(availability_of_flexible_work_predt)


stargazer(availability_of_flexible_work_predt, type = "text")

#######################################################################################################################################################################                   
#######################################################################################################################################################################                   
#######################################################################################################################################################################    

##assesing the accuracy of the model using tabulated values ####

test <- augment(availability_of_flexible_work_predt, newdata = test)

xaxislabels <- c("Flextime available", "Flextime unavailable")
yaxislabels <- c("Predicted values")

B1 <- ggplot(data = test, aes(x = as.factor(availability_of_flexible_work), y = .fitted)) +
  geom_bar(stat = "identity",  color="#f49678", fill= "white")+
  scale_x_discrete(labels= xaxislabels) +
  labs(x="", title="Visualising prediction model results", y="Predicated Values")+
  theme(plot.title = element_text(color = "#923b39", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
B1

#setting the threshold......
test <- test %>% mutate(flexwork_test_predictions = ifelse(.fitted > 0.5,1,0)) 

#table(test$flexwork_test_predictions)
table(test$flexwork_test_predictions, test$availability_of_flexible_work)

#sensitivity score (proportion of true positive per total positives, helps us identify
#how precisely our model is able to predict positive)


true_pos <- 2468
total_pos <- 141 + 2468

sensitivity <- true_pos/total_pos


#specificity score (proportion of true negatives per total negatives, helps us identify
#how precisely our model is able to predict negatives)

true_neg <- 141
total_neg <- 141 + 1041

specificity<- true_neg/total_neg

# results shows that our model is better at prediciting employees who have access to flextime arrangements 
#more than those who don't

# it's necessary to note that the threshold can affect specificity and sensitivity, in the sense that, marginal
#values around the threshold can easily skew the overall values reported 

#accuracy ( tests the models overall ability to make correct predictions)

accuracy <- (sensitivity+specificity)/2

# overall the model is approximately 55% accurate, i.e it makes predicitiona with 55% accuracy, this is not too 
#strong as close to half of the time it may be wrong


###############################################################################################################################################################################
#####################################################################################################################################################################
###############################################################################################################################################################


###visualization 

#plot a bar chart of my outcome variable i would like to show the difference between the actual and predicted observation




########################################################################################################################################################
#######################################################################################################################################################
#######################################################################################################################################################


####lasso regression

#change outcome var to character
flexwork_sep <-flexwork_sep %>%
  mutate( availability_of_flexible_workchar = as.character(flexwork_sep$availability_of_flexible_work))

flexwork_sep <-flexwork_sep %>%
  mutate(  availability_of_flexible_workchar = ifelse(  availability_of_flexible_workchar == "1", "flextimeavailable", "flextimenotavailable" ))

availability_of_flexible_workchar <- as.character(flexwork_sep$availability_of_flexible_work)

str(flexwork_sep$availability_of_flexible_workchar)

flexwork_predt_ii <- (availability_of_flexible_workchar ~ 
                             childcare_support + 
                             gender +
                             caregiver +
                             working_over_48hrs_weekly +
                             control_over_work_tasks + control_over_work_pace + control_over_work_style + control_over_order_of_tasks + control_over_work_start_end_times +
                             does_your_job_require_hardwork + completes_work_in_little_time + is_your_job_secure + Worrying_about_work +
                             word_process + email + stock + records + ordering +monitoring + data_entry +data_analysis + 
                             desk_publish +computer_design + programming + any_task + no_computer)


#split data 80-20

set.seed(1357)

index <- createDataPartition(flexwork_sep$availability_of_flexible_workchar, p= .8, list = FALSE, times = 1)

# share the data between the train and test models
train2 <- flexwork_sep[index,]
test2 <- flexwork_sep[-index,]

#cross- validation (K-fold cross validation)
validation_levels <- trainControl(method = "cv", number = 20, classProbs = TRUE)

#creating lambda values 
lamvec <- 10^seq(5, -5, length = 500)


#create lasso model with train2 data, train data with 10 fold validation
flexwork_lasso_pred <- train(flexwork_predt_ii,
                             data = train2,
                             metric = "Accuracy",
                             method = "glmnet", family = "binomial",
                             tuneGrid=expand.grid(alpha=1,lambda=lamvec),
                             trControl = validation_levels )

table(flexwork_lasso_pred_values, test2$availability_of_flexible_workchar)


#the best lambda value 
flexwork_lasso_pred$bestTune 

#important variables
varImp(flexwork_lasso_pred)

#visualizing the important variables 
importantvars <- ggplot(varImp(flexwork_lasso_pred)) + theme_classic()



