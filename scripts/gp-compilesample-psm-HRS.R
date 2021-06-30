### Transition to Grandparenthood Paper - HRS Sample - performing PSM and compiling final analysis samples ###

# run 'gp-compilesample-imp-HRS.R' first to obtain the files that are loaded here 
# (imputations have a very long run time)

library(tidyverse)
library(MatchIt)
library(readxl)

# load .rda
load(file = "data/processed/HRS/hrslong_cleaned.rda")
load(file = "data/processed/HRS/hrslong_valid.rda")

#### PSM: final covariate preparations ####

# load .rda
load(file = "data/processed/HRS/hrsimp_matching_1.rda")
load(file = "data/processed/HRS/hrsimp_matching_2.rda")
load(file = "data/processed/HRS/hrsimp_matching_3.rda")
load(file = "data/processed/HRS/hrsimp_matching_4.rda")
load(file = "data/processed/HRS/hrsimp_matching_5.rda")

# recode PSM covariates
# a lot of the variables need recoding before they can be fed into the PSM model
  
# create list of all 5 imputation datasets
impdata <- lapply(ls(pattern="hrsimp_matching_[0-9]+"), function(x) get(x))

# categorical nominal variables with k>2 categories are transformed into k-1 dummies 
# --> largest category is reference category without dummy

# do recoding on this list object via 'map'
impdata <- impdata %>%
  map(~mutate(., age = interviewyear - birthyr, # 'interviewyear' better than 'year'
              female = gender - 1,  # We will match exactly on gender (not used as PSM cov)
              black = ifelse(race==2, 1, 0),         #ref: caucasian
              raceother = ifelse(race==3, 1, 0),
              fulltime = ifelse(laborforce==1, 1, 0), #ref: retired
              parttime = ifelse(laborforce==2, 1, 0),
              unemployed = ifelse(laborforce==3, 1, 0),
              partlyretired = ifelse(laborforce==4, 1, 0),
              disabled = ifelse(laborforce==6, 1, 0),
              notinlaborforce = ifelse(laborforce==7, 1, 0),
              spouseabsent = ifelse(marital==2, 1, 0),        #ref: married
              partnered = ifelse(marital==3, 1, 0), 
              separated = ifelse(marital %in% c(4, 6), 1, 0), #6 = separated/divorced (very few cases)
              divorced = ifelse(marital==5, 1, 0), 
              widowed = ifelse(marital==7, 1, 0), 
              nevermarried = ifelse(marital==8, 1, 0), 
              ranchfarm = ifelse(farmranch==1, 1, 0),
              renter = ifelse(ownrent==2, 1, 0), #ref: own (or buying)
              rentfree = ifelse(ownrent==3, 1, 0), 
              rentother = ifelse(ownrent==7, 1, 0), 
              difficultpaybills = ifelse(difficultybills %in% c(4, 5), 1, 0), # very/complety difficult
              notusaborn = ifelse(bornusa==5, 1, 0),
              unsafeneighborhood = ifelse(safetyneighborhood %in% c(4, 5), 1, 0), # fair/poor safety
              secondhouse = ifelse(secondhome==1, 1, 0),
              foodstamps = dplyr::recode(foodstamps, `1`=1L, `5`=0L),
              loghhincome = log(replace(hhincome, hhincome<=1, 1)), #take log of income/wealth
              loghhwealth = log(replace(hhwealth, hhwealth<=1, 1)),
              healthexcellent = ifelse(selfratedhealth==5, 1, 0), # we recoded earlier / ref: good (middle category)
              healthverygood = ifelse(selfratedhealth==4, 1, 0), 
              healthfair = ifelse(selfratedhealth==2, 1, 0), 
              healthpoor = ifelse(selfratedhealth==1, 1, 0), 
              mobilehome = ifelse(typehome==1, 1, 0),   #ref:  2. = ONE-FAMILY HOUSE
              duplexhome = ifelse(typehome==3, 1, 0),  
              apartment = ifelse(typehome %in% c(4, 10), 1, 0),  # APARTMENT/TOWNHOUSE / Condo (number of units and structure unknown)
              homeother = ifelse(typehome %in% c(11, 12, 13, 97), 1, 0), 
              religyear = ifelse(attendreligion==4, 1, 0), # One or more times a year (ref: 5 = Not at all)
              religmonth = ifelse(attendreligion==3, 1, 0), # Two or three times a month
              religweek = ifelse(attendreligion==2, 1, 0), # Once a week
              religmore = ifelse(attendreligion==1, 1, 0), # More than once a week
              roomslessthree = ifelse(rooms==1, 1, 0), #  3 ROOMS OR LESS (ref: 3 = 6-7 rooms)
              roomsfourfive = ifelse(rooms==2, 1, 0), #  4-5 ROOMS
              roomsmoreeight = ifelse(rooms==4, 1, 0), #  8 ROOMS OR MORE
              paidwork = dplyr::recode(paidwork, `1`=1L, `5`=0L), # 1 = performs paid work
              selfemployed = dplyr::recode(selfemployed, `0`=0L, `1`=1L, `2`=0L),
              housekeeper = ifelse(jobstatus==6, 1, 0), # homemaker
              # children variables
              childrenclose = ifelse(children10m==1, 1, 0),
              kid1female = ifelse(KAGENDERBG_1==2, 1, 0), # if we filter to parents, all 0s will have a male firstborn
              kid2female = ifelse(KAGENDERBG_2==2, 1, 0), # there will be some 0s in here who do not have a 2nd child but these are captured in the model by 'secondkid' & 'thirdkid'
              kid3female = ifelse(KAGENDERBG_3==2, 1, 0), # same as kid2female
              kid1age = ifelse(nokids==0 & !is.na(KABYEARBG_1), interviewyear - KABYEARBG_1, NA), # not imputed (see above)
              kid2age = ifelse(secondkid==1 & KABYEARBG_2!=0, interviewyear - KABYEARBG_2, 0), # imputed
              # 5 observations where the birthyear of 2nd child lies before the interview
              kid2age = replace(kid2age, kid2age<0, 0), 
              kid3age = ifelse(thirdkid==1 & KABYEARBG_3!=0, interviewyear - KABYEARBG_3, 0), 
              # 3 cases for 3rd child
              kid3age = replace(kid3age, kid3age<0, 0), 
              kid1educ = ifelse(nokids==0, KAEDUC_1, 0), 
              kid2educ = ifelse(secondkid==1, KAEDUC_2, 0), 
              kid3educ = ifelse(thirdkid==1, KAEDUC_3, 0),
              livetogether = ifelse(livetogether %in% c(1,3), 1, 0)
  )) %>%
  map(~select(., -c(birthyr, race, laborforce, marital, children10m, farmranch, ownrent,
              difficultybills, bornusa, safetyneighborhood, secondhome,
              hhincome, hhwealth, selfratedhealth, attendreligion, 
              typehome, rooms, jobstatus, gender, coupleness, 
              starts_with("KA"), ))) # these are dropped because of recoding

# unlist and save as original data frames
x <- NULL;
for (i in seq_along(impdata)){
  x <- paste("hrsimp_matching", i, sep="_")
  eval(call("<-", as.name(x), impdata[[i]]))
}

#### PSM: compile datasets to be matched ####

# Big5/SWLS are administered every 4 years, shifted for half the sample. One half has
# 2006/2010/2014, and the other half has 2008, 2012, 2016 (given that they continually
# partake in the survey). 
# If we want to match at the last valid assessment before the transition to grandparenthood AND
# include Big5/SWLS as PSM covariates, there are two possible time points for matching:
# Option 1)  time==-2   --> In this case, the first (valid) assessment is at time==2.
# Option 2)  time==-4   --> In this case, the first (valid) assessment is at time==0.
# --> We will use 'valid' variable we coded earlier.

# We will perform 2 matchings: 
# 1) Grandparents-to-be matched with parents (but not grandparents) with at 
#    least one child in reproductive age (>=15) via 'MatchIt' R package
# 2) Grandparents-to-be matched with nonparents via 'MatchIt' R package

# We need to subset two datasets:
# 1) Grandparents-to-be (single observation at their last valid assessment before
#    the transition) & parents (but not grandparents) with at least one child in
#    reproductive age (all valid longitudinal observations)
# 2) Grandparents-to-be (single observation at their last valid assessment before
#    the transition) & nonparents (all valid longitudinal observations)

# 1) 
hrsimp_matching_1 %>% 
  group_by(grandparent, nokids) %>% summarise(n = n(), N = n_distinct(HHIDPN))
hrsimp_matching_1 %>% 
  filter((grandparent==1) | 
         (grandparent==0 & nokids==0)) %>% 
  group_by(grandparent, nokids) %>% summarise(n = n(), N = n_distinct(HHIDPN))
hrsimp_matching_1 %>% 
  filter((grandparent==1 & nokids==0) | 
         (grandparent==0 & nokids==0 & kid1age %in% c(15:65))) %>% 
  group_by(grandparent, nokids) %>% summarise(n = n(), N = n_distinct(HHIDPN))
hrsimp_matching_1 %>% 
  filter((grandparent==1 & nokids==0 & valid==-1 & droplater==F) | 
         (grandparent==0 & nokids==0 & kid1age %in% c(15:65))) %>% 
  group_by(grandparent, nokids) %>% summarise(n = n(), N = n_distinct(HHIDPN))

# Compared to the (old) method that does not filter by 'nokids==0', we 
# drop 10 grandparents (these are not contained in RAND HRS Family data and
# have 0 kids according to 'totalresidentkids' & 'totalnonresidentkids'):
hrsimp_matching_1 %>% filter(grandparent==1 & nokids==1 & valid==-1 & droplater==F)

# 2)
hrsimp_matching_1 %>% 
  group_by(grandparent, nokids) %>% summarise(n = n(), N = n_distinct(HHIDPN))
hrsimp_matching_1 %>% 
  filter((grandparent==1 & nokids==0 & valid==-1 & droplater==F) | # GP group is the same!
         (grandparent==0 & nokids==1)) %>% 
  group_by(grandparent, nokids) %>% summarise(n = n(), N = n_distinct(HHIDPN))
# Control group is relatively small here
# Alternatively, we could rely on just the 'nokids' information and infer
# that these respondents do not have grandkids.
hrsimp_matching_1 %>% 
  filter((grandparent==1 & nokids==0 & valid==-1 & droplater==F) | # GP group is the same!
         (grandparent %in% c(0, NA) & nokids==1)) %>% 
  group_by(grandparent, nokids) %>% summarise(n = n(), N = n_distinct(HHIDPN))
# This way, we would gain 418 observations (N=350).

# subsetting imputed datasets:

# parents
hrsimp_parents_ps_1 <- hrsimp_matching_1 %>% 
  filter((grandparent==1 & nokids==0 & valid==-1 & droplater==F & !is.na(kid1age)) | 
         (grandparent==0 & nokids==0 & kid1age %in% c(15:65))) %>% 
  select(-c(droplater, time, valid, nokids))
# two GP observations (500298010, 500298020) have NA for kid1age
hrslongvalid %>% filter(HHIDPN %in% c(500298010, 500298020)) %>% print(width=Inf)
# these report having kids for the first time in 2014 (but have personality assessments at 2008, 2012, 2016)

# sample size before matching:
hrsimp_parents_ps_1 %>% 
  group_by(grandparent) %>% summarise(n = n(), N = n_distinct(HHIDPN))

# build a data frame that I can later import into the .Rmd file in order to report the sample size flow
load(file = "data/processed/HRS/hrs_sampleflow_gp.rda")
if(nrow(hrs_sampleflow_gp)==3){ # added this condition in case I only execute the later parts of the script
  hrs_sampleflow_gp[nrow(hrs_sampleflow_gp)+1, ] <- 
    c(4, (hrsimp_parents_ps_1 %>% group_by(grandparent) %>% summarise(n = n(), N = n_distinct(HHIDPN)))[2,3]) # add step 4
}
save(hrs_sampleflow_gp, file = "data/processed/HRS/hrs_sampleflow_gp.rda") # save for later import

# another one for the non-grandparent control subjects
hrs_sampleflow_nongp <- data.frame(group = numeric(0), obs = numeric(0), n = numeric(0))
hrs_sampleflow_nongp[nrow(hrs_sampleflow_nongp)+1, ] <- 
  c(1, (hrsimp_parents_ps_1 %>% group_by(grandparent) %>% 
          summarise(n = n(), N = n_distinct(HHIDPN)))[1, c(2,3)]) # add parent control group

# nonparents
hrsimp_nonparents_ps_1 <- hrsimp_matching_1 %>% 
  filter((grandparent==1 & nokids==0 & valid==-1 & droplater==F & !is.na(kid1age)) | 
         (grandparent %in% c(0, NA) & nokids==1)) %>% 
  select(-c(droplater, time, valid, nokids, childrenclose, contains("kid"))) %>% 
  mutate(grandparent = replace_na(grandparent, 0)) 
# Because we know that that these respondents have no children, we can infer 
# that their grandparent status is 0.

# sample size before matching:
hrsimp_nonparents_ps_1 %>% 
  group_by(grandparent) %>% summarise(n = n(), N = n_distinct(HHIDPN))

# for the non-grandparent control subjects
hrs_sampleflow_nongp[nrow(hrs_sampleflow_nongp)+1, ] <- 
  c(2, (hrsimp_nonparents_ps_1 %>% group_by(grandparent) %>% 
          summarise(n = n(), N = n_distinct(HHIDPN)))[1, c(2,3)]) # add nonparent control group
hrs_sampleflow_nongp$group <- factor(c("parents", "nonparents")) 
save(hrs_sampleflow_nongp, file = "data/processed/HRS/hrs_sampleflow_nongp.rda") # save for later import

#same for the other imputations...
# 2  
hrsimp_parents_ps_2 <- hrsimp_matching_2 %>% 
  filter((grandparent==0 & nokids==0 & kid1age %in% c(15:65)) | 
         (grandparent==1 & nokids==0 & valid==-1 & droplater==F & !is.na(kid1age))) %>% 
  select(-c(droplater, time, valid, nokids))

hrsimp_nonparents_ps_2 <- hrsimp_matching_2 %>% 
  filter((grandparent %in% c(0, NA) & nokids==1) | 
         (grandparent==1 & nokids==0 & valid==-1 & droplater==F & !is.na(kid1age))) %>% 
  select(-c(droplater, time, valid, nokids, childrenclose, contains("kid"))) %>% 
  mutate(grandparent = replace_na(grandparent, 0)) 

# 3
hrsimp_parents_ps_3 <- hrsimp_matching_3 %>% 
  filter((grandparent==0 & nokids==0 & kid1age %in% c(15:65)) | 
         (grandparent==1 & nokids==0 & valid==-1 & droplater==F & !is.na(kid1age))) %>% 
  select(-c(droplater, time, valid, nokids))

hrsimp_nonparents_ps_3 <- hrsimp_matching_3 %>% 
  filter((grandparent %in% c(0, NA) & nokids==1) | 
         (grandparent==1 & nokids==0 & valid==-1 & droplater==F & !is.na(kid1age))) %>% 
  select(-c(droplater, time, valid, nokids, childrenclose, contains("kid"))) %>% 
  mutate(grandparent = replace_na(grandparent, 0)) 

# 4
hrsimp_parents_ps_4 <- hrsimp_matching_4 %>% 
  filter((grandparent==0 & nokids==0 & kid1age %in% c(15:65)) | 
         (grandparent==1 & nokids==0 & valid==-1 & droplater==F & !is.na(kid1age))) %>% 
  select(-c(droplater, time, valid, nokids))

hrsimp_nonparents_ps_4 <- hrsimp_matching_4 %>% 
  filter((grandparent %in% c(0, NA) & nokids==1) | 
         (grandparent==1 & nokids==0 & valid==-1 & droplater==F & !is.na(kid1age))) %>% 
  select(-c(droplater, time, valid, nokids, childrenclose, contains("kid"))) %>% 
  mutate(grandparent = replace_na(grandparent, 0)) 

# 5
hrsimp_parents_ps_5 <- hrsimp_matching_5 %>% 
  filter((grandparent==0 & nokids==0 & kid1age %in% c(15:65)) | 
         (grandparent==1 & nokids==0 & valid==-1 & droplater==F & !is.na(kid1age))) %>% 
  select(-c(droplater, time, valid, nokids))

hrsimp_nonparents_ps_5 <- hrsimp_matching_5 %>% 
  filter((grandparent %in% c(0, NA) & nokids==1) | 
         (grandparent==1 & nokids==0 & valid==-1 & droplater==F & !is.na(kid1age))) %>% 
  select(-c(droplater, time, valid, nokids, childrenclose, contains("kid"))) %>% 
  mutate(grandparent = replace_na(grandparent, 0)) 


# Sadly, some dummy covariates I picked have a very one-sided distribution in the (smaller) GP group where 
# some variables have 0 (or only 1, 2, or 3) cases in one group. Example:
table(hrsimp_parents_ps_1$grandparent, hrsimp_parents_ps_1$rentother)
# Later on, this leads to problems with logistic regression:
# https://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression
# Therefore, I'll remove all these variables here. (see "Overview covariates.xlsx" -> Sheet "Frequencies")
remove_infrequent_hrs <- function(x) { 
  x %>% 
    select(-c(spouseabsent, rentother, homeother), # too infrequent
           -c(hhmembers, marriagesnum, doctor, hospital, psyche, bmi, cancer, # these are not important (substantively) / or redundant
              diabetes, stroke, heart, hiemployer, higovt, hispousal, hiother, 
              partnered, separated, nevermarried, ranchfarm, rentfree, mobilehome, 
              duplexhome, apartment, homeother, housekeeper, 
              fulltime, parttime, unemployed, partlyretired, disabled, notinlaborforce,
              unsafeneighborhood, secondhouse, selfemployed, # 
              difficultpaybills, foodstamps)) }

list_remove_infrequent_hrs <- list(hrsimp_parents_ps_1, hrsimp_parents_ps_2, 
                                    hrsimp_parents_ps_3, hrsimp_parents_ps_4, 
                                    hrsimp_parents_ps_5,
                                    hrsimp_nonparents_ps_1, hrsimp_nonparents_ps_2, 
                                    hrsimp_nonparents_ps_3, hrsimp_nonparents_ps_4, 
                                    hrsimp_nonparents_ps_5) %>%
  lapply(remove_infrequent_hrs)

names(list_remove_infrequent_hrs) <- c("hrsimp_parents_ps_1", "hrsimp_parents_ps_2", 
                                        "hrsimp_parents_ps_3", "hrsimp_parents_ps_4", 
                                        "hrsimp_parents_ps_5",
                                        "hrsimp_nonparents_ps_1", "hrsimp_nonparents_ps_2", 
                                        "hrsimp_nonparents_ps_3", "hrsimp_nonparents_ps_4", 
                                        "hrsimp_nonparents_ps_5")
list2env(list_remove_infrequent_hrs, .GlobalEnv)
rm(list_remove_infrequent_hrs)


#### PSM: compute propensity scores ####

# 1st step -> estimate propensity scores for all obs.
# do 5 times, once for each of m=5 imputations, then compute mean PS (see Mitra 2016)

# 1) Parent control group
ps_model_parents <- as.formula("grandparent ~ . - year - female - HHIDPN") # all vars but ...

ps_logit_parents_m1 <- glm(ps_model_parents, family = binomial(link='logit'), data = hrsimp_parents_ps_1)
summary(ps_logit_parents_m1)
ps_logit_parents_m2 <- glm(ps_model_parents, family = binomial(link='logit'), data = hrsimp_parents_ps_2)
summary(ps_logit_parents_m2)
ps_logit_parents_m3 <- glm(ps_model_parents, family = binomial(link='logit'), data = hrsimp_parents_ps_3)
summary(ps_logit_parents_m3)
ps_logit_parents_m4 <- glm(ps_model_parents, family = binomial(link='logit'), data = hrsimp_parents_ps_4)
summary(ps_logit_parents_m4)
ps_logit_parents_m5 <- glm(ps_model_parents, family = binomial(link='logit'), data = hrsimp_parents_ps_5)
summary(ps_logit_parents_m5)

#proceed with one of the m=5 imputation dataset 
# (does not matter which one - imputation was only used for PS computation)
hrsimp_parents_ps_2$pscore_m1 <- fitted(ps_logit_parents_m1)
hrsimp_parents_ps_2$pscore_m2 <- fitted(ps_logit_parents_m2)
hrsimp_parents_ps_2$pscore_m3 <- fitted(ps_logit_parents_m3)
hrsimp_parents_ps_2$pscore_m4 <- fitted(ps_logit_parents_m4)
hrsimp_parents_ps_2$pscore_m5 <- fitted(ps_logit_parents_m5)
hrsimp_matching_parents <- hrsimp_parents_ps_2 %>% 
  mutate(pscore = (pscore_m1 + pscore_m2 + pscore_m3 + pscore_m4 + pscore_m5)/5)
# compute mean propensity score of imp=5 imputed datasets 

hrsimp_matching_parents <- hrsimp_matching_parents %>% 
  select(HHIDPN, year, female, grandparent, pscore) 

# 2) Nonparent control group
ps_model_nonparents <- as.formula("grandparent ~ . - year - female - HHIDPN") # all vars but ...
# have already dropped the all kid-related covariates from the nonparent samples 

ps_logit_nonparents_m1 <- glm(ps_model_nonparents, family = binomial(link='logit'), data = hrsimp_nonparents_ps_1)
summary(ps_logit_nonparents_m1)
ps_logit_nonparents_m2 <- glm(ps_model_nonparents, family = binomial(link='logit'), data = hrsimp_nonparents_ps_2)
summary(ps_logit_nonparents_m2)
ps_logit_nonparents_m3 <- glm(ps_model_nonparents, family = binomial(link='logit'), data = hrsimp_nonparents_ps_3)
summary(ps_logit_nonparents_m3)
ps_logit_nonparents_m4 <- glm(ps_model_nonparents, family = binomial(link='logit'), data = hrsimp_nonparents_ps_4)
summary(ps_logit_nonparents_m4)
ps_logit_nonparents_m5 <- glm(ps_model_nonparents, family = binomial(link='logit'), data = hrsimp_nonparents_ps_5)
summary(ps_logit_nonparents_m5)

#proceed with one of the m=5 imputation dataset 
# (does not matter which one - imputation was only used for PS computation)
hrsimp_nonparents_ps_2$pscore_m1 <- fitted(ps_logit_nonparents_m1)
hrsimp_nonparents_ps_2$pscore_m2 <- fitted(ps_logit_nonparents_m2)
hrsimp_nonparents_ps_2$pscore_m3 <- fitted(ps_logit_nonparents_m3)
hrsimp_nonparents_ps_2$pscore_m4 <- fitted(ps_logit_nonparents_m4)
hrsimp_nonparents_ps_2$pscore_m5 <- fitted(ps_logit_nonparents_m5)
hrsimp_matching_nonparents <- hrsimp_nonparents_ps_2 %>% 
  mutate(pscore = (pscore_m1 + pscore_m2 + pscore_m3 + pscore_m4 + pscore_m5)/5)
# compute mean propensity score of imp=5 imputed datasets 

hrsimp_matching_nonparents <- hrsimp_matching_nonparents %>% 
  select(HHIDPN, year, female, grandparent, pscore) 

# to help with memory capacity issues
rm(h96data, h98data, h00data, h02data, h04data, h06data, 
   h08data, h10data, h12data, h14data, h16data) 


#### PSM: 'matchit' with replacement -> (1) parent control group ####

table(hrsimp_matching_parents$grandparent)
table(hrsimp_matching_parents$grandparent, hrsimp_matching_parents$year)

#we also need the 'time' value for grandparents (either -4 or -2) and the 'valid' variable
hrsimp_parents <- left_join(hrsimp_matching_parents, 
                                       hrsimp_matching_1, by=c("HHIDPN", "year")) %>% 
  filter(droplater==F) %>% 
  select(HHIDPN, year, female.x, grandparent.x, pscore, time, valid) %>% 
  rename(female = female.x, grandparent = grandparent.x)

table(hrsimp_parents$grandparent, hrsimp_parents$time)
table(hrsimp_parents$grandparent, hrsimp_parents$valid)

# perform matching on previously computed propensity score which is stored in 'pscore'
hrs_parents_matchit <- matchit(grandparent ~ pscore, data=hrsimp_parents, distance="mahalanobis",
                               replace=T, exact=c("female"), ratio=4) # exact matching on gender
hrs_parents_matchit
summary(hrs_parents_matchit) # with replacement
hrs_data_parents <- get_matches(hrs_parents_matchit, data=hrsimp_parents)
str(hrs_data_parents)

hrs_data_parents <- hrs_data_parents %>% group_by(subclass) %>% 
  mutate(time = ifelse(is.na(time), max(time, na.rm = T), time),
         valid = ifelse(is.na(valid), max(valid, na.rm = T), valid)) %>% ungroup %>% 
  ungroup() %>% select(-weights, -id) # -subclass,  -- need subclass later for recoding of moderator care

# "with replacement" in two ways: 
# - the same control observation appearing multiple times in the matched data 
#   (this is because we allowed "replace=T" above)
# - control subjects appearing multiple times in the matched data 
#   (because there were multiple available obervations, i.e., time points, per control subject in the data)
hrs_data_parents %>% group_by(grandparent) %>% summarise(N=n_distinct(HHIDPN))
hrs_data_parents %>% group_by(grandparent) %>% summarise(N=n_distinct(HHIDPN, year)) 
# matches number above, see  summary(hrs_parents_matchit)

# build a data.frame for import of these numbers to .Rmd (papaja)
hrs_replacement_controls <- data.frame(group = numeric(0), obs = numeric(0), n = numeric(0))
hrs_replacement_controls[nrow(hrs_replacement_controls)+1, ] <- 
  c(1,
    (hrs_data_parents %>% group_by(grandparent) %>% summarise(N=n_distinct(HHIDPN, year)))[1,2],
    (hrs_data_parents %>% group_by(grandparent) %>% summarise(N=n_distinct(HHIDPN)))[1,2]) # add parent control group

table(hrs_data_parents$grandparent, hrs_data_parents$year)
table(hrs_data_parents$grandparent, hrs_data_parents$time)
table(hrs_data_parents$grandparent, hrs_data_parents$valid)
table(hrs_data_parents$grandparent, hrs_data_parents$female)

hrs_data_parents <- left_join(hrs_data_parents, hrsimp_parents_ps_1,
                                         by = c("HHIDPN", "year", "grandparent", "female"))

# for balance assessment (at the time of matching - using the variables containing imputed values)
hrs_bal_parents <- hrs_data_parents %>%
  select(HHIDPN, grandparent, pscore, female, everything(), -time, -year, -valid, -subclass)

hrs_data_parents <- hrs_data_parents %>% 
  select(HHIDPN, year, grandparent, time, valid, pscore, subclass) %>% 
  rename(match_year = year, time_match = time, valid_match = valid) %>% 
  mutate(match_number = row_number()) # if we allow duplicate matches, we need an unambiguous identifier for later

# compile analysis sample with all longitudinal observations
hrsanalysis_parents <- left_join(hrs_data_parents, hrslongvalid,
                                            by = c("HHIDPN", "grandparent")) %>% select(-droplater)

# create time variable for controls relative to the time point of matching 
hrsanalysis_parents <- hrsanalysis_parents %>% mutate(
  time = replace(time, is.na(time) & match_year==year & time_match==-4, -4),
  # variable 'match_year' relates to the controls!
  valid = replace(valid, is.na(valid) & match_year==year, -1)
)
hrsanalysis_parents <- hrsanalysis_parents %>% mutate(
  time = replace(time, is.na(time) & match_year==year & time_match==-2, -2)
)
table(hrsanalysis_parents$grandparent, hrsanalysis_parents$time)

hrsanalysis_parents <- hrsanalysis_parents %>% filter(!is.na(pscore)) %>% 
  mutate(
    time = ifelse(grandparent==0 & is.na(time) & time_match==-4, (year - match_year) - 4, time),
    time = ifelse(grandparent==0 & is.na(time) & time_match==-2, (year - match_year) - 2, time))

hrsanalysis_parents <- hrsanalysis_parents %>% group_by(match_number) %>% 
  mutate(lastcount = ifelse(grandparent==0 & valid==-1, row_number(), NA)) %>% ungroup()
hrsanalysis_parents <- hrsanalysis_parents %>% group_by(match_number) %>% 
  mutate(lastcount = max(lastcount, na.rm = T)) %>% ungroup() %>% 
  mutate(lastcount = replace(lastcount, lastcount==-Inf, NA)) # regular NA pls

# finish coding 'valid' for controls
# grouping by 'match_number' here instead of 'HHIDPN' because we allowed duplicate matches
hrsanalysis_parents <- hrsanalysis_parents %>% group_by(match_number) %>% 
  mutate(helpcount = row_number()) %>% ungroup()
hrsanalysis_parents <- hrsanalysis_parents %>% 
  mutate(valid = ifelse(is.na(valid) & grandparent==0, helpcount - lastcount - 1, valid)) %>% 
  select(-helpcount, -lastcount)

table(hrsanalysis_parents$grandparent, hrsanalysis_parents$time)
table(hrsanalysis_parents$grandparent, hrsanalysis_parents$valid)
table(hrsanalysis_parents$grandparent, hrsanalysis_parents$year)

hrsanalysis_parents <- hrsanalysis_parents %>% filter(time %in% c(-6:6)) %>% 
  select(-match_year, -valid_match)

# save .rda 
save(hrsanalysis_parents, file = "data/processed/HRS/hrsanalysis_parents.rda")
hrsanalysis_parents %>% group_by(grandparent) %>% summarise(N = n_distinct(HHIDPN))
# duplicates in the controls


#### PSM: 'matchit' with replacement -> (2) nonparent control group ####

table(hrsimp_matching_nonparents$grandparent)
table(hrsimp_matching_nonparents$grandparent, hrsimp_matching_nonparents$year)

#we also need the 'time' value for grandparents (either -4 or -2) and the 'valid' variable
hrsimp_nonparents <- left_join(hrsimp_matching_nonparents, 
                                          hrsimp_matching_1, by=c("HHIDPN", "year")) %>% 
  filter(droplater==F) %>% 
  select(HHIDPN, year, female.x, grandparent.x, pscore, time, valid) %>% 
  rename(female = female.x, grandparent = grandparent.x)

table(hrsimp_nonparents$grandparent, hrsimp_nonparents$time)
table(hrsimp_nonparents$grandparent, hrsimp_nonparents$valid)

# perform matching on previously computed propensity score which is stored in 'pscore'
hrs_nonparents_matchit <- matchit(grandparent ~ pscore, data=hrsimp_nonparents, distance="mahalanobis",
                               replace=T, exact=c("female"), ratio=4) # exact matching on gender
hrs_nonparents_matchit
summary(hrs_nonparents_matchit) # with replacement
hrs_data_nonparents <- get_matches(hrs_nonparents_matchit, data=hrsimp_nonparents)
str(hrs_data_nonparents)

hrs_data_nonparents <- hrs_data_nonparents %>% group_by(subclass) %>% 
  mutate(time = ifelse(is.na(time), max(time, na.rm = T), time),
         valid = ifelse(is.na(valid), max(valid, na.rm = T), valid)) %>% ungroup %>% 
  ungroup() %>% select(-weights, -id) # -subclass,  -- need subclass later for recoding of moderator care

# "with replacement" in two ways: 
# - the same control observation appearing multiple times in the matched data 
#   (this is because we allowed "replace=T" above)
# - control subjects appearing multiple times in the matched data 
#   (because there were multiple available obervations, i.e., time points, per control subject in the data)
hrs_data_nonparents %>% group_by(grandparent) %>% summarise(N=n_distinct(HHIDPN))
hrs_data_nonparents %>% group_by(grandparent) %>% summarise(N=n_distinct(HHIDPN, year)) 
# matches number above, see  summary(hrs_nonparents_matchit)

# build a data.frame for import of these numbers to .Rmd (papaja)
hrs_replacement_controls[nrow(hrs_replacement_controls)+1, ] <- 
  c(2,
    (hrs_data_nonparents %>% group_by(grandparent) %>% summarise(N=n_distinct(HHIDPN, year)))[1,2],
    (hrs_data_nonparents %>% group_by(grandparent) %>% summarise(N=n_distinct(HHIDPN)))[1,2]) # add nonparent control group
hrs_replacement_controls$group <- factor(c("parents", "nonparents")) 
save(hrs_replacement_controls, file = "data/processed/HRS/hrs_replacement_controls.rda") # save for later import

table(hrs_data_nonparents$grandparent, hrs_data_nonparents$year)
table(hrs_data_nonparents$grandparent, hrs_data_nonparents$time)
table(hrs_data_nonparents$grandparent, hrs_data_nonparents$valid)
table(hrs_data_nonparents$grandparent, hrs_data_nonparents$female)

hrs_data_nonparents <- left_join(hrs_data_nonparents, hrsimp_nonparents_ps_1,
                                            by = c("HHIDPN", "year", "grandparent", "female"))

# for balance assessment (at the time of matching - using the variables containing imputed values)
hrs_bal_nonparents <- hrs_data_nonparents %>%
  select(HHIDPN, grandparent, pscore, female, everything(), -time, -year, -valid, -subclass)

hrs_data_nonparents <- hrs_data_nonparents %>% 
  select(HHIDPN, year, grandparent, time, valid, pscore, subclass) %>% 
  rename(match_year = year, time_match = time, valid_match = valid) %>% 
  mutate(match_number = row_number()) # if we allow duplicate matches, we need an unambiguous identifier for later

# compile analysis sample with all longitudinal observations
# same adjustment (NA -> 0) as for the other matching method (see above)
hrsanalysis_nonparents <- hrslongvalid %>% 
  mutate(grandparent = replace(grandparent, totalnonresidentkids==0 & 
                                 totalresidentkids==0 & is.na(has_ch1) & 
                                 is.na(grandparent), 0)) %>% # adjusting grandparent status
  filter(!is.na(grandparent) & droplater!=T) 

hrsanalysis_nonparents <- left_join(hrs_data_nonparents, 
                                               hrsanalysis_nonparents,
                                               by = c("HHIDPN", "grandparent")) %>% select(-droplater)

table(hrsanalysis_nonparents$grandparent, hrsanalysis_nonparents$time_match) # already transferred to controls 
table(hrsanalysis_nonparents$grandparent, hrsanalysis_nonparents$valid)
table(hrsanalysis_nonparents$grandparent, hrsanalysis_nonparents$valid_match)

# create time variable for controls relative to the time point of matching 
hrsanalysis_nonparents <- hrsanalysis_nonparents %>% mutate(
  time = replace(time, is.na(time) & match_year==year & time_match==-4, -4),
  # variable 'match_year' relates to the controls!
  valid = replace(valid, is.na(valid) & match_year==year, -1)
)
hrsanalysis_nonparents <- hrsanalysis_nonparents %>% mutate(
  time = replace(time, is.na(time) & match_year==year & time_match==-2, -2)
)
table(hrsanalysis_nonparents$time, hrsanalysis_nonparents$grandparent)

hrsanalysis_nonparents <- hrsanalysis_nonparents %>% filter(!is.na(pscore)) %>% 
  mutate(
    time = ifelse(grandparent==0 & is.na(time) & time_match==-4, (year - match_year) - 4, time),
    time = ifelse(grandparent==0 & is.na(time) & time_match==-2, (year - match_year) - 2, time))

hrsanalysis_nonparents <- hrsanalysis_nonparents %>% group_by(match_number) %>% 
  mutate(lastcount = ifelse(grandparent==0 & valid==-1, row_number(), NA)) %>% ungroup()
hrsanalysis_nonparents <- hrsanalysis_nonparents %>% group_by(match_number) %>% 
  mutate(lastcount = max(lastcount, na.rm = T)) %>% ungroup() %>% 
  mutate(lastcount = replace(lastcount, lastcount==-Inf, NA)) # regular NA pls

# finish coding 'valid' for controls
# grouping by 'match_number' here instead of 'HHIDPN' because we allowed duplicate matches
hrsanalysis_nonparents <- hrsanalysis_nonparents %>% group_by(match_number) %>% 
  mutate(helpcount = row_number()) %>% ungroup()
hrsanalysis_nonparents <- hrsanalysis_nonparents %>% 
  mutate(valid = ifelse(is.na(valid) & grandparent==0, helpcount - lastcount - 1, valid)) %>% 
  select(-helpcount, -lastcount)

table(hrsanalysis_nonparents$grandparent, hrsanalysis_nonparents$time)
table(hrsanalysis_nonparents$grandparent, hrsanalysis_nonparents$valid)
table(hrsanalysis_nonparents$grandparent, hrsanalysis_nonparents$year)

hrsanalysis_nonparents <- hrsanalysis_nonparents %>% filter(time %in% c(-6:6)) %>% 
  select(-match_year, -valid_match)

# save .rda 
save(hrsanalysis_nonparents, file = "data/processed/HRS/hrsanalysis_nonparents.rda")
hrsanalysis_nonparents %>% group_by(grandparent) %>% summarise(N = n_distinct(HHIDPN)) 
# duplicates in the controls


#### PSM: covariate balance assessment ####

# balance after matching: stand. mean differences
# Standardized differences between means ((mu_t-mu_c)/sd_t)<0.50 (sometimes <0.25 is rule of thumb)
# according to Gary T. Henry's slides it's sd_c, but according to Stuart (2010) it should be sd_t

# define function:
stdmeandiff <- function(var, treat, data) {
  mu_t <- eval(substitute(mean(var[treat==1], na.rm=T)), data)
  mu_c <- eval(substitute(mean(var[treat==0], na.rm=T)), data)
  sd_t <- eval(substitute(sd(var[treat==1], na.rm=T)), data)
  (mu_t - mu_c) / sd_t
}

# balance BEFORE matching
hrs_bal_parents_before <- hrsimp_parents_ps_2 %>% # this already has the correct number of covars
  mutate(pscore = (pscore_m1 + pscore_m2 + pscore_m3 + pscore_m4 + pscore_m5)/5) %>% 
  select(-starts_with("pscore_m"), -year) %>% 
  select(HHIDPN, grandparent, pscore, female, everything())
summary(hrs_bal_parents_before)

names(hrs_bal_parents_before) # column names must be aligned!
names(hrs_bal_parents)

hrs_bal_nonparents_before <- hrsimp_nonparents_ps_2 %>% # this already has the correct number of covars
  mutate(pscore = (pscore_m1 + pscore_m2 + pscore_m3 + pscore_m4 + pscore_m5)/5) %>% 
  select(-starts_with("pscore_m"), -year) %>% 
  select(HHIDPN, grandparent, pscore, female, everything())
summary(hrs_bal_nonparents_before)

names(hrs_bal_nonparents_before) # column names must be aligned!
names(hrs_bal_nonparents)

# evaluate standardized difference in means
# create maxtrix object with empty vectors 'stddiff'
# (1) PARENTS
varnum_parents <-  1:(length(hrs_bal_parents)-2)
covar_parents <-  colnames(hrs_bal_parents[3:paste(length(hrs_bal_parents))])
stddiff_before_parents <- numeric(length = length(hrs_bal_parents_before)-2)     #before matching
stddiff_after1_parents <- numeric(length = length(hrs_bal_parents)-2)            #after matching

coln_parents <- c("varnum_parents", "covar_parents", "stddiff_before_parents", 
                  "stddiff_after1_parents") # defining column names 
# creating matrix 
hrs_balance_matrix_parents <- matrix(c(varnum_parents, covar_parents, stddiff_before_parents,
                                       stddiff_after1_parents), ncol = 4, 
                                       dimnames = list(varnum_parents, coln_parents))

# use custom function in for-loop to fill matrix vectors 'stddiff'
for (i in seq_along(hrs_balance_matrix_parents[varnum_parents])) {
  hrs_balance_matrix_parents[[i, 3]] <- stdmeandiff(get(hrs_balance_matrix_parents[[i, 2]]), 
                                                    grandparent, hrs_bal_parents_before)     #before matching
  hrs_balance_matrix_parents[[i, 4]] <- stdmeandiff(get(hrs_balance_matrix_parents[[i, 2]]), 
                                                    grandparent, hrs_bal_parents)            #after matching
  }
#hrs_balance_matrix_parents[, 3:4] <- round(as.numeric(hrs_balance_matrix_parents[, 3:4]), 3)

#kable(hrs_balance_matrix_parents[, 2:4], format="rst", 
#      col.names = c("Covariate", "Before Matching",
#                    "After Matching"), 
#      align = "lcc", digits=2, caption = "Table 1. Covariate Balance")


# (2) NONPARENTS

# create maxtrix object with empty vectors 'stddiff'
varnum_nonparents <-  1:(length(hrs_bal_nonparents)-2)
covar_nonparents <-  colnames(hrs_bal_nonparents[3:paste(length(hrs_bal_nonparents))])
stddiff_before_nonparents <- numeric(length = length(hrs_bal_nonparents_before)-2)     #before matching
stddiff_after1_nonparents <- numeric(length = length(hrs_bal_nonparents)-2)            #after matching

coln_nonparents <- c("varnum_nonparents", "covar_nonparents", "stddiff_before_nonparents", 
                     "stddiff_after1_nonparents") # defining column names 
# creating matrix 
hrs_balance_matrix_nonparents <- matrix(c(varnum_nonparents, covar_nonparents, stddiff_before_nonparents,
                                       stddiff_after1_nonparents), ncol = 4, 
                                       dimnames = list(varnum_nonparents, coln_nonparents))

# use custom function in for-loop to fill matrix vectors 'stddiff'
for (i in seq_along(hrs_balance_matrix_nonparents[varnum_nonparents])) {
  hrs_balance_matrix_nonparents[[i, 3]] <- stdmeandiff(get(hrs_balance_matrix_nonparents[[i, 2]]), 
                                                    grandparent, hrs_bal_nonparents_before)     #before matching
  hrs_balance_matrix_nonparents[[i, 4]] <- stdmeandiff(get(hrs_balance_matrix_nonparents[[i, 2]]), 
                                                    grandparent, hrs_bal_nonparents)            #after matching
}
#hrs_balance_matrix_nonparents[, 3:4] <- round(as.numeric(hrs_balance_matrix_nonparents[, 3:4]), 3)

#kable(hrs_balance_matrix_nonparents[, 2:4], format="rst", 
#      col.names = c("Covariate", "Before Matching",
#                    "After Matching"), 
#      align = "lcc", digits=2, caption = "Table 1. Covariate Balance")

# build a common data.frame for later import into .Rmd (papaja)
hrs_balance_parents_df <- as.data.frame(hrs_balance_matrix_parents)[2:4]
hrs_balance_nonparents_df <- as.data.frame(hrs_balance_matrix_nonparents)[2:4]
colnames(hrs_balance_parents_df)[1] <- "Covariate"
colnames(hrs_balance_nonparents_df)[1] <- "Covariate"
hrs_balance_df <- full_join(hrs_balance_parents_df, hrs_balance_nonparents_df)
hrs_balance_df <- hrs_balance_df %>% mutate_at(vars(-Covariate), funs(as.numeric))
# draw information from excel sheet 'gp-covariates-overview.xlsx' (& correct order of covariates)
covar_info <- read_excel("gp-covariates-overview.xlsx", sheet = 1, range = "B4:D62",
                         col_names = c("Description", "Raw variable", "Covariate")) %>% 
  filter(!is.na(Description))

hrs_balance_df <- left_join(covar_info,
                            hrs_balance_df,
                            by = "Covariate") %>% # sets correct order
  select(Covariate, everything())
save(hrs_balance_df, file = "data/processed/HRS/hrs_balance_df.rda")

