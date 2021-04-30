### Transition to Grandparenthood Paper - LISS Sample - performing PSM and compiling final analysis samples ###

# run 'gp-compilesample-imp-LISS.R' first to obtain the files that are loaded here 
# (imputations have a very long run time)

library(tidyverse)
library(MatchIt)

load(file = "data/processed/LISS/lisslong_cleaned.rda")
load(file = "data/processed/LISS/lisslong_valid.rda")

#### PSM: datasets and final covariate preparation ####

# load .rda
load(file = "data/processed/LISS/lissimp_matching_1.rda")
load(file = "data/processed/LISS/lissimp_matching_2.rda")
load(file = "data/processed/LISS/lissimp_matching_3.rda")
load(file = "data/processed/LISS/lissimp_matching_4.rda")
load(file = "data/processed/LISS/lissimp_matching_5.rda")

# recode PSM covariates
# a lot of the variables need recoding before they can be fed into the PSM model

# create list of all 5 imputation datasets
impdata <- lapply(ls(pattern="lissimp_matching_[0-9]+"), function(x) get(x))

# categorical nominal variables with >2 categories are transformed into k-1 dummies 
# --> largest category is reference category without dummy

# do recoding on this list object via 'map'
impdata <- impdata %>%
  map(~mutate(., rental = ifelse(ownrent==2, 1, 0), #ref: self-owned
              rentfree = ifelse(ownrent==4, 1, 0),
              flatapartment = ifelse(typedwelling==5, 1, 0), #ref: single family home
              farmhouse = ifelse(typedwelling==6, 1, 0), 
              businessdwelling = ifelse(typedwelling %in% c(7, 8), 1, 0),
              otherdwelling = ifelse(typedwelling %in% c(9, 10), 1, 0), 
              familybusiness = ifelse(employment_status==2, 1, 0), #ref: Paid employment 
              freelancer = ifelse(employment_status==3, 1, 0), 
              jobseeker  = ifelse(employment_status %in%c(4, 5, 6), 1, 0), 
              inschool = ifelse(employment_status==7, 1, 0), 
              housekeeper = ifelse(employment_status==8, 1, 0), 
              pensioner = ifelse(employment_status==9, 1, 0), 
              disability = ifelse(employment_status==10, 1, 0), 
              voluntary = ifelse(employment_status %in% c(11, 12), 1, 0), 
              jobother = ifelse(employment_status %in% c(13, 14), 1, 0), 
              primaryschool = ifelse(education==1, 1, 0), #ref: vmbo (intermediate secondary education, US: junior high school)
              degreehighersec = ifelse(education==3, 1, 0), 
              degreevocational = ifelse(education==4, 1, 0), 
              degreecollege = ifelse(education==5, 1, 0), 
              degreeuniversity = ifelse(education==6, 1, 0), 
              degreeother = ifelse(education==7, 1, 0),
              nodegreeyet = ifelse(education %in% c(8, 9), 1, 0),
              separated = ifelse(marital==2, 1, 0), #ref: married
              divorced = ifelse(marital==3, 1, 0), 
              widowed = ifelse(marital==4, 1, 0), 
              single = ifelse(marital==5, 1, 0), 
              extremelyurban = ifelse(urban==1, 1, 0), #ref: 2 very urban
              moderatelyurban = ifelse(urban==3, 1, 0), 
              slightlyurban = ifelse(urban==4, 1, 0), 
              noturban = ifelse(urban==5, 1, 0),
              secondhouse = 2 - secondhouse, 
              religion = 2 - religion, 
              speakdutch = 2 - speakdutch, 
              chronicdisease = 2 - chronicdisease, 
              logincome = log(replace(nettoink, nettoink<=1, 1)), #take log of income
              worsehealth = ifelse(!is.na(subjhealth), ifelse(subjhealth %in% c(1,2), 1, 0), NA), #ref: 3 good -- binning 1=poor and 2=moderate because almost no-one in the matching sample answered 1=poor
              betterhealth = ifelse(!is.na(subjhealth), ifelse(subjhealth %in% c(4,5), 1, 0), NA), # binning 4=very good and 5=excellent because almost no-one in the matching sample answered 5=excellent
              # children variables
              kid1female = ifelse(kid1gender==2, 1, 0), # if we filter to parents, all 0s will have a male first child
              kid2female = ifelse(kid2gender==2, 1, 0), # there will be some 0s in here who do not have a 2nd child but these are captured in the model by 'secondkid' & 'thirdkid'
              kid3female = ifelse(kid3gender==2, 1, 0), # same as kid2female
              kid1age = ifelse(nokids==0 & !is.na(kid1byear), year - kid1byear, NA), # kid1byear not imputed (see above)
              kid2age = ifelse(secondkid==1 & kid2byear!=0, year - kid2byear, 0), # kid2byear imputed
              kid3age = ifelse(thirdkid==1 & kid3byear!=0, year - kid2byear, 0), 
              kid1home = ifelse(nokids==0 & kid1home==1, 1, 0), 
              kid2home = ifelse(secondkid==1 & kid2home==1, 1, 0), 
              kid3home = ifelse(thirdkid==1 & kid3home==1, 1, 0),
              livedhere = ifelse(!is.na(movedinyear), year - movedinyear, NA)
  )) %>%
  map(~select(., -c(ownrent, typedwelling, employment_status, education, 
                    marital, urban, nettoink, subjhealth, kid1byear,
                    kid2byear, kid3byear, kid1gender, kid2gender,
                    kid3gender, movedinyear))) %>% # these are dropped because of recoding
  map(~select(., -c(inschool, nodegreeyet, voluntary, # these are dropped because they do not
                    separated, jobother)))            # apply to anyone from the GP group (all 0s)

# unlist and save as original data frames
x <- NULL;
for (i in seq_along(impdata)){
  x <- paste("lissimp_matching", i, sep="_")
  eval(call("<-", as.name(x), impdata[[i]]))
}

#### PSM: compile datasets to be matched ####

# Big5/SWLS are administered every 1-3 years. theoretically, they should have been assessed every
# year, but this was not the case in practice.
# We want to match at the last valid assessment before the transition to grandparenthood which is
# earlier than one year before the transition was first reported (time < -1) AND
# include Big5/SWLS as PSM covariates. 
# There are multiple possible time points for matching as shown in coding the 'valid' variable
# (see above).

# We will match at the last valid pre-treatment assessment (earlier than time==-1) if a 
# valid post-treatment assessment is available. To aid this, we have already coded the 
# timing variable 'valid' (and the variable 'droplater').

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
lissimp_matching_1 %>% 
  group_by(grandparent, nokids) %>% summarise(n = n(), N = n_distinct(nomem_encr))
lissimp_matching_1 %>% 
  filter((grandparent==1) | 
         (grandparent==0 & nokids==0)) %>% 
  group_by(grandparent, nokids) %>% summarise(n = n(), N = n_distinct(nomem_encr))
lissimp_matching_1 %>% 
  filter((grandparent==1 & nokids==0) | 
         (grandparent==0 & nokids==0 & kid1age %in% c(15:65))) %>% 
  group_by(grandparent, nokids) %>% summarise(n = n(), N = n_distinct(nomem_encr))
lissimp_matching_1 %>% 
  filter((grandparent==1 & nokids==0 & time==matchtime & droplater==F) | 
         (grandparent==0 & nokids==0 & kid1age %in% c(15:65))) %>% 
  group_by(grandparent, nokids) %>% summarise(n = n(), N = n_distinct(nomem_encr))

# Compared to the (old) method that does not filter by 'nokids==0', we 
# drop 3 grandparents 
lissimp_matching_1 %>% filter(grandparent==1 & nokids==1 & valid==-1 & droplater==F) %>% print(width=Inf)
# 890490, 808758, 815246
lisslongvalid %>% filter(nomem_encr %in% c(890490, 808758, 815246)) %>% print(width=Inf)
# totalresidentkids==0, totalchildren is 0 or NA (except for 890490 in 2017) - I'd say we drop them

# 2)
lissimp_matching_1 %>% 
  group_by(grandparent, nokids) %>% summarise(n = n(), N = n_distinct(nomem_encr))
lissimp_matching_1 %>% 
  filter((grandparent==1 & nokids==0 & time==matchtime & droplater==F) | # GP group is the same!
         (grandparent==0 & nokids==1)) %>% 
  group_by(grandparent, nokids) %>% summarise(n = n(), N = n_distinct(nomem_encr))
# -> Decent size (potential) control group 

# one more correction: 
# we also want to use 'kid1age' as a covariate (must not have NAs)
lissimp_matching_1 %>% 
  filter(grandparent==1 & nokids==0 & time==matchtime & droplater==F & is.na(kid1age)) %>% print(width=Inf)
# 7 GP observations have NA for kid1age (842779, 807870, 872305, 811608, 839028, 841723, 851635) 
lisslongvalid %>% 
  filter(nomem_encr %in% c(842779, 807870, 872305, 811608, 839028, 841723, 851635)) %>% 
  select(nomem_encr, year, time, totalchildren, kid1byear, kid2byear, kid3byear, validper, validfam) %>%
  print(n=50, width=Inf)
# These missings are only present in single waves (where 'validfam' indicates that theses
# respondents did not participate in Family and Household survey) 
# -> missing information can be filled in!
lissimp_matching_1 %>% 
  filter(nomem_encr %in% c(842779, 807870, 872305, 811608, 839028, 841723, 851635)) %>% 
  arrange(nomem_encr, year) %>% 
  select(nomem_encr, year, time,  contains("kid"), currentpartner, livetogether, 
         totalchildren, -totalresidentkids) %>% print(width=Inf)

# I have literally searched multiple hours for a more elegant way to code this & failed...
# tried: across() / mutate_at() / lag() / lead() / fill()
replace_with_laglead <- function(x) { 
    x %>%       
    mutate(secondkid = ifelse(nomem_encr==811608 & year==2013, 1, secondkid), # leading
           thirdkid = ifelse(nomem_encr==811608 & year==2013, 1, thirdkid),
           kid1age = ifelse(nomem_encr==811608 & year==2013, 29, kid1age),
           kid2age = ifelse(nomem_encr==811608 & year==2013, 22, kid2age),
           kid3age = ifelse(nomem_encr==811608 & year==2013, 22, kid3age),
           kid2home = ifelse(nomem_encr==811608 & year==2013, 1, kid2home),
           totalchildren = ifelse(nomem_encr==811608 & year==2013, 3, totalchildren)) %>% 
    mutate(secondkid = ifelse(nomem_encr==839028 & year==2013, 1, secondkid),
           thirdkid = ifelse(nomem_encr==839028 & year==2013, 1, thirdkid),
           kid1female = ifelse(nomem_encr==839028 & year==2013, 1, kid1female),
           kid2female = ifelse(nomem_encr==839028 & year==2013, 1, kid2female),
           kid3female = ifelse(nomem_encr==839028 & year==2013, 1, kid3female),
           kid1age = ifelse(nomem_encr==839028 & year==2013, 17, kid1age),
           kid2age = ifelse(nomem_encr==839028 & year==2013, 15, kid2age),
           kid3age = ifelse(nomem_encr==839028 & year==2013, 15, kid3age),
           kid1home = ifelse(nomem_encr==839028 & year==2013, 1, kid1home),
           kid2home = ifelse(nomem_encr==839028 & year==2013, 1, kid2home),
           kid3home = ifelse(nomem_encr==839028 & year==2013, 1, kid3home)) %>% 
    mutate(secondkid = ifelse(nomem_encr==842779 & year==2011, 1, secondkid),
           kid1female = ifelse(nomem_encr==842779 & year==2011, 1, kid1female),
           kid1age = ifelse(nomem_encr==842779 & year==2011, 26, kid1age),
           kid2age = ifelse(nomem_encr==842779 & year==2011, 23 , kid2age),
           kid1home = ifelse(nomem_encr==842779 & year==2011, 1, kid1home)) %>% 
    mutate(secondkid = ifelse(nomem_encr==807870 & year==2012, 1, secondkid), # lagging
           kid1age = ifelse(nomem_encr==807870 & year==2012, 24, kid1age),
           kid2age = ifelse(nomem_encr==807870 & year==2012, 19, kid2age)) %>% 
    mutate(secondkid = ifelse(nomem_encr==841723 & year==2014, 1, secondkid),
           kid1female = ifelse(nomem_encr==841723 & year==2014, 1, kid1female),
           kid2female = ifelse(nomem_encr==841723 & year==2014, 1, kid2female),
           kid1age = ifelse(nomem_encr==841723 & year==2014, 30, kid1age),
           kid2age = ifelse(nomem_encr==841723 & year==2014, 27, kid2age),
           kid2home = ifelse(nomem_encr==841723 & year==2014, 1, kid2home),
           totalchildren = ifelse(nomem_encr==841723 & year==2014, 2, totalchildren)) %>% 
    mutate(secondkid = ifelse(nomem_encr==851635 & year==2014, 1, secondkid),
           kid1female = ifelse(nomem_encr==851635 & year==2014, 1, kid1female),
           kid2female = ifelse(nomem_encr==851635 & year==2014, 1, kid2female),
           kid1age = ifelse(nomem_encr==851635 & year==2014, 30, kid1age),
           kid2age = ifelse(nomem_encr==851635 & year==2014, 27, kid2age)) %>% 
    mutate(secondkid = ifelse(nomem_encr==872305 & year==2012, 1, secondkid),
           kid1age = ifelse(nomem_encr==872305 & year==2012, 31, kid1age),
           kid2age = ifelse(nomem_encr==872305 & year==2012, 28, kid2age),
           totalchildren = ifelse(nomem_encr==872305 & year==2012, 2, totalchildren))  }

list_laglead <- list(lissimp_matching_1, lissimp_matching_2, lissimp_matching_3,
                     lissimp_matching_4, lissimp_matching_5) %>%
  lapply(replace_with_laglead)
names(list_laglead) <- c("lissimp_matching_1", "lissimp_matching_2", "lissimp_matching_3",
                         "lissimp_matching_4", "lissimp_matching_5")
list2env(list_laglead, .GlobalEnv)
rm(list_laglead)

lissimp_matching_3 %>% 
  filter(nomem_encr %in% c(842779, 807870, 872305, 811608, 839028, 841723, 851635)) %>% 
  arrange(nomem_encr, year) %>% 
  select(nomem_encr, year, time,  contains("kid"), currentpartner, livetogether, 
         totalchildren, -totalresidentkids) %>% print(width=Inf)

# subsetting imputed datasets:

# PARENTS
subset_parents_liss <- function(x) { 
  x %>% 
    filter((grandparent==1 & nokids==0 & time==matchtime & droplater==F) | # cases
           (grandparent==0 & nokids==0 & kid1age %in% c(15:65))) %>%       # (potential) controls
    select(-c(droplater, time, valid, nokids, matchtime)) }

list_subset_parents <- list(lissimp_matching_1, lissimp_matching_2, lissimp_matching_3,
                            lissimp_matching_4, lissimp_matching_5) %>%
  lapply(subset_parents_liss)
names(list_subset_parents) <- c("lissimp_parents_ps_1", "lissimp_parents_ps_2", "lissimp_parents_ps_3",
                                "lissimp_parents_ps_4", "lissimp_parents_ps_5")
list2env(list_subset_parents, .GlobalEnv)
rm(list_subset_parents)

# sample size before matching:
lissimp_parents_ps_1 %>% 
  group_by(grandparent) %>% summarise(n = n(), N = n_distinct(nomem_encr))

# build a data frame that I can later import into the .Rmd file in order to report the sample size flow
load(file = "data/processed/LISS/liss_sampleflow_gp.rda")
if(nrow(liss_sampleflow_gp)==3){ # added this condition in case I only execute the later parts of the script
  liss_sampleflow_gp[nrow(liss_sampleflow_gp)+1, ] <- 
    c(4, (lissimp_parents_ps_1 %>% group_by(grandparent) %>% summarise(n = n(), N = n_distinct(nomem_encr)))[2,3]) # add step 4
}
save(liss_sampleflow_gp, file = "data/processed/LISS/liss_sampleflow_gp.rda") # save for later import

# another one for the non-grandparent control subjects
liss_sampleflow_nongp <- data.frame(group = numeric(0), obs = numeric(0), n = numeric(0))
liss_sampleflow_nongp[nrow(liss_sampleflow_nongp)+1, ] <- 
  c(1, (lissimp_parents_ps_1 %>% group_by(grandparent) %>% 
          summarise(n = n(), N = n_distinct(nomem_encr)))[1, c(2,3)]) # add parent control group

# NONPARENTS
subset_nonparents_liss <- function(x) { 
  x %>% 
    filter((grandparent==1 & nokids==0 & time==matchtime & droplater==F) | # cases
           (grandparent==0 & nokids==1)) %>%                               # (potential) controls
    select(-c(droplater, time, valid, matchtime, nokids, 
              contains("kid"), totalchildren)) } # kid variables not used here
  
list_subset_nonparents <- list(lissimp_matching_1, lissimp_matching_2, lissimp_matching_3,
                               lissimp_matching_4, lissimp_matching_5) %>%
  lapply(subset_nonparents_liss)
names(list_subset_nonparents) <- c("lissimp_nonparents_ps_1", "lissimp_nonparents_ps_2", 
                                   "lissimp_nonparents_ps_3", "lissimp_nonparents_ps_4", 
                                   "lissimp_nonparents_ps_5")
list2env(list_subset_nonparents, .GlobalEnv)
rm(list_subset_nonparents)

# sample size before matching:
lissimp_nonparents_ps_1 %>% 
  group_by(grandparent) %>% summarise(n = n(), N = n_distinct(nomem_encr))

# for the non-grandparent control subjects
liss_sampleflow_nongp[nrow(liss_sampleflow_nongp)+1, ] <- 
  c(2, (lissimp_nonparents_ps_1 %>% group_by(grandparent) %>% 
          summarise(n = n(), N = n_distinct(nomem_encr)))[1, c(2,3)]) # add nonparent control group
liss_sampleflow_nongp$group <- factor(c("parents", "nonparents")) 
save(liss_sampleflow_nongp, file = "data/processed/LISS/liss_sampleflow_nongp.rda") # save for later import

# Sadly, some dummy covariates I picked have a very one-sided distribution in the (smaller) GP group where 
# some variables have 0 (or only 1, 2, or 3) cases in one group. Example:
table(lissimp_parents_ps_1$grandparent, lissimp_parents_ps_1$disability)
# Later on, this leads to problems with logistic regression:
# https://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression
# Therefore, I'll remove all these variables here. (see "Overview covariates.xlsx" -> Sheet "Frequencies")
remove_infrequent_liss <- function(x) { 
  x %>% 
    select(-c(retire_early, retirement, heartattack, stroke, cancer, rentfree, businessdwelling, otherdwelling, # too infrequent
              jobseeker, pensioner, disability, primaryschool),
           -c(paid_work, more_paid_work, difficultybills, secondhouse, # these are not important (substantively) / or redundant
              bmi, chronicdisease, diabetes, nodisease, flatapartment, 
              farmhouse, familybusiness, freelancer, housekeeper, degreeother),
           -c(currentpartner, hhmembers, single, 
              extremelyurban, moderatelyurban, slightlyurban, noturban,
              livedhere)) }

list_remove_infrequent_liss <- list(lissimp_parents_ps_1, lissimp_parents_ps_2, 
                                    lissimp_parents_ps_3, lissimp_parents_ps_4, 
                                    lissimp_parents_ps_5,
                                    lissimp_nonparents_ps_1, lissimp_nonparents_ps_2, 
                                    lissimp_nonparents_ps_3, lissimp_nonparents_ps_4, 
                                    lissimp_nonparents_ps_5) %>%
  lapply(remove_infrequent_liss)

names(list_remove_infrequent_liss) <- c("lissimp_parents_ps_1", "lissimp_parents_ps_2", 
                                        "lissimp_parents_ps_3", "lissimp_parents_ps_4", 
                                        "lissimp_parents_ps_5",
                                        "lissimp_nonparents_ps_1", "lissimp_nonparents_ps_2", 
                                        "lissimp_nonparents_ps_3", "lissimp_nonparents_ps_4", 
                                        "lissimp_nonparents_ps_5")
list2env(list_remove_infrequent_liss, .GlobalEnv)
rm(list_remove_infrequent_liss)

#### PSM: compute propensity scores ####

# 1st step -> estimate propensity scores for all obs.
# do 5 times, once for each of m=5 imputations, then compute mean PS (see Mitra 2016)

imp <- 5

# 1) PARENT control group

# we match exactly on 'female', year = interview year is relevant here (in HRS this is a separate variable) 
liss_ps_model_parents <- as.formula("grandparent ~ . - nomem_encr - female") # all vars but ... - year 

help1 <- NULL;
help2 <- NULL;
help3 <- NULL;
for (i in c(1:imp)){
  help1 <- paste("lissimp_parents_ps", i, sep="_")
  help2 <- paste("lissimp_parents_ps", i, "main", sep="_")
  help3 <- paste("liss_ps_logit_parents_m", i, "_main", sep="")
  #save subset data (2014 separately because of missing health covariates)
  eval(call("<-", as.name(help2), get(help1) %>% filter(year %in% c(2008:2013, 2015:2018)))) #2008:2013
  #logistic regressions
  eval(call("<-", as.name(help3), glm(liss_ps_model_parents, family = binomial(link='logit'), data = get(help2))))
}

summary(liss_ps_logit_parents_m1_main)
summary(liss_ps_logit_parents_m2_main)
summary(liss_ps_logit_parents_m3_main)
summary(liss_ps_logit_parents_m4_main)
summary(liss_ps_logit_parents_m5_main)

#save fitted values (=propensity scores) -> proceed with one of the m=5 imputation dataset 
#(does not matter which one - imputation was only used for PS computation - I choose m=2)
lissimp_parents_ps_2_main$pscore_m1 <- fitted(liss_ps_logit_parents_m1_main)
lissimp_parents_ps_2_main$pscore_m2 <- fitted(liss_ps_logit_parents_m2_main)
lissimp_parents_ps_2_main$pscore_m3 <- fitted(liss_ps_logit_parents_m3_main)
lissimp_parents_ps_2_main$pscore_m4 <- fitted(liss_ps_logit_parents_m4_main)
lissimp_parents_ps_2_main$pscore_m5 <- fitted(liss_ps_logit_parents_m5_main)
lissimp_pscore_parents_main <- lissimp_parents_ps_2_main %>% 
  mutate(pscore = (pscore_m1 + pscore_m2 + pscore_m3 + pscore_m4 + pscore_m5)/5)

# 2014 
#liss_ps_model_2014 <- as.formula("grandparent ~ . - year - nomem_encr - bmi - 
#                                  chronicdisease - heartattack - stroke - cancer - diabetes - 
#                                  nodisease - mobility - dep") # all vars but ... # does not work for some reason
liss_ps_model_parents_2014 <- as.formula("grandparent ~ . - year - nomem_encr - female")  # no variation in year (only 2014)

help1 <- NULL;
help2 <- NULL;
help3 <- NULL;
for (i in c(1:imp)){
  help1 <- paste("lissimp_parents_ps", i, sep="_")
  help2 <- paste("lissimp_parents_ps", i, "14", sep="_")
  help3 <- paste("liss_ps_logit_parents_m", i, "_14", sep="")
  #save subset data (2014 separately because of missing health covariates)
  eval(call("<-", as.name(help2), get(help1) %>% filter(year==2014)))
  eval(call("<-", as.name(help2), get(help2) %>% 
              select(-c(#bmi, chronicdisease, diabetes, nodisease, # health variables not assessed in 2014 
                        mobility, dep, worsehealth, betterhealth, 
                        widowed)))) # 0 widowed GPs in 2014
                        #more_paid_work, speakdutch, farmhouse, # these were too infrequent in 2014 (in GP group)
                        #housekeeper, degreeother, widowed, 
                        #difficultybills, single))))
  #logistic regressions
  eval(call("<-", as.name(help3), glm(liss_ps_model_parents_2014, family = binomial(link='logit'), data = get(help2))))
}

summary(liss_ps_logit_parents_m1_14)
summary(liss_ps_logit_parents_m2_14)
summary(liss_ps_logit_parents_m3_14)
summary(liss_ps_logit_parents_m4_14)
summary(liss_ps_logit_parents_m5_14)

#save fitted values (=propensity scores) -> proceed with one of the m=5 imputation dataset 
#(does not matter which one - imputation was only used for PS computation - I choose m=2)
lissimp_parents_ps_2_14$pscore_m1 <- fitted(liss_ps_logit_parents_m1_14)
lissimp_parents_ps_2_14$pscore_m2 <- fitted(liss_ps_logit_parents_m2_14)
lissimp_parents_ps_2_14$pscore_m3 <- fitted(liss_ps_logit_parents_m3_14)
lissimp_parents_ps_2_14$pscore_m4 <- fitted(liss_ps_logit_parents_m4_14)
lissimp_parents_ps_2_14$pscore_m5 <- fitted(liss_ps_logit_parents_m5_14)
lissimp_pscore_parents_14 <- lissimp_parents_ps_2_14 %>% 
  mutate(pscore = (pscore_m1 + pscore_m2 + pscore_m3 + pscore_m4 + pscore_m5)/5)

# bind all years back together!
lissimp_matching_parents <- bind_rows(lissimp_pscore_parents_main, lissimp_pscore_parents_14)
#lissimp_matching_parents <- lissimp_pscore_parents_main

lissimp_matching_parents <- lissimp_matching_parents %>% 
  select(nomem_encr, year, female, grandparent, pscore) 

# 2) NONPARENT control group

liss_ps_model_nonparents <- as.formula("grandparent ~ . - nomem_encr - female") # all vars but ... # - year 

help1 <- NULL;
help2 <- NULL;
help3 <- NULL;
for (i in c(1:imp)){
  help1 <- paste("lissimp_nonparents_ps", i, sep="_")
  help2 <- paste("lissimp_nonparents_ps", i, "main", sep="_")
  help3 <- paste("liss_ps_logit_nonparents_m", i, "_main", sep="")
  #save subset data (2014 separately because of missing health covariates)
  eval(call("<-", as.name(help2), get(help1) %>% filter(year %in% c(2008:2013, 2015:2018)))) #2008:2013
  #logistic regressions
  eval(call("<-", as.name(help3), glm(liss_ps_model_nonparents, family = binomial(link='logit'), data = get(help2))))
}

summary(liss_ps_logit_nonparents_m1_main)
summary(liss_ps_logit_nonparents_m2_main)
summary(liss_ps_logit_nonparents_m3_main)
summary(liss_ps_logit_nonparents_m4_main)
summary(liss_ps_logit_nonparents_m5_main)

#save fitted values (=propensity scores) -> proceed with one of the m=5 imputation dataset 
#(does not matter which one - imputation was only used for PS computation - I choose m=2)
lissimp_nonparents_ps_2_main$pscore_m1 <- fitted(liss_ps_logit_nonparents_m1_main)
lissimp_nonparents_ps_2_main$pscore_m2 <- fitted(liss_ps_logit_nonparents_m2_main)
lissimp_nonparents_ps_2_main$pscore_m3 <- fitted(liss_ps_logit_nonparents_m3_main)
lissimp_nonparents_ps_2_main$pscore_m4 <- fitted(liss_ps_logit_nonparents_m4_main)
lissimp_nonparents_ps_2_main$pscore_m5 <- fitted(liss_ps_logit_nonparents_m5_main)
lissimp_pscore_nonparents_main <- lissimp_nonparents_ps_2_main %>% 
  mutate(pscore = (pscore_m1 + pscore_m2 + pscore_m3 + pscore_m4 + pscore_m5)/5)

# 2014 
#liss_ps_model_2014 <- as.formula("grandparent ~ . - year - nomem_encr - bmi - 
#                                  chronicdisease - heartattack - stroke - cancer - diabetes - 
#                                  nodisease - mobility - dep") # all vars but ... # does not work for some reason
liss_ps_model_nonparents_2014 <- as.formula("grandparent ~ . - year - nomem_encr - female") # no variation in year (only 2014)

help1 <- NULL;
help2 <- NULL;
help3 <- NULL;
for (i in c(1:imp)){
  help1 <- paste("lissimp_nonparents_ps", i, sep="_")
  help2 <- paste("lissimp_nonparents_ps", i, "14", sep="_")
  help3 <- paste("liss_ps_logit_nonparents_m", i, "_14", sep="")
  #save subset data (2014 separately because of missing health covariates)
  eval(call("<-", as.name(help2), get(help1) %>% filter(year==2014)))
  eval(call("<-", as.name(help2), get(help2) %>% 
              select(-c(#bmi, chronicdisease, diabetes, nodisease, # health variables not assessed in 2014 
                        mobility, dep, worsehealth, betterhealth, 
                        widowed)))) # no widowed GPs in 2014
                        #more_paid_work, speakdutch, farmhouse, # these were too infrequent in 2014 (in GP group)
                        #housekeeper, degreeother, widowed, 
                        #difficultybills, single))))
  #logistic regressions
  eval(call("<-", as.name(help3), glm(liss_ps_model_nonparents_2014, family = binomial(link='logit'), data = get(help2))))
}

summary(liss_ps_logit_nonparents_m1_14)
summary(liss_ps_logit_nonparents_m2_14)
summary(liss_ps_logit_nonparents_m3_14)
summary(liss_ps_logit_nonparents_m4_14)
summary(liss_ps_logit_nonparents_m5_14)

#save fitted values (=propensity scores) -> proceed with one of the m=5 imputation dataset 
#(does not matter which one - imputation was only used for PS computation - I choose m=2)
lissimp_nonparents_ps_2_14$pscore_m1 <- fitted(liss_ps_logit_nonparents_m1_14)
lissimp_nonparents_ps_2_14$pscore_m2 <- fitted(liss_ps_logit_nonparents_m2_14)
lissimp_nonparents_ps_2_14$pscore_m3 <- fitted(liss_ps_logit_nonparents_m3_14)
lissimp_nonparents_ps_2_14$pscore_m4 <- fitted(liss_ps_logit_nonparents_m4_14)
lissimp_nonparents_ps_2_14$pscore_m5 <- fitted(liss_ps_logit_nonparents_m5_14)
lissimp_pscore_nonparents_14 <- lissimp_nonparents_ps_2_14 %>% 
  mutate(pscore = (pscore_m1 + pscore_m2 + pscore_m3 + pscore_m4 + pscore_m5)/5)

# bind all years back together!
lissimp_matching_nonparents <- bind_rows(lissimp_pscore_nonparents_main, lissimp_pscore_nonparents_14)
#lissimp_matching_nonparents <- lissimp_pscore_nonparents_main

lissimp_matching_nonparents <- lissimp_matching_nonparents %>% 
  select(nomem_encr, year, female, grandparent, pscore) 


#### PSM: 'matchit' with replacement -> (1) parent control group ####

table(lissimp_matching_parents$grandparent)
table(lissimp_matching_parents$grandparent, lissimp_matching_parents$year)

# we also need the 'time' value for grandparents (-5, -4, -3 or -2) and the 'valid' variable
lissimp_parents <- left_join(lissimp_matching_parents, 
                                       lissimp_matching_1, by=c("nomem_encr", "year")) %>% 
  filter(droplater==F) %>% 
  select(nomem_encr, year, female.x, grandparent.x, pscore, time, valid) %>% 
  rename(female = female.x, grandparent = grandparent.x)

table(lissimp_parents$grandparent, lissimp_parents$time)
table(lissimp_parents$grandparent, lissimp_parents$valid)

# perform matching on previously computed propensity score which is stored in 'pscore'
liss_parents_matchit <- matchit(grandparent ~ pscore, data=lissimp_parents, distance="mahalanobis",
                               replace=T, exact=c("female"), ratio=1) # exact matching on gender
liss_parents_matchit
summary(liss_parents_matchit) # with replacement
liss_data_parents <- get_matches(liss_parents_matchit, data=lissimp_parents)
str(liss_data_parents)

liss_data_parents <- liss_data_parents %>% group_by(subclass) %>% 
  mutate(time = ifelse(is.na(time), max(time, na.rm = T), time),
         valid = ifelse(is.na(valid), max(valid, na.rm = T), valid)) %>% ungroup %>% 
  ungroup() %>% select(-subclass, -weights, -id)

# "with replacement" in two ways: 
# - the same control observation appearing multiple times in the matched data 
#   (this is because we allowed "replace=T" above)
# - control subjects appearing multiple times in the matched data 
#   (because there were multiple available obervations, i.e., time points, per control subject in the data)
liss_data_parents %>% group_by(grandparent) %>% summarise(N=n_distinct(nomem_encr))
liss_data_parents %>% group_by(grandparent) %>% summarise(N=n_distinct(nomem_encr, year)) 
# matches number above, see  summary(liss_parents_matchit)

# build a data.frame for import of these numbers to .Rmd (papaja)
liss_replacement_controls <- data.frame(group = numeric(0), obs = numeric(0), n = numeric(0))
liss_replacement_controls[nrow(liss_replacement_controls)+1, ] <- 
  c(1,
    (liss_data_parents %>% group_by(grandparent) %>% summarise(N=n_distinct(nomem_encr, year)))[1,2],
    (liss_data_parents %>% group_by(grandparent) %>% summarise(N=n_distinct(nomem_encr)))[1,2]) # add parent control group

table(liss_data_parents$grandparent, liss_data_parents$year)
table(liss_data_parents$grandparent, liss_data_parents$time)
table(liss_data_parents$grandparent, liss_data_parents$valid)
table(liss_data_parents$grandparent, liss_data_parents$female)

liss_data_parents <- left_join(liss_data_parents, lissimp_parents_ps_1,
                                         by = c("nomem_encr", "year", "grandparent", "female"))

# for balance assessment (at the time of matching - using the variables containing imputed values)
liss_bal_parents <- liss_data_parents %>%
  select(nomem_encr, grandparent, pscore, female, everything(), -time, -valid) # -year, 

liss_data_parents <- liss_data_parents %>% 
  select(nomem_encr, year, grandparent, time, valid, pscore) %>% 
  rename(match_year = year, time_match = time, valid_match = valid) %>% 
  mutate(match_number = row_number()) # if we allow duplicate matches, we need an unambiguous identifier for later

# compile analysis sample with all longitudinal observations
lissanalysis_parents <- left_join(liss_data_parents, lisslongvalid,
                                            by = c("nomem_encr", "grandparent"))

table(lissanalysis_parents$grandparent, lissanalysis_parents$time_match) # already transferred to controls 
table(lissanalysis_parents$grandparent, lissanalysis_parents$matchtime)

lissanalysis_parents <- lissanalysis_parents %>%
  select(-matchtime) %>% rename(matchtime = time_match) 

table(lissanalysis_parents$grandparent, lissanalysis_parents$valid)
table(lissanalysis_parents$grandparent, lissanalysis_parents$valid_match)

# create time variable for controls relative to the time point of matching (& 'valid' variable)
lissanalysis_parents <- lissanalysis_parents %>% mutate(
  time = ifelse(is.na(time) & match_year==year, matchtime, time),
  # variable 'match_year' relates to the controls!
  valid = ifelse(is.na(valid) & match_year==year, valid_match, valid)
)
#reminder: 'time' counts calendar years in relation to transition to GP, 'valid' only valid assessments
table(lissanalysis_parents$grandparent, lissanalysis_parents$time)
table(lissanalysis_parents$grandparent, lissanalysis_parents$valid)

lissanalysis_parents <- lissanalysis_parents %>% filter(!is.na(pscore)) %>% 
  mutate(
    time = ifelse(grandparent==0 & is.na(time) & matchtime==-5, (year - match_year) - 5, time),
    time = ifelse(grandparent==0 & is.na(time) & matchtime==-4, (year - match_year) - 4, time),
    time = ifelse(grandparent==0 & is.na(time) & matchtime==-3, (year - match_year) - 3, time),
    time = ifelse(grandparent==0 & is.na(time) & matchtime==-2, (year - match_year) - 2, time))

lissanalysis_parents <- lissanalysis_parents %>% group_by(match_number) %>% 
  mutate(lastcount = ifelse(grandparent==0 & valid==valid_match, row_number(), NA)) %>% ungroup()
lissanalysis_parents <- lissanalysis_parents %>% group_by(match_number) %>% 
  mutate(lastcount = max(lastcount, na.rm = T)) %>% ungroup() %>% 
  mutate(lastcount = replace(lastcount, lastcount==-Inf, NA)) # regular NA pls

# finish coding 'valid' for controls 
# grouping by 'match_number' here instead of 'nomem_encr' because we allowed duplicate matches
lissanalysis_parents <- lissanalysis_parents %>% group_by(match_number) %>% 
  mutate(helpcount = row_number()) %>% ungroup()
lissanalysis_parents <- lissanalysis_parents %>% 
  mutate(valid = ifelse(is.na(valid) & grandparent==0, helpcount - lastcount + valid_match, valid)) %>% 
  select(-helpcount, -lastcount)

table(lissanalysis_parents$grandparent, lissanalysis_parents$time)
table(lissanalysis_parents$grandparent, lissanalysis_parents$valid)
table(lissanalysis_parents$grandparent, lissanalysis_parents$year)

# cells at time > 6 too small
lissanalysis_parents <- lissanalysis_parents %>% filter(time %in% c(-6:6)) %>% 
  select(-match_year, -valid_match, -nohouse_encr, -droplater)

# save .rda 
save(lissanalysis_parents, file = "data/processed/LISS/lissanalysis_parents.rda")
lissanalysis_parents %>% group_by(grandparent) %>% summarise(N = n_distinct(nomem_encr))
# duplicates in the controls, controls matched to cases 


#### PSM: 'matchit' with replacement -> (2) nonparent control group ####

table(lissimp_matching_nonparents$grandparent)
table(lissimp_matching_nonparents$grandparent, lissimp_matching_nonparents$year)

# we also need the 'time' value for grandparents (-5, -4, -3 or -2) and the 'valid' variable
lissimp_nonparents <- left_join(lissimp_matching_nonparents, 
                                        lissimp_matching_1, by=c("nomem_encr", "year")) %>% 
  filter(droplater==F) %>% 
  select(nomem_encr, year, female.x, grandparent.x, pscore, time, valid) %>% 
  rename(female = female.x, grandparent = grandparent.x)

table(lissimp_nonparents$grandparent, lissimp_nonparents$time)
table(lissimp_nonparents$grandparent, lissimp_nonparents$valid)

# perform matching on previously computed propensity score which is stored in 'pscore'
liss_nonparents_matchit <- matchit(grandparent ~ pscore, data=lissimp_nonparents, distance="mahalanobis",
                                  replace=T, exact=c("female"), ratio=1) # exact matching on gender
liss_nonparents_matchit
summary(liss_nonparents_matchit) # with replacement
liss_data_nonparents <- get_matches(liss_nonparents_matchit, data=lissimp_nonparents)
str(liss_data_nonparents)

liss_data_nonparents <- liss_data_nonparents %>% group_by(subclass) %>% 
  mutate(time = ifelse(is.na(time), max(time, na.rm = T), time),
         valid = ifelse(is.na(valid), max(valid, na.rm = T), valid)) %>% ungroup %>% 
  ungroup() %>% select(-subclass, -weights, -id)

# "with replacement" in two ways: 
# - the same control observation appearing multiple times in the matched data 
#   (this is because we allowed "replace=T" above)
# - control subjects appearing multiple times in the matched data 
#   (because there were multiple available obervations, i.e., time points, per control subject in the data)
liss_data_nonparents %>% group_by(grandparent) %>% summarise(N=n_distinct(nomem_encr))
liss_data_nonparents %>% group_by(grandparent) %>% summarise(N=n_distinct(nomem_encr, year)) 
# matches number above, see  summary(liss_parents_matchit)

# build a data.frame for import of these numbers to .Rmd (papaja)
liss_replacement_controls[nrow(liss_replacement_controls)+1, ] <- 
  c(2,
    (liss_data_nonparents %>% group_by(grandparent) %>% summarise(N=n_distinct(nomem_encr, year)))[1,2],
    (liss_data_nonparents %>% group_by(grandparent) %>% summarise(N=n_distinct(nomem_encr)))[1,2]) # add nonparent control group
liss_replacement_controls$group <- factor(c("parents", "nonparents")) 
save(liss_replacement_controls, file = "data/processed/LISS/liss_replacement_controls.rda") # save for later import

table(liss_data_nonparents$grandparent, liss_data_nonparents$year)
table(liss_data_nonparents$grandparent, liss_data_nonparents$time)
table(liss_data_nonparents$grandparent, liss_data_nonparents$valid)
table(liss_data_nonparents$grandparent, liss_data_nonparents$female)

liss_data_nonparents <- left_join(liss_data_nonparents, lissimp_nonparents_ps_1,
                                          by = c("nomem_encr", "year", "grandparent", "female"))

# for balance assessment (at the time of matching - using the variables containing imputed values)
liss_bal_nonparents <- liss_data_nonparents %>%
  select(nomem_encr, grandparent, pscore, female, everything(), -time, -valid) # -year, 

liss_data_nonparents <- liss_data_nonparents %>% 
  select(nomem_encr, year, grandparent, time, valid, pscore) %>% 
  rename(match_year = year, time_match = time, valid_match = valid) %>% 
  mutate(match_number = row_number()) # if we allow duplicate matches, we need an unambiguous identifier for later

# compile analysis sample with all longitudinal observations
lissanalysis_nonparents <- left_join(liss_data_nonparents, lisslongvalid,
                                             by = c("nomem_encr", "grandparent"))

table(lissanalysis_nonparents$grandparent, lissanalysis_nonparents$time_match) # already transferred to controls 
table(lissanalysis_nonparents$grandparent, lissanalysis_nonparents$matchtime)

lissanalysis_nonparents <- lissanalysis_nonparents %>%
  select(-matchtime) %>% rename(matchtime = time_match)

table(lissanalysis_nonparents$grandparent, lissanalysis_nonparents$valid)
table(lissanalysis_nonparents$grandparent, lissanalysis_nonparents$valid_match)

# create time variable for controls relative to the time point of matching (& 'valid' variable)
lissanalysis_nonparents <- lissanalysis_nonparents %>% mutate(
  time = ifelse(is.na(time) & match_year==year, matchtime, time),
  # variable 'match_year' relates to the controls!
  valid = ifelse(is.na(valid) & match_year==year, valid_match, valid)
)
#reminder: 'time' counts calendar years in relation to transition to GP, 'valid' only valid assessments
table(lissanalysis_nonparents$grandparent, lissanalysis_nonparents$time)
table(lissanalysis_nonparents$grandparent, lissanalysis_nonparents$valid)

lissanalysis_nonparents <- lissanalysis_nonparents %>% filter(!is.na(pscore)) %>% 
  mutate(
    time = ifelse(grandparent==0 & is.na(time) & matchtime==-5, (year - match_year) - 5, time),
    time = ifelse(grandparent==0 & is.na(time) & matchtime==-4, (year - match_year) - 4, time),
    time = ifelse(grandparent==0 & is.na(time) & matchtime==-3, (year - match_year) - 3, time),
    time = ifelse(grandparent==0 & is.na(time) & matchtime==-2, (year - match_year) - 2, time))

lissanalysis_nonparents <- lissanalysis_nonparents %>% group_by(match_number) %>% 
  mutate(lastcount = ifelse(grandparent==0 & valid==valid_match, row_number(), NA)) %>% ungroup()
lissanalysis_nonparents <- lissanalysis_nonparents %>% group_by(match_number) %>% 
  mutate(lastcount = max(lastcount, na.rm = T)) %>% ungroup() %>% 
  mutate(lastcount = replace(lastcount, lastcount==-Inf, NA)) # regular NA pls

# finish coding 'valid' for controls 
# grouping by 'match_number' here instead of 'nomem_encr' because we allowed duplicate matches
lissanalysis_nonparents <- lissanalysis_nonparents %>% group_by(match_number) %>% 
  mutate(helpcount = row_number()) %>% ungroup()
lissanalysis_nonparents <- lissanalysis_nonparents %>% 
  mutate(valid = ifelse(is.na(valid) & grandparent==0, helpcount - lastcount + valid_match, valid)) %>% 
  select(-helpcount, -lastcount)

table(lissanalysis_nonparents$grandparent, lissanalysis_nonparents$time)
table(lissanalysis_nonparents$grandparent, lissanalysis_nonparents$valid)
table(lissanalysis_nonparents$grandparent, lissanalysis_nonparents$year)

# cells at time > 6 too small 
lissanalysis_nonparents <- lissanalysis_nonparents %>% filter(time %in% c(-6:6)) %>% 
  select(-match_year, -valid_match, -nohouse_encr, -droplater)

# save .rda 
save(lissanalysis_nonparents, file = "data/processed/LISS/lissanalysis_nonparents.rda")
lissanalysis_nonparents %>% group_by(grandparent) %>% summarise(N = n_distinct(nomem_encr)) 
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
liss_bal_parents_before <- lissimp_matching_parents %>% 
  select(nomem_encr, year, grandparent, pscore) # PS averaged from imp=5
liss_bal_parents_before <- left_join(liss_bal_parents_before, lissimp_matching_1)
liss_bal_parents_before <- liss_bal_parents_before %>% 
  select(nomem_encr, grandparent, pscore, female, everything(), 
         -c(time, valid, droplater, matchtime, nokids),
         -c(retire_early, retirement, heartattack, stroke, cancer, rentfree, businessdwelling, otherdwelling, # too infrequent in some cells
            jobseeker, pensioner, disability, primaryschool), 
         -c(paid_work, more_paid_work, difficultybills, secondhouse, # these are not important (substantively) / or redundant
            bmi, chronicdisease, diabetes, nodisease, flatapartment, 
            farmhouse, familybusiness, freelancer, housekeeper, degreeother),
         -c(currentpartner, hhmembers, single, 
            extremelyurban, moderatelyurban, slightlyurban, noturban,
            livedhere))
summary(liss_bal_parents_before)

names(liss_bal_parents_before) # column names must be aligned!
names(liss_bal_parents)

liss_bal_nonparents_before <- lissimp_matching_nonparents %>% 
  select(nomem_encr, year, grandparent, pscore) # PS averaged from imp=5
liss_bal_nonparents_before <- left_join(liss_bal_nonparents_before, lissimp_matching_1)
liss_bal_nonparents_before <- liss_bal_nonparents_before %>% 
  select(nomem_encr, grandparent, pscore, female, everything(), 
         -c(time, valid, droplater, matchtime, contains("kid"), totalchildren), 
         -c(retire_early, retirement, heartattack, stroke, cancer, rentfree, businessdwelling, otherdwelling, # too infrequent
            jobseeker, pensioner, disability, primaryschool), 
         -c(paid_work, more_paid_work, difficultybills, secondhouse, # these are not important (substantively) / or redundant
            bmi, chronicdisease, diabetes, nodisease, flatapartment,
            farmhouse, familybusiness, freelancer, housekeeper, degreeother),
         -c(currentpartner, hhmembers, single, 
            extremelyurban, moderatelyurban, slightlyurban, noturban,
            livedhere))
summary(liss_bal_nonparents_before)

names(liss_bal_nonparents_before) # column names must be aligned!
names(liss_bal_nonparents)

# evaluate standardized difference in means
# create maxtrix object with empty vectors 'stddiff'
# (1) PARENTS
varnum_parents <-  1:(length(liss_bal_parents)-2)
covar_parents <-  colnames(liss_bal_parents[3:paste(length(liss_bal_parents))])
stddiff_before_parents <- numeric(length = length(liss_bal_parents_before)-2)     #before matching
stddiff_after1_parents <- numeric(length = length(liss_bal_parents)-2)            #after matching

coln_parents <- c("varnum_parents", "covar_parents", "stddiff_before_parents", 
                  "stddiff_after1_parents") # defining column names 
# creating matrix 
liss_balance_matrix_parents <- matrix(c(varnum_parents, covar_parents, stddiff_before_parents,
                                       stddiff_after1_parents), ncol = 4, 
                                     dimnames = list(varnum_parents, coln_parents))

# use custom function in for-loop to fill matrix vectors 'stddiff'
for (i in seq_along(liss_balance_matrix_parents[varnum_parents])) {
  liss_balance_matrix_parents[[i, 3]] <- stdmeandiff(get(liss_balance_matrix_parents[[i, 2]]), 
                                                    grandparent, liss_bal_parents_before)     #before matching
  liss_balance_matrix_parents[[i, 4]] <- stdmeandiff(get(liss_balance_matrix_parents[[i, 2]]), 
                                                    grandparent, liss_bal_parents)            #after matching
}
#liss_balance_matrix_parents[, 3:4] <- round(as.numeric(liss_balance_matrix_parents[, 3:4]), 3)

#kable(liss_balance_matrix_parents[, 2:4], format="rst", 
#      col.names = c("Covariate", "Before Matching",
#                    "After Matching"), 
#      align = "lcc", digits=2, caption = "Table 1. Covariate Balance")


# (2) NONPARENTS

# create maxtrix object with empty vectors 'stddiff'
varnum_nonparents <-  1:(length(liss_bal_nonparents)-2)
covar_nonparents <-  colnames(liss_bal_nonparents[3:paste(length(liss_bal_nonparents))])
stddiff_before_nonparents <- numeric(length = length(liss_bal_nonparents_before)-2)     #before matching
stddiff_after1_nonparents <- numeric(length = length(liss_bal_nonparents)-2)            #after matching

coln_nonparents <- c("varnum_nonparents", "covar_nonparents", "stddiff_before_nonparents", 
                     "stddiff_after1_nonparents") # defining column names 
# creating matrix 
liss_balance_matrix_nonparents <- matrix(c(varnum_nonparents, covar_nonparents, stddiff_before_nonparents,
                                          stddiff_after1_nonparents), ncol = 4, 
                                        dimnames = list(varnum_nonparents, coln_nonparents))

# use custom function in for-loop to fill matrix vectors 'stddiff'
for (i in seq_along(liss_balance_matrix_nonparents[varnum_nonparents])) {
  liss_balance_matrix_nonparents[[i, 3]] <- stdmeandiff(get(liss_balance_matrix_nonparents[[i, 2]]), 
                                                       grandparent, liss_bal_nonparents_before)     #before matching
  liss_balance_matrix_nonparents[[i, 4]] <- stdmeandiff(get(liss_balance_matrix_nonparents[[i, 2]]), 
                                                       grandparent, liss_bal_nonparents)            #after matching
}
#liss_balance_matrix_nonparents[, 3:4] <- round(as.numeric(liss_balance_matrix_nonparents[, 3:4]), 3)

#kable(liss_balance_matrix_nonparents[, 2:4], format="rst", 
#      col.names = c("Covariate", "Before Matching",
#                    "After Matching"), 
#      align = "lcc", digits=2, caption = "Table 1. Covariate Balance")

# build a common data.frame for later import into .Rmd (papaja)
liss_balance_parents_df <- as.data.frame(liss_balance_matrix_parents)[2:4]
liss_balance_nonparents_df <- as.data.frame(liss_balance_matrix_nonparents)[2:4]
colnames(liss_balance_parents_df)[1] <- "Covariate"
colnames(liss_balance_nonparents_df)[1] <- "Covariate"
liss_balance_df <- full_join(liss_balance_parents_df, liss_balance_nonparents_df)
liss_balance_df <- liss_balance_df %>% mutate_at(vars(-Covariate), funs(as.numeric))
# draw information from excel sheet 'gp-covariates-overview.xlsx' (& correct order of covariates)
covar_info <- read_excel("gp-covariates-overview.xlsx", sheet = 1, range = "F4:H62",
                         col_names = c("Description", "Raw variable", "Covariate")) %>% 
  filter(!is.na(Description))

liss_balance_df <- left_join(covar_info,
                            liss_balance_df,
                            by = "Covariate") %>% # sets correct order
  select(Covariate, everything())
save(liss_balance_df, file = "data/processed/LISS/liss_balance_df.rda")
