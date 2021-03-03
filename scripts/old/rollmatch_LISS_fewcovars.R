#install.packages("rollmatch")

library(rollmatch)

liss_rolling <- lissimp_matching_1 %>% 
  filter(droplater==F & (grandparent==0 | (grandparent==1 & !is.na(valid)))) %>% 
  arrange(nomem_encr, year) %>% select(-droplater)

#rolling_merge_liss <- lisslong %>% filter(!is.na(transityear)) %>% 
#  select(nomem_encr, year, transityear)
#liss_rolling <- left_join(liss_rolling, rolling_merge_liss)
#
##we probably need another row for some of the participants, so that everyone has a row
##with transityear==year 
#rolling_append_liss <- lisslong %>% filter(transityear==year) %>% mutate(
#  validx = ifelse(!is.na(extra) | !is.na(con) | !is.na(open) | 
#                 !is.na(agree) | !is.na(neur) | !is.na(swls), 1, 0)
#) %>% filter(validx==0) %>% select(nomem_encr, year, transityear, grandparent)
#
#liss_rolling <- bind_rows(liss_rolling, rolling_append_liss)
#liss_rolling <- liss_rolling %>% arrange(nomem_encr, year)


#I think 'rollmatch' has trouble dealing with the year variable as time because of the 4 year
#gap between (almost all) waves
# -> alternative coding:
liss_rolling <- liss_rolling %>% group_by(nomem_encr) %>% mutate(
  time_integer = as.integer(row_number()),
  first = ifelse(valid==0, time_integer, 0))

liss_rolling <- liss_rolling %>% group_by(nomem_encr) %>% mutate(
  first = max(first))
#all control observations need a valid value for entry="first" (?)
liss_rolling <- liss_rolling %>% ungroup() %>% mutate(
  first = ifelse(grandparent==0, time_integer + 1, first),
  nomem_encr = as.integer(nomem_encr),
  first = as.integer(first),
  valid = as.integer(valid)
)
liss_rolling <- liss_rolling %>% ungroup()
liss_rolling <- as.data.frame(liss_rolling) # tibbles lead to error messages in this pkg

table(liss_rolling$grandparent, liss_rolling$first) 
#matching is performed for rows with equal 'entry' values
#this is why we had to do --> first = time_integer + 1 for the controls

liss_reduced_data <- reduce_data(data = liss_rolling, treat = "grandparent",
                            tm = "time_integer", entry = "first",
                            id = "nomem_encr", lookback = 1)

table(liss_reduced_data$grandparent, liss_reduced_data$first)
#temporal alignment worked
# 67   +64  + 59 +  28  + 58 +  35 = 311
table(liss_reduced_data$grandparent, liss_reduced_data$time)
#31 +104+ 176 = 311 --> these are the grandparent cases with valid pre- and post-event assessments

#liss_fm <- as.formula(grandparent ~ . - year - nomem_encr) # all vars but ...
# does not work... looks like I have to list them all...
liss_fm <- as.formula(grandparent ~ female + retire_early + retirement + paid_work + more_paid_work + 
                      disabled + financialsit + rooms + movedinyear + secondhouse + religion + 
                      speakdutch + age + hhmembers + totalresidentkids + participation + swls + 
                      agree + con + extra + neur + open + rental + rentfree + flatapartment + 
                      farmhouse + businessdwelling + otherdwelling + familybusiness + freelancer + 
                      jobseeker + housekeeper + pensioner + disability + 
                      primaryschool + degreehighersec + degreevocational + degreecollege + 
                      degreeuniversity + degreeother + divorced + widowed + 
                      single + extremelyurban + moderatelyurban + slightlyurban + noturban + 
                      ppincome1000)

liss_vars <- colnames(liss_reduced_data[, 3:55]) #!!!! treatment variable has to be included here!
#liss_vars <- liss_vars[!liss_vars %in% c("time")]

liss_scored_data <- score_data(reduced_data = liss_reduced_data,
                          model_type = "logistic", match_on = "logit",
                          fm = liss_fm, treat = "grandparent",
                          tm = "time_integer", entry = "first", id = "nomem_encr")

# pscore only based on one m=1 of imp=5 imputation datasets in 'rollmatch' (implement all imp=5 ?)

liss_output <- rollmatch(liss_scored_data, data=liss_rolling, treat = "grandparent",
                    tm = "time_integer", entry = "first", id = "nomem_encr",
                    vars = liss_vars, lookback = 1, alpha = 4,
                    standard_deviation = "average", num_matches = 1,
                    replacement = F)

liss_output$summary
liss_output$balance

liss_rollmatch_info <- liss_output$matched_data 
# I used this to inspect that the correct longitudinal control observations were picked 
# Yes, matches are created when 'first' is equal in both groups

# create a matched dataset / compare balance
# cases
liss_rollmatch_cases <- liss_output$data
liss_rollmatch_cases <- liss_rollmatch_cases %>% filter(!is.na(control_id.1)) %>% 
  select(nomem_encr, year, grandparent, valid, time)
table(liss_rollmatch_cases$time)
table(liss_rollmatch_cases$valid) 

rollmatch_scores_merge_liss <- liss_scored_data %>% select(nomem_encr, year, first, score, valid) 
liss_rollmatch_cases <- left_join(liss_rollmatch_cases, rollmatch_scores_merge_liss) #merge scores

# controls
liss_rollmatch_controls <- liss_output$data
liss_rollmatch_controls <- liss_rollmatch_controls %>% filter(!is.na(control_id.1)) %>% 
  select(control_id.1, first, time) %>% rename(nomem_encr = control_id.1) %>% 
  mutate(grandparent=0)

# merge scores and also get the corresponding 'year' for the controls
liss_rollmatch_controls <- left_join(liss_rollmatch_controls, rollmatch_scores_merge_liss)

# append cases and controls for balance assessment
liss_matches_rollmatch <- bind_rows(liss_rollmatch_cases, liss_rollmatch_controls) %>% 
  arrange(nomem_encr, year)
liss_matches_rollmatch <- left_join(liss_matches_rollmatch, lissimp_matching_2, #merge imputed covars
                                   by = c("nomem_encr", "year", "grandparent", "valid")) %>% 
  select(-time.y) %>% rename(time = time.x) %>% 
  mutate(valid = replace(valid, is.na(valid), -1))
table(liss_matches_rollmatch$grandparent, liss_matches_rollmatch$time)
table(liss_matches_rollmatch$grandparent, liss_matches_rollmatch$valid)
table(liss_matches_rollmatch$grandparent, liss_matches_rollmatch$year)

# check covariate balance
bal4 <- liss_matches_rollmatch %>% select(-year, -time, -first, -valid, -droplater) %>% 
  rename(pscore=score) %>% select(nomem_encr, grandparent, pscore, female, everything())

liss_matches_rollmatch <- liss_matches_rollmatch %>% 
  select(nomem_encr, year, grandparent, time, valid, score)

# compile analysis sample with all longitudinal observations
liss_rollmatch_analysis <- lisslongvalid %>% 
  filter(droplater==F & (grandparent==0 | (grandparent==1 & !is.na(valid)))) %>% 
  select(-droplater, starts_with("nohouse"))
# Had to do this more complicated filtering here. Otherwise I'd include respondent 823052,
# who has no valid pre-transition assessment.
liss_rollmatch_analysis <- left_join(liss_rollmatch_analysis, liss_matches_rollmatch,
                                    by = c("nomem_encr", "year", "grandparent"))

liss_rollmatch_analysis <- liss_rollmatch_analysis %>% mutate(
  time = ifelse(!is.na(time.x), time.x, time.y),
  valid = ifelse(!is.na(valid.x), valid.x, valid.y)) %>% 
  select(-starts_with("time."), -starts_with("valid."))

liss_rollmatch_merge <- liss_rollmatch_analysis %>% filter(grandparent==0 & valid==-1) %>% 
  rename(match_year = year) %>% select(nomem_encr, match_year)
  
liss_rollmatch_analysis <- left_join(liss_rollmatch_analysis, liss_rollmatch_merge)
liss_rollmatch_analysis <- liss_rollmatch_analysis %>% group_by(nomem_encr) %>% 
  mutate(time_match = ifelse(grandparent==0, max(time, na.rm = T), NA),
         pscore = max(score, na.rm = T)) %>% ungroup() %>% 
  filter(pscore!=-Inf) %>% select(-score) 

table(liss_rollmatch_analysis$time, liss_rollmatch_analysis$grandparent)

liss_rollmatch_analysis <- liss_rollmatch_analysis %>% mutate(
  time = ifelse(is.na(time) & time_match==-3, (year - match_year) - 3, time),
  time = ifelse(is.na(time) & time_match==-2, (year - match_year) - 2, time),
  time = ifelse(is.na(time) & time_match==-1, (year - match_year) - 1, time))

liss_rollmatch_analysis <- liss_rollmatch_analysis %>% group_by(nomem_encr) %>% 
  mutate(lastcount = ifelse(grandparent==0 & valid==-1, row_number(), NA)) %>% ungroup()
liss_rollmatch_analysis <- liss_rollmatch_analysis %>% group_by(nomem_encr) %>% 
  mutate(lastcount = max(lastcount, na.rm = T)) %>% ungroup() %>% 
  mutate(lastcount = replace(lastcount, lastcount==-Inf, NA))

# finish coding 'valid' for controls
liss_rollmatch_analysis <- liss_rollmatch_analysis %>% group_by(nomem_encr) %>% 
  mutate(helpcount = row_number()) %>% ungroup()
liss_rollmatch_analysis <- liss_rollmatch_analysis %>% 
  mutate(valid = ifelse(is.na(valid) & grandparent==0, helpcount - lastcount - 1, valid)) %>% 
  select(-helpcount, -lastcount)

table(liss_rollmatch_analysis$grandparent, liss_rollmatch_analysis$time)
table(liss_rollmatch_analysis$grandparent, liss_rollmatch_analysis$valid)
table(liss_rollmatch_analysis$grandparent, liss_rollmatch_analysis$year)

liss_rollmatch_analysis <- liss_rollmatch_analysis %>% filter(time %in% c(-5:5)) %>% 
  select(-time_match, -match_year, -starts_with("nohouse"), )
