#install.packages("rollmatch")

library(rollmatch)

hrs_rolling <- hrsimp_matching_1 %>% 
  filter(droplater==F & (grandparent==0 | (grandparent==1 & !is.na(valid)))) %>% 
  arrange(HHIDPN, year) %>% select(-droplater)

#rolling_merge_hrs <- hrslong %>% filter(!is.na(transityear)) %>% select(HHIDPN, year, transityear)
#hrs_rolling <- left_join(hrs_rolling, rolling_merge_hrs)
#
##we probably need another row for some of the participants, so that everyone has a row
##with transityear==year
#rolling_append_hrs <- hrslong %>% filter(transityear==year) %>% mutate(
#  valid = ifelse(!is.na(extra) | !is.na(con) | !is.na(open) | 
#                 !is.na(agree) | !is.na(neur) | !is.na(swls), 1, 0)
#) %>% filter(valid==0) %>% select(HHIDPN, year, transityear, grandparent)
#
#hrs_rolling <- bind_rows(hrs_rolling, rolling_append_hrs)
#hrs_rolling <- hrs_rolling %>% arrange(HHIDPN, year)

#I think 'rollmatch' has trouble dealing with the year variable as time because of the 4 year
#gap between (almost all) waves
# -> alternative coding:
hrs_rolling <- hrs_rolling %>% group_by(HHIDPN) %>% mutate(
  time_integer = as.integer(row_number()),
  first = ifelse(valid==0, time_integer, 0)
)
hrs_rolling <- hrs_rolling %>% group_by(HHIDPN) %>% mutate(
  first = max(first))
#all control observations need a valid value for entry="first" (?)
hrs_rolling <- hrs_rolling %>% ungroup() %>% mutate(
  first = ifelse(grandparent==0, time_integer + 1, first),
  HHIDPN = as.integer(HHIDPN),
  grandparent = as.integer(grandparent),
  first = as.integer(first)
)
hrs_rolling <- hrs_rolling %>% ungroup()
hrs_rolling <- as.data.frame(hrs_rolling) # tibbles lead to error messages in this pkg

table(hrs_rolling$grandparent, hrs_rolling$first) 
#matching is performed for rows with equal 'entry' values
#this is why we had to do --> first = time_integer + 1 for the controls

hrs_reduced_data <- reduce_data(data = hrs_rolling, treat = "grandparent",
                            tm = "time_integer", entry = "first",
                            id = "HHIDPN", lookback = 1)

table(hrs_reduced_data$grandparent, hrs_reduced_data$first)
#temporal alignment worked
#553+167+1 = 721
table(hrs_reduced_data$grandparent, hrs_reduced_data$time)
#335+386 = 721 --> these are the grandparent cases with valid pre- and post-event assessments

#hrs_fm <- as.formula(grandparent ~ . - year - HHIDPN) # all vars but ...
# does not work... looks like I have to list them all...
hrs_fm <- as.formula(grandparent ~ gender +	schlyrs+interviewyear+	hhmembers+
                     marriagesnum	+siblings+	totalnonresidentkids+	totalresidentkids+
                     conde+	doctor+	hospital+	psyche+
                     bmi+	cancer+	cesd+	diabetes+
                     stroke+	heart+	mobilitydiff+	hiemployer+
                     higovt+	hispousal+	hiother+	jobhours+
                     foodstamps+	swls+	agree+
                     con+	extra+	neur+	open+
                     participation+	age+	black+
                     raceother+	fulltime+	parttime+	unemployed+
                     partlyretired+	disabled+	notinlaborforce+	spouseabsent+
                     partnered+	separated+	divorced+	widowed+
                     nevermarried+	childrenclose+	ranchfarm+	renter+
                     rentfree+	rentother+	retiredcompl+	retiredpartly+
                     difficultpaybills+	notusaborn+	unsafeneighborhood+	secondhouse+
                     regnortheast+	regmidwest+	regwest+	birthnewengland+
                     birthmidatlantic+	birthencentral+	birthwncentral+	birthescentral+
                     birthwscentral+	birthmountain+	birthpacific+	birthusother+
                     birthnotus+	hhincome1000+	hhwealth1000+	healthexcellent+
                     healthverygood+	healthfair+	healthpoor+	nolightpyhs+
                     nomoderatephys+	novigorousphys+	mobilehome+	duplexhome+
                     apartment+	homeother+	intjan+	intfeb+
                     intmar+	intapr+	intjun+	intjul+
                     intaug+	intsep+	intoct+	intnov+ intdec)

hrs_vars <- colnames(hrs_reduced_data[, 3:102])
hrs_vars <- hrs_vars[!hrs_vars %in% c("time")]

hrs_scored_data <- score_data(reduced_data = hrs_reduced_data,
                          model_type = "logistic", match_on = "logit",
                          fm = hrs_fm, treat = "grandparent",
                          tm = "time_integer", entry = "first", id = "HHIDPN")

hrs_output <- rollmatch(hrs_scored_data, data=hrs_rolling, treat = "grandparent",
                    tm = "time_integer", entry = "first", id = "HHIDPN",
                    vars = hrs_vars, lookback = 1, alpha = 4,
                    standard_deviation = "average", num_matches = 1,
                    replacement = F)

hrs_output$summary
hrs_output$balance

hrs_rollmatch_info <- hrs_output$matched_data 
# I used this to inspect that the correct longitudinal control observations were picked 
# Yes, matches are created when 'first' is equal in both groups

# create a matched dataset / compare balance
# cases
hrs_rollmatch_cases <- hrs_output$data
hrs_rollmatch_cases <- hrs_rollmatch_cases %>% filter(!is.na(control_id.1)) %>% 
  select(HHIDPN, year, grandparent, valid, time)
table(hrs_rollmatch_cases$time)
table(hrs_rollmatch_cases$valid)

rollmatch_scores_merge <- hrs_scored_data %>% select(HHIDPN, year, first, score, valid) 
hrs_rollmatch_cases <- left_join(hrs_rollmatch_cases, rollmatch_scores_merge) #merge scores

# controls
hrs_rollmatch_controls <- hrs_output$data
hrs_rollmatch_controls <- hrs_rollmatch_controls %>% filter(!is.na(control_id.1)) %>% 
  select(control_id.1, first, time) %>% rename(HHIDPN = control_id.1) %>% 
  mutate(grandparent=0)

# merge scores and also get the corresponding 'year' for the controls
hrs_rollmatch_controls <- left_join(hrs_rollmatch_controls, rollmatch_scores_merge)

# append cases and controls for balance assessment
hrs_matches_rollmatch <- bind_rows(hrs_rollmatch_cases, hrs_rollmatch_controls) %>% 
  arrange(HHIDPN, year)
hrs_matches_rollmatch <- left_join(hrs_matches_rollmatch, hrsimp_matching_2, #merge imputed covars
                                   by = c("HHIDPN", "year", "grandparent", "valid")) %>% 
  select(-time.y) %>% rename(time = time.x) %>% 
  mutate(valid = replace(valid, is.na(valid), -1))
table(hrs_matches_rollmatch$grandparent, hrs_matches_rollmatch$time)
table(hrs_matches_rollmatch$grandparent, hrs_matches_rollmatch$valid)
table(hrs_matches_rollmatch$grandparent, hrs_matches_rollmatch$year)

# check covariate balance
bal4 <- hrs_matches_rollmatch %>% select(-year, -time, -first, -valid, -droplater) %>% 
  rename(pscore=score) %>% select(HHIDPN, grandparent, pscore, gender, everything())

hrs_matches_rollmatch <- hrs_matches_rollmatch %>% 
  select(HHIDPN, year, grandparent, time, valid, score)

# compile analysis sample with all longitudinal observations
hrs_rollmatch_analysis <- hrslongvalid %>% 
  filter(droplater==F & (grandparent==0 | (grandparent==1 & !is.na(valid)))) %>% 
  select(-droplater)
hrs_rollmatch_analysis <- left_join(hrs_rollmatch_analysis, hrs_matches_rollmatch,
                                    by = c("HHIDPN", "year", "grandparent"))

hrs_rollmatch_analysis <- hrs_rollmatch_analysis %>% mutate(
  time = ifelse(!is.na(time.x), time.x, time.y),
  valid = ifelse(!is.na(valid.x), valid.x, valid.y)) %>% 
  select(-starts_with("time."), -starts_with("valid.")) 

hrs_rollmatch_merge <- hrs_rollmatch_analysis %>% filter(grandparent==0 & valid==-1) %>% 
  rename(match_year = year) %>% select(HHIDPN, match_year)
  
hrs_rollmatch_analysis <- left_join(hrs_rollmatch_analysis, hrs_rollmatch_merge)
hrs_rollmatch_analysis <- hrs_rollmatch_analysis %>% group_by(HHIDPN) %>% 
  mutate(time_match = ifelse(grandparent==0, max(time, na.rm = T), NA),
         pscore = max(score, na.rm = T)) %>% ungroup() %>% 
  filter(pscore!=-Inf) %>% select(-score) 

table(hrs_rollmatch_analysis$time, hrs_rollmatch_analysis$grandparent)

hrs_rollmatch_analysis <- hrs_rollmatch_analysis %>% mutate(
  time = ifelse(grandparent==0 & is.na(time) & time_match==-4, (year - match_year) - 4, time),
  time = ifelse(grandparent==0 & is.na(time) & time_match==-2, (year - match_year) - 2, time))

hrs_rollmatch_analysis <- hrs_rollmatch_analysis %>% group_by(HHIDPN) %>% 
  mutate(lastcount = ifelse(grandparent==0 & valid==-1, row_number(), NA)) %>% ungroup()
hrs_rollmatch_analysis <- hrs_rollmatch_analysis %>% group_by(HHIDPN) %>% 
  mutate(lastcount = max(lastcount, na.rm = T)) %>% ungroup() %>% 
  mutate(lastcount = replace(lastcount, lastcount==-Inf, NA))

# finish coding 'valid' for controls
hrs_rollmatch_analysis <- hrs_rollmatch_analysis %>% group_by(HHIDPN) %>% 
  mutate(helpcount = row_number()) %>% ungroup()
hrs_rollmatch_analysis <- hrs_rollmatch_analysis %>% 
  mutate(valid = ifelse(is.na(valid) & grandparent==0, helpcount - lastcount - 1, valid)) %>% 
  select(-helpcount, -lastcount)

table(hrs_rollmatch_analysis$grandparent, hrs_rollmatch_analysis$time)
table(hrs_rollmatch_analysis$grandparent, hrs_rollmatch_analysis$valid)
table(hrs_rollmatch_analysis$grandparent, hrs_rollmatch_analysis$year)

hrs_rollmatch_analysis <- hrs_rollmatch_analysis %>% filter(time %in% c(-6:6)) %>% 
  select(-time_match, -match_year)

