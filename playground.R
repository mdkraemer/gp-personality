# Model playground

# load final analyses samples
load(file = "data/processed/LISS/lissanalysis_parents.rda")
load(file = "data/processed/LISS/lissanalysis_nonparents.rda")
load(file = "data/processed/HRS/hrsanalysis_parents.rda")
load(file = "data/processed/HRS/hrsanalysis_nonparents.rda")

#### code piecewise regression coefficients (see Table S1) ####
lissanalysis_parents <- lissanalysis_parents %>% mutate(
  before = ifelse(time<0, time + 6, 5), 
  after = ifelse(time<0, 0, time + 1), 
  shift = ifelse(time<0, 0, 1)
)
lissanalysis_nonparents <- lissanalysis_nonparents %>% mutate(
  before = ifelse(time<0, time + 6, 5), 
  after = ifelse(time<0, 0, time + 1), 
  shift = ifelse(time<0, 0, 1)
)
hrsanalysis_parents <- hrsanalysis_parents %>% mutate(
  before = ifelse(time>=-2, 2, ifelse(time==-6, 0, 1)), 
  after = ifelse(time<0, 0, ifelse(time==2, 2, ifelse(time==4, 3, ifelse(time==6, 4, 1)))), 
  shift = ifelse(time<0, 0, 1)
)
hrsanalysis_nonparents <- hrsanalysis_nonparents %>% mutate(
  before = ifelse(time>=-2, 2, ifelse(time==-6, 0, 1)), 
  after = ifelse(time<0, 0, ifelse(time==2, 2, ifelse(time==4, 3, ifelse(time==6, 4, 1)))), 
  shift = ifelse(time<0, 0, 1)
)
# create hid for HRS (remove last three digits)
hrsanalysis_parents$hid <- as.numeric(gsub('.{3}$', '', hrsanalysis_parents$HHIDPN))
hrsanalysis_nonparents$hid <- as.numeric(gsub('.{3}$', '', hrsanalysis_nonparents$HHIDPN))

hrsanalysis_parents <- hrsanalysis_parents %>% mutate(female = gender - 1) %>% 
  rename(pid = HHIDPN)
hrsanalysis_nonparents <- hrsanalysis_nonparents %>% mutate(female = gender - 1) %>% 
  rename(pid = HHIDPN)
lissanalysis_parents <- lissanalysis_parents %>% rename(pid = nomem_encr, hid = nohouse_encr)
lissanalysis_nonparents <- lissanalysis_nonparents %>% rename(pid = nomem_encr, hid = nohouse_encr)

# code other moderators (only HRS)
hrsanalysis_parents <- hrsanalysis_parents %>% 
  mutate(working = ifelse(paidwork %in% c(1,5), ifelse(paidwork==1, 1, 0), NA))
hrsanalysis_nonparents <- hrsanalysis_nonparents %>% 
  mutate(working = ifelse(paidwork %in% c(1,5), ifelse(paidwork==1, 1, 0), NA))

df_outcomes <- rbind(cbind(maj="Agreeableness", min="agreeableness"),
                     cbind(maj="Conscientiousness", min="conscientiousness"),
                     cbind(maj="Extraversion", min="extraversion"),
                     cbind(maj="Neuroticism", min="neuroticism"),
                     cbind(maj="Openness", min="openness"),
                     cbind(maj="Life Satisfaction", min="life satisfaction"))
rownames(df_outcomes) <- c("agree", "con", "extra", "neur", "open", "swls")


#### descriptives ####
lissanalysis_parents %>% group_by(time) %>% select(agree, con, extra, neur, open, swls) %>% 
  summarize(a_M = mean(agree), 
            a_SD = sd(agree),
            c_M = mean(con), 
            c_SD = sd(con),
            e_M = mean(extra), 
            e_SD = sd(extra),
            n_M = mean(neur), 
            n_SD = sd(neur),
            o_M = mean(open), 
            o_SD = sd(open),
            l_M = mean(swls), 
            l_SD = sd(swls))

descriptives_draw <- function(x) { 
  x %>% 
  group_by(grandparent, time) %>% summarize(a.M = mean(agree, na.rm = T), 
                                            a.SD = sd(agree, na.rm = T),
                                            c.M = mean(con, na.rm = T), 
                                            c.SD = sd(con, na.rm = T),
                                            e.M = mean(extra, na.rm = T), 
                                            e.SD = sd(extra, na.rm = T),
                                            n.M = mean(neur, na.rm = T), 
                                            n.SD = sd(neur, na.rm = T),
                                            o.M = mean(open, na.rm = T), 
                                            o.SD = sd(open, na.rm = T),
                                            l.M = mean(swls, na.rm = T), 
                                            l.SD = sd(swls, na.rm = T)) %>%
  ungroup() %>% 
  mutate_at(.vars = vars(-c(grandparent, time)), 
            ~format(round(., digits=2), nsmall = 2)) %>% # fix at 2 decimal places
  pivot_wider(names_from = time, values_from = -c(time, grandparent)) %>% 
  mutate_if(is.numeric, as.character) %>% # transform num to chr
  mutate_at(.vars = vars(contains("SD")), 
            ~paste0("(", ., ")")) %>% # add parantheses for SD
  pivot_longer(cols = -grandparent, names_to = c("var", ".value"), names_pattern = "(.*)_(.*)") }

list_descriptives <- list(lissanalysis_parents, lissanalysis_nonparents, 
                         hrsanalysis_parents, hrsanalysis_nonparents) %>%
  lapply(descriptives_draw)
names(list_descriptives) <- c("descriptives_liss_p", "descriptives_liss_np", 
                             "descriptives_hrs_p", "descriptives_hrs_np")
list2env(list_descriptives, .GlobalEnv)
rm(list_descriptives)

# we need grandparent obs only once per sample
descriptives_liss_p <- descriptives_liss_p %>% arrange(desc(grandparent))
descriptives_liss_np <- descriptives_liss_np %>% filter(grandparent==0)
descriptives_hrs_p <- descriptives_hrs_p %>% arrange(desc(grandparent))
descriptives_hrs_np <- descriptives_hrs_np %>% filter(grandparent==0)

descriptives_liss <- as.data.frame(rbind(descriptives_liss_p, descriptives_liss_np))
descriptives_liss <- descriptives_liss %>% 
  slice(1:2, 13:14, 25:26, 3:4, 15:16, 27:28, 5:6, 17:18, 29:30, 7:8, 19:20, 31:32, 
        9:10, 21:22, 33:34, 11:12, 23:24, 35:36)
descriptives_liss <- descriptives_liss[,-c(1,2)]
descriptives_hrs <- as.data.frame(rbind(descriptives_hrs_p, descriptives_hrs_np))
descriptives_hrs <- descriptives_hrs %>% 
  slice(1:2, 13:14, 25:26, 3:4, 15:16, 27:28, 5:6, 17:18, 29:30, 7:8, 19:20, 31:32, 
        9:10, 21:22, 33:34, 11:12, 23:24, 35:36)
descriptives_hrs <- descriptives_hrs[,-c(1,2)]
descriptives_hrs <- descriptives_hrs %>% add_column(`-5` = "", `-3` = "", `-1` = "", 
                                                  `1` = "", `3` = "", `5` = "") %>% 
  select(`-6`, `-5`, `-4`, `-3`, `-2`, `-1`, `0`, `1`, `2`, `3`, `4`, `5`, `6`)

#### icc loop ####

# ICCs = proportion of the total variation explained by the respective blocking factor
# ICC_pid = correlation between two randomly selected observations from the same respondent 
# ICC_hid = correlation between two randomly selected observations from the same household
# ICC_pid/hid = correlation between two randomly selected observations from the same respondent and the same household
# https://stats.stackexchange.com/questions/18088/intraclass-correlation-icc-for-an-interaction
outcomes <- c("agree", "con", "extra", "neur", "open", "swls")
datasets <- c("lissanalysis_parents", "lissanalysis_nonparents", 
              "hrsanalysis_parents", "hrsanalysis_nonparents")
icc_list <- as.data.frame(rbind(icc_liss_parents_pid = rep(NA, 6), icc_liss_parents_hid = rep(NA, 6), 
                                icc_liss_parents_pidhid = rep(NA, 6), icc_liss_nonparents_pid = rep(NA, 6),
                                icc_liss_nonparents_hid = rep(NA, 6), 
                                icc_liss_nonparents_pidhid = rep(NA, 6), icc_hrs_parents_pid = rep(NA, 6), 
                                icc_hrs_parents_hid = rep(NA, 6), icc_hrs_parents_pidhid = rep(NA, 6), 
                                icc_hrs_nonparents_pid = rep(NA, 6), icc_hrs_nonparents_hid = rep(NA, 6),
                                icc_hrs_nonparents_pidhid = rep(NA, 6)))
colnames(icc_list) <- outcomes

for (i in 1:length(outcomes)){
  outcome = outcomes[i]
  for (j in 1:length(datasets)){
    dataset = datasets[j]
    pos = seq(from = 1, to = 10, by = 3)[j]
    model <- lme4::lmer(get(outcome) ~ 1 + (1 | pid) + (1 | hid), REML = FALSE, data = get(dataset))
    var <- as.data.frame(summary(model)$varcor)
    icc_pid <- var[1,4] / (var[1,4] + var[2,4] + var[3,4])
    icc_hid <- var[2,4] / (var[1,4] + var[2,4] + var[3,4])
    icc_pidhid <- (var[1,4] + var[2,4]) / (var[1,4] + var[2,4] + var[3,4])
    icc_list[pos, i] <- icc_pid # rows = datasets, columns = outcomes
    icc_list[pos + 1, i] <- icc_hid
    icc_list[pos + 2, i] <- icc_pidhid
  }
}

testiccmod <- lme4::lmer(formula = swls ~ 1 + (1 | pid) + (1 | hid), REML = FALSE, data = hrsanalysis_parents) 
testvar <- as.data.frame(summary(testiccmod)$varcor)
test_icc_pid <- testvar[1,4] / (testvar[1,4] + testvar[2,4] + testvar[3,4])
test_icc_hid <- testvar[2,4] / (testvar[1,4] + testvar[2,4] + testvar[3,4])
test_icc_pidhid <- (testvar[1,4] + testvar[2,4]) / (testvar[1,4] + testvar[2,4] + testvar[3,4])

#### models for mean-level changes ####

# define formulae which will be passed to both lme4::lmer() and lmerTest::lmer()
swls_formula <- as.formula("swls ~ 1 + pscore + before + after + shift + grandparent + 
                                  grandparent:before + grandparent:after + grandparent:shift + 
                                  (1 | pid) + (1 | hid)")
swls_formula_sex <- as.formula("swls ~ 1 + pscore + (before + after + shift + grandparent + 
                                  grandparent:before + grandparent:after + grandparent:shift)*female + 
                                  (1 | pid) + (1 | hid)")

#### swls: LISS parents ####

### basic model

# 1st model is needed (later) because apa_print() and printnum() do not support lmerTest objects yet (or nlme)
liss_swls_parents <- lme4::lmer(formula = swls_formula, REML = FALSE, data = lissanalysis_parents) 
liss_swls_parents_summary <- apa_print(liss_swls_parents)

# 2nd model is needed because we need p-values in the manuscript tables
liss_swls_parents_test <- lmerTest::lmer(formula = swls_formula, REML = FALSE, data = lissanalysis_parents) 

# for better formatting in text
# p-values
liss_swls_parents_p <- as.data.frame(summary(liss_swls_parents_test)$coefficients) %>% 
  rownames_to_column() %>% select(rowname, "Pr(>|t|)") %>% rename(p = "Pr(>|t|)") %>% 
  mutate(p = scales::pvalue(p, prefix = c("$p$ < ", "$p$ = ", "$p$ > "))) %>% column_to_rownames()
liss_swls_parents_p["p"] <- # remove "0" from the chr's 
  lapply(liss_swls_parents_p["p"], gsub, pattern="0\\.", replacement="\\.")
# change beta^hat to gamma^hat (not sure whether this can be done more parsimoniously...)
# Intercept, gamma_00
liss_swls_parents_summary$estimate$Intercept <- 
  lapply(liss_swls_parents_summary$estimate$Intercept, gsub, pattern="beta", replacement="gamma}_{00")
liss_swls_parents_summary$full_result$Intercept <- 
  lapply(liss_swls_parents_summary$full_result$Intercept, gsub, pattern="beta", replacement="gamma}_{00")
# pscore, gamma_02
liss_swls_parents_summary$estimate$pscore <- 
  lapply(liss_swls_parents_summary$estimate$pscore, gsub, pattern="beta", replacement="gamma}_{02")
liss_swls_parents_summary$full_result$pscore <- 
  lapply(liss_swls_parents_summary$full_result$pscore, gsub, pattern="beta", replacement="gamma}_{02")
# before, gamma_10
liss_swls_parents_summary$estimate$before <- 
  lapply(liss_swls_parents_summary$estimate$before, gsub, pattern="beta", replacement="gamma}_{10")
liss_swls_parents_summary$full_result$before <- 
  lapply(liss_swls_parents_summary$full_result$before, gsub, pattern="beta", replacement="gamma}_{10")
# after, gamma_20
liss_swls_parents_summary$estimate$after <- 
  lapply(liss_swls_parents_summary$estimate$after, gsub, pattern="beta", replacement="gamma}_{20")
liss_swls_parents_summary$full_result$after <- 
  lapply(liss_swls_parents_summary$full_result$after, gsub, pattern="beta", replacement="gamma}_{20")
# shift, gamma_30
liss_swls_parents_summary$estimate$shift <- 
  lapply(liss_swls_parents_summary$estimate$shift, gsub, pattern="beta", replacement="gamma}_{30")
liss_swls_parents_summary$full_result$shift <- 
  lapply(liss_swls_parents_summary$full_result$shift, gsub, pattern="beta", replacement="gamma}_{30")
# grandparent, gamma_01
liss_swls_parents_summary$estimate$grandparent <- 
  lapply(liss_swls_parents_summary$estimate$grandparent, gsub, pattern="beta", replacement="gamma}_{01")
liss_swls_parents_summary$full_result$grandparent <- 
  lapply(liss_swls_parents_summary$full_result$grandparent, gsub, pattern="beta", replacement="gamma}_{01")
# before_grandparent, gamma_11
liss_swls_parents_summary$estimate$before_grandparent <- 
  lapply(liss_swls_parents_summary$estimate$before_grandparent, gsub, pattern="beta", replacement="gamma}_{11")
liss_swls_parents_summary$full_result$before_grandparent <- 
  lapply(liss_swls_parents_summary$full_result$before_grandparent, gsub, pattern="beta", replacement="gamma}_{11")
# after_grandparent, gamma_21
liss_swls_parents_summary$estimate$after_grandparent <- 
  lapply(liss_swls_parents_summary$estimate$after_grandparent, gsub, pattern="beta", replacement="gamma}_{21")
liss_swls_parents_summary$full_result$after_grandparent <- 
  lapply(liss_swls_parents_summary$full_result$after_grandparent, gsub, pattern="beta", replacement="gamma}_{21")
# shift_grandparent, gamma_31
liss_swls_parents_summary$estimate$shift_grandparent <- 
  lapply(liss_swls_parents_summary$estimate$shift_grandparent, gsub, pattern="beta", replacement="gamma}_{31")
liss_swls_parents_summary$full_result$shift_grandparent <- 
  lapply(liss_swls_parents_summary$full_result$shift_grandparent, gsub, pattern="beta", replacement="gamma}_{31")

# effect sizes: R^2 for the proportion of explained total variance
pred_swls_parents_liss <- cbind(lissanalysis_parents %>% select(pid, time, swls), 
                                 pred = predict(liss_swls_parents, re.form=NA))
r2_swls_parents_liss <- cor(pred_swls_parents_liss$swls, pred_swls_parents_liss$pred)^2

### moderation by gender model

liss_swls_parents_gender <- lme4::lmer(formula = swls_formula_sex, REML = FALSE, data = lissanalysis_parents) 
liss_swls_parents_gender_summary <- apa_print(liss_swls_parents_gender)

liss_swls_parents_gender_test <- lmerTest::lmer(formula = swls_formula_sex, REML = FALSE, data = lissanalysis_parents) 

# for better formatting in text
# p-values
liss_swls_parents_gender_p <- as.data.frame(summary(liss_swls_parents_gender_test)$coefficients) %>% 
  rownames_to_column() %>% select(rowname, "Pr(>|t|)") %>% rename(p = "Pr(>|t|)") %>% 
  mutate(p = scales::pvalue(p, prefix = c("$p$ < ", "$p$ = ", "$p$ > "))) %>% column_to_rownames()
liss_swls_parents_gender_p["p"] <- # remove "0" from the chr's 
  lapply(liss_swls_parents_gender_p["p"], gsub, pattern="0\\.", replacement="\\.")
# change beta^hat to gamma^hat (not sure whether this can be done more parsimoniously...)
# Intercept, gamma_00
liss_swls_parents_gender_summary$estimate$Intercept <- 
  lapply(liss_swls_parents_gender_summary$estimate$Intercept, gsub, pattern="beta", replacement="gamma}_{00")
liss_swls_parents_gender_summary$full_result$Intercept <- 
  lapply(liss_swls_parents_gender_summary$full_result$Intercept, gsub, pattern="beta", replacement="gamma}_{00")
# pscore, gamma_04
liss_swls_parents_gender_summary$estimate$pscore <- 
  lapply(liss_swls_parents_gender_summary$estimate$pscore, gsub, pattern="beta", replacement="gamma}_{04")
liss_swls_parents_gender_summary$full_result$pscore <- 
  lapply(liss_swls_parents_gender_summary$full_result$pscore, gsub, pattern="beta", replacement="gamma}_{04")
# before, gamma_10
liss_swls_parents_gender_summary$estimate$before <- 
  lapply(liss_swls_parents_gender_summary$estimate$before, gsub, pattern="beta", replacement="gamma}_{10")
liss_swls_parents_gender_summary$full_result$before <- 
  lapply(liss_swls_parents_gender_summary$full_result$before, gsub, pattern="beta", replacement="gamma}_{10")
# after, gamma_20
liss_swls_parents_gender_summary$estimate$after <- 
  lapply(liss_swls_parents_gender_summary$estimate$after, gsub, pattern="beta", replacement="gamma}_{20")
liss_swls_parents_gender_summary$full_result$after <- 
  lapply(liss_swls_parents_gender_summary$full_result$after, gsub, pattern="beta", replacement="gamma}_{20")
# shift, gamma_30
liss_swls_parents_gender_summary$estimate$shift <- 
  lapply(liss_swls_parents_gender_summary$estimate$shift, gsub, pattern="beta", replacement="gamma}_{30")
liss_swls_parents_gender_summary$full_result$shift <- 
  lapply(liss_swls_parents_gender_summary$full_result$shift, gsub, pattern="beta", replacement="gamma}_{30")
# grandparent, gamma_01
liss_swls_parents_gender_summary$estimate$grandparent <- 
  lapply(liss_swls_parents_gender_summary$estimate$grandparent, gsub, pattern="beta", replacement="gamma}_{01")
liss_swls_parents_gender_summary$full_result$grandparent <- 
  lapply(liss_swls_parents_gender_summary$full_result$grandparent, gsub, pattern="beta", replacement="gamma}_{01")
# female, gamma_02
liss_swls_parents_gender_summary$estimate$female <- 
  lapply(liss_swls_parents_gender_summary$estimate$female, gsub, pattern="beta", replacement="gamma}_{02")
liss_swls_parents_gender_summary$full_result$female <- 
  lapply(liss_swls_parents_gender_summary$full_result$female, gsub, pattern="beta", replacement="gamma}_{02")
# before_grandparent, gamma_11
liss_swls_parents_gender_summary$estimate$before_grandparent <- 
  lapply(liss_swls_parents_gender_summary$estimate$before_grandparent, gsub, pattern="beta", replacement="gamma}_{11")
liss_swls_parents_gender_summary$full_result$before_grandparent <- 
  lapply(liss_swls_parents_gender_summary$full_result$before_grandparent, gsub, pattern="beta", replacement="gamma}_{11")
# after_grandparent, gamma_21
liss_swls_parents_gender_summary$estimate$after_grandparent <- 
  lapply(liss_swls_parents_gender_summary$estimate$after_grandparent, gsub, pattern="beta", replacement="gamma}_{21")
liss_swls_parents_gender_summary$full_result$after_grandparent <- 
  lapply(liss_swls_parents_gender_summary$full_result$after_grandparent, gsub, pattern="beta", replacement="gamma}_{21")
# shift_grandparent, gamma_31
liss_swls_parents_gender_summary$estimate$shift_grandparent <- 
  lapply(liss_swls_parents_gender_summary$estimate$shift_grandparent, gsub, pattern="beta", replacement="gamma}_{31")
liss_swls_parents_gender_summary$full_result$shift_grandparent <- 
  lapply(liss_swls_parents_gender_summary$full_result$shift_grandparent, gsub, pattern="beta", replacement="gamma}_{31")
# before_female, gamma_12
liss_swls_parents_gender_summary$estimate$before_female <- 
  lapply(liss_swls_parents_gender_summary$estimate$before_female, gsub, pattern="beta", replacement="gamma}_{12")
liss_swls_parents_gender_summary$full_result$before_female <- 
  lapply(liss_swls_parents_gender_summary$full_result$before_female, gsub, pattern="beta", replacement="gamma}_{12")
# after_female, gamma_22
liss_swls_parents_gender_summary$estimate$after_female <- 
  lapply(liss_swls_parents_gender_summary$estimate$after_female, gsub, pattern="beta", replacement="gamma}_{22")
liss_swls_parents_gender_summary$full_result$after_female <- 
  lapply(liss_swls_parents_gender_summary$full_result$after_female, gsub, pattern="beta", replacement="gamma}_{22")
# shift_female, gamma_32
liss_swls_parents_gender_summary$estimate$shift_female <- 
  lapply(liss_swls_parents_gender_summary$estimate$shift_female, gsub, pattern="beta", replacement="gamma}_{32")
liss_swls_parents_gender_summary$full_result$shift_female <- 
  lapply(liss_swls_parents_gender_summary$full_result$shift_female, gsub, pattern="beta", replacement="gamma}_{32")
# grandparent_female, gamma_03
liss_swls_parents_gender_summary$estimate$grandparent_female <- 
  lapply(liss_swls_parents_gender_summary$estimate$grandparent_female, gsub, pattern="beta", replacement="gamma}_{03")
liss_swls_parents_gender_summary$full_result$grandparent_female <- 
  lapply(liss_swls_parents_gender_summary$full_result$grandparent_female, gsub, pattern="beta", replacement="gamma}_{03")
# before_grandparent_female, gamma_13
liss_swls_parents_gender_summary$estimate$before_grandparent_female <- 
  lapply(liss_swls_parents_gender_summary$estimate$before_grandparent_female, gsub, pattern="beta", replacement="gamma}_{13")
liss_swls_parents_gender_summary$full_result$before_grandparent_female <- 
  lapply(liss_swls_parents_gender_summary$full_result$before_grandparent_female, gsub, pattern="beta", replacement="gamma}_{13")
# after_grandparent_female, gamma_23
liss_swls_parents_gender_summary$estimate$after_grandparent_female <- 
  lapply(liss_swls_parents_gender_summary$estimate$after_grandparent_female, gsub, pattern="beta", replacement="gamma}_{23")
liss_swls_parents_gender_summary$full_result$after_grandparent_female <- 
  lapply(liss_swls_parents_gender_summary$full_result$after_grandparent_female, gsub, pattern="beta", replacement="gamma}_{23")
# shift_grandparent_female, gamma_33
liss_swls_parents_gender_summary$estimate$shift_grandparent_female <- 
  lapply(liss_swls_parents_gender_summary$estimate$shift_grandparent_female, gsub, pattern="beta", replacement="gamma}_{33")
liss_swls_parents_gender_summary$full_result$shift_grandparent_female <- 
  lapply(liss_swls_parents_gender_summary$full_result$shift_grandparent_female, gsub, pattern="beta", replacement="gamma}_{33")

# effect sizes: R^2 for the proportion of explained total variance
pred_swls_parents_gender_liss <- cbind(lissanalysis_parents %>% select(pid, time, swls), 
                                pred = predict(liss_swls_parents_gender, re.form=NA))
r2_swls_parents_gender_liss <- cor(pred_swls_parents_gender_liss$swls, pred_swls_parents_gender_liss$pred)^2

#### swls: LISS nonparents ####

### basic model

liss_swls_nonparents <- lme4::lmer(formula = swls_formula, REML = FALSE, data = lissanalysis_nonparents) 
liss_swls_nonparents_summary <- apa_print(liss_swls_nonparents)

liss_swls_nonparents_test <- lmerTest::lmer(formula = swls_formula, REML = FALSE, data = lissanalysis_nonparents) 

# for better formatting in text
# p-values
liss_swls_nonparents_p <- as.data.frame(summary(liss_swls_nonparents_test)$coefficients) %>% 
  rownames_to_column() %>% select(rowname, "Pr(>|t|)") %>% rename(p = "Pr(>|t|)") %>% 
  mutate(p = scales::pvalue(p, prefix = c("$p$ < ", "$p$ = ", "$p$ > "))) %>% column_to_rownames()
liss_swls_nonparents_p["p"] <- # remove "0" from the chr's 
  lapply(liss_swls_nonparents_p["p"], gsub, pattern="0\\.", replacement="\\.")
# change beta^hat to gamma^hat (not sure whether this can be done more parsimoniously...)
# Intercept, gamma_00
liss_swls_nonparents_summary$estimate$Intercept <- 
  lapply(liss_swls_nonparents_summary$estimate$Intercept, gsub, pattern="beta", replacement="gamma}_{00")
liss_swls_nonparents_summary$full_result$Intercept <- 
  lapply(liss_swls_nonparents_summary$full_result$Intercept, gsub, pattern="beta", replacement="gamma}_{00")
# pscore, gamma_02
liss_swls_nonparents_summary$estimate$pscore <- 
  lapply(liss_swls_nonparents_summary$estimate$pscore, gsub, pattern="beta", replacement="gamma}_{02")
liss_swls_nonparents_summary$full_result$pscore <- 
  lapply(liss_swls_nonparents_summary$full_result$pscore, gsub, pattern="beta", replacement="gamma}_{02")
# before, gamma_10
liss_swls_nonparents_summary$estimate$before <- 
  lapply(liss_swls_nonparents_summary$estimate$before, gsub, pattern="beta", replacement="gamma}_{10")
liss_swls_nonparents_summary$full_result$before <- 
  lapply(liss_swls_nonparents_summary$full_result$before, gsub, pattern="beta", replacement="gamma}_{10")
# after, gamma_20
liss_swls_nonparents_summary$estimate$after <- 
  lapply(liss_swls_nonparents_summary$estimate$after, gsub, pattern="beta", replacement="gamma}_{20")
liss_swls_nonparents_summary$full_result$after <- 
  lapply(liss_swls_nonparents_summary$full_result$after, gsub, pattern="beta", replacement="gamma}_{20")
# shift, gamma_30
liss_swls_nonparents_summary$estimate$shift <- 
  lapply(liss_swls_nonparents_summary$estimate$shift, gsub, pattern="beta", replacement="gamma}_{30")
liss_swls_nonparents_summary$full_result$shift <- 
  lapply(liss_swls_nonparents_summary$full_result$shift, gsub, pattern="beta", replacement="gamma}_{30")
# grandparent, gamma_01
liss_swls_nonparents_summary$estimate$grandparent <- 
  lapply(liss_swls_nonparents_summary$estimate$grandparent, gsub, pattern="beta", replacement="gamma}_{01")
liss_swls_nonparents_summary$full_result$grandparent <- 
  lapply(liss_swls_nonparents_summary$full_result$grandparent, gsub, pattern="beta", replacement="gamma}_{01")
# before_grandparent, gamma_11
liss_swls_nonparents_summary$estimate$before_grandparent <- 
  lapply(liss_swls_nonparents_summary$estimate$before_grandparent, gsub, pattern="beta", replacement="gamma}_{11")
liss_swls_nonparents_summary$full_result$before_grandparent <- 
  lapply(liss_swls_nonparents_summary$full_result$before_grandparent, gsub, pattern="beta", replacement="gamma}_{11")
# after_grandparent, gamma_21
liss_swls_nonparents_summary$estimate$after_grandparent <- 
  lapply(liss_swls_nonparents_summary$estimate$after_grandparent, gsub, pattern="beta", replacement="gamma}_{21")
liss_swls_nonparents_summary$full_result$after_grandparent <- 
  lapply(liss_swls_nonparents_summary$full_result$after_grandparent, gsub, pattern="beta", replacement="gamma}_{21")
# shift_grandparent, gamma_31
liss_swls_nonparents_summary$estimate$shift_grandparent <- 
  lapply(liss_swls_nonparents_summary$estimate$shift_grandparent, gsub, pattern="beta", replacement="gamma}_{31")
liss_swls_nonparents_summary$full_result$shift_grandparent <- 
  lapply(liss_swls_nonparents_summary$full_result$shift_grandparent, gsub, pattern="beta", replacement="gamma}_{31")

# effect sizes: R^2 for the proportion of explained total variance
pred_swls_nonparents_liss <- cbind(lissanalysis_nonparents %>% select(pid, time, swls), 
                           pred = predict(liss_swls_nonparents, re.form=NA))
r2_swls_nonparents_liss <- cor(pred_swls_nonparents_liss$swls, pred_swls_nonparents_liss$pred)^2

### moderation by gender model

liss_swls_nonparents_gender <- lme4::lmer(formula = swls_formula_sex, REML = FALSE, data = lissanalysis_nonparents) 
liss_swls_nonparents_gender_summary <- apa_print(liss_swls_nonparents_gender)

liss_swls_nonparents_gender_test <- lmerTest::lmer(formula = swls_formula_sex, REML = FALSE, data = lissanalysis_nonparents) 

# for better formatting in text
# p-values
liss_swls_nonparents_gender_p <- as.data.frame(summary(liss_swls_nonparents_gender_test)$coefficients) %>% 
  rownames_to_column() %>% select(rowname, "Pr(>|t|)") %>% rename(p = "Pr(>|t|)") %>% 
  mutate(p = scales::pvalue(p, prefix = c("$p$ < ", "$p$ = ", "$p$ > "))) %>% column_to_rownames()
liss_swls_nonparents_gender_p["p"] <- # remove "0" from the chr's 
  lapply(liss_swls_nonparents_gender_p["p"], gsub, pattern="0\\.", replacement="\\.")
# change beta^hat to gamma^hat (not sure whether this can be done more parsimoniously...)
# Intercept, gamma_00
liss_swls_nonparents_gender_summary$estimate$Intercept <- 
  lapply(liss_swls_nonparents_gender_summary$estimate$Intercept, gsub, pattern="beta", replacement="gamma}_{00")
liss_swls_nonparents_gender_summary$full_result$Intercept <- 
  lapply(liss_swls_nonparents_gender_summary$full_result$Intercept, gsub, pattern="beta", replacement="gamma}_{00")
# pscore, gamma_04
liss_swls_nonparents_gender_summary$estimate$pscore <- 
  lapply(liss_swls_nonparents_gender_summary$estimate$pscore, gsub, pattern="beta", replacement="gamma}_{04")
liss_swls_nonparents_gender_summary$full_result$pscore <- 
  lapply(liss_swls_nonparents_gender_summary$full_result$pscore, gsub, pattern="beta", replacement="gamma}_{04")
# before, gamma_10
liss_swls_nonparents_gender_summary$estimate$before <- 
  lapply(liss_swls_nonparents_gender_summary$estimate$before, gsub, pattern="beta", replacement="gamma}_{10")
liss_swls_nonparents_gender_summary$full_result$before <- 
  lapply(liss_swls_nonparents_gender_summary$full_result$before, gsub, pattern="beta", replacement="gamma}_{10")
# after, gamma_20
liss_swls_nonparents_gender_summary$estimate$after <- 
  lapply(liss_swls_nonparents_gender_summary$estimate$after, gsub, pattern="beta", replacement="gamma}_{20")
liss_swls_nonparents_gender_summary$full_result$after <- 
  lapply(liss_swls_nonparents_gender_summary$full_result$after, gsub, pattern="beta", replacement="gamma}_{20")
# shift, gamma_30
liss_swls_nonparents_gender_summary$estimate$shift <- 
  lapply(liss_swls_nonparents_gender_summary$estimate$shift, gsub, pattern="beta", replacement="gamma}_{30")
liss_swls_nonparents_gender_summary$full_result$shift <- 
  lapply(liss_swls_nonparents_gender_summary$full_result$shift, gsub, pattern="beta", replacement="gamma}_{30")
# grandparent, gamma_01
liss_swls_nonparents_gender_summary$estimate$grandparent <- 
  lapply(liss_swls_nonparents_gender_summary$estimate$grandparent, gsub, pattern="beta", replacement="gamma}_{01")
liss_swls_nonparents_gender_summary$full_result$grandparent <- 
  lapply(liss_swls_nonparents_gender_summary$full_result$grandparent, gsub, pattern="beta", replacement="gamma}_{01")
# female, gamma_02
liss_swls_nonparents_gender_summary$estimate$female <- 
  lapply(liss_swls_nonparents_gender_summary$estimate$female, gsub, pattern="beta", replacement="gamma}_{02")
liss_swls_nonparents_gender_summary$full_result$female <- 
  lapply(liss_swls_nonparents_gender_summary$full_result$female, gsub, pattern="beta", replacement="gamma}_{02")
# before_grandparent, gamma_11
liss_swls_nonparents_gender_summary$estimate$before_grandparent <- 
  lapply(liss_swls_nonparents_gender_summary$estimate$before_grandparent, gsub, pattern="beta", replacement="gamma}_{11")
liss_swls_nonparents_gender_summary$full_result$before_grandparent <- 
  lapply(liss_swls_nonparents_gender_summary$full_result$before_grandparent, gsub, pattern="beta", replacement="gamma}_{11")
# after_grandparent, gamma_21
liss_swls_nonparents_gender_summary$estimate$after_grandparent <- 
  lapply(liss_swls_nonparents_gender_summary$estimate$after_grandparent, gsub, pattern="beta", replacement="gamma}_{21")
liss_swls_nonparents_gender_summary$full_result$after_grandparent <- 
  lapply(liss_swls_nonparents_gender_summary$full_result$after_grandparent, gsub, pattern="beta", replacement="gamma}_{21")
# shift_grandparent, gamma_31
liss_swls_nonparents_gender_summary$estimate$shift_grandparent <- 
  lapply(liss_swls_nonparents_gender_summary$estimate$shift_grandparent, gsub, pattern="beta", replacement="gamma}_{31")
liss_swls_nonparents_gender_summary$full_result$shift_grandparent <- 
  lapply(liss_swls_nonparents_gender_summary$full_result$shift_grandparent, gsub, pattern="beta", replacement="gamma}_{31")
# before_female, gamma_12
liss_swls_nonparents_gender_summary$estimate$before_female <- 
  lapply(liss_swls_nonparents_gender_summary$estimate$before_female, gsub, pattern="beta", replacement="gamma}_{12")
liss_swls_nonparents_gender_summary$full_result$before_female <- 
  lapply(liss_swls_nonparents_gender_summary$full_result$before_female, gsub, pattern="beta", replacement="gamma}_{12")
# after_female, gamma_22
liss_swls_nonparents_gender_summary$estimate$after_female <- 
  lapply(liss_swls_nonparents_gender_summary$estimate$after_female, gsub, pattern="beta", replacement="gamma}_{22")
liss_swls_nonparents_gender_summary$full_result$after_female <- 
  lapply(liss_swls_nonparents_gender_summary$full_result$after_female, gsub, pattern="beta", replacement="gamma}_{22")
# shift_female, gamma_32
liss_swls_nonparents_gender_summary$estimate$shift_female <- 
  lapply(liss_swls_nonparents_gender_summary$estimate$shift_female, gsub, pattern="beta", replacement="gamma}_{32")
liss_swls_nonparents_gender_summary$full_result$shift_female <- 
  lapply(liss_swls_nonparents_gender_summary$full_result$shift_female, gsub, pattern="beta", replacement="gamma}_{32")
# grandparent_female, gamma_03
liss_swls_nonparents_gender_summary$estimate$grandparent_female <- 
  lapply(liss_swls_nonparents_gender_summary$estimate$grandparent_female, gsub, pattern="beta", replacement="gamma}_{03")
liss_swls_nonparents_gender_summary$full_result$grandparent_female <- 
  lapply(liss_swls_nonparents_gender_summary$full_result$grandparent_female, gsub, pattern="beta", replacement="gamma}_{03")
# before_grandparent_female, gamma_13
liss_swls_nonparents_gender_summary$estimate$before_grandparent_female <- 
  lapply(liss_swls_nonparents_gender_summary$estimate$before_grandparent_female, gsub, pattern="beta", replacement="gamma}_{13")
liss_swls_nonparents_gender_summary$full_result$before_grandparent_female <- 
  lapply(liss_swls_nonparents_gender_summary$full_result$before_grandparent_female, gsub, pattern="beta", replacement="gamma}_{13")
# after_grandparent_female, gamma_23
liss_swls_nonparents_gender_summary$estimate$after_grandparent_female <- 
  lapply(liss_swls_nonparents_gender_summary$estimate$after_grandparent_female, gsub, pattern="beta", replacement="gamma}_{23")
liss_swls_nonparents_gender_summary$full_result$after_grandparent_female <- 
  lapply(liss_swls_nonparents_gender_summary$full_result$after_grandparent_female, gsub, pattern="beta", replacement="gamma}_{23")
# shift_grandparent_female, gamma_33
liss_swls_nonparents_gender_summary$estimate$shift_grandparent_female <- 
  lapply(liss_swls_nonparents_gender_summary$estimate$shift_grandparent_female, gsub, pattern="beta", replacement="gamma}_{33")
liss_swls_nonparents_gender_summary$full_result$shift_grandparent_female <- 
  lapply(liss_swls_nonparents_gender_summary$full_result$shift_grandparent_female, gsub, pattern="beta", replacement="gamma}_{33")

# effect sizes: R^2 for the proportion of explained total variance
pred_swls_nonparents_gender_liss <- cbind(lissanalysis_nonparents %>% select(pid, time, swls), 
                                       pred = predict(liss_swls_nonparents_gender, re.form=NA))
r2_swls_nonparents_gender_liss <- cor(pred_swls_nonparents_gender_liss$swls, pred_swls_nonparents_gender_liss$pred)^2

#### swls: HRS parents ####

### basic model

hrs_swls_parents <- lme4::lmer(formula = swls_formula, REML = FALSE, data = hrsanalysis_parents) 
hrs_swls_parents_summary <- apa_print(hrs_swls_parents)

hrs_swls_parents_test <- lmerTest::lmer(formula = swls_formula, REML = FALSE, data = hrsanalysis_parents) 

# for better formatting in text
# p-values
hrs_swls_parents_p <- as.data.frame(summary(hrs_swls_parents_test)$coefficients) %>% 
  rownames_to_column() %>% select(rowname, "Pr(>|t|)") %>% rename(p = "Pr(>|t|)") %>% 
  mutate(p = scales::pvalue(p, prefix = c("$p$ < ", "$p$ = ", "$p$ > "))) %>% column_to_rownames()
hrs_swls_parents_p["p"] <- # remove "0" from the chr's 
  lapply(hrs_swls_parents_p["p"], gsub, pattern="0\\.", replacement="\\.")
# change beta^hat to gamma^hat (not sure whether this can be done more parsimoniously...)
# Intercept, gamma_00
hrs_swls_parents_summary$estimate$Intercept <- 
  lapply(hrs_swls_parents_summary$estimate$Intercept, gsub, pattern="beta", replacement="gamma}_{00")
hrs_swls_parents_summary$full_result$Intercept <- 
  lapply(hrs_swls_parents_summary$full_result$Intercept, gsub, pattern="beta", replacement="gamma}_{00")
# pscore, gamma_02
hrs_swls_parents_summary$estimate$pscore <- 
  lapply(hrs_swls_parents_summary$estimate$pscore, gsub, pattern="beta", replacement="gamma}_{02")
hrs_swls_parents_summary$full_result$pscore <- 
  lapply(hrs_swls_parents_summary$full_result$pscore, gsub, pattern="beta", replacement="gamma}_{02")
# before, gamma_10
hrs_swls_parents_summary$estimate$before <- 
  lapply(hrs_swls_parents_summary$estimate$before, gsub, pattern="beta", replacement="gamma}_{10")
hrs_swls_parents_summary$full_result$before <- 
  lapply(hrs_swls_parents_summary$full_result$before, gsub, pattern="beta", replacement="gamma}_{10")
# after, gamma_20
hrs_swls_parents_summary$estimate$after <- 
  lapply(hrs_swls_parents_summary$estimate$after, gsub, pattern="beta", replacement="gamma}_{20")
hrs_swls_parents_summary$full_result$after <- 
  lapply(hrs_swls_parents_summary$full_result$after, gsub, pattern="beta", replacement="gamma}_{20")
# shift, gamma_30
hrs_swls_parents_summary$estimate$shift <- 
  lapply(hrs_swls_parents_summary$estimate$shift, gsub, pattern="beta", replacement="gamma}_{30")
hrs_swls_parents_summary$full_result$shift <- 
  lapply(hrs_swls_parents_summary$full_result$shift, gsub, pattern="beta", replacement="gamma}_{30")
# grandparent, gamma_01
hrs_swls_parents_summary$estimate$grandparent <- 
  lapply(hrs_swls_parents_summary$estimate$grandparent, gsub, pattern="beta", replacement="gamma}_{01")
hrs_swls_parents_summary$full_result$grandparent <- 
  lapply(hrs_swls_parents_summary$full_result$grandparent, gsub, pattern="beta", replacement="gamma}_{01")
# before_grandparent, gamma_11
hrs_swls_parents_summary$estimate$before_grandparent <- 
  lapply(hrs_swls_parents_summary$estimate$before_grandparent, gsub, pattern="beta", replacement="gamma}_{11")
hrs_swls_parents_summary$full_result$before_grandparent <- 
  lapply(hrs_swls_parents_summary$full_result$before_grandparent, gsub, pattern="beta", replacement="gamma}_{11")
# after_grandparent, gamma_21
hrs_swls_parents_summary$estimate$after_grandparent <- 
  lapply(hrs_swls_parents_summary$estimate$after_grandparent, gsub, pattern="beta", replacement="gamma}_{21")
hrs_swls_parents_summary$full_result$after_grandparent <- 
  lapply(hrs_swls_parents_summary$full_result$after_grandparent, gsub, pattern="beta", replacement="gamma}_{21")
# shift_grandparent, gamma_31
hrs_swls_parents_summary$estimate$shift_grandparent <- 
  lapply(hrs_swls_parents_summary$estimate$shift_grandparent, gsub, pattern="beta", replacement="gamma}_{31")
hrs_swls_parents_summary$full_result$shift_grandparent <- 
  lapply(hrs_swls_parents_summary$full_result$shift_grandparent, gsub, pattern="beta", replacement="gamma}_{31")

# effect sizes: R^2 for the proportion of explained total variance
pred_swls_parents_hrs <- cbind(hrsanalysis_parents %>% select(pid, time, swls) %>% filter(!is.na(swls)), 
                           pred = predict(hrs_swls_parents, re.form=NA))
r2_swls_parents_hrs <- cor(pred_swls_parents_hrs$swls, pred_swls_parents_hrs$pred)^2

### moderation by gender model

hrs_swls_parents_gender <- lme4::lmer(formula = swls_formula_sex, REML = FALSE, data = hrsanalysis_parents) 
hrs_swls_parents_gender_summary <- apa_print(hrs_swls_parents_gender)

hrs_swls_parents_gender_test <- lmerTest::lmer(formula = swls_formula_sex, REML = FALSE, data = hrsanalysis_parents) 

# for better formatting in text
# p-values
hrs_swls_parents_gender_p <- as.data.frame(summary(hrs_swls_parents_gender_test)$coefficients) %>% 
  rownames_to_column() %>% select(rowname, "Pr(>|t|)") %>% rename(p = "Pr(>|t|)") %>% 
  mutate(p = scales::pvalue(p, prefix = c("$p$ < ", "$p$ = ", "$p$ > "))) %>% column_to_rownames()
hrs_swls_parents_gender_p["p"] <- # remove "0" from the chr's 
  lapply(hrs_swls_parents_gender_p["p"], gsub, pattern="0\\.", replacement="\\.")
# change beta^hat to gamma^hat (not sure whether this can be done more parsimoniously...)
# Intercept, gamma_00
hrs_swls_parents_gender_summary$estimate$Intercept <- 
  lapply(hrs_swls_parents_gender_summary$estimate$Intercept, gsub, pattern="beta", replacement="gamma}_{00")
hrs_swls_parents_gender_summary$full_result$Intercept <- 
  lapply(hrs_swls_parents_gender_summary$full_result$Intercept, gsub, pattern="beta", replacement="gamma}_{00")
# pscore, gamma_04
hrs_swls_parents_gender_summary$estimate$pscore <- 
  lapply(hrs_swls_parents_gender_summary$estimate$pscore, gsub, pattern="beta", replacement="gamma}_{04")
hrs_swls_parents_gender_summary$full_result$pscore <- 
  lapply(hrs_swls_parents_gender_summary$full_result$pscore, gsub, pattern="beta", replacement="gamma}_{04")
# before, gamma_10
hrs_swls_parents_gender_summary$estimate$before <- 
  lapply(hrs_swls_parents_gender_summary$estimate$before, gsub, pattern="beta", replacement="gamma}_{10")
hrs_swls_parents_gender_summary$full_result$before <- 
  lapply(hrs_swls_parents_gender_summary$full_result$before, gsub, pattern="beta", replacement="gamma}_{10")
# after, gamma_20
hrs_swls_parents_gender_summary$estimate$after <- 
  lapply(hrs_swls_parents_gender_summary$estimate$after, gsub, pattern="beta", replacement="gamma}_{20")
hrs_swls_parents_gender_summary$full_result$after <- 
  lapply(hrs_swls_parents_gender_summary$full_result$after, gsub, pattern="beta", replacement="gamma}_{20")
# shift, gamma_30
hrs_swls_parents_gender_summary$estimate$shift <- 
  lapply(hrs_swls_parents_gender_summary$estimate$shift, gsub, pattern="beta", replacement="gamma}_{30")
hrs_swls_parents_gender_summary$full_result$shift <- 
  lapply(hrs_swls_parents_gender_summary$full_result$shift, gsub, pattern="beta", replacement="gamma}_{30")
# grandparent, gamma_01
hrs_swls_parents_gender_summary$estimate$grandparent <- 
  lapply(hrs_swls_parents_gender_summary$estimate$grandparent, gsub, pattern="beta", replacement="gamma}_{01")
hrs_swls_parents_gender_summary$full_result$grandparent <- 
  lapply(hrs_swls_parents_gender_summary$full_result$grandparent, gsub, pattern="beta", replacement="gamma}_{01")
# female, gamma_02
hrs_swls_parents_gender_summary$estimate$female <- 
  lapply(hrs_swls_parents_gender_summary$estimate$female, gsub, pattern="beta", replacement="gamma}_{02")
hrs_swls_parents_gender_summary$full_result$female <- 
  lapply(hrs_swls_parents_gender_summary$full_result$female, gsub, pattern="beta", replacement="gamma}_{02")
# before_grandparent, gamma_11
hrs_swls_parents_gender_summary$estimate$before_grandparent <- 
  lapply(hrs_swls_parents_gender_summary$estimate$before_grandparent, gsub, pattern="beta", replacement="gamma}_{11")
hrs_swls_parents_gender_summary$full_result$before_grandparent <- 
  lapply(hrs_swls_parents_gender_summary$full_result$before_grandparent, gsub, pattern="beta", replacement="gamma}_{11")
# after_grandparent, gamma_21
hrs_swls_parents_gender_summary$estimate$after_grandparent <- 
  lapply(hrs_swls_parents_gender_summary$estimate$after_grandparent, gsub, pattern="beta", replacement="gamma}_{21")
hrs_swls_parents_gender_summary$full_result$after_grandparent <- 
  lapply(hrs_swls_parents_gender_summary$full_result$after_grandparent, gsub, pattern="beta", replacement="gamma}_{21")
# shift_grandparent, gamma_31
hrs_swls_parents_gender_summary$estimate$shift_grandparent <- 
  lapply(hrs_swls_parents_gender_summary$estimate$shift_grandparent, gsub, pattern="beta", replacement="gamma}_{31")
hrs_swls_parents_gender_summary$full_result$shift_grandparent <- 
  lapply(hrs_swls_parents_gender_summary$full_result$shift_grandparent, gsub, pattern="beta", replacement="gamma}_{31")
# before_female, gamma_12
hrs_swls_parents_gender_summary$estimate$before_female <- 
  lapply(hrs_swls_parents_gender_summary$estimate$before_female, gsub, pattern="beta", replacement="gamma}_{12")
hrs_swls_parents_gender_summary$full_result$before_female <- 
  lapply(hrs_swls_parents_gender_summary$full_result$before_female, gsub, pattern="beta", replacement="gamma}_{12")
# after_female, gamma_22
hrs_swls_parents_gender_summary$estimate$after_female <- 
  lapply(hrs_swls_parents_gender_summary$estimate$after_female, gsub, pattern="beta", replacement="gamma}_{22")
hrs_swls_parents_gender_summary$full_result$after_female <- 
  lapply(hrs_swls_parents_gender_summary$full_result$after_female, gsub, pattern="beta", replacement="gamma}_{22")
# shift_female, gamma_32
hrs_swls_parents_gender_summary$estimate$shift_female <- 
  lapply(hrs_swls_parents_gender_summary$estimate$shift_female, gsub, pattern="beta", replacement="gamma}_{32")
hrs_swls_parents_gender_summary$full_result$shift_female <- 
  lapply(hrs_swls_parents_gender_summary$full_result$shift_female, gsub, pattern="beta", replacement="gamma}_{32")
# grandparent_female, gamma_03
hrs_swls_parents_gender_summary$estimate$grandparent_female <- 
  lapply(hrs_swls_parents_gender_summary$estimate$grandparent_female, gsub, pattern="beta", replacement="gamma}_{03")
hrs_swls_parents_gender_summary$full_result$grandparent_female <- 
  lapply(hrs_swls_parents_gender_summary$full_result$grandparent_female, gsub, pattern="beta", replacement="gamma}_{03")
# before_grandparent_female, gamma_13
hrs_swls_parents_gender_summary$estimate$before_grandparent_female <- 
  lapply(hrs_swls_parents_gender_summary$estimate$before_grandparent_female, gsub, pattern="beta", replacement="gamma}_{13")
hrs_swls_parents_gender_summary$full_result$before_grandparent_female <- 
  lapply(hrs_swls_parents_gender_summary$full_result$before_grandparent_female, gsub, pattern="beta", replacement="gamma}_{13")
# after_grandparent_female, gamma_23
hrs_swls_parents_gender_summary$estimate$after_grandparent_female <- 
  lapply(hrs_swls_parents_gender_summary$estimate$after_grandparent_female, gsub, pattern="beta", replacement="gamma}_{23")
hrs_swls_parents_gender_summary$full_result$after_grandparent_female <- 
  lapply(hrs_swls_parents_gender_summary$full_result$after_grandparent_female, gsub, pattern="beta", replacement="gamma}_{23")
# shift_grandparent_female, gamma_33
hrs_swls_parents_gender_summary$estimate$shift_grandparent_female <- 
  lapply(hrs_swls_parents_gender_summary$estimate$shift_grandparent_female, gsub, pattern="beta", replacement="gamma}_{33")
hrs_swls_parents_gender_summary$full_result$shift_grandparent_female <- 
  lapply(hrs_swls_parents_gender_summary$full_result$shift_grandparent_female, gsub, pattern="beta", replacement="gamma}_{33")

# effect sizes: R^2 for the proportion of explained total variance
pred_swls_parents_gender_hrs <- cbind(hrsanalysis_parents %>% select(pid, time, swls) %>% filter(!is.na(swls)), 
                                       pred = predict(hrs_swls_parents_gender, re.form=NA))
r2_swls_parents_gender_hrs <- cor(pred_swls_parents_gender_hrs$swls, pred_swls_parents_gender_hrs$pred)^2

#### swls: HRS nonparents ####

### basic model

hrs_swls_nonparents <- lme4::lmer(formula = swls_formula, REML = FALSE, data = hrsanalysis_nonparents) 
hrs_swls_nonparents_summary <- apa_print(hrs_swls_nonparents)

hrs_swls_nonparents_test <- lmerTest::lmer(formula = swls_formula, REML = FALSE, data = hrsanalysis_nonparents) 

# for better formatting in text
# p-values
hrs_swls_nonparents_p <- as.data.frame(summary(hrs_swls_nonparents_test)$coefficients) %>% 
  rownames_to_column() %>% select(rowname, "Pr(>|t|)") %>% rename(p = "Pr(>|t|)") %>% 
  mutate(p = scales::pvalue(p, prefix = c("$p$ < ", "$p$ = ", "$p$ > "))) %>% column_to_rownames()
hrs_swls_nonparents_p["p"] <- # remove "0" from the chr's 
  lapply(hrs_swls_nonparents_p["p"], gsub, pattern="0\\.", replacement="\\.")
# change beta^hat to gamma^hat (not sure whether this can be done more parsimoniously...)
# Intercept, gamma_00
hrs_swls_nonparents_summary$estimate$Intercept <- 
  lapply(hrs_swls_nonparents_summary$estimate$Intercept, gsub, pattern="beta", replacement="gamma}_{00")
hrs_swls_nonparents_summary$full_result$Intercept <- 
  lapply(hrs_swls_nonparents_summary$full_result$Intercept, gsub, pattern="beta", replacement="gamma}_{00")
# pscore, gamma_02
hrs_swls_nonparents_summary$estimate$pscore <- 
  lapply(hrs_swls_nonparents_summary$estimate$pscore, gsub, pattern="beta", replacement="gamma}_{02")
hrs_swls_nonparents_summary$full_result$pscore <- 
  lapply(hrs_swls_nonparents_summary$full_result$pscore, gsub, pattern="beta", replacement="gamma}_{02")
# before, gamma_10
hrs_swls_nonparents_summary$estimate$before <- 
  lapply(hrs_swls_nonparents_summary$estimate$before, gsub, pattern="beta", replacement="gamma}_{10")
hrs_swls_nonparents_summary$full_result$before <- 
  lapply(hrs_swls_nonparents_summary$full_result$before, gsub, pattern="beta", replacement="gamma}_{10")
# after, gamma_20
hrs_swls_nonparents_summary$estimate$after <- 
  lapply(hrs_swls_nonparents_summary$estimate$after, gsub, pattern="beta", replacement="gamma}_{20")
hrs_swls_nonparents_summary$full_result$after <- 
  lapply(hrs_swls_nonparents_summary$full_result$after, gsub, pattern="beta", replacement="gamma}_{20")
# shift, gamma_30
hrs_swls_nonparents_summary$estimate$shift <- 
  lapply(hrs_swls_nonparents_summary$estimate$shift, gsub, pattern="beta", replacement="gamma}_{30")
hrs_swls_nonparents_summary$full_result$shift <- 
  lapply(hrs_swls_nonparents_summary$full_result$shift, gsub, pattern="beta", replacement="gamma}_{30")
# grandparent, gamma_01
hrs_swls_nonparents_summary$estimate$grandparent <- 
  lapply(hrs_swls_nonparents_summary$estimate$grandparent, gsub, pattern="beta", replacement="gamma}_{01")
hrs_swls_nonparents_summary$full_result$grandparent <- 
  lapply(hrs_swls_nonparents_summary$full_result$grandparent, gsub, pattern="beta", replacement="gamma}_{01")
# before_grandparent, gamma_11
hrs_swls_nonparents_summary$estimate$before_grandparent <- 
  lapply(hrs_swls_nonparents_summary$estimate$before_grandparent, gsub, pattern="beta", replacement="gamma}_{11")
hrs_swls_nonparents_summary$full_result$before_grandparent <- 
  lapply(hrs_swls_nonparents_summary$full_result$before_grandparent, gsub, pattern="beta", replacement="gamma}_{11")
# after_grandparent, gamma_21
hrs_swls_nonparents_summary$estimate$after_grandparent <- 
  lapply(hrs_swls_nonparents_summary$estimate$after_grandparent, gsub, pattern="beta", replacement="gamma}_{21")
hrs_swls_nonparents_summary$full_result$after_grandparent <- 
  lapply(hrs_swls_nonparents_summary$full_result$after_grandparent, gsub, pattern="beta", replacement="gamma}_{21")
# shift_grandparent, gamma_31
hrs_swls_nonparents_summary$estimate$shift_grandparent <- 
  lapply(hrs_swls_nonparents_summary$estimate$shift_grandparent, gsub, pattern="beta", replacement="gamma}_{31")
hrs_swls_nonparents_summary$full_result$shift_grandparent <- 
  lapply(hrs_swls_nonparents_summary$full_result$shift_grandparent, gsub, pattern="beta", replacement="gamma}_{31")

# effect sizes: R^2 for the proportion of explained total variance
pred_swls_nonparents <- cbind(hrsanalysis_nonparents %>% select(pid, time, swls) %>% filter(!is.na(swls)), 
                              pred = predict(hrs_swls_nonparents, re.form=NA))
r2_swls_nonparents <- cor(pred_swls_nonparents$swls, pred_swls_nonparents$pred)^2


### moderation by gender model

hrs_swls_nonparents_gender <- lme4::lmer(formula = swls_formula_sex, REML = FALSE, data = hrsanalysis_nonparents) 
hrs_swls_nonparents_gender_summary <- apa_print(hrs_swls_nonparents_gender)

hrs_swls_nonparents_gender_test <- lmerTest::lmer(formula = swls_formula_sex, REML = FALSE, data = hrsanalysis_nonparents) 

# for better formatting in text
# p-values
hrs_swls_nonparents_gender_p <- as.data.frame(summary(hrs_swls_nonparents_gender_test)$coefficients) %>% 
  rownames_to_column() %>% select(rowname, "Pr(>|t|)") %>% rename(p = "Pr(>|t|)") %>% 
  mutate(p = scales::pvalue(p, prefix = c("$p$ < ", "$p$ = ", "$p$ > "))) %>% column_to_rownames()
hrs_swls_nonparents_gender_p["p"] <- # remove "0" from the chr's 
  lapply(hrs_swls_nonparents_gender_p["p"], gsub, pattern="0\\.", replacement="\\.")
# change beta^hat to gamma^hat (not sure whether this can be done more parsimoniously...)
# Intercept, gamma_00
hrs_swls_nonparents_gender_summary$estimate$Intercept <- 
  lapply(hrs_swls_nonparents_gender_summary$estimate$Intercept, gsub, pattern="beta", replacement="gamma}_{00")
hrs_swls_nonparents_gender_summary$full_result$Intercept <- 
  lapply(hrs_swls_nonparents_gender_summary$full_result$Intercept, gsub, pattern="beta", replacement="gamma}_{00")
# pscore, gamma_04
hrs_swls_nonparents_gender_summary$estimate$pscore <- 
  lapply(hrs_swls_nonparents_gender_summary$estimate$pscore, gsub, pattern="beta", replacement="gamma}_{04")
hrs_swls_nonparents_gender_summary$full_result$pscore <- 
  lapply(hrs_swls_nonparents_gender_summary$full_result$pscore, gsub, pattern="beta", replacement="gamma}_{04")
# before, gamma_10
hrs_swls_nonparents_gender_summary$estimate$before <- 
  lapply(hrs_swls_nonparents_gender_summary$estimate$before, gsub, pattern="beta", replacement="gamma}_{10")
hrs_swls_nonparents_gender_summary$full_result$before <- 
  lapply(hrs_swls_nonparents_gender_summary$full_result$before, gsub, pattern="beta", replacement="gamma}_{10")
# after, gamma_20
hrs_swls_nonparents_gender_summary$estimate$after <- 
  lapply(hrs_swls_nonparents_gender_summary$estimate$after, gsub, pattern="beta", replacement="gamma}_{20")
hrs_swls_nonparents_gender_summary$full_result$after <- 
  lapply(hrs_swls_nonparents_gender_summary$full_result$after, gsub, pattern="beta", replacement="gamma}_{20")
# shift, gamma_30
hrs_swls_nonparents_gender_summary$estimate$shift <- 
  lapply(hrs_swls_nonparents_gender_summary$estimate$shift, gsub, pattern="beta", replacement="gamma}_{30")
hrs_swls_nonparents_gender_summary$full_result$shift <- 
  lapply(hrs_swls_nonparents_gender_summary$full_result$shift, gsub, pattern="beta", replacement="gamma}_{30")
# grandparent, gamma_01
hrs_swls_nonparents_gender_summary$estimate$grandparent <- 
  lapply(hrs_swls_nonparents_gender_summary$estimate$grandparent, gsub, pattern="beta", replacement="gamma}_{01")
hrs_swls_nonparents_gender_summary$full_result$grandparent <- 
  lapply(hrs_swls_nonparents_gender_summary$full_result$grandparent, gsub, pattern="beta", replacement="gamma}_{01")
# female, gamma_02
hrs_swls_nonparents_gender_summary$estimate$female <- 
  lapply(hrs_swls_nonparents_gender_summary$estimate$female, gsub, pattern="beta", replacement="gamma}_{02")
hrs_swls_nonparents_gender_summary$full_result$female <- 
  lapply(hrs_swls_nonparents_gender_summary$full_result$female, gsub, pattern="beta", replacement="gamma}_{02")
# before_grandparent, gamma_11
hrs_swls_nonparents_gender_summary$estimate$before_grandparent <- 
  lapply(hrs_swls_nonparents_gender_summary$estimate$before_grandparent, gsub, pattern="beta", replacement="gamma}_{11")
hrs_swls_nonparents_gender_summary$full_result$before_grandparent <- 
  lapply(hrs_swls_nonparents_gender_summary$full_result$before_grandparent, gsub, pattern="beta", replacement="gamma}_{11")
# after_grandparent, gamma_21
hrs_swls_nonparents_gender_summary$estimate$after_grandparent <- 
  lapply(hrs_swls_nonparents_gender_summary$estimate$after_grandparent, gsub, pattern="beta", replacement="gamma}_{21")
hrs_swls_nonparents_gender_summary$full_result$after_grandparent <- 
  lapply(hrs_swls_nonparents_gender_summary$full_result$after_grandparent, gsub, pattern="beta", replacement="gamma}_{21")
# shift_grandparent, gamma_31
hrs_swls_nonparents_gender_summary$estimate$shift_grandparent <- 
  lapply(hrs_swls_nonparents_gender_summary$estimate$shift_grandparent, gsub, pattern="beta", replacement="gamma}_{31")
hrs_swls_nonparents_gender_summary$full_result$shift_grandparent <- 
  lapply(hrs_swls_nonparents_gender_summary$full_result$shift_grandparent, gsub, pattern="beta", replacement="gamma}_{31")
# before_female, gamma_12
hrs_swls_nonparents_gender_summary$estimate$before_female <- 
  lapply(hrs_swls_nonparents_gender_summary$estimate$before_female, gsub, pattern="beta", replacement="gamma}_{12")
hrs_swls_nonparents_gender_summary$full_result$before_female <- 
  lapply(hrs_swls_nonparents_gender_summary$full_result$before_female, gsub, pattern="beta", replacement="gamma}_{12")
# after_female, gamma_22
hrs_swls_nonparents_gender_summary$estimate$after_female <- 
  lapply(hrs_swls_nonparents_gender_summary$estimate$after_female, gsub, pattern="beta", replacement="gamma}_{22")
hrs_swls_nonparents_gender_summary$full_result$after_female <- 
  lapply(hrs_swls_nonparents_gender_summary$full_result$after_female, gsub, pattern="beta", replacement="gamma}_{22")
# shift_female, gamma_32
hrs_swls_nonparents_gender_summary$estimate$shift_female <- 
  lapply(hrs_swls_nonparents_gender_summary$estimate$shift_female, gsub, pattern="beta", replacement="gamma}_{32")
hrs_swls_nonparents_gender_summary$full_result$shift_female <- 
  lapply(hrs_swls_nonparents_gender_summary$full_result$shift_female, gsub, pattern="beta", replacement="gamma}_{32")
# grandparent_female, gamma_03
hrs_swls_nonparents_gender_summary$estimate$grandparent_female <- 
  lapply(hrs_swls_nonparents_gender_summary$estimate$grandparent_female, gsub, pattern="beta", replacement="gamma}_{03")
hrs_swls_nonparents_gender_summary$full_result$grandparent_female <- 
  lapply(hrs_swls_nonparents_gender_summary$full_result$grandparent_female, gsub, pattern="beta", replacement="gamma}_{03")
# before_grandparent_female, gamma_13
hrs_swls_nonparents_gender_summary$estimate$before_grandparent_female <- 
  lapply(hrs_swls_nonparents_gender_summary$estimate$before_grandparent_female, gsub, pattern="beta", replacement="gamma}_{13")
hrs_swls_nonparents_gender_summary$full_result$before_grandparent_female <- 
  lapply(hrs_swls_nonparents_gender_summary$full_result$before_grandparent_female, gsub, pattern="beta", replacement="gamma}_{13")
# after_grandparent_female, gamma_23
hrs_swls_nonparents_gender_summary$estimate$after_grandparent_female <- 
  lapply(hrs_swls_nonparents_gender_summary$estimate$after_grandparent_female, gsub, pattern="beta", replacement="gamma}_{23")
hrs_swls_nonparents_gender_summary$full_result$after_grandparent_female <- 
  lapply(hrs_swls_nonparents_gender_summary$full_result$after_grandparent_female, gsub, pattern="beta", replacement="gamma}_{23")
# shift_grandparent_female, gamma_33
hrs_swls_nonparents_gender_summary$estimate$shift_grandparent_female <- 
  lapply(hrs_swls_nonparents_gender_summary$estimate$shift_grandparent_female, gsub, pattern="beta", replacement="gamma}_{33")
hrs_swls_nonparents_gender_summary$full_result$shift_grandparent_female <- 
  lapply(hrs_swls_nonparents_gender_summary$full_result$shift_grandparent_female, gsub, pattern="beta", replacement="gamma}_{33")

# effect sizes: R^2 for the proportion of explained total variance
pred_swls_nonparents_gender_hrs <- cbind(hrsanalysis_nonparents %>% select(pid, time, swls) %>% filter(!is.na(swls)), 
                                      pred = predict(hrs_swls_nonparents_gender, re.form=NA))
r2_swls_nonparents_gender_hrs <- cor(pred_swls_nonparents_gender_hrs$swls, pred_swls_nonparents_gender_hrs$pred)^2


#### models in a loop ####

outcomes <- c("agree", "con", "extra", "neur", "open", "swls")
datasets <- c("lissanalysis_parents", "lissanalysis_nonparents", 
              "hrsanalysis_parents", "hrsanalysis_nonparents")
hid_icc_tbl <- c("icc_liss_parents_hid", "icc_liss_nonparents_hid",
                 "icc_hrs_parents_hid", "icc_hrs_nonparents_hid") # see table 'icc_list'
mod_summaries <- list()
mod_summaries_test <- list()
mod_summaries_gender <- list()
mod_summaries_gender_test <- list()

# run models (save in list object)
for (i in 1:length(outcomes)){
  outcome = outcomes[i]
  print(outcome)
  pos = seq(from = 0, to = 24, by = 4)[i]
  for (j in 1:length(datasets)){
    dataset = datasets[j]
    print(dataset)
    # basic models
    if (icc_list[hid_icc_tbl[j], outcome] < 0.05){ # nesting in hid leads to singular fit for some models
      model_1 <- lme4::lmer(get(outcome) ~ 1 + pscore + before + after + shift + grandparent + 
                              grandparent:before + grandparent:after + grandparent:shift + 
                              (1 | pid), REML = FALSE, data = get(dataset))
      model_2 <- lmerTest::lmer(get(outcome) ~ 1 + pscore + before + after + shift + grandparent + 
                                  grandparent:before + grandparent:after + grandparent:shift + 
                                  (1 | pid), REML = FALSE, data = get(dataset))
    } else { # cross-nesting in pid & hid for all other models
      model_1 <- lme4::lmer(get(outcome) ~ 1 + pscore + before + after + shift + grandparent + 
                              grandparent:before + grandparent:after + grandparent:shift + 
                              (1 | pid) + (1 | hid), REML = FALSE, data = get(dataset))
      model_2 <- lmerTest::lmer(get(outcome) ~ 1 + pscore + before + after + shift + grandparent + 
                                  grandparent:before + grandparent:after + grandparent:shift + 
                                  (1 | pid) + (1 | hid), REML = FALSE, data = get(dataset))
    }      
    mod_summaries[[pos + j]] <- model_1
    names(mod_summaries)[[pos + j]] <- paste0(outcome, "_", dataset)
    mod_summaries_test[[pos + j]] <- model_2
    names(mod_summaries_test)[[pos + j]] <- paste0(outcome, "_", dataset)
    # moderation by gender models
    if (icc_list[hid_icc_tbl[j], outcome] < 0.05){ # nesting in hid leads to singular fit for some models
      model_3 <- lme4::lmer(get(outcome) ~ 1 + pscore + (before + after + shift + grandparent + 
                              grandparent:before + grandparent:after + grandparent:shift)*female + 
                              (1 | pid), REML = FALSE, data = get(dataset))
      model_4 <- lmerTest::lmer(get(outcome) ~ 1 + pscore + (before + after + shift + grandparent + 
                                  grandparent:before + grandparent:after + grandparent:shift)*female + 
                                  (1 | pid), REML = FALSE, data = get(dataset))
    } else { # cross-nesting in pid & hid for all other models
      model_3 <- lme4::lmer(get(outcome) ~ 1 + pscore + (before + after + shift + grandparent + 
                              grandparent:before + grandparent:after + grandparent:shift)*female + 
                              (1 | pid) + (1 | hid), REML = FALSE, data = get(dataset))
      model_4 <- lmerTest::lmer(get(outcome) ~ 1 + pscore + (before + after + shift + grandparent + 
                                  grandparent:before + grandparent:after + grandparent:shift)*female + 
                                  (1 | pid) + (1 | hid), REML = FALSE, data = get(dataset))
    }      
    mod_summaries_gender[[pos + j]] <- model_3
    names(mod_summaries_gender)[[pos + j]] <- paste0(outcome, "_", dataset)
    mod_summaries_gender_test[[pos + j]] <- model_4
    names(mod_summaries_gender_test)[[pos + j]] <- paste0(outcome, "_", dataset)
    # R^2
    pred <- cbind(subset(get(dataset), !is.na(get(outcome)), select = c(pid, time, get(outcome))), 
                  pred_outcome = predict(model_1, re.form=NA)) # model predicted outcome values (basic)
    r2 <- cor(pred[outcomes[i]], pred$pred_outcome)^2
    r2_name <- paste0("r2_", outcome, "_", dataset)
    eval(call("<-", as.name(r2_name), r2))
    pred_gender <- cbind(subset(get(dataset), !is.na(get(outcome)), select = c(pid, time, get(outcome))), 
                         pred_outcome = predict(model_3, re.form=NA)) # model predicted outcome values (gender)
    r2_gender <- cor(pred_gender[outcomes[i]], pred_gender$pred_outcome)^2
    r2_gender_name <- paste0("r2_", outcome, "_", dataset, "_gender")
    eval(call("<-", as.name(r2_gender_name), r2_gender))
  }
}

datasets_short <- c("liss_parents", "liss_nonparents", "hrs_parents", "hrs_nonparents")
coefs <- c("Intercept", "pscore", "before", "after", "shift", "grandparent", 
           "before_grandparent", "after_grandparent", "shift_grandparent") # the way they are named in the 'papaja' objects
gammas <- c("gamma}_{00", "gamma}_{02", "gamma}_{10", "gamma}_{20", "gamma}_{30", "gamma}_{01", 
            "gamma}_{11", "gamma}_{21", "gamma}_{31")
coefs_gender <- c("Intercept", "pscore", "before", "after", "shift", "grandparent", 
                  "female", "before_grandparent", "after_grandparent", "shift_grandparent",
                  "before_female", "after_female", "shift_female", "grandparent_female", 
                  "before_grandparent_female", "after_grandparent_female", "shift_grandparent_female")
gammas_gender <- c("gamma}_{00", "gamma}_{04", "gamma}_{10", "gamma}_{20", "gamma}_{30", "gamma}_{01", 
                   "gamma}_{02", "gamma}_{11", "gamma}_{21", "gamma}_{31",
                   "gamma}_{12", "gamma}_{22", "gamma}_{32", "gamma}_{03", 
                   "gamma}_{13", "gamma}_{23", "gamma}_{33")

for (i in 1:length(outcomes)){
  outcome = outcomes[i]
  pos = seq(from = 0, to = 24, by = 4)[i]
  for (j in 1:length(datasets_short)){
    ### basic models
    dataset = datasets_short[j]
    obj = apa_print(mod_summaries[[pos + j]]) # unfold list objects (lme4)
    obj_test = mod_summaries_test[[pos + j]] # unfold list objects (lmerTest)
    # reformatting: change beta^hat to gamma^hat (plus subscripts)
    for (k in 1:length(coefs)){ 
      coef <- coefs[k] # 9 coefficients for each model (object)
      gamma <- gammas[k]
      obj$estimate[coef] <- lapply(obj$estimate[coef], gsub, pattern="beta", replacement=gamma)
      obj$full_result[coef] <- lapply(obj$full_result[coef], gsub, pattern="beta", replacement=gamma)
    }
    obj_name <- paste0(outcome, "_", dataset, "_summary")
    eval(call("<-", as.name(obj_name), obj)) # save lme4 model
    obj_name_test <- paste0(outcome, "_", dataset, "_test")
    eval(call("<-", as.name(obj_name_test), obj_test)) # save lmerTest model
    # for better formatting in text: p-values
    obj_p <- as.data.frame(summary(mod_summaries_test[[pos + j]])$coefficients) %>% # unfold list
      rownames_to_column() %>% select(rowname, "Pr(>|t|)") %>% rename(p = "Pr(>|t|)") %>% 
      mutate(p = scales::pvalue(p, prefix = c("$p$ < ", "$p$ = ", "$p$ > "))) %>% column_to_rownames()
    obj_p["p"] <- # remove "0" from the chr's 
      lapply(obj_p["p"], gsub, pattern="0\\.", replacement="\\.")
    obj_name_p <- paste0(outcome, "_", dataset, "_p")
    eval(call("<-", as.name(obj_name_p), obj_p)) # save objects
    ### moderation by gender models
    obj_gender = apa_print(mod_summaries_gender[[pos + j]]) # unfold list objects
    obj_gender_test = mod_summaries_gender_test[[pos + j]] # unfold list objects (lmerTest)
    # reformatting: change beta^hat to gamma^hat (plus subscripts)
    for (k in 1:length(coefs_gender)){ 
      coef <- coefs_gender[k] # 17 coefficients for each model (object)
      gamma <- gammas_gender[k]
      obj_gender$estimate[coef] <- lapply(obj_gender$estimate[coef], gsub, pattern="beta", replacement=gamma)
      obj_gender$full_result[coef] <- lapply(obj_gender$full_result[coef], gsub, pattern="beta", replacement=gamma)
    }
    obj_gender_name <- paste0(outcome, "_", dataset, "_gender_summary")
    eval(call("<-", as.name(obj_gender_name), obj_gender)) # save object
    obj_gender_test_name <- paste0(outcome, "_", dataset, "_gender_test")
    eval(call("<-", as.name(obj_gender_test_name), obj_gender_test)) # save object
    # for better formatting in text: p-values
    obj_gender_p <- as.data.frame(summary(mod_summaries_gender_test[[pos + j]])$coefficients) %>% # unfold list
      rownames_to_column() %>% select(rowname, "Pr(>|t|)") %>% rename(p = "Pr(>|t|)") %>% 
      mutate(p = scales::pvalue(p, prefix = c("$p$ < ", "$p$ = ", "$p$ > "))) %>% column_to_rownames()
    obj_gender_p["p"] <- # remove "0" from the chr's 
      lapply(obj_gender_p["p"], gsub, pattern="0\\.", replacement="\\.")
    obj_gender_name_p <- paste0(outcome, "_", dataset, "_gender_p")
    eval(call("<-", as.name(obj_gender_name_p), obj_gender_p)) # save objects  
    }
}
# create predicted values data.frames for plots
# sources: 
# http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#lme/
# https://stackoverflow.com/questions/14358811/extract-prediction-band-from-lme-fit/
# https://stats.stackexchange.com/questions/29690/getting-fixed-effect-only-predictions-from-mixed-model-on-new-data-in-r/

for (i in 1:length(outcomes)){
  outcome = outcomes[i]
  pos = seq(from = 0, to = 24, by = 4)[i]
  for (j in 1:length(datasets_short)){
    ### basic models
    dataset = datasets[j]
    dataset_short = datasets_short[j]
    obj_test = mod_summaries_test[[pos + j]] # unfold list objects (lmerTest)
    # create data.frame (with all predictors)
    ps_mean <- as.data.frame(subset(get(dataset), !is.na(get(outcome)) & time==0) %>% 
                               group_by(grandparent) %>% summarise(pscore = mean(pscore))) # pscore mean
    # different time sequences for LISS/HRS
    if (dataset_short %in% c("liss_parents", "liss_nonparents")){ 
      dframe <- data.frame( # for LISS
        pscore = c(rep(ps_mean[1,2], 13),  # controls
                   rep(ps_mean[2,2], 13)), # grandparents
        before = rep(c(0:5, rep(5, 7)), 2),
        after = rep(c(rep(0, 6), 1:7), 2),
        shift = rep(c(rep(0, 6), rep(1, 7)), 2),
        grandparent = c(rep(0, 13), rep(1, 13)),
        x = rep(-6:6, 2)
      )
    } else { # for HRS
      dframe <- data.frame(
        pscore = c(rep(ps_mean[1,2], 7), 
                   rep(ps_mean[2,2], 7)),
        before = rep(c(0:2, rep(2, 4)), 2),
        after = rep(c(rep(0, 3), 1:4), 2),
        shift = rep(c(rep(0, 3), rep(1, 4)), 2),
        grandparent = c(rep(0, 7), rep(1, 7)),
        x = rep(seq(-6, 6, by=2), 2)
      )
    }      
    # predict response (again, same procedure for LISS & HRS)
    dframe$pred <- predict(obj_test, newdata = dframe, re.form=NA)
    # create design matrix
    designmat <- model.matrix(as.formula(lme4::nobars(formula(obj_test))[-2]), 
                              dframe) # [-2] drops response from formula
    # compute standard error
    predvar <- diag(designmat %*% vcov(obj_test) %*% t(designmat)) 
    dframe$SE <- sqrt(predvar) # for confidence intervals
    # add grandparent variable as a factor
    if (dataset_short %in% c("liss_parents", "hrs_parents")){ 
      dframe$gpgroup <- factor(dframe$grandparent, labels=c("Parent\nControls","Grandparents"))
    } else { # for nonparent controls
      dframe$gpgroup <- factor(dframe$grandparent, labels=c("Nonparent\nControls","Grandparents"))
    }      
    dframe$gpgroup <- fct_rev(dframe$gpgroup)
    dframe_name <- paste0("dframe_", outcome, "_", dataset_short)
    eval(call("<-", as.name(dframe_name), dframe)) # save data.frame for later ggplot use
    ### moderation by gender models
    obj_test_gender = mod_summaries_gender_test[[pos + j]] # unfold list objects (lmerTest)
    # create data.frame (with all predictors)
    ps_mean_gender <- as.data.frame(subset(get(dataset), !is.na(get(outcome)) & time==0) %>% 
                               group_by(grandparent, female) %>% summarise(pscore = mean(pscore)))
    # different time sequences for LISS/HRS
    if (dataset_short %in% c("liss_parents", "liss_nonparents")){ 
      dframe_gender <- data.frame(
        pscore = c(rep(ps_mean_gender[1,3], 13),  # male controls
                   rep(ps_mean_gender[2,3], 13),  # female controls
                   rep(ps_mean_gender[3,3], 13),  # male grandparents
                   rep(ps_mean_gender[4,3], 13)), # female grandparents
        before = rep(c(0:5, rep(5, 7)), 4),
        after = rep(c(rep(0, 6), 1:7), 4),
        shift = rep(c(rep(0, 6), rep(1, 7)), 4),
        grandparent = c(rep(0, 26), rep(1, 26)),
        female = rep(c(rep(0, 13), rep(1, 13)), 2),
        x = rep(-6:6, 4)
      )
    } else { # for HRS
      dframe_gender <- data.frame(
        pscore = c(rep(ps_mean_gender[1,3], 7),  # male controls
                   rep(ps_mean_gender[2,3], 7),  # female controls
                   rep(ps_mean_gender[3,3], 7),  # male grandparents
                   rep(ps_mean_gender[4,3], 7)), # female grandparents
        before = rep(c(0:2, rep(2, 4)), 4),
        after = rep(c(rep(0, 3), 1:4), 4),
        shift = rep(c(rep(0, 3), rep(1, 4)), 4),
        grandparent = c(rep(0, 14), rep(1, 14)),
        female = rep(c(rep(0, 7), rep(1, 7)), 2),
        x = rep(seq(-6, 6, by=2), 2)
      )
    }      
    # predict response (again, same procedure for LISS & HRS)
    dframe_gender$pred <- predict(obj_test_gender, newdata = dframe_gender, re.form=NA)
    # create design matrix
    designmat_gender <- model.matrix(as.formula(lme4::nobars(formula(obj_test_gender))[-2]), 
                              dframe_gender) # [-2] drops response from formula
    # compute standard error
    predvar_gender <- diag(designmat_gender %*% vcov(obj_test_gender) %*% t(designmat_gender)) 
    dframe_gender$SE <- sqrt(predvar_gender) # for confidence intervals
    # add grandparent variable as a factor
    if (dataset_short %in% c("liss_parents", "hrs_parents")){ 
      dframe_gender$gpgroup <- factor(dframe_gender$grandparent, 
                                      labels=c("Parent\nControls","Grandparents"))
    } else { # for nonparent controls
      dframe_gender$gpgroup <- factor(dframe_gender$grandparent, 
                                      labels=c("Nonparent\nControls","Grandparents"))
    }      
    dframe_gender$gpgroup <- fct_rev(dframe_gender$gpgroup)
    # add female variable as a factor
    dframe_gender$gender <- factor(dframe_gender$female, labels=c("Men","Women"))
    dframe_gender$gender <- fct_rev(dframe_gender$gender)
    dframe_name_gender <- paste0("dframe_", outcome, "_", dataset_short, "_gender")
    eval(call("<-", as.name(dframe_name_gender), dframe_gender)) # save data.frame for later ggplot use
  }
}

# remove list objects from environment
rm(mod_summaries, mod_summaries_test, mod_summaries_gender, mod_summaries_gender_test)


#### plots ####

# create data.frame with new values for predictors
pscore_liss_parents <- as.data.frame(lissanalysis_parents %>% # take pscore mean
  filter(time==0 & !is.na(swls)) %>% group_by(grandparent) %>% summarise(pscore = mean(pscore)))

dframe.swls_liss_parents <- data.frame(
  pscore = c(rep(pscore_liss_parents[1,2], 13), 
             rep(pscore_liss_parents[2,2], 13)),
  before = rep(c(0:5, rep(5, 7)), 2),
  after = rep(c(rep(0, 6), 1:7), 2),
  shift = rep(c(rep(0, 6), rep(1, 7)), 2),
  grandparent= c(rep(0, 13), rep(1, 13))
)
# sources: 
# http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#lme/
# https://stackoverflow.com/questions/14358811/extract-prediction-band-from-lme-fit/
# https://stats.stackexchange.com/questions/29690/getting-fixed-effect-only-predictions-from-mixed-model-on-new-data-in-r/

# predict response
dframe.swls_liss_parents$pred <- predict(swls_liss_parents_test, 
                                         newdata = dframe.swls_liss_parents, re.form=NA)
# create design matrix
designmat.swls_liss_parents <- model.matrix(as.formula(lme4::nobars(formula(swls_liss_parents_test))[-2]), 
                                            dframe.swls_liss_parents) # [-2] drops response from formula
# compute standard error for predictions
predvar.swls_liss_parents <- diag(designmat.swls_liss_parents %*% 
                                    vcov(swls_liss_parents_test) %*% 
                                    t(designmat.swls_liss_parents)) 
dframe.swls_liss_parents$SE <- sqrt(predvar.swls_liss_parents) # for confidence intervals

# add x-axis variable
dframe.swls_liss_parents$x <- rep(-6:6, 2)
# add parent variable as a factor
dframe.swls_liss_parents$gpgroup <- factor(dframe.swls_liss_parents$grandparent, 
                                           labels=c("Parent Controls","Grandparents"))
dframe.swls_liss_parents$gpgroup <- fct_rev(dframe.swls_liss_parents$gpgroup)

plot.swls_liss_parents <- ggplot(dframe.swls_liss_parents, aes(x=x,y=pred,colour=gpgroup))+
  geom_line(position=position_dodge(width=0.2),size=1.15)+
  geom_point(position=position_dodge(width=0.2),size=1.75)+
  scale_colour_discrete(name="Group")+
  geom_errorbar(aes(ymin=pred-1.96*SE,ymax=pred+1.96*SE),width=0.6,position=position_dodge(width=0.2))+
  coord_cartesian(ylim=c(4,6))+
  geom_vline(xintercept=-0.5)+
  theme(axis.text = element_text(face="bold", size=14),
        axis.title = element_text(size=18),
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        panel.border=element_rect(colour = "black", fill=NA, size=1))+
  scale_x_continuous(name="Time (in Years)",breaks=c(-6:6))+
  scale_y_continuous(name="Life Satisfaction")
plot.swls_liss_parents


#create data.frame with new values for predictors
#more than one predictor is possible
pscore_liss_parents_gender <- as.data.frame(lissanalysis_parents %>% # take pscore mean
                                       filter(time==0 & !is.na(swls)) %>% group_by(grandparent, female) %>% summarise(pscore = mean(pscore)))

dframe.swls_liss_parents_gender <- data.frame(
  pscore = c(rep(pscore_liss_parents_gender[1,3], 13),  # male controls
             rep(pscore_liss_parents_gender[2,3], 13),  # female controls
             rep(pscore_liss_parents_gender[3,3], 13),  # male grandparents
             rep(pscore_liss_parents_gender[4,3], 13)), # female grandparents
  before = rep(c(0:5, rep(5, 7)), 4),
  after = rep(c(rep(0, 6), 1:7), 4),
  shift = rep(c(rep(0, 6), rep(1, 7)), 4),
  grandparent = rep(c(rep(0, 13), rep(1, 13)), 2),
  female = c(rep(0, 26), rep(1, 26))
)

#predict response
dframe.life.ints$pred <- predict(life_holdout1_ints,newdata=dframe.life.ints,level=0)
#create design matrix
designmat.life.ints <- model.matrix(formula(life_holdout1_ints)[-2], dframe.life.ints) #[-2] drops response from formula
#compute standard error for predictions
predvar.life.ints <- diag(designmat.life.ints %*% vcov(life_holdout1_ints) %*% t(designmat.life.ints)) 
dframe.life.ints$SE <- sqrt(predvar.life.ints) #for confidence intervals

#add x-axis variable
dframe.life.ints$x <- c(0,1,2,3,4,5,6,7,0,1,2,3,4,5,6,7,0,1,2,3,4,5,6,7,0,1,2,3,4,5,6,7)
#add parent variable as a factor 
dframe.life.ints$parentgroup <- factor(dframe.life.ints$parent, labels=c("Controls","Parents"))
dframe.life.ints$parentgroup <- reverse.levels(dframe.life.ints$parentgroup) #from 'likert' library
#add gender variable as a factor
dframe.life.ints$gender <- factor(dframe.life.ints$female, labels=c("Men","Women"))
dframe.life.ints$gender <- reverse.levels(dframe.life.ints$gender) #from 'likert' library

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# To use for line and point colors, add
scale_colour_manual(values=cbbPalette)

plot.swls_liss_parents_gender <- ggplot(dframe_swls_liss_parents_gender,aes(x=x,y=pred,colour=gpgroup,linetype=gender))+
  geom_line(position=position_dodge(width=0.2),size=1.15)+
  geom_point(position=position_dodge(width=0.2),size=1.75)+
  #scale_colour_discrete()+
  scale_colour_brewer(palette = "Set1", name="Group")+ 
  scale_linetype_discrete(name="Gender")+
  geom_errorbar(aes(ymin=pred-1.96*SE,ymax=pred+1.96*SE),width=0.6,position=position_dodge(width=0.2))+
  coord_cartesian(ylim=c(4,6))+
  geom_vline(xintercept=-0.5)+
  theme(axis.text = element_text(face="bold", size=14),
        axis.title = element_text(size=18),
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        panel.border=element_rect(colour = "black", fill=NA, size=1))+
  scale_x_continuous(name="Time (in Years)",breaks=c(-6:6))+
  scale_y_continuous(name="Life Satisfaction")
plot.swls_liss_parents_gender

#dframe_swls_liss_gender_3G <- rbind(dframe_swls_liss_parents_gender, 
#                                    dframe_swls_liss_nonparents_gender[1:26,])

#### plot loop ####
outcomes_plots <- c(rep("Agreeableness", 4), rep("Conscientiousness", 4), rep("Extraversion", 4),  
                   rep("Neuroticism", 4), rep("Openness", 4), rep("Life Satisfaction", 4))
# y-axis limits (same span but different sections)
limits_lower <- c(rep(3, 4), rep(3, 4), rep(2.5, 4), rep(1.5, 4), rep(2.5, 4), rep(4.25, 4)) 
limits_upper <- c(rep(4.5, 4), rep(4.5, 4), rep(4, 4), rep(3, 4), rep(4, 4), rep(5.75, 4))
limits <- cbind(limits_lower, limits_upper)
rownames(limits) <- outcomes_plots

# collect data frames in correct order (ACENO+LS -> LISS-p, LISS-np, HRS-p, HRS-np)
dframes <- c(sort(ls()[grep("^(?=.*dframe_agree)(?!.*gender)(?!.*care)", ls(), perl=T)], decreasing=T),
             sort(ls()[grep("^(?=.*dframe_con)(?!.*gender)(?!.*care)", ls(), perl=T)], decreasing=T),
             sort(ls()[grep("^(?=.*dframe_extra)(?!.*gender)(?!.*care)", ls(), perl=T)], decreasing=T),
             sort(ls()[grep("^(?=.*dframe_neur)(?!.*gender)(?!.*care)", ls(), perl=T)], decreasing=T),
             sort(ls()[grep("^(?=.*dframe_open)(?!.*gender)(?!.*care)", ls(), perl=T)], decreasing=T),
             sort(ls()[grep("^(?=.*dframe_swls)(?!.*gender)(?!.*care)", ls(), perl=T)], decreasing=T))
dframes_gender <- c(sort(ls()[grep("^(?=.*dframe_agree)(?=.*gender)", ls(), perl=T)], decreasing=T),
                    sort(ls()[grep("^(?=.*dframe_con)(?=.*gender)", ls(), perl=T)], decreasing=T),
                    sort(ls()[grep("^(?=.*dframe_extra)(?=.*gender)", ls(), perl=T)], decreasing=T),
                    sort(ls()[grep("^(?=.*dframe_neur)(?=.*gender)", ls(), perl=T)], decreasing=T),    
                    sort(ls()[grep("^(?=.*dframe_open)(?=.*gender)", ls(), perl=T)], decreasing=T),
                    sort(ls()[grep("^(?=.*dframe_swls)(?=.*gender)", ls(), perl=T)], decreasing=T))

for (i in 1:length(dframes)){
  ### basic
  plot <- ggplot(get(dframes[i]), aes(x=x,y=pred,colour=gpgroup))+
    geom_line(position=position_dodge(width=0.2),size=1.15)+
    geom_point(position=position_dodge(width=0.2),size=1.75)+
    scale_colour_brewer(palette = "Set1", name="Group")+ 
    geom_errorbar(aes(ymin=pred-1.96*SE,ymax=pred+1.96*SE),width=0.6,position=position_dodge(width=0.2))+
    coord_cartesian(ylim=c(limits[i, 1], limits[i, 2]))+ # loop over limits as defined above
    theme(axis.text = element_text(face="bold"), #, size=14
          #axis.title = element_text(size=18),
          #legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
          #legend.text=element_text(size=14),
          #legend.title=element_text(size=14),
          legend.position = "none",
          panel.border=element_rect(colour = "black", fill=NA, size=1))+
    scale_y_continuous(name=outcomes_plots[i]) 
  if (dframes[i] %in% dframes[grep("^(?=.*liss)", dframes, perl=T)]){ # filter LISS
    plot <- plot + scale_x_continuous(name="Time (in Years)",breaks=c(-6:6)) +
      geom_vline(xintercept=-0.5, colour="darkgrey") # LISS
  } else { 
    plot <- plot + scale_x_continuous(name="Time (in Years)",breaks=seq(-6,6,2)) + 
    geom_vline(xintercept=-1, colour="darkgrey") # HRS
  }      
  if (dframes[i] %in% dframes[grep("^(?=.*_parents)", dframes, perl=T)]){ # filter parent df's
    plot <- plot + theme(axis.title.x=element_blank())
  } 
  plot_name <- gsub("dframe", "plot", dframes[i])
  eval(call("<-", as.name(plot_name), plot)) # save plots for later assembly
  ### moderation by gender
  plot_gender <- ggplot(get(dframes_gender[i]), aes(x=x,y=pred,colour=gpgroup,linetype=gender))+
    geom_line(position=position_dodge(width=0.2),size=1.15)+
    geom_point(position=position_dodge(width=0.2),size=1.75)+
    scale_colour_brewer(palette = "Set1", name="Group")+ 
    scale_linetype_discrete(name="Gender")+
    geom_errorbar(aes(ymin=pred-1.96*SE,ymax=pred+1.96*SE),width=0.6,position=position_dodge(width=0.2))+
    coord_cartesian(ylim=c(limits[i, 1], limits[i, 2]))+ # loop over limits as defined above
    theme(axis.text = element_text(face="bold"), #, size=14
          #axis.title = element_text(size=18),
          legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
          #legend.text=element_text(size=14),
          #legend.title=element_text(size=14),
          panel.border=element_rect(colour = "black", fill=NA, size=1),
          axis.title.y=element_blank())+
    guides(colour = guide_legend(order = 1), linetype = guide_legend(order = 2))+ # legend element order
    scale_y_continuous() # name=outcomes_plots[i]
  if (dframes_gender[i] %in% dframes_gender[grep("^(?=.*liss)", dframes_gender, perl=T)]){ # filter LISS
    plot_gender <- plot_gender + scale_x_continuous(name="Time (in Years)",breaks=c(-6:6)) +
      geom_vline(xintercept=-0.5, colour="darkgrey") # LISS
  } else { 
    plot_gender <- plot_gender + scale_x_continuous(name="Time (in Years)",breaks=seq(-6,6,2)) + 
      geom_vline(xintercept=-1, colour="darkgrey") # HRS
  }      
  if (dframes_gender[i] %in% dframes_gender[grep("^(?=.*_parents_)", dframes_gender, perl=T)]){ 
    plot_gender <- plot_gender + theme(axis.title.x=element_blank()) # omit x-axis title for parent plots
  } 
  plot_name_gender <- gsub("dframe", "plot", dframes_gender[i])
  eval(call("<-", as.name(plot_name_gender), plot_gender)) # save plots for later assembly
}

#### plot grid ####

# extract the legends
legend_parents <- get_legend(plot_agree_liss_parents_gender + 
                               theme(legend.box.margin = margin(0, 0, 0, 20)))
legend_nonparents <- get_legend(plot_agree_liss_nonparents_gender + 
                                  theme(legend.box.margin = margin(0, 0, 0, 20)))

# add the legend to the row we made earlier - adjust width via rel_widths
grid_agree_liss <- cowplot::plot_grid(
  plot_agree_liss_parents, 
  plot_agree_liss_parents_gender + theme(legend.position="none"),
  plot_agree_liss_nonparents, 
  plot_agree_liss_nonparents_gender + theme(legend.position="none"), 
  align = "vh", ncol = 2)
grid_agree_liss <- cowplot::plot_grid(
  grid_agree_liss, 
  cowplot::plot_grid(legend_parents, legend_nonparents, ncol = 1),
  rel_widths = c(3, .4), ncol = 2)

grid_agree_hrs <- cowplot::plot_grid(
  plot_agree_hrs_parents, 
  plot_agree_hrs_parents_gender + theme(legend.position="none"),
  plot_agree_hrs_nonparents, 
  plot_agree_hrs_nonparents_gender + theme(legend.position="none"), 
  align = "vh", ncol = 2)
grid_agree_hrs <- cowplot::plot_grid(
  grid_agree_hrs, 
  cowplot::plot_grid(legend_parents, legend_nonparents, ncol = 1),
  rel_widths = c(3, .4), ncol = 2)

# now add the title
title_liss <- ggdraw() + 
  draw_label("LISS", fontface = 'bold', x = 0, hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))
title_HRS <- ggdraw() + 
  draw_label("HRS", fontface = 'bold', x = 0, hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))
plot_grid(
  title_liss, grid_agree_liss, title_HRS, grid_agree_hrs, 
  ncol = 1, rel_heights = c(0.1, 1, 0.1, 1)
)

#### linearHypothesis ####

# test linear contrasts in cases where estimates of interest are represented by multiple FE coefs
to_be_tested <- c("shift_control", # shift of controls vs. 0
                  "shift_gp", # shift of GPs vs. 0
                  "shift_gp_vs_control", # shift of GPs vs. controls
                  "before_gp", # before-slope of GPs vs. 0
                  "after_gp") # after-slope of GPs vs. 0
contrasts <- list(c("shift", "after"),
                 c("shift", "after", "shift:grandparent", "after:grandparent"),
                 c("shift:grandparent", "after:grandparent"), 
                 c("before", "before:grandparent"),
                 c("after", "after:grandparent"))
to_be_tested_gender <- c("shift_control_male", # shift of male controls vs. 0
                         "shift_control_female", # shift of female controls vs. 0
                         "shift_gp_male", # shift of male GPs vs. 0
                         "shift_gp_female", # shift of female GPs vs. 0
                         "shift_gp_vs_control_men", # shift of m. GPs vs. m. controls
                         "before_gp_vs_control_women", # before-slope of f. GPs vs. f. controls
                         "after_gp_vs_control_women", # after-slope of f. GPs vs. f. controls
                         "shift_gp_vs_control_women", # shift of f. GPs vs. f. controls
                         "shift_male_vs_female_control", # shift of m. vs. f. controls
                         "before_male_vs_female_gp", # before-slope of m. vs. f. GPs
                         "after_male_vs_female_gp", # after-slope of m. vs. f. GPs
                         "shift_male_vs_female_gp") # shift of m. vs. f. GPs
contrasts_gender <- list(c("shift", "after"),
                         c("shift", "after", "shift:female", "after:female"),
                         c("shift", "after", "shift:grandparent", "after:grandparent"),
                         c("shift", "after", "shift:female", "after:female", "shift:grandparent", 
                           "after:grandparent", "shift:grandparent:female", "after:grandparent:female"),
                         c("shift:grandparent", "after:grandparent"), 
                         c("before:grandparent", "before:grandparent:female"), 
                         c("after:grandparent", "after:grandparent:female"), 
                         c("shift:grandparent", "after:grandparent", 
                           "shift:grandparent:female", "after:grandparent:female"), 
                         c("shift:female", "after:female"),
                         c("before:female", "before:grandparent:female"),
                         c("after:female", "after:grandparent:female"),
                         c("shift:female", "after:female", 
                           "shift:grandparent:female", "after:grandparent:female"))

for (i in 1:length(outcomes)){
  for (j in 1:length(datasets_short)){
    ### basic models
    model <- get(paste0(outcomes[i], "_", datasets_short[j], "_test"))
    for (k in 1:length(to_be_tested)){
      contrast <- as.data.frame(
        cbind(est = sum(fixef(model)[contrasts[[k]]]), # estimate is the sum of all involved FE coefs
              chi = linearHypothesis(model, paste(contrasts[[k]], collapse = " + "))[2, "Chisq"], 
              df = linearHypothesis(model, paste(contrasts[[k]], collapse = " + "))[2, "Df"], 
              p = linearHypothesis(model, paste(contrasts[[k]], collapse = " + "))[2, "Pr(>Chisq)"])
        )
      contrast <- contrast %>% mutate( # reformat for reporting in text
        est_num = printnum(est),
        chi_print = paste0("$\\chi^2$", " (", contrast$df, ") = ", printnum(contrast$chi)),
        p_print = scales::pvalue(p, prefix = c("$p$ < ", "$p$ = ", "$p$ > "))
      )
      contrast_name <- paste0(to_be_tested[k], "_", outcomes[i], "_", datasets_short[j])
      eval(call("<-", as.name(contrast_name), contrast))
    }
    collect_contrasts <- do.call(rbind, lapply(paste0(to_be_tested, "_", 
                              outcomes[i], "_", datasets_short[j]), get)) # all k's
    rownames(collect_contrasts) <- to_be_tested
    collected_name <- paste0("contrasts_", outcomes[i], "_", datasets_short[j])
    eval(call("<-", as.name(collected_name), collect_contrasts))
  }
  listed_contrasts <- do.call(list, lapply(paste0("contrasts_", 
                           outcomes[i], "_", datasets_short), get)) # all j's
  names(listed_contrasts) <- datasets_short
  listed_name <- paste0("contrasts_", outcomes[i])
  eval(call("<-", as.name(listed_name), listed_contrasts))
}

for (i in 1:length(outcomes)){
  for (j in 1:length(datasets_short)){
    ### moderation by gender models
    model_gender <- get(paste0(outcomes[i], "_", datasets_short[j], "_gender_test"))
    for (k in 1:length(to_be_tested_gender)){
      contrast_gender <- as.data.frame(
        cbind(est = sum(fixef(model_gender)[contrasts_gender[[k]]]), 
              chi = linearHypothesis(model_gender, paste(contrasts_gender[[k]], 
                                                         collapse = " + "))[2, "Chisq"], 
              df = linearHypothesis(model_gender, paste(contrasts_gender[[k]], 
                                                        collapse = " + "))[2, "Df"], 
              p = linearHypothesis(model_gender, paste(contrasts_gender[[k]], 
                                                       collapse = " + "))[2, "Pr(>Chisq)"])
      )
      contrast_gender <- contrast_gender %>% mutate( # reformat for reporting in text
        est_num = printnum(est),
        chi_print = paste0("$\\chi^2$", " (", contrast_gender$df, ") = ", printnum(contrast_gender$chi)),
        p_print = scales::pvalue(p, prefix = c("$p$ < ", "$p$ = ", "$p$ > "))
      )
      contrast_gender_name <- paste0(to_be_tested_gender[k], "_", outcomes[i], "_", datasets_short[j])
      eval(call("<-", as.name(contrast_gender_name), contrast_gender))
  }
    collect_contrasts_gender <- do.call(rbind, lapply(paste0(to_be_tested_gender, "_", 
                                                      outcomes[i], "_", datasets_short[j]), get)) #all k's
    rownames(collect_contrasts_gender) <- to_be_tested_gender
    collected_gender_name <- paste0("contrasts_", outcomes[i], "_", datasets_short[j], "_gender")
    eval(call("<-", as.name(collected_gender_name), collect_contrasts_gender))
}
  listed_contrasts_gender <- do.call(list, lapply(paste0("contrasts_", outcomes[i], "_", 
                                                         datasets_short, "_gender"), get)) # all j's
  names(listed_contrasts_gender) <- datasets_short
  listed_gender_name <- paste0("contrasts_gender_", outcomes[i])
  eval(call("<-", as.name(listed_gender_name), listed_contrasts_gender))
}

# remove unnecessary objects
rm(list = ls(pattern = paste0(c(to_be_tested, to_be_tested_gender), collapse="|")))
rm(list = ls(pattern = paste0("contrasts_", outcomes, "_", collapse = "|")))

#### Moderator: paid work ####

hrs_swls_parents_test <- lmerTest::lmer(swls ~ 1 + pscore + (before + after + shift + grandparent + 
                                         grandparent:before + grandparent:after + grandparent:shift)*working + 
                                         (1 | pid) + (1 | hid), REML = FALSE, data = hrsanalysis_parents) 
summary(hrs_swls_parents_test)

mod_summaries_work <- list()
mod_summaries_work_test <- list()

for (i in 1:length(outcomes)){
  outcome = outcomes[i]
  pos = seq(from = -2, to = 8, by = 2)[i] # changed this because we now only have the 2 HRS datasets
  for (j in 3:4){
    dataset = datasets[j]
    ### moderator: paid work (only HRS)
    if (icc_list[hid_icc_tbl[j], outcome] < 0.05){ # nesting in hid leads to singular fit for some models
      model_1 <- lme4::lmer(get(outcome) ~ 1 + pscore + (before + after + shift + grandparent + 
                                                           grandparent:before + grandparent:after + grandparent:shift)*working + 
                              (1 | pid), REML = FALSE, data = get(dataset))
      model_2 <- lmerTest::lmer(get(outcome) ~ 1 + pscore + (before + after + shift + grandparent + 
                                                               grandparent:before + grandparent:after + grandparent:shift)*working + 
                                  (1 | pid), REML = FALSE, data = get(dataset))
    } else { # cross-nesting in pid & hid for all other models
      model_1 <- lme4::lmer(get(outcome) ~ 1 + pscore + (before + after + shift + grandparent + 
                                                           grandparent:before + grandparent:after + grandparent:shift)*working + 
                              (1 | pid) + (1 | hid), REML = FALSE, data = get(dataset))
      model_2 <- lmerTest::lmer(get(outcome) ~ 1 + pscore + (before + after + shift + grandparent + 
                                                               grandparent:before + grandparent:after + grandparent:shift)*working + 
                                  (1 | pid) + (1 | hid), REML = FALSE, data = get(dataset))
    }      
    mod_summaries_work[[pos + j]] <- model_1
    names(mod_summaries_work)[[pos + j]] <- paste0(outcome, "_", dataset)
    mod_summaries_work_test[[pos + j]] <- model_2
    names(mod_summaries_work_test)[[pos + j]] <- paste0(outcome, "_", dataset)
    # compute R^2
    pred_work <- cbind(subset(get(dataset), !is.na(get(outcome)) & !is.na(working), 
                       select = c(pid, time, get(outcome))), 
                       pred_outcome = predict(model_1, re.form=NA)) # model outcome values (work)
    r2_work <- cor(pred_work[outcomes[i]], pred_work$pred_outcome)^2
    r2_work_name <- paste0("r2_", outcome, "_", dataset, "_work")
    eval(call("<-", as.name(r2_work_name), r2_work))
  }
}

# data.frames for plots for moderation by 'paid work'
for (i in 1:length(outcomes)){
  outcome = outcomes[i]
  pos = seq(from = -2, to = 8, by = 2)[i] # changed this because we now only have the 2 HRS datasets
  for (j in 3:4){
    dataset = datasets[j]
    dataset_short = datasets_short[j]
    ### moderation by paid work models
    obj_test_work = mod_summaries_work_test[[pos + j]] # unfold list objects (lmerTest)
    # create data.frame (with all predictors)
    ps_mean_work <- as.data.frame(subset(get(dataset), !is.na(get(outcome)) & 
                                           !is.na(working) & time==0) %>% 
                                    group_by(grandparent, working) %>% summarise(pscore = mean(pscore)))
    # only HRS
    dframe_work <- data.frame(
      pscore = c(rep(ps_mean_work[1,3], 7),  # non-working controls
                 rep(ps_mean_work[2,3], 7),  # working controls
                 rep(ps_mean_work[3,3], 7),  # non-working grandparents
                 rep(ps_mean_work[4,3], 7)), # working grandparents
      before = rep(c(0:2, rep(2, 4)), 4),
      after = rep(c(rep(0, 3), 1:4), 4),
      shift = rep(c(rep(0, 3), rep(1, 4)), 4),
      grandparent = c(rep(0, 14), rep(1, 14)),
      working = rep(c(rep(0, 7), rep(1, 7)), 2),
      x = rep(seq(-6, 6, by=2), 2)
    )
    # predict response (again, same procedure for LISS & HRS)
    dframe_work$pred <- predict(obj_test_work, newdata = dframe_work, re.form=NA)
    # create design matrix
    designmat_work <- model.matrix(as.formula(lme4::nobars(formula(obj_test_work))[-2]), 
                                   dframe_work) # [-2] drops response from formula
    # compute standard error
    predvar_work <- diag(designmat_work %*% vcov(obj_test_work) %*% t(designmat_work)) 
    dframe_work$SE <- sqrt(predvar_work) # for confidence intervals
    # add grandparent variable as a factor
    if (dataset_short=="hrs_parents"){ 
      dframe_work$gpgroup <- factor(dframe_work$grandparent, 
                                    labels=c("Parent\nControls","Grandparents"))
    } else { # for nonparent controls
      dframe_work$gpgroup <- factor(dframe_work$grandparent, 
                                    labels=c("Nonparent\nControls","Grandparents"))
    }      
    dframe_work$gpgroup <- fct_rev(dframe_work$gpgroup)
    # add female variable as a factor
    dframe_work$work <- factor(dframe_work$working, labels=c("Not Working","Working"))
    dframe_work$work <- fct_rev(dframe_work$work)
    dframe_name_work <- paste0("dframe_", outcome, "_", dataset_short, "_work")
    eval(call("<-", as.name(dframe_name_work), dframe_work)) # save data.frame for later ggplot use
  }
}


#### Moderator: grandchild care ####

hrsanalysis_parents %>% filter(grandparent==1) %>% group_by(grandkids100h, time) %>% summarise(n = n())

# only post-event period
hrsanalysis_care_parents <- hrsanalysis_parents %>% filter(time %in% c(0:6))
hrsanalysis_care_nonparents <- hrsanalysis_nonparents %>% filter(time %in% c(0:6))

# transfer grandparents' values of grandchild care (grandkids100h) to their matched controls
# careful to regard temporal changes, too
hrsanalysis_care_parents %>% filter(subclass==99) %>% # example cases 4 , 200
  select(pid, grandparent, time, subclass, time_match, match_number, grandkids100h)

hrsanalysis_care_parents <- hrsanalysis_care_parents %>% group_by(subclass, time) %>% 
  mutate(grandkids100h = max(grandkids100h, na.rm = T)) %>% ungroup() %>% 
  mutate(grandkids100h = replace(grandkids100h, grandkids100h==-Inf, NA)) # regular NA pls
hrsanalysis_care_nonparents <- hrsanalysis_care_nonparents %>% group_by(subclass, time) %>% 
  mutate(grandkids100h = max(grandkids100h, na.rm = T)) %>% ungroup() %>% 
  mutate(grandkids100h = replace(grandkids100h, grandkids100h==-Inf, NA)) # regular NA pls

hrsanalysis_care_parents %>% group_by(grandparent, time, grandkids100h) %>%
  summarise(n = n()) %>% print(n=30)

hrsanalysis_care_parents %>% filter(!is.na(grandkids100h) & grandkids100h!=8) %>% 
  group_by(grandparent, time) %>% summarise(n = n())

hrsanalysis_care_parents <- hrsanalysis_care_parents %>% 
  mutate(caring = ifelse(grandkids100h %in% c(1,5), ifelse(grandkids100h==1, 1, 0), NA)) %>% 
  filter(!is.na(caring))
hrsanalysis_care_nonparents <- hrsanalysis_care_nonparents %>% 
  mutate(caring = ifelse(grandkids100h %in% c(1,5), ifelse(grandkids100h==1, 1, 0), NA)) %>% 
  filter(!is.na(caring))


hrs_swls_parents_care_test <- lmerTest::lmer(swls ~ 1 + pscore + (after + grandparent + grandparent:after)*caring + 
                                          (1 | pid) + (1 | hid), REML = FALSE, data = hrsanalysis_care_parents) 
summary(hrs_swls_parents_care_test)

mod_summaries_care <- list()
mod_summaries_care_test <- list()

datasets_care <- c("hrsanalysis_care_parents", "hrsanalysis_care_nonparents")
  
for (i in 1:length(outcomes)){
  outcome = outcomes[i]
  pos = seq(from = 0, to = 10, by = 2)[i] 
  for (j in 1:length(datasets_care)){
    dataset = datasets_care[j]
    ### moderator: grandchild care (only HRS)
    if (icc_list[hid_icc_tbl[j], outcome] < 0.05){ # nesting in hid leads to singular fit for some models
      model_1 <- lme4::lmer(get(outcome) ~ 1 + pscore + (after + grandparent + grandparent:after)*caring + 
                              (1 | pid), REML = FALSE, data = get(dataset))
      model_2 <- lmerTest::lmer(get(outcome) ~ 1 + pscore + (after + grandparent + grandparent:after)*caring + 
                                  (1 | pid), REML = FALSE, data = get(dataset))
    } else { # cross-nesting in pid & hid for all other models
      model_1 <- lme4::lmer(get(outcome) ~ 1 + pscore + (after + grandparent + grandparent:after)*caring + 
                              (1 | pid) + (1 | hid), REML = FALSE, data = get(dataset))
      model_2 <- lmerTest::lmer(get(outcome) ~ 1 + pscore + (after + grandparent + grandparent:after)*caring + 
                                  (1 | pid) + (1 | hid), REML = FALSE, data = get(dataset))
    }      
    mod_summaries_care[[pos + j]] <- model_1
    names(mod_summaries_care)[[pos + j]] <- paste0(outcome, "_", dataset)
    mod_summaries_care_test[[pos + j]] <- model_2
    names(mod_summaries_care_test)[[pos + j]] <- paste0(outcome, "_", dataset)
    # compute R^2
    pred_care <- cbind(subset(get(dataset), !is.na(get(outcome)) & !is.na(caring), 
                              select = c(pid, time, get(outcome))), 
                       pred_outcome = predict(model_1, re.form=NA)) # model outcome values (care)
    r2_care <- cor(pred_care[outcomes[i]], pred_care$pred_outcome)^2
    r2_care_name <- paste0("r2_", outcome, "_", dataset, "_care")
    eval(call("<-", as.name(r2_care_name), r2_care))
  }
}

coefs_care <- c("Intercept", "pscore", "after", "grandparent", 
                "caring", "after_grandparent", 
                "after_caring", "grandparent_caring", 
                "after_grandparent_caring")
gammas_care <- c("gamma}_{00", "gamma}_{04", "gamma}_{10", "gamma}_{01", 
                 "gamma}_{02", "gamma}_{11",
                 "gamma}_{12", "gamma}_{03", 
                 "gamma}_{13")

for (i in 1:length(outcomes)){
  outcome = outcomes[i]
  pos = seq(from = -2, to = 8, by = 2)[i] # changed this because we now only have the 2 HRS datasets
  for (j in 3:4){
    dataset = datasets_short[j]
    ### moderation by grandchild care
    obj_care = apa_print(mod_summaries_care[[pos + j]]) # unfold list objects
    obj_care_test = mod_summaries_care_test[[pos + j]] # unfold list objects (lmerTest)
    # reformatting: change beta^hat to gamma^hat (plus subscripts)
    for (k in 1:length(coefs_care)){ 
      coef <- coefs_care[k] # 17 coefficients for each model (object)
      gamma <- gammas_care[k]
      obj_care$estimate[coef] <- lapply(obj_care$estimate[coef], gsub, pattern="beta", replacement=gamma)
      obj_care$full_result[coef] <- lapply(obj_care$full_result[coef], gsub, pattern="beta", replacement=gamma)
    }
    obj_care_name <- paste0(outcome, "_", dataset, "_care_summary")
    eval(call("<-", as.name(obj_care_name), obj_care)) # save object
    obj_care_test_name <- paste0(outcome, "_", dataset, "_care_test")
    eval(call("<-", as.name(obj_care_test_name), obj_care_test)) # save object
    # for better formatting in text: p-values
    obj_care_p <- as.data.frame(summary(mod_summaries_care_test[[pos + j]])$coefficients) %>% # unfold list
      rownames_to_column() %>% select(rowname, "Pr(>|t|)") %>% rename(p = "Pr(>|t|)") %>% 
      mutate(p = scales::pvalue(p, prefix = c("$p$ < ", "$p$ = ", "$p$ > "))) %>% column_to_rownames()
    obj_care_p["p"] <- # remove "0" from the chr's 
      lapply(obj_care_p["p"], gsub, pattern="0\\.", replacement="\\.")
    obj_care_name_p <- paste0(outcome, "_", dataset, "_care_p")
    eval(call("<-", as.name(obj_care_name_p), obj_care_p)) # save objects  
  }
}

# data.frames for plots for moderation by 'grandchild care'
for (i in 1:length(outcomes)){
  outcome = outcomes[i]
  pos = seq(from = 0, to = 10, by = 2)[i] 
  for (j in 1:length(datasets_care)){
    dataset = datasets_care[j]
    dataset_short = datasets_short[j+2] # only HRS
    ### moderation by grandchild care models
    obj_test_care = mod_summaries_care_test[[pos + j]] # unfold list objects (lmerTest)
    # create data.frame (with all predictors)
    ps_mean_care <- as.data.frame(subset(get(dataset), !is.na(get(outcome)) & 
                                           !is.na(caring) & time==0) %>% 
                                    group_by(grandparent, caring) %>% summarise(pscore = mean(pscore)))
    # only HRS (simplified post-transition model)
    dframe_care <- data.frame(
      pscore = c(rep(ps_mean_care[1,3], 4),  # non-caring controls
                 rep(ps_mean_care[2,3], 4),  # caring controls
                 rep(ps_mean_care[3,3], 4),  # non-caring grandparents (>100 h)
                 rep(ps_mean_care[4,3], 4)), # caring grandparents
      after = rep(c(1:4), 4),
      grandparent = c(rep(0, 8), rep(1, 8)),
      caring = rep(c(rep(0, 4), rep(1, 4)), 2),
      x = rep(seq(0, 6, by=2), 4)
    )
    # predict response (again, same procedure for LISS & HRS)
    dframe_care$pred <- predict(obj_test_care, newdata = dframe_care, re.form=NA)
    # create design matrix
    designmat_care <- model.matrix(as.formula(lme4::nobars(formula(obj_test_care))[-2]), 
                                   dframe_care) # [-2] drops response from formula
    # compute standard error
    predvar_care <- diag(designmat_care %*% vcov(obj_test_care) %*% t(designmat_care)) 
    dframe_care$SE <- sqrt(predvar_care) # for confidence intervals
    # add grandparent variable as a factor
    if (dataset_short=="hrs_parents"){ 
      dframe_care$gpgroup <- factor(dframe_care$grandparent, 
                                    labels=c("Parent\nControls","Grandparents"))
    } else { # for nonparent controls
      dframe_care$gpgroup <- factor(dframe_care$grandparent, 
                                    labels=c("Nonparent\nControls","Grandparents"))
    }      
    dframe_care$gpgroup <- fct_rev(dframe_care$gpgroup)
    # add female variable as a factor
    dframe_care$care <- factor(dframe_care$caring, labels=c("Grandchild\nCare < 100h","Grandchild\nCare >= 100h"))
    dframe_care$care <- fct_rev(dframe_care$care)
    dframe_name_care <- paste0("dframe_", outcome, "_", dataset_short, "_care")
    eval(call("<-", as.name(dframe_name_care), dframe_care)) # save data.frame for later ggplot use
  }
}

# linear contrasts

to_be_tested_care <- c("after_gp_vs_control_care", # after-slope of caring GPs vs. caring controls
                       "after_nocare_vs_care_gp") # after-slope of not-caring vs. caring GPs

contrasts_care <- list(c("after:grandparent", "after:grandparent:caring"),
                       c("after:caring", "after:grandparent:caring"))

for (i in 1:length(outcomes)){
  for (j in 1:length(datasets_short_hrs)){ # now only for the 2 HRS datasets
    ### moderation by grandchild care
    model_care <- get(paste0(outcomes[i], "_", datasets_short_hrs[j], "_care_test"))
    for (k in 1:length(to_be_tested_care)){
      contrast_care <- as.data.frame(
        cbind(est = sum(fixef(model_care)[contrasts_care[[k]]]), 
              chi = linearHypothesis(model_care, paste(contrasts_care[[k]], 
                                                       collapse = " + "))[2, "Chisq"], 
              df = linearHypothesis(model_care, paste(contrasts_care[[k]], 
                                                      collapse = " + "))[2, "Df"], 
              p = linearHypothesis(model_care, paste(contrasts_care[[k]], 
                                                     collapse = " + "))[2, "Pr(>Chisq)"])
      )
      contrast_care <- contrast_care %>% mutate( # reformat for reporting in text
        est_num = printnum(est),
        chi_print = paste0("$\\chi^2$", " (", contrast_care$df, ") = ", printnum(contrast_care$chi)),
        p_print = scales::pvalue(p, prefix = c("$p$ < ", "$p$ = ", "$p$ > "))
      )
      contrast_care_name <- paste0(to_be_tested_care[k], "_", outcomes[i], "_", datasets_short_hrs[j])
      eval(call("<-", as.name(contrast_care_name), contrast_care))
    }
    collect_contrasts_care <- do.call(rbind, lapply(paste0(to_be_tested_care, "_", 
                                                           outcomes[i], "_", datasets_short_hrs[j]), get)) #all k's
    rownames(collect_contrasts_care) <- to_be_tested_care
    collected_care_name <- paste0("contrasts_", outcomes[i], "_", datasets_short_hrs[j], "_care")
    eval(call("<-", as.name(collected_care_name), collect_contrasts_care))
  }
  listed_contrasts_care <- do.call(list, lapply(paste0("contrasts_", outcomes[i], "_", 
                                                       datasets_short_hrs, "_care"), get)) # all j's
  names(listed_contrasts_care) <- datasets_short_hrs
  listed_care_name <- paste0("contrasts_care_", outcomes[i])
  eval(call("<-", as.name(listed_care_name), listed_contrasts_care))
}

for (i in 1:length(dframes_care)){
  ### moderation by grandchild care
  plot_care <- ggplot(get(dframes_care[i]), aes(x=x,y=pred,colour=gpgroup,linetype=care))+
    geom_line(position=position_dodge(width=0.2),size=1)+
    geom_point(position=position_dodge(width=0.2),size=1.5)+
    scale_colour_brewer(palette = "Set1", name="Group")+ 
    scale_linetype_discrete(name="Grandchild Care")+
    geom_errorbar(aes(ymin=pred-1.96*SE,ymax=pred+1.96*SE),width=0.6,position=position_dodge(width=0.2))+
    coord_cartesian(ylim=c(limits[i*2, 1], limits[i*2, 2]))+ # loop over limits (but *2 because of length)
    scale_x_continuous(name="Time (in Years)",breaks=seq(0,6,2))+ # starts at 0 now
    theme(#axis.text = element_text(face="bold"), #, size=14
      #axis.title = element_text(size=18),
      #legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
      #legend.text=element_text(size=14),
      #legend.title=element_text(size=14),
      panel.border=element_rect(colour="darkgrey", fill=NA, size=1),
      axis.title.y=element_blank())+
    guides(colour = guide_legend(order = 1), linetype = guide_legend(order = 2)) # legend element order
    #scale_y_continuous(name=outcomes_plots[i*2])
    if (dframes_care[i] %in% dframes_care[grep("^(?=.*_parents_)", dframes_care, perl=T)]){ 
      plot_care <- plot_care + theme(axis.title.x=element_blank()) # omit x-axis title for parent plots
    } 
  plot_name_care <- gsub("dframe", "plot", dframes_care[i])
  eval(call("<-", as.name(plot_name_care), plot_care)) # save plots for later assembly
}

#### H2: Individual differences in intraindividual change #### 

swls_formula <- as.formula("swls ~ 1 + pscore + before + after + shift + grandparent + 
                                  grandparent:before + grandparent:after + grandparent:shift + 
                                  (1 | pid) + (1 | hid)")

m1 <- lme4::lmer(neur ~ 1 + pscore + before + after + shift + grandparent + 
                                  grandparent:before + grandparent:after + grandparent:shift + 
                                  (1 | pid), REML = FALSE, data = hrsanalysis_nonparents) 

m1 <- mod_summaries[[4]]
m1 <- update(m1, agree ~ ., data = hrsanalysis_nonparents)

summary(m1)

ctrl <- lmerControl(maxIter = 10000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 10000)

m2 <- update(m1, agree ~ . -(1 | pid) +(1 + before | pid), data = hrsanalysis_nonparents)
summary(m2)

(a1 <- anova(m1, m2))



anova_summaries <- list()

for (i in 1:length(outcomes)){
  outcome = outcomes[i]
  pos = seq(from = 0, to = 20, by = 4)[i]
  for (j in 1:length(datasets)){
    dataset = datasets[j]
    # update basic models with random slopes (one at a time) -> random slopes for pid (not hid)
    rs_before <- update(mod_summaries[[pos + j]], . ~ . -(1 | pid) + (1 + before | pid), 
                        control = lme4::lmerControl(optimizer="bobyqa")) # some nonconvergence with default opt
    rs_after <-  update(mod_summaries[[pos + j]], . ~ . -(1 | pid) + (1 + after | pid), 
                        control = lme4::lmerControl(optimizer="bobyqa"))
    rs_shift <-  update(mod_summaries[[pos + j]], . ~ . -(1 | pid) + (1 + shift | pid), 
                        control = lme4::lmerControl(optimizer="bobyqa"))
    # run anova() for model comparison
    before_anov <- as.numeric(anova(mod_summaries[[pos + j]], rs_before)[2, c("Chisq", "Pr(>Chisq)")])
    after_anov <-  as.numeric(anova(mod_summaries[[pos + j]], rs_after)[2, c("Chisq", "Pr(>Chisq)")])
    shift_anov <-  as.numeric(anova(mod_summaries[[pos + j]], rs_shift)[2, c("Chisq", "Pr(>Chisq)")])
    # together
    comp_anov <- as.data.frame(rbind(before_anov, after_anov, shift_anov))
    comp_anov <- comp_anov %>% mutate(
      chi2 = printnum(V1),
      p = scales::pvalue(V2, prefix = c("$p$ < ", "$p$ = ", "$p$ > ")),
      ) %>% select(chi2, p)
    comp_anov["p"] <- # remove "0" from the chr's
      lapply(comp_anov["p"], gsub, pattern="0\\.", replacement="\\.")
    rownames(comp_anov) <- c("before", "after", "shift")
    # save in list object
    anova_summaries[[pos + j]] <- comp_anov
    names(anova_summaries)[[pos + j]] <- paste0(outcome, "_", dataset, "_comp")
  }
}
# all p < .001

# alternative: only for grandparents with simplified MLMs
anova_summaries_gp <- list()

for (i in 1:length(outcomes)){
  outcome = outcomes[i]
  print(outcome)
  pos = seq(from = 0, to = 20, by = 4)[i]
  for (j in 1:length(datasets)){
    dataset = datasets[j]
    print(dataset)
    # basic model only for GPs (fixed slopes)
    if (icc_list[hid_icc_tbl[j], outcome] < 0.05){ # nesting in hid leads to singular fit for some models
      fs_mod <- lme4::lmer(get(outcome) ~ 1 + pscore + before + after + shift + 
                              (1 | pid), REML = FALSE, data = get(dataset) %>% filter(grandparent==1))
    } else { # cross-nesting in pid & hid for all other models
      fs_mod <- lme4::lmer(get(outcome) ~ 1 + pscore + before + after + shift + 
                              (1 | pid) + (1 | hid), REML = FALSE, data = get(dataset) %>% filter(grandparent==1))
    }      
    # update basic models with random slopes (one at a time) -> random slopes for pid (not hid)
    print("before")
    rs_before <- update(fs_mod, . ~ . -(1 | pid) + (1 + before | pid), 
                        control = lme4::lmerControl(optimizer="bobyqa")) # some nonconvergence with default opt
    print("after")
    rs_after <-  update(fs_mod, . ~ . -(1 | pid) + (1 + after | pid), 
                        control = lme4::lmerControl(optimizer="bobyqa"))
    print("shift")
    rs_shift <-  update(fs_mod, . ~ . -(1 | pid) + (1 + shift | pid), 
                        control = lme4::lmerControl(optimizer="bobyqa"))
    # run anova() for model comparison
    before_anov <- as.numeric(anova(fs_mod, rs_before)[2, c("Chisq", "Pr(>Chisq)")])
    after_anov <-  as.numeric(anova(fs_mod, rs_after)[2, c("Chisq", "Pr(>Chisq)")])
    shift_anov <-  as.numeric(anova(fs_mod, rs_shift)[2, c("Chisq", "Pr(>Chisq)")])
    # together
    comp_anov <- as.data.frame(rbind(before_anov, after_anov, shift_anov))
    comp_anov <- comp_anov %>% mutate(
      chi2 = printnum(V1),
      p = scales::pvalue(V2, prefix = c("$p$ < ", "$p$ = ", "$p$ > ")),
    ) %>% select(chi2, p)
    comp_anov["p"] <- # remove "0" from the chr's
      lapply(comp_anov["p"], gsub, pattern="0\\.", replacement="\\.")
    rownames(comp_anov) <- c("before", "after", "shift")
    # save in list object
    anova_summaries_gp[[pos + j]] <- comp_anov
    names(anova_summaries_gp)[[pos + j]] <- paste0(outcome, "_", dataset, "_comp")
  }
}


# heterogeneous variance models in 'nlme' (also possible in 'lme4' but we preregistered 'nlme')
hetmod1 <- nlme::lme(swls ~ 0 + dummy(grandparent,"0") + dummy(grandparent,"1") + 
                             time:dummy(grandparent,"0") + time:dummy(grandparent,"1"),
                     random=list(pid = nlme::pdBlocked(list(nlme::pdSymm(~ 0 + dummy(grandparent,"0") +
                                                                           time:dummy(grandparent,"0")),
                                                            nlme::pdSymm(~ 0 + dummy(grandparent,"1") + 
                                                                           time:dummy(grandparent,"1"))))), 
                     data = hrsanalysis_parents, method = 'REML', na.action = 'na.omit')
summary(hetmod1)
VarCorr(hetmod1)

hetmod1b <- lme4::lmer(swls ~ 0 + dummy(grandparent, "0") + dummy(grandparent, "1") + 
                              time:dummy(grandparent, "0") + time:dummy(grandparent, "1") + 
                              (0 + dummy(grandparent, "0") + time:dummy(grandparent, "0") | pid) + 
                              (0 + dummy(grandparent, "1") + time:dummy(grandparent, "1") | pid) + (1 | hid),
                            data = hrsanalysis_parents,
                       control = lmerControl(optimizer="bobyqa"),
                            REML = TRUE)
summary(hetmod1b)
VarCorr(hetmod1b)

hetmod2 <- nlme::lme(swls ~ 0 + dummy(grandparent,"0") + dummy(grandparent,"1") + 
                       time:dummy(grandparent,"0") + time:dummy(grandparent,"1"),
                            random=list(pid = nlme::pdBlocked(list(nlme::pdSymm(~ 0 + dummy(grandparent,"0")),
                                                                   nlme::pdSymm(~ 0 + dummy(grandparent,"1")),
                                                                   nlme::pdSymm(~ 0 + time)))), 
                            data = hrsanalysis_parents, method = 'REML', na.action = 'na.omit')
summary(hetmod2)
VarCorr(hetmod2)

anova(hetmod1, hetmod2)

m1 <- lme4::lmer(neur ~ 1 + pscore + before + after + shift + grandparent + 
                   grandparent:before + grandparent:after + grandparent:shift + 
                   (1 | pid), REML = FALSE, data = hrsanalysis_nonparents) 

# heterogeneous variance models in 'nlme' (also possible in 'lme4' but we preregistered 'nlme')
# same number of fixed effects just coded differently 
hetmod3 <- nlme::lme(swls ~ 0 + pscore + dummy(grandparent,"0") + dummy(grandparent,"1") + 
                       before:dummy(grandparent,"0") + before:dummy(grandparent,"1") +
                       after:dummy(grandparent,"0") + after:dummy(grandparent,"1") +
                       shift:dummy(grandparent,"0") + shift:dummy(grandparent,"1"),
                     random=list(pid = nlme::pdBlocked(list(nlme::pdSymm(~ 0 + dummy(grandparent,"0") +
                                                                           shift:dummy(grandparent,"0")),
                                                            nlme::pdSymm(~ 0 + dummy(grandparent,"1") + 
                                                                           shift:dummy(grandparent,"1"))))), 
                     data = hrsanalysis_nonparents, method = 'REML', na.action = 'na.omit')
summary(hetmod3)
VarCorr(hetmod3)

hetmod4 <- nlme::lme(swls ~ 0 + pscore + dummy(grandparent,"0") + dummy(grandparent,"1") + 
                       before:dummy(grandparent,"0") + before:dummy(grandparent,"1") +
                       after:dummy(grandparent,"0") + after:dummy(grandparent,"1") +
                       shift:dummy(grandparent,"0") + shift:dummy(grandparent,"1"),
                     random=list(pid = nlme::pdBlocked(list(nlme::pdSymm(~ 0 + dummy(grandparent,"0")),
                                                            nlme::pdSymm(~ 0 + dummy(grandparent,"1")),
                                                            nlme::pdSymm(~ 0 + shift)))), 
                     data = hrsanalysis_nonparents, method = 'REML', na.action = 'na.omit')
summary(hetmod4)
VarCorr(hetmod4)

anova(hetmod3, hetmod4)


hetvar_summaries <- list()

for (i in 1:length(outcomes)){
  outcome = outcomes[i]
  print(outcome)
  pos = seq(from = 0, to = 20, by = 4)[i]
  for (j in 1:length(datasets)){
    dataset = datasets[j]
    print(dataset)
    # test heterogeneous variance ('slopes' = model with separate random slope variances for each group)
    # heterogeneous variance models in 'nlme' (also possible in 'lme4' but we preregistered 'nlme')
    # same number of fixed effects just coded differently 
    dummyfixed <- formula(paste(outcome, '0 + pscore + dummy(grandparent,"0") + dummy(grandparent,"1") + 
                  before:dummy(grandparent,"0") + before:dummy(grandparent,"1") +
                  after:dummy(grandparent,"0") + after:dummy(grandparent,"1") +
                  shift:dummy(grandparent,"0") + shift:dummy(grandparent,"1")', sep = "~"))
    ### before-slope -> random slope
    before_mod_hetvar_slopes <- 
      nlme::lme(fixed = dummyfixed,
                random=list(pid = nlme::pdBlocked(list(nlme::pdSymm(~ 0 + dummy(grandparent,"0") +
                                                                  before:dummy(grandparent,"0")),
                                                       nlme::pdSymm(~ 0 + dummy(grandparent,"1") + 
                                                                  before:dummy(grandparent,"1"))))), 
                data = get(dataset), method = 'REML', na.action = 'na.omit')
    # base model with same FE specification but uniform random slope variance
    before_mod_hetvar_base <- 
      nlme::lme(fixed = dummyfixed,
                random=list(pid = nlme::pdBlocked(list(nlme::pdSymm(~ 0 + dummy(grandparent,"0")),
                                                       nlme::pdSymm(~ 0 + dummy(grandparent,"1")),
                                                       nlme::pdSymm(~ 0 + before)))), 
                data = get(dataset), method = 'REML', na.action = 'na.omit')
    ### after-slope -> random slope
    after_mod_hetvar_slopes <- 
      nlme::lme(fixed = dummyfixed,
                random=list(pid = nlme::pdBlocked(list(nlme::pdSymm(~ 0 + dummy(grandparent,"0") +
                                                                      after:dummy(grandparent,"0")),
                                                       nlme::pdSymm(~ 0 + dummy(grandparent,"1") + 
                                                                      after:dummy(grandparent,"1"))))), 
                data = get(dataset), method = 'REML', na.action = 'na.omit')
    # base model with same FE specification but uniform random slope variance
    after_mod_hetvar_base <- 
      nlme::lme(fixed = dummyfixed,
                random=list(pid = nlme::pdBlocked(list(nlme::pdSymm(~ 0 + dummy(grandparent,"0")),
                                                       nlme::pdSymm(~ 0 + dummy(grandparent,"1")),
                                                       nlme::pdSymm(~ 0 + after)))), 
                data = get(dataset), method = 'REML', na.action = 'na.omit')
    ### shift -> random slope
    shift_mod_hetvar_slopes <- 
      nlme::lme(fixed = dummyfixed,
                random=list(pid = nlme::pdBlocked(list(nlme::pdSymm(~ 0 + dummy(grandparent,"0") +
                                                                      shift:dummy(grandparent,"0")),
                                                       nlme::pdSymm(~ 0 + dummy(grandparent,"1") + 
                                                                      shift:dummy(grandparent,"1"))))), 
                data = get(dataset), method = 'REML', na.action = 'na.omit')
    # base model with same FE specification but uniform random slope variance
    shift_mod_hetvar_base <- 
      nlme::lme(fixed = dummyfixed,
                random=list(pid = nlme::pdBlocked(list(nlme::pdSymm(~ 0 + dummy(grandparent,"0")),
                                                       nlme::pdSymm(~ 0 + dummy(grandparent,"1")),
                                                       nlme::pdSymm(~ 0 + shift)))), 
                data = get(dataset), method = 'REML', na.action = 'na.omit')
    # variance estimates
    varest <- # row1 = single random slope var; row2 / row3 = het. random slope var (controls / GPs) etc.
      as.data.frame(rbind(
        cbind(as.numeric(VarCorr(before_mod_hetvar_base)[3, 1]),
              as.numeric(VarCorr(before_mod_hetvar_base)[3, 2])), 
        cbind(as.numeric(VarCorr(before_mod_hetvar_slopes)[c(2,4), 1]), 
              as.numeric(VarCorr(before_mod_hetvar_slopes)[c(2,4), 2])),
        cbind(as.numeric(VarCorr(after_mod_hetvar_base)[3, 1]), 
              as.numeric(VarCorr(after_mod_hetvar_base)[3, 2])), 
        cbind(as.numeric(VarCorr(after_mod_hetvar_slopes)[c(2,4), 1]), 
              as.numeric(VarCorr(after_mod_hetvar_slopes)[c(2,4), 2])),
        cbind(as.numeric(VarCorr(shift_mod_hetvar_base)[3, 1]), 
              as.numeric(VarCorr(shift_mod_hetvar_base)[3, 2])), 
        cbind(as.numeric(VarCorr(shift_mod_hetvar_slopes)[c(2,4), 1]), 
              as.numeric(VarCorr(shift_mod_hetvar_slopes)[c(2,4), 2]))))
    colnames(varest) <- c("var", "sd")
    varest_names <- c("before_uni_rand_slope", "before_control_rand_slope", "before_gp_rand_slope",
                      "after_uni_rand_slope", "after_control_rand_slope", "after_gp_rand_slope",
                      "shift_uni_rand_slope", "shift_control_rand_slope", "shift_gp_rand_slope")
    rownames(varest) <- varest_names
    # run anova() for model comparison
    before_hetvar_anov <- as.numeric(anova(before_mod_hetvar_base, 
                                           before_mod_hetvar_slopes)[2, c("L.Ratio", "p-value")])
    after_hetvar_anov <-  as.numeric(anova(after_mod_hetvar_base, 
                                           after_mod_hetvar_slopes)[2, c("L.Ratio", "p-value")])
    shift_hetvar_anov <-  as.numeric(anova(shift_mod_hetvar_base, 
                                           shift_mod_hetvar_slopes)[2, c("L.Ratio", "p-value")])
    before_hetvar_anov[3] <- ifelse(varest["before_control_rand_slope","var"] < 
                                      varest["before_gp_rand_slope","var"], "yes", "no")
    after_hetvar_anov[3] <- ifelse(varest["after_control_rand_slope","var"] < 
                                      varest["after_gp_rand_slope","var"], "yes", "no")
    shift_hetvar_anov[3] <- ifelse(varest["shift_control_rand_slope","var"] < 
                                      varest["shift_gp_rand_slope","var"], "yes", "no")
    # together
    comp_hetvar_anov <- as.data.frame(rbind(
      do.call("rbind", replicate(3, before_hetvar_anov, simplify = FALSE)), # three times for later cbind
      do.call("rbind", replicate(3, after_hetvar_anov, simplify = FALSE)),
      do.call("rbind", replicate(3, shift_hetvar_anov, simplify = FALSE))))
    comp_hetvar_anov <- comp_hetvar_anov %>% mutate(
      lratio = printnum(as.numeric(V1)),
      p = scales::pvalue(as.numeric(V2), prefix = c("$p$ < ", "$p$ = ", "$p$ > ")),
    ) %>% select(lratio, p, gp_greater = V3)
    comp_hetvar_anov["p"] <- # remove "0" from the chr's 
      lapply(comp_hetvar_anov["p"], gsub, pattern="0\\.", replacement="\\.")
    rownames(comp_hetvar_anov) <- varest_names
    # bind variance estimates + anova results
    varest <- cbind(varest, comp_hetvar_anov)
    # save in list object
    hetvar_summaries[[pos + j]] <- varest
    names(hetvar_summaries)[[pos + j]] <- paste0(outcome, "_", dataset, "_hetvar")
  }
}

#### H3: rank-order stability ####

# construct data set with the time of matching and the first post-transition assessment (within-person)
lissanalysis_parents_rank_below <- lissanalysis_parents %>% 
  filter(time==matchtime) %>% 
  select(match_number, grandparent, time_pre = time, # using match_number for join instead of pid
         agree_pre = agree, con_pre = con, extra_pre = extra, 
         neur_pre = neur, open_pre = open, swls_pre = swls)
  
lissanalysis_parents_rank_above <- lissanalysis_parents %>% 
  filter(time>=0) %>% group_by(match_number) %>% slice_min(time) %>% ungroup() %>% 
  select(match_number, grandparent, time, all_of(outcomes))
# slightly smaller n because of attrition in the controls

lissanalysis_parents_rank <- left_join(lissanalysis_parents_rank_above, lissanalysis_parents_rank_below)
lissanalysis_parents_rank <- lissanalysis_parents_rank %>% mutate(yr_lag = time - time_pre)

# alternative coding:
# remove pid duplicates resulting from matching with replacement 
# (might bias results towards greater stability in the controls)
lissanalysis_parents_rank_below <- lissanalysis_parents %>% 
  group_by(pid) %>% 
  filter(time==matchtime) %>% arrange(desc(time)) %>% 
  slice(n=1) %>% ungroup() %>% 
  select(pid, grandparent, time_pre = time, 
         agree_pre = agree, con_pre = con, extra_pre = extra, 
         neur_pre = neur, open_pre = open, swls_pre = swls)

lissanalysis_parents_rank_above <- lissanalysis_parents %>% 
  filter(time>=0) %>% group_by(match_number) %>% slice_min(time) %>% ungroup() %>% 
  group_by(pid) %>% slice(n=1) %>% ungroup() %>% 
  select(pid, grandparent, time, all_of(outcomes))

lissanalysis_parents_rank <- left_join(lissanalysis_parents_rank_above, lissanalysis_parents_rank_below)
lissanalysis_parents_rank <- lissanalysis_parents_rank %>% mutate(yr_lag = time - time_pre)


draw_below <- function(x) { 
  x %>% 
    filter(time<0) %>% group_by(match_number) %>% slice_max(time) %>% ungroup() %>%
    select(match_number, grandparent, time_pre = time, # using match_number instead of pid
           agree_pre = agree, con_pre = con, extra_pre = extra, 
           neur_pre = neur, open_pre = open, swls_pre = swls) }
draw_above <- function(x) { 
  x %>% 
  filter(time>=0) %>% group_by(match_number) %>% slice_min(time) %>% ungroup() %>% 
  select(match_number, grandparent, time, all_of(outcomes)) }

list_below <- list(lissanalysis_parents, lissanalysis_nonparents,
                   hrsanalysis_parents, hrsanalysis_nonparents) %>% lapply(draw_below)
list_above <- list(lissanalysis_parents, lissanalysis_nonparents,
                   hrsanalysis_parents, hrsanalysis_nonparents) %>% lapply(draw_above)
names(list_below) <- paste0(datasets, "_rank_below")
names(list_above) <- paste0(datasets, "_rank_above")

list2env(c(list_below, list_above), .GlobalEnv)
rm(list_below, list_above)

# liss
lissanalysis_parents_rank <- left_join(lissanalysis_parents_rank_above, 
                                       lissanalysis_parents_rank_below) %>% 
  mutate(yr_lag = time - time_pre)
lissanalysis_nonparents_rank <- left_join(lissanalysis_nonparents_rank_above, 
                                          lissanalysis_nonparents_rank_below) %>% 
  mutate(yr_lag = time - time_pre)
# hrs
hrsanalysis_parents_rank <- left_join(hrsanalysis_parents_rank_above, 
                                      hrsanalysis_parents_rank_below) %>% 
  mutate(yr_lag = time - time_pre)
hrsanalysis_nonparents_rank <- left_join(hrsanalysis_nonparents_rank_above, 
                                         hrsanalysis_nonparents_rank_below) %>% 
  mutate(yr_lag = time - time_pre)

# modeling rank-order stability
cor(lissanalysis_parents_rank$agree, lissanalysis_parents_rank$agree_pre)
cor(subset(lissanalysis_parents_rank, grandparent==1)$agree,
    subset(lissanalysis_parents_rank, grandparent==1)$agree_pre)
cor(subset(lissanalysis_parents_rank, grandparent==0)$agree,
    subset(lissanalysis_parents_rank, grandparent==0)$agree_pre)
rank_order_int <- lm(agree ~ agree_pre*grandparent, data = lissanalysis_parents_rank)


rank_order_df <- as.data.frame(
  cbind(cor_all = rep(NA, 24), cor_gp = rep(NA, 24), cor_con = rep(NA, 24), p = rep(NA, 24)))

for (i in 1:length(outcomes)){
  outcome = outcomes[i]
  pos = seq(from = 0, to = 20, by = 4)[i]
  for (j in 1:length(datasets)){
    dataset = paste0(datasets[j], "_rank")
    # simple correlations
    cor_all <- as.numeric(get(dataset) %>% 
      filter(!is.na(get(outcome)) & !is.na(get(paste0(outcome, "_pre")))) %>% 
      summarise(cor(get(outcome), get(paste0(outcome, "_pre")))))
    cor_gp <-  as.numeric(get(dataset) %>% 
      filter(grandparent==1 & !is.na(get(outcome)) & !is.na(get(paste0(outcome, "_pre")))) %>% 
      summarise(cor(get(outcome), get(paste0(outcome, "_pre")))))
    cor_con <- as.numeric(get(dataset) %>% 
      filter(grandparent==0 & !is.na(get(outcome)) & !is.na(get(paste0(outcome, "_pre")))) %>% 
      summarise(cor(get(outcome), get(paste0(outcome, "_pre")))))
    # interaction model as significance test for group differences
    formula_rank <- formula(paste(outcome, paste0(outcome, "_pre*grandparent"), sep = "~"))
    rank_order_int <- lm(formula = formula_rank, data = get(dataset))
    # save in df
    rank_order_df[pos + j, ] <- 
      c(cor_all, cor_gp, cor_con, summary(rank_order_int)$coef[4, 4]) # interaction p-val. 
    rownames(rank_order_df)[pos + j] <- paste0(outcomes[i], "_", datasets_short[j], "_rank")
  }
}

#### LOESS-type plots ####

lissanalysis_allgroups <- bind_rows(
  lissanalysis_nonparents %>% 
    filter(grandparent==0) %>% 
    mutate(group = factor(0, labels="Nonparent Controls")) %>% 
    select(-grandparent),
  lissanalysis_parents %>% 
    mutate(group = factor(grandparent, labels=c("Parent Controls","Grandparents"))) %>% 
    select(-grandparent)
    )

hrsanalysis_allgroups <- bind_rows(
  hrsanalysis_nonparents %>% 
    filter(grandparent==0) %>% 
    mutate(group = factor(0, labels="Nonparent Controls")) %>% 
    select(-grandparent),
  hrsanalysis_parents %>% 
    mutate(group = factor(grandparent, labels=c("Parent Controls","Grandparents"))) %>% 
    select(-grandparent)
)

loess_agree_lissparents <- ggplot(lissanalysis_allgroups, aes(factor(time), agree)) +
  geom_violin() +
  geom_smooth(span = 0.8, aes(group=1), method="loess") +
  facet_wrap(~group) + 
  scale_y_continuous(name="Agreeableness") +
  scale_x_discrete(name="Time (in Years)")

loess_agree_hrsparents <- ggplot(hrsanalysis_allgroups %>% filter(!is.na(agree)), 
                                 aes(factor(time), agree)) +
  geom_violin() +
  geom_smooth(span = 0.8, aes(group=1), method="loess") +
  #stat_summary(fun=mean, colour="blue", geom="line", size = 3) +
  facet_wrap(~group) + 
  scale_y_continuous(name="Agreeableness") +
  scale_x_discrete(name="Time (in Years)")

loess_neur_lissparents <- ggplot(lissanalysis_allgroups, aes(factor(time), neur)) +
  geom_violin() +
  geom_smooth(span = 0.75, aes(group=1), method="loess") +
  facet_wrap(~group) + 
  scale_y_continuous(name="Neuroticism") +
  scale_x_discrete(name="Time (in Years)")
loess_neur_lissparents

loess_neur_hrsparents <- ggplot(hrsanalysis_allgroups %>% filter(!is.na(neur)), 
                                 aes(factor(time), neur)) +
  geom_violin() +
  geom_smooth(span = 0.9, aes(group=1), method="loess") +
  facet_wrap(~group) + 
  scale_y_continuous(name="Neuroticism") +
  scale_x_discrete(name="Time (in Years)") +
  stat_summary(fun=mean, geom="point", shape=23, size=1.5, color="blue")
loess_neur_hrsparents

library("ggdist")
hrsanalysis_allgroups %>% filter(!is.na(extra)) %>% 
  ggplot(aes(x = factor(time), y = extra)) +
  stat_slab(side = "left", scale = 0.5) +
  stat_dotsinterval(scale = 0.5, quantiles = 100, position=position_dodge(width=0.2)) +
  geom_smooth(span = 0.9, aes(group=1), method="loess") +
  facet_wrap(~group)

#### propensity overlap plots ####
overlap_ps_lp <- lissanalysis_parents %>% filter(time==matchtime) %>% select(pid, grandparent, pscore)
overlap_ps_lp_plot <- Hmisc::histbackback(split(overlap_ps_lp$pscore, overlap_ps_lp$grandparent), 
                    brks=seq(0, max(overlap_ps_lp$pscore), (max(overlap_ps_lp$pscore))/25),
                    xlab=c("Parent Controls", "Grandparents"))

plotit<-function(data, conlab, title){
  Hmisc::histbackback(split(data$pscore, data$grandparent), 
                      brks=seq(0, max(data$pscore), (max(data$pscore))/25),
                      xlab=c(conlab, "Grandparents"), main= title)
}
overlap_plot_lp<-function(){plotit(overlap_ps_lp, "Parent Controls", "LISS")}
overlap_plot_ln<-function(){plotit(overlap_ps_ln, "Nonparent Controls", NULL)}
overlap_plot_hp<-function(){plotit(overlap_ps_hp, "Parent Controls", "HRS")}
overlap_plot_hn<-function(){plotit(overlap_ps_hn, "Nonparent Controls", NULL)}

par(mfrow=c(2,2))
overlap_plot_lp()
overlap_plot_hp()
overlap_plot_ln()
overlap_plot_hn()
dev.off()

overlap_plot_lp()

overlap_ps_lp_plot <- ggplot(df, aes(x)) + geom_histogram( aes(x = x, y = ..density..),
                                         binwidth = diff(range(df$x))/30, fill="blue") + 
  geom_histogram( aes(x = x2, y = -..density..), binwidth = diff(range(df$x))/30, fill= "green")
print(g)


overlap_ps_ln <- lissanalysis_nonparents %>% filter(time==matchtime) %>% select(pid, grandparent, pscore)
overlap_ps_hp <- hrsanalysis_parents %>% filter(time==matchtime) %>% select(pid, grandparent, pscore)
overlap_ps_hn <- hrsanalysis_nonparents %>% filter(time==matchtime) %>% select(pid, grandparent, pscore)

cowplot::plot_grid(
  Hmisc::histbackback(split(overlap_ps_lp$pscore, overlap_ps_lp$grandparent), 
                      brks=seq(0, max(overlap_ps_lp$pscore), (max(overlap_ps_lp$pscore))/25),
                      main= "LISS", xlab=c("Parent Controls", "Grandparents")),
  Hmisc::histbackback(split(overlap_ps_hp$pscore, overlap_ps_hp$grandparent), 
                      brks=seq(0, max(overlap_ps_hp$pscore), (max(overlap_ps_hp$pscore))/25),
                      main= "HRS", xlab=c("Parent Controls", "Grandparents")),
  Hmisc::histbackback(split(overlap_ps_ln$pscore, overlap_ps_ln$grandparent), 
                      brks=seq(0, max(overlap_ps_ln$pscore), (max(overlap_ps_ln$pscore))/25),
                      xlab=c("Parent Controls", "Grandparents")),
  Hmisc::histbackback(split(overlap_ps_hn$pscore, overlap_ps_hn$grandparent), 
                      brks=seq(0, max(overlap_ps_hn$pscore), (max(overlap_ps_hn$pscore))/25),
                      xlab=c("Parent Controls", "Grandparents")),
  align = "vh", ncol = 2)

Hmisc::histbackback(split(lissanalysis_nonparents$pscore[lissanalysis_nonparents$time==lissanalysis_nonparents$matchtime], lissanalysis_nonparents$grandparent[lissanalysis_nonparents$time==lissanalysis_nonparents$matchtime]), 
                    brks=seq(0, max(lissanalysis_nonparents$pscore[lissanalysis_nonparents$time==lissanalysis_nonparents$matchtime]), (max(lissanalysis_nonparents$pscore[lissanalysis_nonparents$time==lissanalysis_nonparents$matchtime]))/25), 
                    main= "Propensity Score", xlab=c("Nonparent Controls", "Grandparents"))

Hmisc::histbackback(split(hrsanalysis_parents$pscore[hrsanalysis_parents$time==hrsanalysis_parents$matchtime], hrsanalysis_parents$grandparent[hrsanalysis_parents$time==hrsanalysis_parents$matchtime]), 
                    brks=seq(0, max(hrsanalysis_parents$pscore[hrsanalysis_parents$time==hrsanalysis_parents$matchtime]), (max(hrsanalysis_parents$pscore[hrsanalysis_parents$time==hrsanalysis_parents$matchtime]))/25), 
                    main= "Propensity Score", xlab=c("Parent Controls", "Grandparents"))

Hmisc::histbackback(split(hrsanalysis_nonparents$pscore[hrsanalysis_nonparents$time==hrsanalysis_nonparents$matchtime], hrsanalysis_nonparents$grandparent[hrsanalysis_nonparents$time==hrsanalysis_nonparents$matchtime]), 
                    brks=seq(0, max(hrsanalysis_nonparents$pscore[hrsanalysis_nonparents$time==hrsanalysis_nonparents$matchtime]), (max(hrsanalysis_nonparents$pscore[hrsanalysis_nonparents$time==hrsanalysis_nonparents$matchtime]))/25), 
                    main= "Propensity Score", xlab=c("Nonparent Controls", "Grandparents"))

