### Transition to Grandparenthood Paper - HRS Sample ###

#version.string R version 3.6.3 (2020-02-29)
#nickname       Holding the Windsock    

library(haven)
library(tidyverse)
library(psych)
library(knitr)

#### HRS data: draw raw data, select variables ####

# load raw data files (SAS format, downloaded from HRS website)
hrsrand <- read_sas("data/raw/HRS/randhrs1992_2016v2.sas7bdat")
h06data <- read_sas("data/raw/HRS/h06f3a.sas7bdat")
h08data <- read_sas("data/raw/HRS/h08f3a.sas7bdat")
h10data <- read_sas("data/raw/HRS/hd10f5e.sas7bdat")
h12data <- read_sas("data/raw/HRS/h12f2a.sas7bdat")
h14data <- read_sas("data/raw/HRS/h14f2a.sas7bdat")
h16data <- read_sas("data/raw/HRS/h16f2a.sas7bdat")

#from main tracker file, select variables of interest
hrsdata <- hrsrand %>%
  select(#demographics
    HACOHORT, HHID, PN, RABYEAR, 
    #RABPLACE, # 1.new england, 2.mid atlantic, 3.en central, 4.wn central, 5.s atlantic, 6.es central, 7.ws central, 8.mountain, 9.pacific, 10.us/na division, 11.not us/inc us terr
    RAGENDER, RARACEM, RAEDYRS, # 1.white/caucasian 2.black/african american 3.other
    matches("^R.*MSTAT$"), # marital status
    matches("^R.*DOCTOR$"), #doctor visits
    matches("^R.*HOSP$"), #hospital visits
    #matches("^R.*DRUGS$"), #Rx drugs
    #matches("^R.*OUTPT$"), #outpatient surgery
    #matches("^R.*DENTST$"), #dentist
    #matches("^R.*SPCFAC$"), #special health facility
    matches("^R.*HIGOV$"), #health insurance - govt
    matches("^R.*COVR$"), # health insurance - employer
    matches("^R.*COVS$"), # health insurance - spouse's employer
    matches("^R.*HIOTHP$"), #health insurance - other
    matches("^R.*CONDE$"), #sum of health conditions
    matches("^R.*PSYCHE$"), # psychiatric problems
    matches("^R.*LBRF$"), # labor force status
    #matches("^R.*SAYRET$"), # considers self retired (already included in labor force st.)
    matches("^R.*JHOURS$"), # hours worked/week main job
    matches("^R.*MOBILA$"), R1MOBILW, # some diff-mobility /0-5   (different for Wave 1)
    matches("^R.*CESD$"), # CESD score
    matches("^R.*BMI$"), # self-reported body mass index=kg/m2
    matches("^H.*HHRES$"), # number of people in hh
    matches("^R.*LIVSIB$"), # number of living siblings
    #matches("^R.*LTACTX$"), # freq light phys activ {finer scale}    (only W7-W13)
    #matches("^R.*MDACTX$"), # freq moderate phys activ {finer scale} (only W7-W13)
    #matches("^R.*VGACTX$"), # freq vigorous phys activ {finer scale} (only W7-W13)
    #matches("^R.*CENREG$"), # census region 1.northeast, 2.midwest, 3.south, 4.west, 5.other
    matches("^R.*MRCT$"), # number marriages
    matches("^R.*STROKE$"), # ever had stroke
    matches("^R.*CANCRE$"), # ever had cancer
    matches("^R.*DIABE$"), # ever had diabetes
    matches("^R.*HEARTE$"), # ever had heart problems
    matches("^R.*SHLT$")) %>% # self-rated health
  select(-starts_with("RE"), -ends_with("INLBRF"), -ends_with("PMBMI")) %>% 
  rename(R1MOBILA = R1MOBILW)

#save working memory by removing RAND file
rm(hrsrand)

#rename columns in RAND file
colnames(hrsdata) <- gsub("^RA","",colnames(hrsdata)) # ^ means "begins with"
colnames(hrsdata)[colnames(hrsdata) == "BYEAR"] <- "birthyr"
colnames(hrsdata)[colnames(hrsdata) == "RACEM"] <- "race"
colnames(hrsdata)[colnames(hrsdata) == "EDYRS"] <- "schlyrs"
colnames(hrsdata)[colnames(hrsdata) == "GENDER"] <- "gender"
#colnames(hrsdata)[colnames(hrsdata) == "BPLACE"] <- "birthplace"

colnames(hrsdata) <- ifelse(grepl("^(H)([0-9]+)", colnames(hrsdata)), # for HwHHRES
                            gsub("^(H)([0-9]+)", "", paste("R", colnames(hrsdata), sep="")), colnames(hrsdata))
colnames(hrsdata) <- gsub("^RH","R",colnames(hrsdata))

#separate out AHEAD cohort, because their waves are in slightly different years
ahead <- subset(hrsdata, HACOHORT %in% c(0,1))
hrsdata <- subset(hrsdata, !(HACOHORT %in% c(0,1)))

colnames(hrsdata) <- ifelse(grepl("R13",colnames(hrsdata)),
                            gsub("R13","", paste(colnames(hrsdata),"_2016",sep="")), colnames(hrsdata))
colnames(hrsdata) <- ifelse(grepl("R12",colnames(hrsdata)),
                            gsub("R12","", paste(colnames(hrsdata),"_2014",sep="")), colnames(hrsdata))
colnames(hrsdata) <- ifelse(grepl("R11",colnames(hrsdata)),
                            gsub("R11","", paste(colnames(hrsdata),"_2012",sep="")),
                            colnames(hrsdata))
colnames(hrsdata) <- ifelse(grepl("R10",colnames(hrsdata)),
                            gsub("R10","", paste(colnames(hrsdata),"_2010",sep="")),
                            colnames(hrsdata))
colnames(hrsdata) <- ifelse(grepl("R9",colnames(hrsdata)),
                            gsub("R9","", paste(colnames(hrsdata),"_2008",sep="")),
                            colnames(hrsdata))
colnames(hrsdata) <- ifelse(grepl("R8",colnames(hrsdata)),
                            gsub("R8","", paste(colnames(hrsdata),"_2006",sep="")),
                            colnames(hrsdata))
colnames(hrsdata) <- ifelse(grepl("R7",colnames(hrsdata)),
                            gsub("R7","", paste(colnames(hrsdata),"_2004",sep="")),
                            colnames(hrsdata))
colnames(hrsdata) <- ifelse(grepl("R6",colnames(hrsdata)),
                            gsub("R6","", paste(colnames(hrsdata),"_2002",sep="")),
                            colnames(hrsdata))
colnames(hrsdata) <- ifelse(grepl("R5",colnames(hrsdata)),
                            gsub("R5","", paste(colnames(hrsdata),"_2000",sep="")),
                            colnames(hrsdata))
colnames(hrsdata) <- ifelse(grepl("R4",colnames(hrsdata)),
                            gsub("R4","", paste(colnames(hrsdata),"_1998",sep="")),
                            colnames(hrsdata))
colnames(hrsdata) <- ifelse(grepl("R3",colnames(hrsdata)),
                            gsub("R3","", paste(colnames(hrsdata),"_1996",sep="")),
                            colnames(hrsdata))
colnames(hrsdata) <- ifelse(grepl("R2",colnames(hrsdata)),
                            gsub("R2","", paste(colnames(hrsdata),"_1994",sep="")),
                            colnames(hrsdata))
colnames(hrsdata) <- ifelse(grepl("R1",colnames(hrsdata)),
                            gsub("R1","", paste(colnames(hrsdata),"_1992",sep="")),
                            colnames(hrsdata))

colnames(ahead) <- ifelse(grepl("R13",colnames(ahead)),
                          gsub("R13","", paste(colnames(ahead),"_2016",sep="")),
                          colnames(ahead))
colnames(ahead) <- ifelse(grepl("R12",colnames(ahead)),
                          gsub("R12","", paste(colnames(ahead),"_2014",sep="")),
                          colnames(ahead))
colnames(ahead) <- ifelse(grepl("R11",colnames(ahead)),
                          gsub("R11","", paste(colnames(ahead),"_2012",sep="")),
                          colnames(ahead))
colnames(ahead) <- ifelse(grepl("R10",colnames(ahead)),
                          gsub("R10","", paste(colnames(ahead),"_2010",sep="")),
                          colnames(ahead))
colnames(ahead) <- ifelse(grepl("R9",colnames(ahead)),
                          gsub("R9","", paste(colnames(ahead),"_2008",sep="")),
                          colnames(ahead))
colnames(ahead) <- ifelse(grepl("R8",colnames(ahead)),
                          gsub("R8","", paste(colnames(ahead),"_2006",sep="")),
                          colnames(ahead))
colnames(ahead) <- ifelse(grepl("R7",colnames(ahead)),
                          gsub("R7","", paste(colnames(ahead),"_2004",sep="")),
                          colnames(ahead))
colnames(ahead) <- ifelse(grepl("R6",colnames(ahead)),
                          gsub("R6","", paste(colnames(ahead),"_2002",sep="")),
                          colnames(ahead))
colnames(ahead) <- ifelse(grepl("R5",colnames(ahead)),
                          gsub("R5","", paste(colnames(ahead),"_2000",sep="")),
                          colnames(ahead))
colnames(ahead) <- ifelse(grepl("R4",colnames(ahead)),
                          gsub("R4","", paste(colnames(ahead),"_1998",sep="")),
                          colnames(ahead))
colnames(ahead) <- ifelse(grepl("R3",colnames(ahead)),
                          gsub("R3","", paste(colnames(ahead),"_1995",sep="")),
                          colnames(ahead))
colnames(ahead) <- ifelse(grepl("R2",colnames(ahead)),
                          gsub("R2","", paste(colnames(ahead),"_1993",sep="")),
                          colnames(ahead))
colnames(ahead) <- ifelse(grepl("R1",colnames(ahead)),
                          gsub("R1","", paste(colnames(ahead),"_1992",sep="")),
                          colnames(ahead))
hrsdata <- merge(hrsdata, ahead, all.x=T, all.y=T)
rm(ahead)

#nit picky renaming
colnames(hrsdata) <- gsub("MSTAT","marital",colnames(hrsdata))
colnames(hrsdata) <- gsub("SHLT","selfratedhealth",colnames(hrsdata))
colnames(hrsdata) <- gsub("DOCTOR","doctor",colnames(hrsdata))
colnames(hrsdata) <- gsub("HOSP","hospital",colnames(hrsdata))
#colnames(hrsdata) <- gsub("DENTST","Dentist",colnames(hrsdata))
colnames(hrsdata) <- gsub("DRUGS","medication",colnames(hrsdata))
colnames(hrsdata) <- gsub("CONDE","conde",colnames(hrsdata))
colnames(hrsdata) <- gsub("PSYCHE","psyche",colnames(hrsdata))
#colnames(hrsdata) <- gsub("OUTPT","Outpatient",colnames(hrsdata))
#colnames(hrsdata) <- gsub("SPCFAC","SpecialFac",colnames(hrsdata))
colnames(hrsdata) <- gsub("HIGOV","higovt",colnames(hrsdata))
colnames(hrsdata) <- gsub("COVR","hiemployer",colnames(hrsdata))
colnames(hrsdata) <- gsub("COVS","hispousal",colnames(hrsdata))
colnames(hrsdata) <- gsub("HIOTHP","hiother",colnames(hrsdata))

colnames(hrsdata) <- gsub("LBRF","laborforce",colnames(hrsdata))
#colnames(hrsdata) <- gsub("SAYRET","sayretired",colnames(hrsdata))
colnames(hrsdata) <- gsub("JHOURS","jobhours",colnames(hrsdata))
colnames(hrsdata) <- gsub("MOBILA","mobilitydiff",colnames(hrsdata))
colnames(hrsdata) <- gsub("CESD","cesd",colnames(hrsdata))
colnames(hrsdata) <- gsub("BMI","bmi",colnames(hrsdata))
colnames(hrsdata) <- gsub("HHRES","hhmembers",colnames(hrsdata))
colnames(hrsdata) <- gsub("LIVSIB","siblings",colnames(hrsdata))
#colnames(hrsdata) <- gsub("LTACTX","lightphys",colnames(hrsdata))
#colnames(hrsdata) <- gsub("MDACTX","moderatephys",colnames(hrsdata))
#colnames(hrsdata) <- gsub("VGACTX","vigorousphys",colnames(hrsdata))
#colnames(hrsdata) <- gsub("CENREG","region",colnames(hrsdata))
colnames(hrsdata) <- gsub("MRCT","marriagesnum",colnames(hrsdata))
colnames(hrsdata) <- gsub("STROKE","stroke",colnames(hrsdata))
colnames(hrsdata) <- gsub("CANCRE","cancer",colnames(hrsdata))
colnames(hrsdata) <- gsub("DIABE","diabetes",colnames(hrsdata))
colnames(hrsdata) <- gsub("HEARTE","heart",colnames(hrsdata))


#reverse code self-rated health variables
hrsdata[,grepl("selfratedhealth",
               colnames(hrsdata))] <- hrsdata[,grepl("selfratedhealth",
                                                     colnames(hrsdata))]*-1 + 6

#RAND HRS Detailed Imputations File 2016 (v.2)
hrsrand_imp <- read_sas("data/raw/HRS/randhrs1992_2016v2.sas7bdat")

hrsdata_imp <- hrsrand_imp %>%
  select(
    HHIDPN, HHID, PN,
    matches("^H.*ITOT$"), # Total household income (Respondent & spouse)
    matches("^H.*ATOTB$")) # Total Wealth (Including Secondary Residence)
hrs_cohortinfo <- hrsdata %>% select(HHID, PN, HACOHORT)
hrsdata_imp <- left_join(hrsdata_imp, hrs_cohortinfo)

#separate out AHEAD cohort, because their waves are in slightly different years
ahead_imp <- subset(hrsdata_imp, HACOHORT %in% c(0,1))
hrsdata_imp <- subset(hrsdata_imp, !(HACOHORT %in% c(0,1)))

colnames(hrsdata_imp) <- ifelse(grepl("H13",colnames(hrsdata_imp)),
                            gsub("H13","", paste(colnames(hrsdata_imp),"_2016",sep="")), colnames(hrsdata_imp))
colnames(hrsdata_imp) <- ifelse(grepl("H12",colnames(hrsdata_imp)),
                            gsub("H12","", paste(colnames(hrsdata_imp),"_2014",sep="")), colnames(hrsdata_imp))
colnames(hrsdata_imp) <- ifelse(grepl("H11",colnames(hrsdata_imp)),
                            gsub("H11","", paste(colnames(hrsdata_imp),"_2012",sep="")),
                            colnames(hrsdata_imp))
colnames(hrsdata_imp) <- ifelse(grepl("H10",colnames(hrsdata_imp)),
                            gsub("H10","", paste(colnames(hrsdata_imp),"_2010",sep="")),
                            colnames(hrsdata_imp))
colnames(hrsdata_imp) <- ifelse(grepl("H9",colnames(hrsdata_imp)),
                            gsub("H9","", paste(colnames(hrsdata_imp),"_2008",sep="")),
                            colnames(hrsdata_imp))
colnames(hrsdata_imp) <- ifelse(grepl("H8",colnames(hrsdata_imp)),
                            gsub("H8","", paste(colnames(hrsdata_imp),"_2006",sep="")),
                            colnames(hrsdata_imp))
colnames(hrsdata_imp) <- ifelse(grepl("H7",colnames(hrsdata_imp)),
                            gsub("H7","", paste(colnames(hrsdata_imp),"_2004",sep="")),
                            colnames(hrsdata_imp))
colnames(hrsdata_imp) <- ifelse(grepl("H6",colnames(hrsdata_imp)),
                            gsub("H6","", paste(colnames(hrsdata_imp),"_2002",sep="")),
                            colnames(hrsdata_imp))
colnames(hrsdata_imp) <- ifelse(grepl("H5",colnames(hrsdata_imp)),
                            gsub("H5","", paste(colnames(hrsdata_imp),"_2000",sep="")),
                            colnames(hrsdata_imp))
colnames(hrsdata_imp) <- ifelse(grepl("H4",colnames(hrsdata_imp)),
                            gsub("H4","", paste(colnames(hrsdata_imp),"_1998",sep="")),
                            colnames(hrsdata_imp))
colnames(hrsdata_imp) <- ifelse(grepl("H3",colnames(hrsdata_imp)),
                            gsub("H3","", paste(colnames(hrsdata_imp),"_1996",sep="")),
                            colnames(hrsdata_imp))
colnames(hrsdata_imp) <- ifelse(grepl("H2",colnames(hrsdata_imp)),
                            gsub("H2","", paste(colnames(hrsdata_imp),"_1994",sep="")),
                            colnames(hrsdata_imp))
colnames(hrsdata_imp) <- ifelse(grepl("H1",colnames(hrsdata_imp)),
                            gsub("H1","", paste(colnames(hrsdata_imp),"_1992",sep="")),
                            colnames(hrsdata_imp))

colnames(ahead_imp) <- ifelse(grepl("H13",colnames(ahead_imp)),
                          gsub("H13","", paste(colnames(ahead_imp),"_2016",sep="")),
                          colnames(ahead_imp))
colnames(ahead_imp) <- ifelse(grepl("H12",colnames(ahead_imp)),
                          gsub("H12","", paste(colnames(ahead_imp),"_2014",sep="")),
                          colnames(ahead_imp))
colnames(ahead_imp) <- ifelse(grepl("H11",colnames(ahead_imp)),
                          gsub("H11","", paste(colnames(ahead_imp),"_2012",sep="")),
                          colnames(ahead_imp))
colnames(ahead_imp) <- ifelse(grepl("H10",colnames(ahead_imp)),
                          gsub("H10","", paste(colnames(ahead_imp),"_2010",sep="")),
                          colnames(ahead_imp))
colnames(ahead_imp) <- ifelse(grepl("H9",colnames(ahead_imp)),
                          gsub("H9","", paste(colnames(ahead_imp),"_2008",sep="")),
                          colnames(ahead_imp))
colnames(ahead_imp) <- ifelse(grepl("H8",colnames(ahead_imp)),
                          gsub("H8","", paste(colnames(ahead_imp),"_2006",sep="")),
                          colnames(ahead_imp))
colnames(ahead_imp) <- ifelse(grepl("H7",colnames(ahead_imp)),
                          gsub("H7","", paste(colnames(ahead_imp),"_2004",sep="")),
                          colnames(ahead_imp))
colnames(ahead_imp) <- ifelse(grepl("H6",colnames(ahead_imp)),
                          gsub("H6","", paste(colnames(ahead_imp),"_2002",sep="")),
                          colnames(ahead_imp))
colnames(ahead_imp) <- ifelse(grepl("H5",colnames(ahead_imp)),
                          gsub("H5","", paste(colnames(ahead_imp),"_2000",sep="")),
                          colnames(ahead_imp))
colnames(ahead_imp) <- ifelse(grepl("H4",colnames(ahead_imp)),
                          gsub("H4","", paste(colnames(ahead_imp),"_1998",sep="")),
                          colnames(ahead_imp))
colnames(ahead_imp) <- ifelse(grepl("H3",colnames(ahead_imp)),
                          gsub("H3","", paste(colnames(ahead_imp),"_1995",sep="")),
                          colnames(ahead_imp))
colnames(ahead_imp) <- ifelse(grepl("H2",colnames(ahead_imp)),
                          gsub("H2","", paste(colnames(ahead_imp),"_1993",sep="")),
                          colnames(ahead_imp))
colnames(ahead_imp) <- ifelse(grepl("H1",colnames(ahead_imp)),
                          gsub("H1","", paste(colnames(ahead_imp),"_1992",sep="")),
                          colnames(ahead_imp))
hrsdata_imp <- merge(hrsdata_imp, ahead_imp, all.x=T, all.y=T)
rm(ahead_imp)

#save working memory by removing RAND file
rm(hrsrand_imp)
rm(hrs_cohortinfo)

colnames(hrsdata_imp) <- gsub("ATOTB","hhwealth",colnames(hrsdata_imp))
colnames(hrsdata_imp) <- gsub("ITOT","hhincome",colnames(hrsdata_imp))

#subset leave-behind questionnaires so only personality items remain (and SWLS items)
h06pers <- h06data %>%
  select_if(grepl("\\bHHIDPN\\b",colnames(.)) | grepl("KLB033",colnames(.)) | grepl("KLB003",colnames(.))) %>%
  mutate(n_respond = rowSums(!is.na(.))) %>%
  filter(n_respond > 1) %>%
  select(-n_respond) %>%
  rename(extra1_2006 = KLB033A,
         extra2_2006 = KLB033E,
         extra3_2006 = KLB033I,
         extra4_2006 = KLB033S,
         extra5_2006 = KLB033W,
         agree1_2006 = KLB033B,
         agree2_2006 = KLB033F,
         agree3_2006 = KLB033J,
         agree4_2006 = KLB033O,
         agree5_2006 = KLB033V,
         neur1_2006 = KLB033C,
         neur2_2006 = KLB033G,
         neur3_2006 = KLB033K,
         neur4_2006 = KLB033P,
         con1_2006 = KLB033D,
         con2_2006 = KLB033H,
         con3_2006 = KLB033M,
         con4_2006 = KLB033T,
         con5_2006 = KLB033Z,
         open1_2006 = KLB033L,
         open2_2006 = KLB033N,
         open3_2006 = KLB033Q,
         open4_2006 = KLB033R,
         open5_2006 = KLB033U,
         open6_2006 = KLB033X,
         open7_2006 = KLB033Y,
         swls1_2006 = KLB003A, # SWLS = Satisfaction with Life Scale (Diener)
         swls2_2006 = KLB003B,
         swls3_2006 = KLB003C,
         swls4_2006 = KLB003D,
         swls5_2006 = KLB003E)
# only in 2006, the middle response option "NEITHER AGREE OR DISAGREE" was left out and 
# a 6-pt. scale was used for SWLS
h06pers$swls1_2006 <- dplyr::recode(h06pers$swls1_2006, `1`=1L, `2`=2L, `3`=3L, `4`=5L, `5`=6L, `6`=7L)
h06pers$swls2_2006 <- dplyr::recode(h06pers$swls2_2006, `1`=1L, `2`=2L, `3`=3L, `4`=5L, `5`=6L, `6`=7L)
h06pers$swls3_2006 <- dplyr::recode(h06pers$swls3_2006, `1`=1L, `2`=2L, `3`=3L, `4`=5L, `5`=6L, `6`=7L)
h06pers$swls4_2006 <- dplyr::recode(h06pers$swls4_2006, `1`=1L, `2`=2L, `3`=3L, `4`=5L, `5`=6L, `6`=7L)
h06pers$swls5_2006 <- dplyr::recode(h06pers$swls5_2006, `1`=1L, `2`=2L, `3`=3L, `4`=5L, `5`=6L, `6`=7L)

h08pers <- h08data %>%
  select_if(grepl("\\bHHIDPN\\b",colnames(.)) | grepl("LLB033",colnames(.)) | grepl("LLB003",colnames(.))) %>%
  mutate(n_respond = rowSums(!is.na(.))) %>%
  filter(n_respond > 1) %>%
  select(-n_respond) %>%
  rename(extra1_2008 = LLB033A,
         extra2_2008 = LLB033E,
         extra3_2008 = LLB033I,
         extra4_2008 = LLB033S,
         extra5_2008 = LLB033W,
         agree1_2008 = LLB033B,
         agree2_2008 = LLB033F,
         agree3_2008 = LLB033J,
         agree4_2008 = LLB033O,
         agree5_2008 = LLB033V,
         neur1_2008 = LLB033C,
         neur2_2008 = LLB033G,
         neur3_2008 = LLB033K,
         neur4_2008 = LLB033P,
         con1_2008 = LLB033D,
         con2_2008 = LLB033H,
         con3_2008 = LLB033M,
         con4_2008 = LLB033T,
         con5_2008 = LLB033Z,
         open1_2008 = LLB033L,
         open2_2008 = LLB033N,
         open3_2008 = LLB033Q,
         open4_2008 = LLB033R,
         open5_2008 = LLB033U,
         open6_2008 = LLB033X,
         open7_2008 = LLB033Y,
         swls1_2008 = LLB003A, # SWLS = Satisfaction with Life Scale (Diener)
         swls2_2008 = LLB003B,
         swls3_2008 = LLB003C,
         swls4_2008 = LLB003D,
         swls5_2008 = LLB003E)

h10pers <- h10data %>%
  select_if(grepl("\\bHHIDPN\\b",colnames(.)) | grepl("MLB033",colnames(.)) | grepl("MLB003",colnames(.))) %>%
  mutate(n_respond = rowSums(!is.na(.))) %>%
  filter(n_respond > 1) %>%
  select(-n_respond, -MLB033C, -MLB033R, -MLB033X, -MLB033Z, -MLB033Z_6) %>%
  rename(extra1_2010 = MLB033A,
         extra2_2010 = MLB033F,
         extra3_2010 = MLB033J,
         extra4_2010 = MLB033U,
         extra5_2010 = MLB033Z_2,
         agree1_2010 = MLB033B,
         agree2_2010 = MLB033G,
         agree3_2010 = MLB033K,
         agree4_2010 = MLB033P,
         agree5_2010 = MLB033Y,
         neur1_2010 = MLB033D,
         neur2_2010 = MLB033H,
         neur3_2010 = MLB033L,
         neur4_2010 = MLB033Q,
         con1_2010 = MLB033E,
         con2_2010 = MLB033I,
         con3_2010 = MLB033N,
         con4_2010 = MLB033V,
         con5_2010 = MLB033Z_5,
         open1_2010 = MLB033M,
         open2_2010 = MLB033O,
         open3_2010 = MLB033S,
         open4_2010 = MLB033T,
         open5_2010 = MLB033W,
         open6_2010 = MLB033Z_3,
         open7_2010 = MLB033Z_4,
         swls1_2010 = MLB003A, # SWLS = Satisfaction with Life Scale (Diener)
         swls2_2010 = MLB003B,
         swls3_2010 = MLB003C,
         swls4_2010 = MLB003D,
         swls5_2010 = MLB003E)

h12pers <- h12data %>%
  select_if(grepl("\\bHHIDPN\\b",colnames(.)) | grepl("NLB033",colnames(.)) | grepl("NLB003",colnames(.))) %>%
  mutate(n_respond = rowSums(!is.na(.))) %>%
  filter(n_respond > 1) %>%
  select(-n_respond, -NLB033C, -NLB033R, -NLB033X, -NLB033Z, -NLB033Z_6) %>%
  rename(extra1_2012 = NLB033A,
         extra2_2012 = NLB033F,
         extra3_2012 = NLB033J,
         extra4_2012 = NLB033U,
         extra5_2012 = NLB033Z_2,
         agree1_2012 = NLB033B,
         agree2_2012 = NLB033G,
         agree3_2012 = NLB033K,
         agree4_2012 = NLB033P,
         agree5_2012 = NLB033Y,
         neur1_2012 = NLB033D,
         neur2_2012 = NLB033H,
         neur3_2012 = NLB033L,
         neur4_2012 = NLB033Q,
         con1_2012 = NLB033E,
         con2_2012 = NLB033I,
         con3_2012 = NLB033N,
         con4_2012 = NLB033V,
         con5_2012 = NLB033Z_5,
         open1_2012 = NLB033M,
         open2_2012 = NLB033O,
         open3_2012 = NLB033S,
         open4_2012 = NLB033T,
         open5_2012 = NLB033W,
         open6_2012 = NLB033Z_3,
         open7_2012 = NLB033Z_4,
         swls1_2012 = NLB003A, # SWLS = Satisfaction with Life Scale (Diener)
         swls2_2012 = NLB003B,
         swls3_2012 = NLB003C,
         swls4_2012 = NLB003D,
         swls5_2012 = NLB003E)

h14pers <- h14data %>%
  select_if(grepl("\\bHHIDPN\\b",colnames(.)) | grepl("OLB031",colnames(.)) | grepl("OLB002",colnames(.))) %>%
  mutate(n_respond = rowSums(!is.na(.))) %>%
  filter(n_respond > 1) %>%
  select(-n_respond, -OLB031C, -OLB031R, -OLB031X, -OLB031Z_1, -OLB031Z_6) %>%
  rename(extra1_2014 = OLB031A,
         extra2_2014 = OLB031F,
         extra3_2014 = OLB031J,
         extra4_2014 = OLB031U,
         extra5_2014 = OLB031Z_2,
         agree1_2014 = OLB031B,
         agree2_2014 = OLB031G,
         agree3_2014 = OLB031K,
         agree4_2014 = OLB031P,
         agree5_2014 = OLB031Y,
         neur1_2014 = OLB031D,
         neur2_2014 = OLB031H,
         neur3_2014 = OLB031L,
         neur4_2014 = OLB031Q,
         con1_2014 = OLB031E,
         con2_2014 = OLB031I,
         con3_2014 = OLB031N,
         con4_2014 = OLB031V,
         con5_2014 = OLB031Z_5,
         open1_2014 = OLB031M,
         open2_2014 = OLB031O,
         open3_2014 = OLB031S,
         open4_2014 = OLB031T,
         open5_2014 = OLB031W,
         open6_2014 = OLB031Z_3,
         open7_2014 = OLB031Z_4,
         swls1_2014 = OLB002A, # SWLS = Satisfaction with Life Scale (Diener)
         swls2_2014 = OLB002B,
         swls3_2014 = OLB002C,
         swls4_2014 = OLB002D,
         swls5_2014 = OLB002E)

#colnames(h16data) <- toupper(colnames(h16data))

h16pers <- h16data %>%
  select_if(grepl("\\bHHIDPN\\b",colnames(.)) | grepl("PLB031",colnames(.)) | grepl("PLB002",colnames(.))) %>%
  mutate(n_respond = rowSums(!is.na(.))) %>%
  filter(n_respond > 1) %>%
  select(-n_respond, -PLB031C, -PLB031R, -PLB031X, -PLB031Z_1, -PLB031Z_6) %>%
  rename(extra1_2016 = PLB031A,   # outgoing
         extra2_2016 = PLB031F,   # friendly
         extra3_2016 = PLB031J,   # lively
         extra4_2016 = PLB031U,   # active
         extra5_2016 = PLB031Z_2, # talkative
         agree1_2016 = PLB031B,   # helpful
         agree2_2016 = PLB031G,   # warm
         agree3_2016 = PLB031K,   # caring
         agree4_2016 = PLB031P,   # softhearted
         agree5_2016 = PLB031Y,   # sympathetic
         neur1_2016 = PLB031D,    # moody
         neur2_2016 = PLB031H,    # worrying
         neur3_2016 = PLB031L,    # nervous
         neur4_2016 = PLB031Q,    # calm
         con1_2016 = PLB031E,     # organized
         con2_2016 = PLB031I,     # responsible
         con3_2016 = PLB031N,     # hardworking
         con4_2016 = PLB031V,     # careless
         con5_2016 = PLB031Z_5,   # thorough
         open1_2016 = PLB031M,    # creative
         open2_2016 = PLB031O,    # imaginative
         open3_2016 = PLB031S,    # intelligent
         open4_2016 = PLB031T,    # curious
         open5_2016 = PLB031W,    # broadminded
         open6_2016 = PLB031Z_3,  # sophisticated
         open7_2016 = PLB031Z_4,  # adventurous
         swls1_2016 = PLB002A,    # life is close to ideal
         swls2_2016 = PLB002B,    # conditions of life are excellent
         swls3_2016 = PLB002C,    # satisfied with life
         swls4_2016 = PLB002D,    # have important things in life
         swls5_2016 = PLB002E)    # change nothing if lived life over


# merge together

allpers <- h06pers %>%
  full_join(h08pers) %>%
  full_join(h10pers) %>%
  full_join(h12pers) %>%
  full_join(h14pers) %>%  
  full_join(h16pers) %>%
  # arrange in long-form
  gather(key="key", value="value", which(grepl("_", names(.)))) %>%
  # removing missing years for each participant
  filter(!is.na(value)) %>%
  separate(col="key", into=c("key","year")) %>%
  mutate(year = as.numeric(year)) %>%
  spread(key="key", value="value") %>%
  # select first personality assessment -> no, we'll use all waves
  arrange(HHIDPN, year) #%>%
  #group_by(HHIDPN) %>%
  #filter(row_number() == 1) %>%
  #ungroup()

#calculate alphas
alpha.extra <- allpers %>%
  select_if(grepl("extra", names(.))) %>%
  psych::alpha(check.keys = TRUE)

alpha.agree <- allpers %>%
  select_if(grepl("agree", names(.))) %>%
  psych::alpha(check.keys = TRUE)

alpha.con <- allpers %>%
  select_if(grepl("con", names(.))) %>%
  psych::alpha(check.keys = TRUE)

alpha.neur <- allpers %>%
  select_if(grepl("neur", names(.))) %>%
  psych::alpha(check.keys = TRUE)

alpha.open <- allpers %>%
  select_if(grepl("open", names(.))) %>%
  psych::alpha(check.keys = TRUE)

alpha.swls <- allpers %>%
  select_if(grepl("swls", names(.))) %>%
  psych::alpha(check.keys = TRUE)

#score traits 
allpers$extra <- allpers%>%
  select_if(grepl("extra", names(.))) %>%
  reverse.code(keys=alpha.extra$keys, items = .) %>%
  rowMeans(na.rm=T)
allpers$agree <- allpers%>%
  select_if(grepl("agree", names(.))) %>%
  reverse.code(keys=alpha.agree$keys, items = .) %>%
  rowMeans(na.rm=T)
allpers$con <- allpers%>%
  select_if(grepl("con", names(.))) %>%
  reverse.code(keys=alpha.con$keys, items = .) %>%
  rowMeans(na.rm=T)
allpers$neur <- allpers%>%
  select_if(grepl("neur", names(.))) %>%
  reverse.code(keys=alpha.neur$keys, items = .) %>%
  rowMeans(na.rm=T)
allpers$open <- allpers%>%
  select_if(grepl("open", names(.))) %>%
  reverse.code(keys=alpha.open$keys, items = .) %>%
  rowMeans(na.rm=T)
allpers$swls <- allpers%>%
  select_if(grepl("swls", names(.))) %>%
  reverse.code(keys=alpha.swls$keys, items = .) %>%
  rowMeans(na.rm=T)

#needed for retrospective coding of grandparenthood:
# (I put this part behind the pre-processing of the rand hrs cross-wave file 
#  because I'd get a memory allocation error otherwise)
h96data <- read_sas("data/raw/HRS/h96f4a.sas7bdat")
h98data <- read_sas("data/raw/HRS/h98f2c.sas7bdat")
h00data <- read_sas("data/raw/HRS/h00f1c.sas7bdat")
h02data <- read_sas("data/raw/HRS/h02f2c.sas7bdat")
h04data <- read_sas("data/raw/HRS/h04f1c.sas7bdat")

# select grandparent variables
h96cov <- h96data %>%
  select(HHIDPN,
         E1413) %>%
  mutate(HHIDPN = as.numeric(HHIDPN)) %>% 
  mutate(n_respond = rowSums(!is.na(.))) %>%
  filter(n_respond > 1) %>%
  select(-n_respond) %>%
  rename(#newgrandkids_1996          = KE022,   # new grandchildren
         totalgrandkids_1996        = E1413   # grandchildren total
  )

h98cov <- h98data %>%
  select(HHIDPN,
         F1773, F1819) %>%
  mutate(HHIDPN = as.numeric(HHIDPN)) %>% 
  mutate(n_respond = rowSums(!is.na(.))) %>%
  filter(n_respond > 1) %>%
  select(-n_respond) %>%
  rename(newgrandkids_1998          = F1773,   # new grandchildren
         totalgrandkids_1998        = F1819,   # grandchildren total
  )

h00cov <- h00data %>%
  select(HHIDPN,
         G1989, G2035) %>%
  mutate(HHIDPN = as.numeric(HHIDPN)) %>% 
  mutate(n_respond = rowSums(!is.na(.))) %>%
  filter(n_respond > 1) %>%
  select(-n_respond) %>%
  rename(newgrandkids_2000          = G1989,   # new grandchildren
         totalgrandkids_2000        = G2035,   # grandchildren total
  )

h02cov <- h02data %>%
  select(HHIDPN,
         HE022, HE046) %>%
  mutate(HHIDPN = as.numeric(HHIDPN)) %>% 
  mutate(n_respond = rowSums(!is.na(.))) %>%
  filter(n_respond > 1) %>%
  select(-n_respond) %>%
  rename(newgrandkids_2002          = HE022,   # new grandchildren
         totalgrandkids_2002        = HE046,   # grandchildren total
  )

h04cov <- h04data %>%
  select(HHIDPN,
         JE022, JE046) %>%
  mutate(HHIDPN = as.numeric(HHIDPN)) %>% 
  mutate(n_respond = rowSums(!is.na(.))) %>%
  filter(n_respond > 1) %>%
  select(-n_respond) %>%
  rename(newgrandkids_2004          = JE022,   # new grandchildren
         totalgrandkids_2004        = JE046,   # grandchildren total
  )

# select grandparent variables AND PSM covariates
h06cov <- h06data %>%
  select(HHIDPN, 
         KE012, KE022, KE046, KE060, KE063, KE065, KE066, KH001, KH002, KH004,
         KA099, KA100, KA501, KLB039B,
         KZ230, KH150, KH151, KQ400, KB082, KH147, K066, KJ020, KJ021, KJ005M1) %>%
  mutate(HHIDPN = as.numeric(HHIDPN)) %>% 
  mutate(n_respond = rowSums(!is.na(.))) %>%
  filter(n_respond > 1) %>%
  select(-n_respond) %>%
  rename(#information on grandchildren:
         newgrandkids_2006          = KE022,   # new grandchildren
         totalgrandkids_2006        = KE046,   # grandchildren total
         grandkids100h_2006         = KE060,   # care of grandkids- 100 or more hours
         hoursgrandkids_2006        = KE063,   # r care for grandchild- # hours
         minhoursgrandkids_2006     = KE065,   # r care for grandchild- min hours
         maxhoursgrandkids_2006     = KE066,   # r care for grandchild- max hours
         #variables for PSM covariates:
         children10m_2006           = KE012,   # children live within 10 miles
         farmranch_2006             = KH001,   # live farm or ranch
         typehome_2006              = KH002,   # type home
         ownrent_2006               = KH004,   # own-rent home
         totalresidentkids_2006     = KA099,   # number of resident children						
         totalnonresidentkids_2006  = KA100,   # count of nonresident kids						
         interviewyear_2006         = KA501,   # date	of	interview	-	year		
         difficultybills_2006       = KLB039B, # difficulty	paying	bills			
         bornusa_2006               = KZ230,   # r	us born					
         safetyneighborhood_2006    = KH150,   # safety neighborhood						
         secondhome_2006            = KH151,   # own second home						
         foodstamps_2006            = KQ400,   # hh	food	stamps	since	last	iw
         attendreligion_2006        = KB082,   # how often attend religious serv
         nroomsself_2006            = KH147,   # number of rooms
         nroomsint_2006             = K066,    # number of rooms in housing unit
         paidwork_2006              = KJ020,   # working for pay
         selfemployed_2006          = KJ021,   # work for someone else/slf-employed
         jobstatus_2006             = KJ005M1  # current job status- 1 -> 6=homemaker
  )

h08cov <- h08data %>%
  select(HHIDPN, 
         LE012, LE022, LE046, LE060, LE063, LE065, LE066, LH001, LH002, LH004,
         LA099, LA100, LA501,
         LLB040,LZ230, LH150, LH151, LQ400, LB082, LH147, L066, LJ020, LJ021, LJ005M1) %>%
  mutate(HHIDPN = as.numeric(HHIDPN)) %>% 
  mutate(n_respond = rowSums(!is.na(.))) %>%
  filter(n_respond > 1) %>%
  select(-n_respond) %>%
  rename(#information on grandchildren:
         newgrandkids_2008          = LE022,   # new grandchildren
         totalgrandkids_2008        = LE046,   # grandchildren total
         grandkids100h_2008         = LE060,   # care of grandkids- 100 or more hours
         hoursgrandkids_2008        = LE063,   # r care for grandchild- # hours
         minhoursgrandkids_2008     = LE065,   # r care for grandchild- min hours
         maxhoursgrandkids_2008     = LE066,   # r care for grandchild- max hours
         #variables for PSM covariates:
         children10m_2008           = LE012,   # children live within 10 miles
         farmranch_2008             = LH001,   # live farm or ranch
         typehome_2008              = LH002,   # type home
         ownrent_2008               = LH004,   # own-rent home
         totalresidentkids_2008     = LA099,   # number of resident children						
         totalnonresidentkids_2008  = LA100,   # count of nonresident kids						
         interviewyear_2008         = LA501,   # date	of	interview	-	year		
         difficultybills_2008       = LLB040,  # difficulty	paying	bills			
         bornusa_2008               = LZ230,   # r	us born					
         safetyneighborhood_2008    = LH150,   # safety neighborhood						
         secondhome_2008            = LH151,   # own second home						
         foodstamps_2008            = LQ400,   # hh	food	stamps	since	last	iw	
         attendreligion_2008        = LB082,   # how often attend religious serv
         nroomsself_2008            = LH147,   # number of rooms
         nroomsint_2008             = L066,    # number of rooms in housing unit
         paidwork_2008              = LJ020,   # working for pay
         selfemployed_2008          = LJ021,   # work for someone else/slf-employed
         jobstatus_2008             = LJ005M1  # current job status- 1 -> 6=homemaker
  )

h10cov <- h10data %>%
  select(HHIDPN, 
         ME012, ME022, ME046, ME060, ME063, ME065, ME066, MH001, MH002, MH004,
         MA099, MA100, MA501, MLB040,
         MZ230, MH150, MH151, MQ400, MB082, MH147, M066, MJ020, MJ021, MJ005M1) %>%
  mutate(HHIDPN = as.numeric(HHIDPN)) %>% 
  mutate(n_respond = rowSums(!is.na(.))) %>%
  filter(n_respond > 1) %>%
  select(-n_respond) %>%
  rename(#information on grandchildren:
         newgrandkids_2010          = ME022,   # new grandchildren
         totalgrandkids_2010        = ME046,   # grandchildren total
         grandkids100h_2010         = ME060,   # care of grandkids- 100 or more hours
         hoursgrandkids_2010        = ME063,   # r care for grandchild- # hours
         minhoursgrandkids_2010     = ME065,   # r care for grandchild- min hours
         maxhoursgrandkids_2010     = ME066,   # r care for grandchild- max hours
         #variables for PSM covariates:
         children10m_2010           = ME012,   # children live within 10 miles
         farmranch_2010             = MH001,   # live farm or ranch
         typehome_2010              = MH002,   # type home
         ownrent_2010               = MH004,   # own-rent home
         totalresidentkids_2010     = MA099,   # number of resident children						
         totalnonresidentkids_2010  = MA100,   # count of nonresident kids						
         interviewyear_2010         = MA501,   # date	of	interview	-	year		
         difficultybills_2010       = MLB040,  # difficulty	paying	bills			
         bornusa_2010               = MZ230,   # r	us born					
         safetyneighborhood_2010    = MH150,   # safety neighborhood						
         secondhome_2010            = MH151,   # own second home						
         foodstamps_2010            = MQ400,   # hh	food	stamps	since	last	iw	
         attendreligion_2010        = MB082,   # how often attend religious serv
         nroomsself_2010            = MH147,   # number of rooms
         nroomsint_2010             = M066,    # number of rooms in housing unit
         paidwork_2010              = MJ020,   # working for pay
         selfemployed_2010          = MJ021,   # work for someone else/slf-employed
         jobstatus_2010             = MJ005M1  # current job status- 1 -> 6=homemaker
  )

h12cov <- h12data %>%
  select(HHIDPN,
         NE012, NE022, NE046, NE060, NE063, NE065, NE066, NH001, NH002, NH004,
         NA099, NA100, NA501, NLB040,
         NZ230, NH150, NH151, NQ400, NB082, NH147, N066, NJ020, NJ021, NJ005M1) %>%
  mutate(HHIDPN = as.numeric(HHIDPN)) %>% 
  mutate(n_respond = rowSums(!is.na(.))) %>%
  filter(n_respond > 1) %>%
  select(-n_respond) %>%
  rename(#information on grandchildren:
         newgrandkids_2012          = NE022,   # new grandchildren
         totalgrandkids_2012        = NE046,   # grandchildren total
         grandkids100h_2012         = NE060,   # care of grandkids- 100 or more hours
         hoursgrandkids_2012        = NE063,   # r care for grandchild- # hours
         minhoursgrandkids_2012     = NE065,   # r care for grandchild- min hours
         maxhoursgrandkids_2012     = NE066,   # r care for grandchild- max hours
         #variables for PSM covariates:
         children10m_2012           = NE012,   # children live within 10 miles
         farmranch_2012             = NH001,   # live farm or ranch
         typehome_2012              = NH002,   # type home
         ownrent_2012               = NH004,   # own-rent home
         totalresidentkids_2012     = NA099,   # number of resident children						
         totalnonresidentkids_2012  = NA100,   # count of nonresident kids						
         interviewyear_2012         = NA501,   # date	of	interview	-	year		
         difficultybills_2012       = NLB040,  # difficulty	paying	bills			
         bornusa_2012               = NZ230,   # r	us born					
         safetyneighborhood_2012    = NH150,   # safety neighborhood						
         secondhome_2012            = NH151,   # own second home						
         foodstamps_2012            = NQ400,   # hh	food	stamps	since	last	iw	
         attendreligion_2012        = NB082,   # how often attend religious serv
         nroomsself_2012            = NH147,   # number of rooms
         nroomsint_2012             = N066,    # number of rooms in housing unit
         paidwork_2012              = NJ020,   # working for pay 
         selfemployed_2012          = NJ021,   # work for someone else/slf-employed
         jobstatus_2012             = NJ005M1  # current job status- 1 -> 6=homemaker
  )

h14cov <- h14data %>%
  select(HHIDPN, 
         OE012, OE022, OE046, OE060, OE063, OE065, OE066, OH001, OH002, OH004,
         OA099, OA100, OA501, 
         OLB035, OZ230, OH150, OH151, OQ400, OB082, OH147, O066, OJ020, OJ021, OJ005M1) %>%
  mutate(HHIDPN = as.numeric(HHIDPN)) %>% 
  mutate(n_respond = rowSums(!is.na(.))) %>%
  filter(n_respond > 1) %>%
  select(-n_respond) %>%
  rename(#information on grandchildren:
         newgrandkids_2014          = OE022,   # new grandchildren
         totalgrandkids_2014        = OE046,   # grandchildren total
         grandkids100h_2014         = OE060,   # care of grandkids- 100 or more hours
         hoursgrandkids_2014        = OE063,   # r care for grandchild- # hours
         minhoursgrandkids_2014     = OE065,   # r care for grandchild- min hours
         maxhoursgrandkids_2014     = OE066,   # r care for grandchild- max hours
         #variables for PSM covariates:
         children10m_2014           = OE012,   # children live within 10 miles
         farmranch_2014             = OH001,   # live farm or ranch
         typehome_2014              = OH002,   # type home
         ownrent_2014               = OH004,   # own-rent home
         totalresidentkids_2014     = OA099,   # number of resident children						
         totalnonresidentkids_2014  = OA100,   # count of nonresident kids						
         interviewyear_2014         = OA501,   # date	of	interview	-	year		
         difficultybills_2014       = OLB035,  # difficulty	paying	bills			
         bornusa_2014               = OZ230,   # r	us born					
         safetyneighborhood_2014    = OH150,   # safety neighborhood						
         secondhome_2014            = OH151,   # own second home						
         foodstamps_2014            = OQ400,   # hh	food	stamps	since	last	iw	
         attendreligion_2014        = OB082,   # how often attend religious serv
         nroomsself_2014            = OH147,   # number of rooms
         nroomsint_2014             = O066,    # number of rooms in housing unit
         paidwork_2014              = OJ020,   # working for pay
         selfemployed_2014          = OJ021,   # work for someone else/slf-employed
         jobstatus_2014             = OJ005M1  # current job status- 1 -> 6=homemaker
  )

h16cov <- h16data %>%
  select(HHIDPN, 
         PE012, PE022, PE046, PE060, PE063, PE065, PE066, PH001, PH002, PH004,
         PA099, PA100, PA501,
         PLB035, PZ230, PH150, PH151, PQ400, PB082, PH147, P066, PJ020, PJ021, PJ005M1) %>%
  mutate(HHIDPN = as.numeric(HHIDPN)) %>% 
  mutate(n_respond = rowSums(!is.na(.))) %>%
  filter(n_respond > 1) %>%
  select(-n_respond) %>%
  rename(#information on grandchildren:
         newgrandkids_2016          = PE022,   # new grandchildren
         totalgrandkids_2016        = PE046,   # grandchildren total
         grandkids100h_2016         = PE060,   # care of grandkids- 100 or more hours
         hoursgrandkids_2016        = PE063,   # r care for grandchild- # hours
         minhoursgrandkids_2016     = PE065,   # r care for grandchild- min hours
         maxhoursgrandkids_2016     = PE066,   # r care for grandchild- max hours
         #variables for PSM covariates:
         children10m_2016           = PE012,   # children live within 10 miles
         farmranch_2016             = PH001,   # live farm or ranch
         typehome_2016              = PH002,   # type home
         ownrent_2016               = PH004,   # own-rent home
         totalresidentkids_2016     = PA099,   # number of resident children						
         totalnonresidentkids_2016  = PA100,   # count of nonresident kids						
         interviewyear_2016         = PA501,   # date	of	interview	-	year		
         difficultybills_2016       = PLB035,  # difficulty	paying	bills			
         bornusa_2016               = PZ230,   # r	us born					
         safetyneighborhood_2016    = PH150,   # safety neighborhood						
         secondhome_2016            = PH151,   # own second home						
         foodstamps_2016            = PQ400,   # hh	food	stamps	since	last	iw	
         attendreligion_2016        = PB082,   # how often attend religious serv
         nroomsself_2016            = PH147,   # number of rooms
         nroomsint_2016             = P066,    # number of rooms in housing unit
         paidwork_2016              = PJ020,   # working for pay
         selfemployed_2016          = PJ021,   # work for someone else/slf-employed
         jobstatus_2016             = PJ005M1  # current job status- 1 -> 6=homemaker
  )

# RAND HRS Family Data 2014 - two files: one with household-level and one with 
# child-level information -> we need child-level information! -> variables:
#   kagenderbg: gender (best guess)
#   kabyearbg: birth year (best guess)
#   kaeduc: kid years of education last reported
#   kidid: kid identifier/hhid+lopn

hrsfamk <- read_sas("data/raw/HRS/randhrsfamk1992_2014v1.sas7bdat")
hrsfam <- hrsfamk %>% select(HHIDPN, KIDID, KAGENDERBG, KABYEARBG, KAEDUC)
# count observations (=kids) per HHIDPN
hrsfam <- hrsfam %>% 
  mutate(KABYEARBG = replace(KABYEARBG, is.na(KABYEARBG), 9999)) %>% 
  arrange(HHIDPN, KABYEARBG, KIDID) %>% # sort by birth year
  mutate(KABYEARBG = replace(KABYEARBG, KABYEARBG==9999, NA)) 
hrsfam <- hrsfam %>% group_by(HHIDPN) %>% mutate(kidnr = row_number()) %>% # count
                                          mutate(kidmax = max(kidnr)) %>%  # get maximum
  ungroup() %>% filter(kidnr %in% c(1:3)) # for PSM covars, we focus on 3 oldest kids
# reshape to wide dataset (one row per parent)
hrsfam <- hrsfam %>% select(-KIDID) %>% 
  pivot_wider(names_from = kidnr, 
              values_from = c(KAGENDERBG, KABYEARBG, KAEDUC))
# flag if parent only has 1 or 2 children
hrsfam <- hrsfam %>% mutate(has_ch1 = 1,
                            has_ch2 = ifelse(kidmax>1, 1, 0),
                            has_ch3 = ifelse(kidmax>2, 1, 0)) %>% 
  mutate_at(vars(ends_with("_2")), funs(ifelse(is.na(.) & has_ch2==0, 0, .))) %>% 
  mutate_at(vars(ends_with("_3")), funs(ifelse(is.na(.) & has_ch3==0, 0, .)))
  #last two recodings will later help distinguish different kinds of missings

#### HRS data: merge datasets ####

# merge together
allcov <- h96cov %>%
  full_join(h98cov) %>%
  full_join(h00cov) %>%
  full_join(h02cov) %>%
  full_join(h04cov) %>%
  full_join(h06cov) %>%
  full_join(h08cov) %>%
  full_join(h10cov) %>%
  full_join(h12cov) %>%
  full_join(h14cov) %>%  
  full_join(h16cov) %>%
  # arrange in long-form
  gather(key="key", value="value", which(grepl("_", names(.)))) %>%
  # removing missing years for each participant
  filter(!is.na(value)) %>%
  separate(col="key", into=c("key","year")) %>%
  mutate(year = as.numeric(year)) %>%
  spread(key="key", value="value") %>%
  arrange(HHIDPN, year)

#convert to long form
hrslong <- hrsdata %>%
  mutate(HHID = as.numeric(HHID),
         PN = as.numeric(PN),
         HHIDPN = HHID*1000 + PN) %>% 
  select(-HHID, -PN, -HACOHORT) %>%
  gather(key="variable", value="value", which(grepl("_", names(.)))) %>%
  separate(col="variable", into = c("variable","year")) %>%
  mutate(year = as.numeric(year)) %>%
  subset(!is.na(value)) %>%
  spread(key = "variable", value="value") %>%
  arrange(HHIDPN, year)

#same for income/wealth imputations dataset
hrslong_imp <- hrsdata_imp %>%
  select(-HHID, -PN, -HACOHORT) %>%
  gather(key="variable", value="value", which(grepl("_", names(.)))) %>%
  separate(col="variable", into = c("variable","year")) %>%
  mutate(year = as.numeric(year)) %>%
  subset(!is.na(value)) %>%
  spread(key = "variable", value="value") %>% 
  arrange(HHIDPN, year)

# merge
hrslong <- left_join(hrslong, hrslong_imp,  by=c('HHIDPN', 'year'))
hrslong <- left_join(hrslong, allcov,  by=c('HHIDPN', 'year'))
hrslong <- left_join(hrslong, allpers,  by=c('HHIDPN', 'year'))
hrslong <- left_join(hrslong, hrsfam,  by=c('HHIDPN'))

# recode missing codes in grandparent variable
hrslong <- hrslong %>% 
  mutate(
    totalgrandkids = replace(totalgrandkids, totalgrandkids %in% c(98,99,998,999), -1))
hrslong <- hrslong %>% 
  mutate( # see codebook: "Assumed to be zero"
    totalgrandkids = replace(totalgrandkids, totalgrandkids %in% c(95, 995), 0))

# keep those with valid information on grandchildren
grand <- hrslong %>% 
  filter(!is.na(totalgrandkids)) %>% 
  mutate(grandchildren = ifelse(totalgrandkids==-1, NA, ifelse(totalgrandkids==0, 0, 1))) %>% 
  select(HHIDPN, year, grandchildren, totalgrandkids, totalnonresidentkids, totalresidentkids,
         newgrandkids, grandkids100h, hoursgrandkids, maxhoursgrandkids, minhoursgrandkids)

# check data quality:
#View(grand)
summary(grand)

#### identify transitions to grandparenthood ####

grand %>% group_by(year) %>% 
  dplyr::summarise(mean = mean(grandchildren, na.rm = T))

grand_wide <- grand %>% select(HHIDPN, year, grandchildren) %>% 
  arrange(HHIDPN, year) %>% 
  pivot_wider(names_from = year,
              names_prefix = "grandchildren_",
              values_from = grandchildren
  ) %>% 
  select(sort(tidyselect::peek_vars())) %>% glimpse()

# drop if all years are missing
#grand_wide <- grand_wide %>% 
#  filter(!is.na(grandchildren_2006) | 
#         !is.na(grandchildren_2008) | 
#         !is.na(grandchildren_2010) | 
#         !is.na(grandchildren_2012) | 
#         !is.na(grandchildren_2014) | 
#         !is.na(grandchildren_2016)
######### or better: drop non-grandparents where more than half of years are missing -> see below

grand_wide %>% select(starts_with("grandchildren")) %>% psych::describe()

# first step: code all the 0 to 1 transitions
grand_wide <- grand_wide %>% mutate(
  transit1998 = ifelse(is.na(grandchildren_1996) | is.na(grandchildren_1998), 0,
                       ifelse(grandchildren_1996==0 & grandchildren_1998==1, 1, 0)),
  transit2000 = ifelse(is.na(grandchildren_1998) | is.na(grandchildren_2000), 0,
                       ifelse(grandchildren_1998==0 & grandchildren_2000==1, 1, 0)),
  transit2002 = ifelse(is.na(grandchildren_2000) | is.na(grandchildren_2002), 0,
                       ifelse(grandchildren_2000==0 & grandchildren_2002==1, 1, 0)),
  transit2004 = ifelse(is.na(grandchildren_2002) | is.na(grandchildren_2004), 0,
                       ifelse(grandchildren_2002==0 & grandchildren_2004==1, 1, 0)),
  transit2006 = ifelse(is.na(grandchildren_2004) | is.na(grandchildren_2006), 0,
                       ifelse(grandchildren_2004==0 & grandchildren_2006==1, 1, 0)),
  transit2008 = ifelse(is.na(grandchildren_2006) | is.na(grandchildren_2008), 0,
                       ifelse(grandchildren_2006==0 & grandchildren_2008==1, 1, 0)),
  transit2010 = ifelse(is.na(grandchildren_2008) | is.na(grandchildren_2010), 0,
                       ifelse(grandchildren_2008==0 & grandchildren_2010==1, 1, 0)),
  transit2012 = ifelse(is.na(grandchildren_2010) | is.na(grandchildren_2012), 0,
                       ifelse(grandchildren_2010==0 & grandchildren_2012==1, 1, 0)),
  transit2014 = ifelse(is.na(grandchildren_2012) | is.na(grandchildren_2014), 0,
                       ifelse(grandchildren_2012==0 & grandchildren_2014==1, 1, 0)),
  transit2016 = ifelse(is.na(grandchildren_2014) | is.na(grandchildren_2016), 0,
                       ifelse(grandchildren_2014==0 & grandchildren_2016==1, 1, 0)))

# second step: code all the 1 to 0 transitions (inconsistent longitudinal data)
grand_wide <- grand_wide %>% mutate(
  goback1998 = ifelse(is.na(grandchildren_1996) | is.na(grandchildren_1998), 0,
                      ifelse(grandchildren_1996==1 & grandchildren_1998==0, 1, 0)),
  goback2000 = ifelse(is.na(grandchildren_1998) | is.na(grandchildren_2000), 0,
                      ifelse(grandchildren_1998==1 & grandchildren_2000==0, 1, 0)),
  goback2002 = ifelse(is.na(grandchildren_2000) | is.na(grandchildren_2002), 0,
                      ifelse(grandchildren_2000==1 & grandchildren_2002==0, 1, 0)),
  goback2004 = ifelse(is.na(grandchildren_2002) | is.na(grandchildren_2004), 0,
                      ifelse(grandchildren_2002==1 & grandchildren_2004==0, 1, 0)),
  goback2006 = ifelse(is.na(grandchildren_2004) | is.na(grandchildren_2006), 0,
                      ifelse(grandchildren_2004==1 & grandchildren_2006==0, 1, 0)),
  goback2008 = ifelse(is.na(grandchildren_2006) | is.na(grandchildren_2008), 0,
                      ifelse(grandchildren_2006==1 & grandchildren_2008==0, 1, 0)),
  goback2010 = ifelse(is.na(grandchildren_2008) | is.na(grandchildren_2010), 0,
                      ifelse(grandchildren_2008==1 & grandchildren_2010==0, 1, 0)),
  goback2012 = ifelse(is.na(grandchildren_2010) | is.na(grandchildren_2012), 0,
                      ifelse(grandchildren_2010==1 & grandchildren_2012==0, 1, 0)),
  goback2014 = ifelse(is.na(grandchildren_2012) | is.na(grandchildren_2014), 0,
                      ifelse(grandchildren_2012==1 & grandchildren_2014==0, 1, 0)),
  goback2016 = ifelse(is.na(grandchildren_2014) | is.na(grandchildren_2016), 0,
                      ifelse(grandchildren_2014==1 & grandchildren_2016==0, 1, 0)))

# third step: count both per person
grand_wide <- grand_wide %>% 
  mutate(
    sum_transit = rowSums(select(., starts_with("transit"))),
    sum_goback  = rowSums(select(., starts_with("goback"))))

# identify eligible grandparents (to-be): respondents with exactly 1 transition to 
# grandparenthood and no subsequent transitions back
grand_wide %>% filter(sum_transit==1) %>% group_by(sum_goback) %>% summarise(n()) # N=3091

# identify eligible non-grandparents: respondents starting out with no grandchildren 
# with 0 transitions throughout the observation period
grand_wide %>% 
  filter(grandchildren_1996 %in% c(0, NA) & 
         grandchildren_1998 %in% c(0, NA) & grandchildren_2000 %in% c(0, NA) & 
         grandchildren_2002 %in% c(0, NA) & grandchildren_2004 %in% c(0, NA) & 
         grandchildren_2006 %in% c(0, NA) & grandchildren_2008 %in% c(0, NA) & 
         grandchildren_2010 %in% c(0, NA) & grandchildren_2012 %in% c(0, NA) &
         grandchildren_2014 %in% c(0, NA) & grandchildren_2016 %in% c(0, NA)) %>% 
  group_by(sum_transit) %>% summarise(n()) # N=8387

#code variable to denote eligiblity
grand_wide <- grand_wide %>% 
  mutate(
    grandparent = ifelse(sum_transit==1 & sum_goback==0, 1, ifelse(
      (grandchildren_1996 %in% c(0, NA) & 
       grandchildren_1998 %in% c(0, NA) & grandchildren_2000 %in% c(0, NA) & 
       grandchildren_2002 %in% c(0, NA) & grandchildren_2004 %in% c(0, NA) & 
       grandchildren_2006 %in% c(0, NA) & grandchildren_2008 %in% c(0, NA) & 
       grandchildren_2010 %in% c(0, NA) & grandchildren_2012 %in% c(0, NA) &
       grandchildren_2014 %in% c(0, NA) & grandchildren_2016 %in% c(0, NA)),
      0, NA))
  )
table(grand_wide$grandparent)

#problem: we still have a few respondents with missing data who go back to 0 after a 1, 
#4 cases: 118444010, 118444011, 523882020, 920378010
grand_wide %>% filter(grandparent==1 & grandchildren_2016==0) %>% 
  select(HHIDPN, starts_with("grandchildren")) %>% print(width=Inf)


#recode for 1-wave-gap
grand_wide <- grand_wide %>% mutate(
  goback2000 = replace(goback2000, grandchildren_1996==1 & is.na(grandchildren_1998) &
                         grandchildren_2000==0, 1),
  goback2002 = replace(goback2002, grandchildren_1998==1 & is.na(grandchildren_2000) &
                         grandchildren_2002==0, 1),
  goback2004 = replace(goback2004, grandchildren_2000==1 & is.na(grandchildren_2002) &
                         grandchildren_2004==0, 1),
  goback2006 = replace(goback2006, grandchildren_2002==1 & is.na(grandchildren_2004) &
                         grandchildren_2006==0, 1),
  goback2008 = replace(goback2008, grandchildren_2004==1 & is.na(grandchildren_2006) &
                         grandchildren_2008==0, 1),
  goback2010 = replace(goback2010, grandchildren_2006==1 & is.na(grandchildren_2008) &
                         grandchildren_2010==0, 1),
  goback2012 = replace(goback2012, grandchildren_2008==1 & is.na(grandchildren_2010) &
                         grandchildren_2012==0, 1),
  goback2014 = replace(goback2014, grandchildren_2010==1 & is.na(grandchildren_2012) &
                         grandchildren_2014==0, 1),
  goback2016 = replace(goback2016, grandchildren_2012==1 & is.na(grandchildren_2014) &
                         grandchildren_2016==0, 1))

#recode for 2-wave-gap
grand_wide <- grand_wide %>% mutate(
  goback2002 = replace(goback2002, grandchildren_1996==1 & is.na(grandchildren_1998) &
                         is.na(grandchildren_2000) & grandchildren_2002==0, 1),
  goback2004 = replace(goback2004, grandchildren_1998==1 & is.na(grandchildren_2000) &
                         is.na(grandchildren_2002) & grandchildren_2004==0, 1),
  goback2006 = replace(goback2006, grandchildren_2000==1 & is.na(grandchildren_2002) &
                         is.na(grandchildren_2004) & grandchildren_2006==0, 1),
  goback2008 = replace(goback2008, grandchildren_2002==1 & is.na(grandchildren_2004) &
                         is.na(grandchildren_2006) & grandchildren_2008==0, 1),
  goback2010 = replace(goback2010, grandchildren_2004==1 & is.na(grandchildren_2006) &
                         is.na(grandchildren_2008) & grandchildren_2010==0, 1),
  goback2012 = replace(goback2012, grandchildren_2006==1 & is.na(grandchildren_2008) &
                         is.na(grandchildren_2010) & grandchildren_2012==0, 1),
  goback2014 = replace(goback2014, grandchildren_2008==1 & is.na(grandchildren_2010) &
                         is.na(grandchildren_2012) & grandchildren_2014==0, 1),
  goback2016 = replace(goback2016, grandchildren_2010==1 & is.na(grandchildren_2012) &
                         is.na(grandchildren_2014) & grandchildren_2016==0, 1))

#recode for 3-wave-gap
grand_wide <- grand_wide %>% mutate(
  goback2004 = replace(goback2004, grandchildren_1996==1 & is.na(grandchildren_1998) & is.na(grandchildren_2000) &
                         is.na(grandchildren_2002) & grandchildren_2004==0, 1),
  goback2006 = replace(goback2006, grandchildren_1998==1 & is.na(grandchildren_2000) & is.na(grandchildren_2002) &
                         is.na(grandchildren_2004) & grandchildren_2006==0, 1),
  goback2008 = replace(goback2008, grandchildren_2000==1 & is.na(grandchildren_2002) & is.na(grandchildren_2004) &
                         is.na(grandchildren_2006) & grandchildren_2008==0, 1),
  goback2010 = replace(goback2010, grandchildren_2002==1 & is.na(grandchildren_2004) & is.na(grandchildren_2006) &
                         is.na(grandchildren_2008) & grandchildren_2010==0, 1),
  goback2012 = replace(goback2012, grandchildren_2004==1 & is.na(grandchildren_2006) & is.na(grandchildren_2008) &
                         is.na(grandchildren_2010) & grandchildren_2012==0, 1),
  goback2014 = replace(goback2014, grandchildren_2006==1 & is.na(grandchildren_2008) & is.na(grandchildren_2010) &
                         is.na(grandchildren_2012) & grandchildren_2014==0, 1),
  goback2016 = replace(goback2016, grandchildren_2008==1 & is.na(grandchildren_2010) & is.na(grandchildren_2012) &
                         is.na(grandchildren_2014) & grandchildren_2016==0, 1))
# 2 cases with 3-wave-gap: 14427010 47148011

#recode for 4-wave-gap
grand_wide <- grand_wide %>% mutate(
  goback2006 = replace(goback2006, grandchildren_1996==1 & is.na(grandchildren_1998) & is.na(grandchildren_2000) & is.na(grandchildren_2002) &
                         is.na(grandchildren_2004) & grandchildren_2006==0, 1),
  goback2008 = replace(goback2008, grandchildren_1998==1 & is.na(grandchildren_2000) & is.na(grandchildren_2002) & is.na(grandchildren_2004) &
                         is.na(grandchildren_2006) & grandchildren_2008==0, 1),
  goback2010 = replace(goback2010, grandchildren_2000==1 & is.na(grandchildren_2002) & is.na(grandchildren_2004) & is.na(grandchildren_2006) &
                         is.na(grandchildren_2008) & grandchildren_2010==0, 1),
  goback2012 = replace(goback2012, grandchildren_2002==1 & is.na(grandchildren_2004) & is.na(grandchildren_2006) & is.na(grandchildren_2008) &
                         is.na(grandchildren_2010) & grandchildren_2012==0, 1),
  goback2014 = replace(goback2014, grandchildren_2004==1 & is.na(grandchildren_2006) & is.na(grandchildren_2008) & is.na(grandchildren_2010) &
                         is.na(grandchildren_2012) & grandchildren_2014==0, 1),
  goback2016 = replace(goback2016, grandchildren_2006==1 & is.na(grandchildren_2008) & is.na(grandchildren_2010) & is.na(grandchildren_2012) &
                         is.na(grandchildren_2014) & grandchildren_2016==0, 1))
# 2 cases with 4-wave-gap: 16959010 35950030

#recode for 5-wave-gap
grand_wide <- grand_wide %>% mutate(
  goback2008 = replace(goback2008, grandchildren_1996==1 & is.na(grandchildren_1998) & is.na(grandchildren_2000) & is.na(grandchildren_2002) & is.na(grandchildren_2004) &
                         is.na(grandchildren_2006) & grandchildren_2008==0, 1),
  goback2010 = replace(goback2010, grandchildren_1998==1 & is.na(grandchildren_2000) & is.na(grandchildren_2002) & is.na(grandchildren_2004) & is.na(grandchildren_2006) &
                         is.na(grandchildren_2008) & grandchildren_2010==0, 1),
  goback2012 = replace(goback2012, grandchildren_2000==1 & is.na(grandchildren_2002) & is.na(grandchildren_2004) & is.na(grandchildren_2006) & is.na(grandchildren_2008) &
                         is.na(grandchildren_2010) & grandchildren_2012==0, 1),
  goback2014 = replace(goback2014, grandchildren_2002==1 & is.na(grandchildren_2004) & is.na(grandchildren_2006) & is.na(grandchildren_2008) & is.na(grandchildren_2010) &
                         is.na(grandchildren_2012) & grandchildren_2014==0, 1),
  goback2016 = replace(goback2016, grandchildren_2004==1 & is.na(grandchildren_2006) & is.na(grandchildren_2008) & is.na(grandchildren_2010) & is.na(grandchildren_2012) &
                         is.na(grandchildren_2014) & grandchildren_2016==0, 1))
# 1 case with 5-wave-gap: 65978040

#recode for 6-wave-gap
grand_wide <- grand_wide %>% mutate(
  goback2010 = replace(goback2010, grandchildren_1996==1 & is.na(grandchildren_1998) & is.na(grandchildren_2000) & is.na(grandchildren_2002) & is.na(grandchildren_2004) & is.na(grandchildren_2006) &
                         is.na(grandchildren_2008) & grandchildren_2010==0, 1),
  goback2012 = replace(goback2012, grandchildren_1998==1 & is.na(grandchildren_2000) & is.na(grandchildren_2002) & is.na(grandchildren_2004) & is.na(grandchildren_2006) & is.na(grandchildren_2008) &
                         is.na(grandchildren_2010) & grandchildren_2012==0, 1),
  goback2014 = replace(goback2014, grandchildren_2000==1 & is.na(grandchildren_2002) & is.na(grandchildren_2004) & is.na(grandchildren_2006) & is.na(grandchildren_2008) & is.na(grandchildren_2010) &
                         is.na(grandchildren_2012) & grandchildren_2014==0, 1),
  goback2016 = replace(goback2016, grandchildren_2002==1 & is.na(grandchildren_2004) & is.na(grandchildren_2006) & is.na(grandchildren_2008) & is.na(grandchildren_2010) & is.na(grandchildren_2012) &
                         is.na(grandchildren_2014) & grandchildren_2016==0, 1))
# 0 cases with 6-wave-gap

#recode for 7-wave-gap
grand_wide <- grand_wide %>% mutate(
  goback2012 = replace(goback2012, grandchildren_1996==1 & is.na(grandchildren_1998) & is.na(grandchildren_2000) & is.na(grandchildren_2002) & is.na(grandchildren_2004) & is.na(grandchildren_2006) & is.na(grandchildren_2008) &
                         is.na(grandchildren_2010) & grandchildren_2012==0, 1),
  goback2014 = replace(goback2014, grandchildren_1998==1 & is.na(grandchildren_2000) & is.na(grandchildren_2002) & is.na(grandchildren_2004) & is.na(grandchildren_2006) & is.na(grandchildren_2008) & is.na(grandchildren_2010) &
                         is.na(grandchildren_2012) & grandchildren_2014==0, 1),
  goback2016 = replace(goback2016, grandchildren_2000==1 & is.na(grandchildren_2002) & is.na(grandchildren_2004) & is.na(grandchildren_2006) & is.na(grandchildren_2008) & is.na(grandchildren_2010) & is.na(grandchildren_2012) &
                         is.na(grandchildren_2014) & grandchildren_2016==0, 1))
# 0 cases with 7-wave-gap

#update count
grand_wide <- grand_wide %>% 
  mutate(
    sum_goback  = rowSums(select(., starts_with("goback"))))

grand_wide %>% filter(grandparent==1 & sum_goback>=1) %>% 
  select(HHIDPN, starts_with("grandchildren")) %>% print(width=Inf)
# 52884020 118444010 118444011 500330020 523882020 920378010 -> 6 cases where NAs obscured 
# inconsistent longitudinal data 
# 109 cases in total

grand_wide <- grand_wide %>% 
  mutate(grandparent = replace(grandparent, grandparent==1 & sum_goback>=1, NA))

table(grand_wide$grandparent) # check count: N=2982

#keep analysis sample 2006-2016
grand_wide <- grand_wide %>% select(-num_range("grandchildren_", 1996:2004), 
                                    -num_range("transit", 1996:2004),
                                    -num_range("goback", 1996:2004))
grand_wide <- grand_wide %>% filter((transit2006==1 | transit2008==1 | transit2010==1 | 
                                     transit2012==1 | transit2014==1 | transit2016==1) |
                                     grandparent==0)
  
  
# drop non-grandparents where more than half of years are missing
# -> these will not be very useful for matching
grand_wide <- grand_wide %>% 
  mutate(sum_missings = rowSums(is.na(select(., starts_with("grandchildren")))))
grand_wide <- grand_wide %>% filter((grandparent==0 & sum_missings<=3) | grandparent==1) %>% 
  select(-sum_missings)
table(grand_wide$grandparent)

grand_wide_identify <- grand_wide %>% filter(grandparent %in% c(0,1)) %>% 
  select(HHIDPN, grandparent) # for later merge with long data

# reshape again & merge with full dataset
grand_transits <- grand_wide %>% 
  filter(grandparent %in% c(0,1)) %>% 
  select(HHIDPN, starts_with("transit")) %>% 
  pivot_longer(cols = starts_with("transit"), 
               names_to = "year",
               names_prefix = "transit",
               values_to = "transit",
               values_drop_na = T) %>% 
  mutate(year = as.numeric(year))

# restrict hrs-long data to 2006-2016 where personality data is available 
hrslong <- hrslong %>% filter(year>2004)

# merge transition to grandparenthood info
hrslong <- left_join(hrslong, grand_transits)
hrslong %>% filter(transit==1) %>% group_by(year) %>% summarise(n())

hrslong <- left_join(hrslong, grand_wide_identify) # merge group-identifiying variable
hrslong %>% filter(grandparent==0) %>% group_by(year) %>% summarise(n()) %>% print(n=Inf)
hrslong %>% filter(grandparent==1) %>% group_by(year) %>% summarise(n()) %>% print(n=Inf)
table(hrslong$grandparent)

hrslong %>% group_by(grandparent) %>% summarise(ndist = n_distinct(HHIDPN))
hrslong %>% filter(grandparent==1) %>% group_by(totalgrandkids) %>% summarise(n()) %>% print(n=Inf)

# a few respondents report >5 total grandkids in the survey where they report having 
# grandkids for the first time
# Within 2 years between interviews, how many new grandkids are realistic?
# cutoff -> 10 
hrslong %>% filter(grandparent==1 & transit==1) %>% group_by(totalgrandkids) %>% summarise(n()) %>% print(n=Inf)

hrslong <- hrslong %>% mutate(
  toomanygrandkids = ifelse(grandparent==1 & transit==1 & totalgrandkids>10, 1, 0))
hrslong <- hrslong %>% 
  group_by(HHIDPN) %>% 
  mutate(helpvar = max(toomanygrandkids)) %>% filter(helpvar==0 | is.na(helpvar)) %>% 
  ungroup() %>% 
  select(-toomanygrandkids, -helpvar)
table(hrslong$grandparent)

#### code time in relation to transition ####

# Put all time-person observations into a framework that centers the neighboring observations 
# around the transition

hrslong <- hrslong %>% 
  mutate(transityr = ifelse(transit==1, year, NA)) %>% 
  group_by(HHIDPN) %>% arrange(HHIDPN, year) %>% 
  mutate(
    participation = row_number(),
    participation_max = n())

hrslong_merge <- hrslong %>% select(HHIDPN, transit, transityr) %>% 
  filter(transit==1) %>% 
  rename(transitever=transit,
         transityear=transityr)

hrslong <- left_join(hrslong, hrslong_merge) %>% 
  select(-(transityr))

hrslong <- hrslong %>% 
  mutate(time = year - transityear) #coding time variable
hrslong %>% group_by(time) %>% filter(grandparent==1) %>% summarise(n = n()) %>% print(n=Inf)

options(pillar.sigfig = 5)
hrslong %>% group_by(grandparent) %>% filter(transit==1 | grandparent==0) %>% 
  summarise(meanbirthyr = mean((birthyr), na.rm=T), n = n()) # N = 1786 (grandparents)

# save .rda 
save(hrslong, file = "data/processed/HRS/hrslong_cleaned.rda")

# load .rda
load(file = "data/processed/HRS/hrslong_cleaned.rda")

# filter only valid cases and create count of valid observations
# valid = non-missing in at least one of the outcome variables

hrslongvalid <- hrslong  %>% 
  filter(!is.na(extra) | !is.na(con) | !is.na(open) | !is.na(agree) | !is.na(neur) | !is.na(swls))

hrslongvalid %>% group_by(time) %>% filter(grandparent==1) %>% summarise(n = n()) %>% print(n=Inf)

# code indicator for last (valid) assessment before the transition to GP (survey year varies)
hrs_last_time_point <- hrslongvalid %>% filter(time<0) %>% group_by(HHIDPN) %>% 
  slice(which.max(time)) %>% select(HHIDPN, time) %>% rename(last = time)
table(hrs_last_time_point$last)
# N = 967 grandparents (with a valid pre-transition assessment)
# -10  -8  -6  -4  -2 
#   2  11  18 367 569    --> we will later only use -4 and -2 as matching time points 
hrslongvalid <- left_join(hrslongvalid, hrs_last_time_point) %>% arrange(HHIDPN, year)

# N = 1577 grandparents (in total)
hrslongvalid %>% group_by(grandparent) %>% summarise(ndist = n_distinct(HHIDPN))
# N = 340 grandparents (with a valid pre-transition assessment AND a t=0 post-treatment assessment)
hrslongvalid %>% filter(transit==1 & !is.na(last)) %>% summarise(ndist = n_distinct(HHIDPN))
# however, there are additional grandparents with valid pre- and post-transition assessments 
# (just not at t=0, but later)
hrslongvalid <- hrslongvalid %>% arrange(HHIDPN, year) %>% group_by(HHIDPN) %>% 
  mutate(helpcount = row_number()) %>% ungroup()

hrslongvalid %>% select(HHIDPN, year, time, last, helpcount, grandparent, transit, transityear) %>% 
  filter(grandparent==1) %>% print(n=50)

hrslongvalid <- hrslongvalid %>% 
  mutate(lastcount = ifelse(grandparent==1 & time==last, helpcount, NA),
         valid = ifelse(grandparent==1 & time==last, -1, NA)) %>%  # last assessment before transition gets -1  
  group_by(HHIDPN) %>% mutate(lastcount = max(lastcount, na.rm = T)) %>% ungroup()

hrslongvalid <- hrslongvalid %>% 
  mutate(valid = helpcount - lastcount - 1) %>% select(-helpcount, -lastcount) %>% 
  mutate(valid = replace(valid, valid==Inf, NA))
# Variable 'valid' now counts the valid assessments before the transition (negative values), 
# and afterwards with 0 being the first valid assessment after the transition to GP
# This new count is irrespective of the actual timing of the survey years.
hrslongvalid %>% select(HHIDPN, year, time, valid, last, grandparent) %>% 
  filter(grandparent==1) %>% print(n=50)
table(hrslongvalid$valid, hrslongvalid$last)
(N_GP_hrs <- pull(hrslongvalid %>% filter(valid==0 & last %in% c(-4, -2)) %>% 
                    summarise(n = n()))) # for papaja (?)

# restrict sample to those with at least 1 valid pre- and 1 valid post-treatment assessment
# AND also with -4 or -2 as their last time point before the transition to GP
hrslongvalid <- hrslongvalid %>% group_by(HHIDPN) %>% 
  mutate(droplater = max(valid==0 & last %in% c(-4, -2, NA), na.rm = T)==0) %>% ungroup() 
# after imputations: drop 407 obs. (246 resp.) -> no post-treatment assessment

table(hrslongvalid$grandparent, hrslongvalid$time)
table(hrslongvalid$grandparent, hrslongvalid$valid)

# save .rda 
save(hrslongvalid, file = "data/processed/HRS/hrslong_valid.rda")
#rm(list = ls())

# load .rda
load(file = "data/processed/HRS/hrslong_valid.rda")


#### multiple imputations for variables needed for PSM covariates ####

hrsimplong <- hrslongvalid %>% select(HHIDPN, year, 
                   birthyr, gender, race, schlyrs, 
                   interviewyear, hhmembers, 
                   marital, marriagesnum, siblings, 
                   children10m, totalnonresidentkids, totalresidentkids,
                   conde, doctor, hospital, psyche, bmi, cancer, 
                   cesd, diabetes, stroke, heart, selfratedhealth, mobilitydiff, 
                   hiemployer, higovt, hispousal, hiother, 
                   laborforce, hhincome, hhwealth, jobhours, 
                   bornusa, ownrent, safetyneighborhood, 
                   foodstamps, difficultybills,
                   secondhome, typehome, farmranch, 
                   attendreligion, nroomsself, nroomsint, 
                   paidwork, selfemployed, jobstatus, 
                   starts_with("KAGENDERBG"), starts_with("KABYEARBG"), 
                   starts_with("KAEDUC"), starts_with("has_ch"), 
                   swls, agree, con, extra, neur, open)
# will not impute the 1st child's birthyear, because this variable will be used
# to filter one of the control groups (and will not be used as a PSM covariate
# in the other second control group)
hrsimplong <- hrsimplong %>% select(-KABYEARBG_1)

summary(hrsimplong)

# recoding of HRS-specific missing codes
hrsimplong <- hrsimplong %>% 
  mutate_at(c("children10m", "ownrent", "safetyneighborhood", "foodstamps", 
              "secondhome", "farmranch", "attendreligion", "nroomsint", 
              "paidwork", "selfemployed", "jobstatus"), 
            funs(ifelse(. %in% c(8, 9), NA, .))) %>% 
  mutate_at(c("typehome", "nroomsself", "jobstatus"), #jobstatus has both types
            funs(ifelse(. %in% c(98, 99), NA, .)))

summary(hrsimplong)

# some covars have very large proportions of missing values (per design -> filtered questions)
# difficult to deal with in imputations --> recode 
# children10m: only respondents with kids are asked this q
hrsimplong <- hrsimplong %>% mutate(
  children10m = replace(children10m, is.na(children10m) & totalnonresidentkids==0 & totalresidentkids==0, 5))
# jobhours: only respondents currently working are asked this q
hrsimplong <- hrsimplong %>% mutate(
  jobhours = replace(jobhours, is.na(jobhours) & laborforce %in% c(3, 5, 6, 7), 0))
# farmranch: I have information on other types of houses in other vars... don't need imputation here
hrsimplong <- hrsimplong %>% mutate(
  farmranch = replace(farmranch, is.na(farmranch), 0))
# bmi: very few observations seem unrealistic
hrsimplong <- hrsimplong %>% mutate(
  bmi = ifelse(bmi>70, 70, bmi)
)
# jobhours: very few observations seem unrealistic
hrsimplong <- hrsimplong %>% mutate(
  jobhours = ifelse(jobhours>100, 100, jobhours)
)
# number of rooms: 
# nroomsint (1 = 3 ROOMS OR LESS; 2 = 4-5 ROOMS; 3 = 6-7 ROOMS; 4 = 8 ROOMS OR MORE)
# nroomsself: self-report, range from 1 to 25
hrsimplong <- hrsimplong %>% mutate(rooms = nroomsint) %>% 
  mutate(rooms = replace(rooms, is.na(rooms) & nroomsself %in% c(1:3), 1)) %>% 
  mutate(rooms = replace(rooms, is.na(rooms) & nroomsself %in% c(4:5), 2)) %>% 
  mutate(rooms = replace(rooms, is.na(rooms) & nroomsself %in% c(6:7), 3)) %>% 
  mutate(rooms = replace(rooms, is.na(rooms) & nroomsself %in% c(8:100), 4)) %>% 
  select(-nroomsself, -nroomsint)

# selfemployed: work for someone else/slf-employed
hrsimplong <- hrsimplong %>% mutate(
  selfemployed = replace(selfemployed, is.na(selfemployed) & paidwork==5, 0)
)

# variables related to children: if a respondent has no children, we recode to 0
hrsimplong <- hrsimplong %>% 
  mutate_at(vars(ends_with(c("_1", "_2", "_3"))), 
                 funs(ifelse(is.na(.) & totalnonresidentkids==0 & 
                             totalresidentkids==0, 0, .)))

# Who is childless? -> disagreement in different data sources
# RAND HRS Family Data 2014 vs. RAND HRS Fat Files (*A099 & *A100)
# 3750 missing observations from merge with RAND HRS Family Data 2014
# 3507 observations without children according to 'totalnonresidentkids' (*A100) &
# 'totalresidentkids' (*A099)
hrsimplong %>% mutate(
  childless = ifelse(totalnonresidentkids==0 & totalresidentkids==0, 1, 0)) %>%
  group_by(has_ch1, childless) %>% summarise(n = n(), dis = n_distinct(HHIDPN))
# part of the problem might be that 2016 is not covered in HRS Family Data 
hrsimplong %>% filter(year!=2016) %>% mutate(
  childless = ifelse(totalnonresidentkids==0 & totalresidentkids==0, 1, 0)) %>%
  group_by(has_ch1, childless) %>% summarise(n = n(), dis = n_distinct(HHIDPN))

# this variable will later be used to filter the two matched control groups:
# (playing it safe: only affirmative if childless according to both sources)
hrsimplong <- hrsimplong %>% 
  mutate(nokids = ifelse(totalnonresidentkids==0 & totalresidentkids==0 & 
                  is.na(has_ch1), 1, 0)) %>% select(-has_ch1)
# denote second / third child
hrsimplong <- hrsimplong %>% 
  mutate(secondkid = ifelse(has_ch2 %in% c(0, NA), 0, 1), # need a version without missings
         thirdkid = ifelse(has_ch3 %in% c(0, NA), 0, 1)) %>% select(-has_ch2, -has_ch3)


# seperate imputations for each wave
hrsimp2006 <- hrsimplong %>% filter(year==2006)
summary(hrsimp2006)

hrsimp2008 <- hrsimplong %>% filter(year==2008)
summary(hrsimp2008)

hrsimp2010 <- hrsimplong %>% filter(year==2010)
summary(hrsimp2010)

hrsimp2012 <- hrsimplong %>% filter(year==2012)
summary(hrsimp2012)

hrsimp2014 <- hrsimplong %>% filter(year==2014)
summary(hrsimp2014)

# 2016 will not be used for matching because of missing RAND HRS Family Data 
# and because we match at 'valid'-1, anyways (2016 will be used in longitudinal 
# analyses, though)
# --> Note that this will result in the post-imputation dataset having fewer 
#     observations than 'hrsimplong' (however, the N of grandparents in the 
#     matching datasets will be the same!)
#hrsimp2016 <- hrsimplong %>% filter(year==2016) 
#summary(hrsimp2016)

# perform multiple imputations 
# (required for pscore matching, although imputed data will not be used in later analyses)
library(mice)
library(lattice)

imp <- 5 # Number of multiple imputations

imp2006 <- mice(hrsimp2006, method = "cart", m = imp, maxit = 5, seed = 3000)
summary(imp2006)
#densityplot(imp2006)

imp2008 <- mice(hrsimp2008, method = "cart", m = imp, maxit = 5, seed = 3000)
summary(imp2008)
#densityplot(imp2008)

imp2010 <- mice(hrsimp2010, method = "cart", m = imp, maxit = 5, seed = 3000)
summary(imp2010)
#densityplot(imp2010)

imp2012 <- mice(hrsimp2012, method = "cart", m = imp, maxit = 5, seed = 3000)
summary(imp2012)
#densityplot(imp2012)

imp2014 <- mice(hrsimp2014, method = "cart", m = imp, maxit = 5, seed = 3000)
summary(imp2014)
#densityplot(imp2014)

#imp2016 <- mice(hrsimp2016, method = "cart", m = imp, maxit = 5, seed = 3000)
#summary(imp2016)
#densityplot(imp2016)

# Recent work shows ensemble methods like boosted CART and random
# forests works very well (Setoguchi et al. 2008; Lee et al., 2009).


help1 <- NULL;
help2 <- NULL;
for (yr in c(2006, 2008, 2010, 2012, 2014)){ #2016
  for(i in 1:imp){
    help1 <- paste("imp", yr, "number", i, sep="_")
    help2 <- paste("imp", yr, sep="")
  eval(call("<-", as.name(help1), complete(data = get(help2), i)))
  }
}

summary(imp_2006_number_1)
summary(imp_2006_number_2)
psych::describe(imp_2006_number_1)
psych::describe(imp_2006_number_2)
#etc. 
detach("package:mice", unload=TRUE)

#build 5 long datasets
x <- NULL;
for (i in 1:imp){
  x <- paste("hrsimp_matching", i, sep="_")
  d06 <- paste("imp_2006_number", i, sep="_")
  d08 <- paste("imp_2008_number", i, sep="_")
  d10 <- paste("imp_2010_number", i, sep="_")
  d12 <- paste("imp_2012_number", i, sep="_")
  d14 <- paste("imp_2014_number", i, sep="_")
  #d16 <- paste("imp_2016_number", i, sep="_")
  eval(call("<-", as.name(x), bind_rows(get(d06), get(d08), get(d10), 
                                        get(d12), get(d14)))) #get(d16)
}

# merge variables that identify (or describe) the treatment groups and the 
# timing of treatment (including year of birth of 1st child - we don't want 
# imputed values for this variable)
merge_gp <- hrslongvalid %>% 
  select(HHIDPN, year, grandparent, time, valid, participation, droplater, KABYEARBG_1)
hrsimp_matching_1 <- left_join(hrsimp_matching_1, merge_gp)
hrsimp_matching_2 <- left_join(hrsimp_matching_2, merge_gp)
hrsimp_matching_3 <- left_join(hrsimp_matching_3, merge_gp)
hrsimp_matching_4 <- left_join(hrsimp_matching_4, merge_gp)
hrsimp_matching_5 <- left_join(hrsimp_matching_5, merge_gp)

# save .rda 
save(hrsimp_matching_1, file = "data/processed/HRS/hrsimp_matching_1.rda")
save(hrsimp_matching_2, file = "data/processed/HRS/hrsimp_matching_2.rda")
save(hrsimp_matching_3, file = "data/processed/HRS/hrsimp_matching_3.rda")
save(hrsimp_matching_4, file = "data/processed/HRS/hrsimp_matching_4.rda")
save(hrsimp_matching_5, file = "data/processed/HRS/hrsimp_matching_5.rda")
#rm(list = ls())

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
              kid3educ = ifelse(thirdkid==1, KAEDUC_3, 0) 
  )) %>%
  map(~select(., -c(birthyr, race, laborforce, marital, children10m, farmranch, ownrent,
              difficultybills, bornusa, safetyneighborhood, secondhome,
              hhincome, hhwealth, selfratedhealth, attendreligion, 
              typehome, rooms, jobstatus, gender,
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

# We will perform 2 x 2 matchings: 
# 1) Grandparents-to-be matched with parents (but not grandparents) with at 
#    least one child in reproductive age (>=15) via 'GroupMatch' R package
#    aka 'rollingMatch' package
# 2) Grandparents-to-be matched with parents (but not grandparents) with at 
#    least one child in reproductive age (>=15) via matching loop from K&R (2020)
# 3) Grandparents-to-be matched with nonparents via 'GroupMatch' R package
#    aka 'rollingMatch' package
# 4) Grandparents-to-be matched with nonparents via matching loop from K&R (2020)

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
         (grandparent==0 & nokids==0 & kid1age>=15)) %>% 
  group_by(grandparent, nokids) %>% summarise(n = n(), N = n_distinct(HHIDPN))
hrsimp_matching_1 %>% 
  filter((grandparent==1 & nokids==0 & valid==-1 & droplater==F) | 
         (grandparent==0 & nokids==0 & kid1age>=15)) %>% 
  group_by(grandparent, nokids) %>% summarise(n = n(), N = n_distinct(HHIDPN))

# Compared to the (old) method that does not filter by 'nokids==0', we 
# drop 7 grandparents (these are not contained in RAND HRS Family data and
# have 0 kids according to 'totalresidentkids' & 'totalnonresidentkids'):
hrsimp_matching_1 %>% filter(grandparent==1 & nokids==1 & valid==-1 & droplater==F)

# 2)
hrsimp_matching_1 %>% 
  group_by(grandparent, nokids) %>% summarise(n = n(), N = n_distinct(HHIDPN))
hrsimp_matching_1 %>% 
  filter((grandparent==1 & nokids==0 & valid==-1 & droplater==F) | # GP group is the same!
         (grandparent==0 & nokids==1)) %>% 
  group_by(grandparent, nokids) %>% summarise(n = n(), N = n_distinct(HHIDPN))
# Control group is very small here - might not work for matching!
# Alternatively, we could rely on just the 'nokids' information and infer
# that these respondents do not have grandkids.
hrsimp_matching_1 %>% 
  filter((grandparent==1 & nokids==0 & valid==-1 & droplater==F) | # GP group is the same!
         (grandparent %in% c(0, NA) & nokids==1)) %>% 
  group_by(grandparent, nokids) %>% summarise(n = n(), N = n_distinct(HHIDPN))
# This way, we would gain 248 observations (N=216).

# subsetting imputed datasets:

# parents
hrsimp_parents_ps_1 <- hrsimp_matching_1 %>% 
  filter((grandparent==1 & nokids==0 & valid==-1 & droplater==F & !is.na(kid1age)) | 
         (grandparent==0 & nokids==0 & kid1age>=15)) %>% 
  select(-c(droplater, time, valid, nokids))
# two GP observations (500298010, 500298020) have NA for kid1age
hrslongvalid %>% filter(HHIDPN %in% c(500298010, 500298020)) %>% print(width=Inf)
# these report having kids for the first time in 2014 (but have personality assessments at 2008, 2012, 2016)

# sample size before matching:
hrsimp_parents_ps_1 %>% 
  group_by(grandparent) %>% summarise(n = n(), N = n_distinct(HHIDPN))

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


#same for the other imputations...
# 2  
hrsimp_parents_ps_2 <- hrsimp_matching_2 %>% 
  filter((grandparent==0 & nokids==0 & kid1age>=15) | 
         (grandparent==1 & nokids==0 & valid==-1 & droplater==F & !is.na(kid1age))) %>% 
  select(-c(droplater, time, valid, nokids))

hrsimp_nonparents_ps_2 <- hrsimp_matching_2 %>% 
  filter((grandparent %in% c(0, NA) & nokids==1) | 
         (grandparent==1 & nokids==0 & valid==-1 & droplater==F & !is.na(kid1age))) %>% 
  select(-c(droplater, time, valid, nokids, childrenclose, contains("kid"))) %>% 
  mutate(grandparent = replace_na(grandparent, 0)) 

# 3
hrsimp_parents_ps_3 <- hrsimp_matching_3 %>% 
  filter((grandparent==0 & nokids==0 & kid1age>=15) | 
         (grandparent==1 & nokids==0 & valid==-1 & droplater==F & !is.na(kid1age))) %>% 
  select(-c(droplater, time, valid, nokids))

hrsimp_nonparents_ps_3 <- hrsimp_matching_3 %>% 
  filter((grandparent %in% c(0, NA) & nokids==1) | 
         (grandparent==1 & nokids==0 & valid==-1 & droplater==F & !is.na(kid1age))) %>% 
  select(-c(droplater, time, valid, nokids, childrenclose, contains("kid"))) %>% 
  mutate(grandparent = replace_na(grandparent, 0)) 

# 4
hrsimp_parents_ps_4 <- hrsimp_matching_4 %>% 
  filter((grandparent==0 & nokids==0 & kid1age>=15) | 
         (grandparent==1 & nokids==0 & valid==-1 & droplater==F & !is.na(kid1age))) %>% 
  select(-c(droplater, time, valid, nokids))

hrsimp_nonparents_ps_4 <- hrsimp_matching_4 %>% 
  filter((grandparent %in% c(0, NA) & nokids==1) | 
         (grandparent==1 & nokids==0 & valid==-1 & droplater==F & !is.na(kid1age))) %>% 
  select(-c(droplater, time, valid, nokids, childrenclose, contains("kid"))) %>% 
  mutate(grandparent = replace_na(grandparent, 0)) 

# 5
hrsimp_parents_ps_5 <- hrsimp_matching_5 %>% 
  filter((grandparent==0 & nokids==0 & kid1age>=15) | 
         (grandparent==1 & nokids==0 & valid==-1 & droplater==F & !is.na(kid1age))) %>% 
  select(-c(droplater, time, valid, nokids))

hrsimp_nonparents_ps_5 <- hrsimp_matching_5 %>% 
  filter((grandparent %in% c(0, NA) & nokids==1) | 
         (grandparent==1 & nokids==0 & valid==-1 & droplater==F & !is.na(kid1age))) %>% 
  select(-c(droplater, time, valid, nokids, childrenclose, contains("kid"))) %>% 
  mutate(grandparent = replace_na(grandparent, 0)) 

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

# to help with memory capacity issues
rm(h96data, h98data, h00data, h02data, h04data, h06data, 
   h08data, h10data, h12data, h14data, h16data) 

#### PSM: identify possible matches -> (1) parent control group ####

# DIY matching: 2nd step -> identifying all possible PS-matches per grandparent  
#                           and matching exactly on gender
hrsimp_matching_parents <- hrsimp_matching_parents %>% 
  select(HHIDPN, year, female, grandparent, pscore) 
# sort by random variable in case there are ordering effects
set.seed(123)
rows <- sample(nrow(hrsimp_matching_parents))
hrsimp_matching_parents <- hrsimp_matching_parents[rows, ]
hrsimp_matching_parents <- hrsimp_matching_parents %>% mutate(new_id = row_number())

hrsimp_matching_parents_case <- hrsimp_matching_parents %>% filter(grandparent==1) %>% 
  rename(case_id = new_id,
         pid_case = HHIDPN,
         pscore_case = pscore,
         female_case = female) %>% 
  select(-grandparent, -year)
# removing 'year' here means that 'year' -> 'match_year' will refer to the controls' survey year
# at the time of matching (see below)

hrsimp_matching_parents_control <- hrsimp_matching_parents %>% filter(grandparent==0) %>% 
  rename(control_id = new_id,
         pid_control = HHIDPN,
         pscore_control = pscore,
         female_control = female,
         match_year = year) %>% 
  select(-grandparent)

# matching the two datasets along gender of controls and cases (exact matches)
# crossing = equivalent of STATA joinby command (Cartesian product)
hrsimp_joined_parents <- crossing(hrsimp_matching_parents_case, hrsimp_matching_parents_control)
# 712*3336 = 2375232
# exact matching on gender
hrsimp_joined_parents <- hrsimp_joined_parents %>% filter(female_case==female_control) %>% 
  select(-female_control) %>% 
  rename(female = female_case)

#### PSM: matching loop -> (1) parent control group ####

# DIY matching: 3rd step -> matching the cases and controls according to the similarity of 
#                           their propensity scores (1 to 1 matching; no duplicates)
hrsimp_joined_parents <- hrsimp_joined_parents %>% 
  mutate(ps_diff = abs(pscore_case - pscore_control))
#no midranks or shared ranks - if PS_diff is the same it can be random which one we pick
hrsimp_joined_parents <- hrsimp_joined_parents %>% group_by(pid_case) %>% 
  mutate(rank = rank(ps_diff, ties.method = "random"))

#this new numbering makes the following loop easier  
hrsimp_joined_parents <- hrsimp_joined_parents %>% group_by(case_id) %>%
  mutate(case_id_new = cur_group_id()) %>% 
  arrange(case_id_new, rank) %>% ungroup()

#to help with computation time -> restrict to certain ranks (similar idea to caliper)
#assuming that there will be no valid match according to PS with a higher rank, anyway!
hrsimp_joined_parents_reduced <- hrsimp_joined_parents %>% filter(rank<=400)
(num_cases <- as.vector(summary(hrsimp_joined_parents_reduced$case_id_new)[6]))

hrs_matched_parents <- NULL;
picked <- NULL;
for(num in 1:num_cases)
{
  #pick the highest rank of the (remaining) controls each case is matched with
  picked <- hrsimp_joined_parents_reduced %>% filter(case_id_new==num) %>% 
    mutate(best = min(rank)) %>% filter(rank==best) %>% select(-best)
  #collect valid matches throughout the loop
  hrs_matched_parents <- rbind(hrs_matched_parents, picked)
  #track pid of control matched in every iteration of the loop
  picked <- picked %>% mutate(tracked=1) %>% select(pid_control, tracked)
  #drop all other controls with the tracked pid in the remaining pool of available controls
  hrsimp_joined_parents_reduced <- left_join(hrsimp_joined_parents_reduced, picked)
  hrsimp_joined_parents_reduced <- hrsimp_joined_parents_reduced %>% 
    filter(is.na(tracked)) %>% select(-tracked)
}
# all 712 grandparents sucessfully matched!
summary(hrs_matched_parents$rank)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    1.00    2.00   35.78   32.00  287.00 

summary(hrs_matched_parents$ps_diff)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000003 0.0001345 0.0007313 0.0601761 0.0577291 0.6369752 

# create dataset for merge with controls
hrs_matched_mergecontrols_parents <- hrs_matched_parents %>% rename(
  HHIDPN = pid_control,
  pscore = pscore_control,
  HHIDPN_case = pid_case
) %>% select(HHIDPN, HHIDPN_case, match_year, pscore) %>% 
  arrange(HHIDPN)

# create dataset for merge with cases
hrs_matched_mergecases_parents <- hrs_matched_parents %>% rename(
  HHIDPN = pid_case,
  pscore = pscore_case,
  HHIDPN_control = pid_control
) %>% select(HHIDPN, HHIDPN_control, match_year, pscore) %>% 
  arrange(HHIDPN)

#### compile final analysis samples -> (1) parent control group ####

hrsanalysis_parents <- hrslongvalid %>% filter(!is.na(grandparent) & droplater!=T)

hrsanalysis_parents <- left_join(hrsanalysis_parents, hrs_matched_mergecases_parents, by='HHIDPN')
hrsanalysis_parents <- left_join(hrsanalysis_parents, hrs_matched_mergecontrols_parents, by='HHIDPN') 

# create dataset for merge containing information on whether a control was 
# at time==-4 or time==-2 at the time of matching
# (variable 'valid' denotes valid assessment in relation to the transition to GP for the cases)
time_for_controls_hrs <- hrslongvalid %>% 
  filter(grandparent==1 & valid==-1 & droplater==F & !is.na(KABYEARBG_1)) %>%
  select(HHIDPN, time) %>% rename(time_match = time)
table(time_for_controls_hrs$time_match)
# match via HHIDPN_case in order to transfer 'time' value from the grandparents to 
# controls (at timepoint of matching)
hrsanalysis_parents <- left_join(hrsanalysis_parents, 
                                 time_for_controls_hrs, by=c('HHIDPN_case'='HHIDPN')) 

hrsanalysis_parents <- hrsanalysis_parents %>% mutate(
  pscore = ifelse(!is.na(pscore.x), pscore.x, pscore.y),
  match_year = ifelse(!is.na(match_year.x), match_year.x, match_year.y),
  HHIDPN_match = ifelse(!is.na(HHIDPN_case), HHIDPN_case, HHIDPN_control)
) %>% 
  select(-c(pscore.x, pscore.y, match_year.x, match_year.y, HHIDPN_case, HHIDPN_control)) %>% 
  filter(!is.na(pscore)) # drop all observations from unmatched respondents

# create time variable for controls relative to the time point of matching 
hrsanalysis_parents <- hrsanalysis_parents %>% mutate(
  time = replace(time, is.na(time) & match_year==year & time_match==-4, -4),
  # variable 'match_year' relates to the controls!
  valid = replace(valid, is.na(valid) & match_year==year, -1)
)
# reminder: 'time' counts calendar years in relation to transition to GP, 'valid' only valid assessments

hrsanalysis_parents <- hrsanalysis_parents %>% 
  mutate(time = replace(time, is.na(time) & match_year==year & time_match==-2, -2))

table(hrsanalysis_parents$time, hrsanalysis_parents$grandparent)

hrsanalysis_parents <- hrsanalysis_parents %>% mutate(
  time = ifelse(is.na(time) & time_match==-4, (year - match_year) - 4, time),
  time = ifelse(is.na(time) & time_match==-2, (year - match_year) - 2, time))

hrsanalysis_parents <- hrsanalysis_parents %>% group_by(HHIDPN) %>% 
  mutate(lastcount = ifelse(grandparent==0 & valid==-1, row_number(), NA)) %>% ungroup()
hrsanalysis_parents <- hrsanalysis_parents %>% group_by(HHIDPN) %>% 
  mutate(lastcount = max(lastcount, na.rm = T)) %>% ungroup() %>% 
  mutate(lastcount = replace(lastcount, lastcount==-Inf, NA))

# finish coding 'valid' for controls
hrsanalysis_parents <- hrsanalysis_parents %>% group_by(HHIDPN) %>% 
  mutate(helpcount = row_number()) %>% ungroup()
hrsanalysis_parents <- hrsanalysis_parents %>% 
  mutate(valid = ifelse(is.na(valid) & grandparent==0, helpcount - lastcount - 1, valid)) %>% 
  select(-helpcount, -lastcount)

table(hrsanalysis_parents$valid, hrsanalysis_parents$grandparent)
table(hrsanalysis_parents$time, hrsanalysis_parents$grandparent) # cells earlier than '-6' too small

hrsanalysis_parents <- hrsanalysis_parents %>% filter(time %in% c(-6:6)) %>% 
  select(-time_match, -match_year, -droplater)

# save .rda 
save(hrsanalysis_parents, file = "data/processed/HRS/hrsanalysis_parents.rda")


#### PSM: 'rollingMatch' -> (1) parent control group ####

# GroupMatch package (the development version of the package is called 'rollingMatch')

# build/install development version via devtools commands
#library(devtools)
#build("S:/MA/mkraemer/Paper_Grandparenthood/GroupMatch development version/rollingMatch")
#build("/Users/michaelkramer/Documents/Paper_Grandparenthood/GroupMatch development version/rollingMatch")
#install("/Users/michaelkramer/Documents/Paper_Grandparenthood/GroupMatch development version/rollingMatch")

# this did not work:
#devtools::install_github("jgellar/rollingMatch", auth_token = "506524f03641945dc771f973a3d6131ab9edad26")

library(rollingMatch)

table(hrsimp_matching_parents$grandparent)
table(hrsimp_matching_parents$grandparent, hrsimp_matching_parents$year)

#we also need the 'time' value for grandparents (either -4 or -2) and the 'valid' variable
hrsimp_groupmatch_parents <- left_join(hrsimp_matching_parents, 
                                       hrsimp_matching_1, by=c("HHIDPN", "year")) %>% 
  filter(droplater==F) %>% 
  select(HHIDPN, year, female.x, grandparent.x, pscore, time, valid) %>% 
  rename(female = female.x, grandparent = grandparent.x)

table(hrsimp_groupmatch_parents$grandparent, hrsimp_groupmatch_parents$time)
table(hrsimp_groupmatch_parents$grandparent, hrsimp_groupmatch_parents$valid)

# Description
# This is an adaption of fullmatch to allow for restrictions when control observations 
# are "grouped". The motivating use case is when there are multiple observations of control 
# data for each control subject. In this case, the grouping variable is the subject. We may 
# want to place restrictions, for example that only one observation of a subject can be 
# matched, or in the case of one:many matching, a given control subject can only be matched 
# to a given treated subject once.

#matrix of propensity score distances (uses the same 'pscore' variable as the K&R matching loop)
match_on_parents_ps <- as.matrix(match_on(grandparent~pscore, data=hrsimp_groupmatch_parents))

#caliper matrix to exact-match on gender
match_on_parents_female <- as.matrix(exactMatch(grandparent ~ female, data=hrsimp_groupmatch_parents))

#caliper matrix to match within $500 of income (also possible)
#inc <-  as.matrix(caliper(match_on(treat~income), distance = "Euclidean", width = 500))

#final input to rollingMatch is the sum of these matrices
final_dist_parents <- match_on_parents_ps + match_on_parents_female
# 2375232 elements in matrix -> same as in Cartesian product in DIY matching loop

hrs_groupmatch_parents <- rollingMatch::groupmatch(x=final_dist_parents, 
                             group = hrsimp_groupmatch_parents$HHIDPN, allow_duplicates = F,
                             min.controls = 0, max.controls = 1, omit.fraction = NULL, 
                             mean.controls = NULL, tol = 0.001, data = hrsimp_groupmatch_parents)
# "allow_duplicates = T" leads to better balance, with 617 controls matched to 712 cases 

summary(hrs_groupmatch_parents) #seems to work for 1:1 matching without replacement despite the warning message!

#matched(hrs_groupmatch_parents)
sum(matched(hrs_groupmatch_parents))

#unmatched(hrs_groupmatch_parents)
sum(unmatched(hrs_groupmatch_parents))

#matchfailed(hrs_groupmatch_parents)
sum(matchfailed(hrs_groupmatch_parents))

hrs_groupmatch_data_parents <- cbind(hrsimp_groupmatch_parents, matches=hrs_groupmatch_parents) %>% 
  filter(!is.na(matches))
hrs_groupmatch_data_parents <- hrs_groupmatch_data_parents %>% group_by(matches) %>% 
  mutate(time = ifelse(is.na(time), max(time, na.rm = T), time),
         valid = ifelse(is.na(valid), max(valid, na.rm = T), valid)) %>% ungroup %>% 
  select(-matches)

table(hrs_groupmatch_data_parents$grandparent, hrs_groupmatch_data_parents$year)
table(hrs_groupmatch_data_parents$grandparent, hrs_groupmatch_data_parents$time)
table(hrs_groupmatch_data_parents$grandparent, hrs_groupmatch_data_parents$valid)
table(hrs_groupmatch_data_parents$grandparent, hrs_groupmatch_data_parents$female) # exact matching on gender!

hrs_groupmatch_data_parents <- left_join(hrs_groupmatch_data_parents, hrsimp_parents_ps_1,
                                         by = c("HHIDPN", "year", "grandparent", "female"))

# for balance assessment (at the time of matching)
hrs_bal_parents_groupmatch <- hrs_groupmatch_data_parents %>%
  select(HHIDPN, grandparent, pscore, female, everything(), -time, -year, -valid)

hrs_groupmatch_data_parents <- hrs_groupmatch_data_parents %>% 
  select(HHIDPN, year, grandparent, time, valid, pscore) %>% 
  rename(match_year = year, time_match = time, valid_match = valid)

# compile analysis sample with all longitudinal observations
hrsanalysis_parents_groupmatch <- left_join(hrs_groupmatch_data_parents, hrslongvalid,
                                            by = c("HHIDPN", "grandparent")) %>% select(-droplater)

# create time variable for controls relative to the time point of matching 
hrsanalysis_parents_groupmatch <- hrsanalysis_parents_groupmatch %>% mutate(
  time = replace(time, is.na(time) & match_year==year & time_match==-4, -4),
  # variable 'match_year' relates to the controls!
  valid = replace(valid, is.na(valid) & match_year==year, -1)
)
hrsanalysis_parents_groupmatch <- hrsanalysis_parents_groupmatch %>% mutate(
  time = replace(time, is.na(time) & match_year==year & time_match==-2, -2)
)
table(hrsanalysis_parents_groupmatch$time, hrsanalysis_parents_groupmatch$grandparent)

hrsanalysis_parents_groupmatch <- hrsanalysis_parents_groupmatch %>% filter(!is.na(pscore)) %>% 
  mutate(
    time = ifelse(grandparent==0 & is.na(time) & time_match==-4, (year - match_year) - 4, time),
    time = ifelse(grandparent==0 & is.na(time) & time_match==-2, (year - match_year) - 2, time))

hrsanalysis_parents_groupmatch <- hrsanalysis_parents_groupmatch %>% group_by(HHIDPN) %>% 
  mutate(lastcount = ifelse(grandparent==0 & valid==-1, row_number(), NA)) %>% ungroup()
hrsanalysis_parents_groupmatch <- hrsanalysis_parents_groupmatch %>% group_by(HHIDPN) %>% 
  mutate(lastcount = max(lastcount, na.rm = T)) %>% ungroup() %>% 
  mutate(lastcount = replace(lastcount, lastcount==-Inf, NA)) # regular NA pls

# finish coding 'valid' for controls
hrsanalysis_parents_groupmatch <- hrsanalysis_parents_groupmatch %>% group_by(HHIDPN) %>% 
  mutate(helpcount = row_number()) %>% ungroup()
hrsanalysis_parents_groupmatch <- hrsanalysis_parents_groupmatch %>% 
  mutate(valid = ifelse(is.na(valid) & grandparent==0, helpcount - lastcount - 1, valid)) %>% 
  select(-helpcount, -lastcount)

table(hrsanalysis_parents_groupmatch$grandparent, hrsanalysis_parents_groupmatch$time)
table(hrsanalysis_parents_groupmatch$grandparent, hrsanalysis_parents_groupmatch$valid)
table(hrsanalysis_parents_groupmatch$grandparent, hrsanalysis_parents_groupmatch$year)

hrsanalysis_parents_groupmatch <- hrsanalysis_parents_groupmatch %>% filter(time %in% c(-6:6)) %>% 
  select(-time_match, -match_year, -valid_match)

#sample comparison - different matching methods
table(hrsanalysis_parents$grandparent, hrsanalysis_parents$time)
table(hrsanalysis_parents_groupmatch$grandparent, hrsanalysis_parents_groupmatch$time)

# save .rda 
save(hrsanalysis_parents_groupmatch, file = "data/processed/HRS/hrsanalysis_parents_groupmatch.rda")


#### PSM: identify possible matches -> (2) nonparent control group ####

# DIY matching: 2nd step -> identifying all possible PS-matches per grandparent  
#                           and matching exactly on gender
hrsimp_matching_nonparents <- hrsimp_matching_nonparents %>% 
  select(HHIDPN, year, female, grandparent, pscore) 
# sort by random variable in case there are ordering effects
set.seed(123)
rows <- sample(nrow(hrsimp_matching_nonparents))
hrsimp_matching_nonparents <- hrsimp_matching_nonparents[rows, ]
hrsimp_matching_nonparents <- hrsimp_matching_nonparents %>% mutate(new_id = row_number())

hrsimp_matching_nonparents_case <- hrsimp_matching_nonparents %>% filter(grandparent==1) %>% 
  rename(case_id = new_id,
         pid_case = HHIDPN,
         pscore_case = pscore,
         female_case = female) %>% 
  select(-grandparent, -year)
# removing 'year' here means that 'year' -> 'match_year' will refer to the controls' survey year
# at the time of matching (see below)

hrsimp_matching_nonparents_control <- hrsimp_matching_nonparents %>% filter(grandparent==0) %>% 
  rename(control_id = new_id,
         pid_control = HHIDPN,
         pscore_control = pscore,
         female_control = female,
         match_year = year) %>% 
  select(-grandparent)

# matching the two datasets along gender of controls and cases (exact matches)
# crossing = equivalent of STATA joinby command (Cartesian product)
hrsimp_joined_nonparents <- crossing(hrsimp_matching_nonparents_case, hrsimp_matching_nonparents_control)
# 712*2357 = 1678184
# exact matching on gender
hrsimp_joined_nonparents <- hrsimp_joined_nonparents %>% filter(female_case==female_control) %>% 
  select(-female_control) %>% 
  rename(female = female_case)

#### PSM: matching loop -> (2) nonparent control group ####

# DIY matching: 3rd step -> matching the cases and controls according to the similarity of 
#                           their propensity scores (1 to 1 matching; no duplicates)
hrsimp_joined_nonparents <- hrsimp_joined_nonparents %>% 
  mutate(ps_diff = abs(pscore_case - pscore_control))
#no midranks or shared ranks - if PS_diff is the same it can be random which one we pick
hrsimp_joined_nonparents <- hrsimp_joined_nonparents %>% group_by(pid_case) %>% 
  mutate(rank = rank(ps_diff, ties.method = "random"))

#this new numbering makes the following loop easier  
hrsimp_joined_nonparents <- hrsimp_joined_nonparents %>% group_by(case_id) %>%
  mutate(case_id_new = cur_group_id()) %>% 
  arrange(case_id_new, rank) %>% ungroup()

#to help with computation time -> restrict to certain ranks (similar idea to caliper)
#assuming that there will be no valid match according to PS with a higher rank, anyway!
hrsimp_joined_nonparents_reduced <- hrsimp_joined_nonparents %>% filter(rank<=600)
(num_cases <- as.vector(summary(hrsimp_joined_nonparents_reduced$case_id_new)[6]))

hrs_matched_nonparents <- NULL;
picked <- NULL;
for(num in 1:num_cases)
{
  #pick the highest rank of the (remaining) controls each case is matched with
  picked <- hrsimp_joined_nonparents_reduced %>% filter(case_id_new==num) %>% 
    mutate(best = min(rank)) %>% filter(rank==best) %>% select(-best)
  #collect valid matches throughout the loop
  hrs_matched_nonparents <- rbind(hrs_matched_nonparents, picked)
  #track pid of control matched in every iteration of the loop
  picked <- picked %>% mutate(tracked=1) %>% select(pid_control, tracked)
  #drop all other controls with the tracked pid in the remaining pool of available controls
  hrsimp_joined_nonparents_reduced <- left_join(hrsimp_joined_nonparents_reduced, picked)
  hrsimp_joined_nonparents_reduced <- hrsimp_joined_nonparents_reduced %>% 
    filter(is.na(tracked)) %>% select(-tracked)
}
# all 712 grandparents sucessfully matched!
summary(hrs_matched_nonparents$rank)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0     2.0    89.0   150.1   270.0   556.0 

summary(hrs_matched_nonparents$ps_diff)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000039 0.0011630 0.1459282 0.2261260 0.4114583 0.8758500 

# create dataset for merge with controls
hrs_matched_mergecontrols_nonparents <- hrs_matched_nonparents %>% rename(
  HHIDPN = pid_control,
  pscore = pscore_control,
  HHIDPN_case = pid_case
) %>% select(HHIDPN, HHIDPN_case, match_year, pscore) %>% 
  arrange(HHIDPN)

# create dataset for merge with cases
hrs_matched_mergecases_nonparents <- hrs_matched_nonparents %>% rename(
  HHIDPN = pid_case,
  pscore = pscore_case,
  HHIDPN_control = pid_control
) %>% select(HHIDPN, HHIDPN_control, match_year, pscore) %>% 
  arrange(HHIDPN)

#### compile final analysis samples -> (2) nonparent control group ####

# Problem: 
# Respondents in the nonparent control group were allowed to have missing information
# on grandparent status if we knew that they had no children (around 10% of obs.).
# However, this is not represented in 'hrslongvalid' where these cases still 
# have grandparent==NA
summary(hrslongvalid$grandparent)

hrsanalysis_nonparents <- hrslongvalid %>% 
  mutate(grandparent = replace(grandparent, totalnonresidentkids==0 & 
                               totalresidentkids==0 & is.na(has_ch1) & 
                               is.na(grandparent), 0)) %>% # adjusting grandparent status
  filter(!is.na(grandparent) & droplater!=T) 

hrsanalysis_nonparents <- left_join(hrsanalysis_nonparents, hrs_matched_mergecases_nonparents, by='HHIDPN')
hrsanalysis_nonparents <- left_join(hrsanalysis_nonparents, hrs_matched_mergecontrols_nonparents, by='HHIDPN') 

# create dataset for merge containing information on whether a control was 
# at time==-4 or time==-2 at the time of matching
# (variable 'valid' denotes valid assessment in relation to the transition to GP for the cases)
time_for_controls_hrs <- hrslongvalid %>% 
  filter(grandparent==1 & valid==-1 & droplater==F & !is.na(KABYEARBG_1)) %>%
  select(HHIDPN, time) %>% rename(time_match = time)
table(time_for_controls_hrs$time_match)
# match via HHIDPN_case in order to transfer 'time' value from the grandparents to 
# controls (at timepoint of matching)
hrsanalysis_nonparents <- left_join(hrsanalysis_nonparents, time_for_controls_hrs, by=c('HHIDPN_case'='HHIDPN')) 

hrsanalysis_nonparents <- hrsanalysis_nonparents %>% mutate(
  pscore = ifelse(!is.na(pscore.x), pscore.x, pscore.y),
  match_year = ifelse(!is.na(match_year.x), match_year.x, match_year.y),
  HHIDPN_match = ifelse(!is.na(HHIDPN_case), HHIDPN_case, HHIDPN_control)
) %>% 
  select(-c(pscore.x, pscore.y, match_year.x, match_year.y, HHIDPN_case, HHIDPN_control)) %>% 
  filter(!is.na(pscore)) # drop all observations from unmatched respondents

# create time variable for controls relative to the time point of matching 
hrsanalysis_nonparents <- hrsanalysis_nonparents %>% mutate(
  time = replace(time, is.na(time) & match_year==year & time_match==-4, -4),
  # variable 'match_year' relates to the controls!
  valid = replace(valid, is.na(valid) & match_year==year, -1)
)
# reminder: 'time' counts calendar years in relation to transition to GP, 'valid' only valid assessments

hrsanalysis_nonparents <- hrsanalysis_nonparents %>% 
  mutate(time = replace(time, is.na(time) & match_year==year & time_match==-2, -2))

table(hrsanalysis_nonparents$time, hrsanalysis_nonparents$grandparent)

hrsanalysis_nonparents <- hrsanalysis_nonparents %>% mutate(
  time = ifelse(is.na(time) & time_match==-4, (year - match_year) - 4, time),
  time = ifelse(is.na(time) & time_match==-2, (year - match_year) - 2, time))

hrsanalysis_nonparents <- hrsanalysis_nonparents %>% group_by(HHIDPN) %>% 
  mutate(lastcount = ifelse(grandparent==0 & valid==-1, row_number(), NA)) %>% ungroup()
hrsanalysis_nonparents <- hrsanalysis_nonparents %>% group_by(HHIDPN) %>% 
  mutate(lastcount = max(lastcount, na.rm = T)) %>% ungroup() %>% 
  mutate(lastcount = replace(lastcount, lastcount==-Inf, NA))

# finish coding 'valid' for controls
hrsanalysis_nonparents <- hrsanalysis_nonparents %>% group_by(HHIDPN) %>% 
  mutate(helpcount = row_number()) %>% ungroup()
hrsanalysis_nonparents <- hrsanalysis_nonparents %>% 
  mutate(valid = ifelse(is.na(valid) & grandparent==0, helpcount - lastcount - 1, valid)) %>% 
  select(-helpcount, -lastcount)

table(hrsanalysis_nonparents$valid, hrsanalysis_nonparents$grandparent)
table(hrsanalysis_nonparents$time, hrsanalysis_nonparents$grandparent) # cells earlier than '-6' too small

hrsanalysis_nonparents <- hrsanalysis_nonparents %>% filter(time %in% c(-6:6)) %>% 
  select(-time_match, -match_year, -droplater)

# save .rda 
save(hrsanalysis_nonparents, file = "data/processed/HRS/hrsanalysis_nonparents.rda")


#### PSM: 'rollingMatch' -> (2) nonparent control group ####

#library(rollingMatch) # see above for package installation

table(hrsimp_matching_nonparents$grandparent)
table(hrsimp_matching_nonparents$grandparent, hrsimp_matching_nonparents$year)

#we also need the 'time' value for grandparents (either -4 or -2) and the 'valid' variable
hrsimp_groupmatch_nonparents <- left_join(hrsimp_matching_nonparents, 
                                          hrsimp_matching_1, by=c("HHIDPN", "year")) %>% 
  filter(droplater==F) %>% 
  select(HHIDPN, year, female.x, grandparent.x, pscore, time, valid) %>% 
  rename(female = female.x, grandparent = grandparent.x)

table(hrsimp_groupmatch_nonparents$grandparent, hrsimp_groupmatch_nonparents$time)
table(hrsimp_groupmatch_nonparents$grandparent, hrsimp_groupmatch_nonparents$valid)

# Description
# This is an adaption of fullmatch to allow for restrictions when control observations 
# are "grouped". The motivating use case is when there are multiple observations of control 
# data for each control subject. In this case, the grouping variable is the subject. We may 
# want to place restrictions, for example that only one observation of a subject can be 
# matched, or in the case of one:many matching, a given control subject can only be matched 
# to a given treated subject once.

#matrix of propensity score distances (uses the same 'pscore' variable as the K&R matching loop)
match_on_nonparents_ps <- as.matrix(match_on(grandparent~pscore, data=hrsimp_groupmatch_nonparents))

#caliper matrix to exact-match on gender
match_on_nonparents_female <- as.matrix(exactMatch(grandparent ~ female, data=hrsimp_groupmatch_nonparents))

#caliper matrix to match within $500 of income (also possible)
#inc <-  as.matrix(caliper(match_on(treat~income), distance = "Euclidean", width = 500))

#final input to rollingMatch is the sum of these matrices
final_dist_nonparents <- match_on_nonparents_ps + match_on_nonparents_female
# 1678184 elements in matrix -> same as in Cartesian product in DIY matching loop

hrs_groupmatch_nonparents <- groupmatch(x=final_dist_nonparents, 
                                     group = hrsimp_groupmatch_nonparents$HHIDPN, allow_duplicates = F,
                                     min.controls = 0, max.controls = 1, omit.fraction = NULL, 
                                     mean.controls = NULL, tol = 0.001, data = hrsimp_groupmatch_nonparents)
# "allow_duplicates = T" leads to better balance, with 520 controls matched to 712 cases 

summary(hrs_groupmatch_nonparents) #seems to work for 1:1 matching without replacement despite the warning message!

#matched(hrs_groupmatch_nonparents)
sum(matched(hrs_groupmatch_nonparents))

#unmatched(hrs_groupmatch_nonparents)
sum(unmatched(hrs_groupmatch_nonparents))

#matchfailed(hrs_groupmatch_nonparents)
sum(matchfailed(hrs_groupmatch_nonparents))

hrs_groupmatch_data_nonparents <- cbind(hrsimp_groupmatch_nonparents, matches=hrs_groupmatch_nonparents) %>% 
  filter(!is.na(matches))
hrs_groupmatch_data_nonparents <- hrs_groupmatch_data_nonparents %>% group_by(matches) %>% 
  mutate(time = ifelse(is.na(time), max(time, na.rm = T), time),
         valid = ifelse(is.na(valid), max(valid, na.rm = T), valid)) %>% ungroup %>% 
  select(-matches)

table(hrs_groupmatch_data_nonparents$grandparent, hrs_groupmatch_data_nonparents$year)
table(hrs_groupmatch_data_nonparents$grandparent, hrs_groupmatch_data_nonparents$time)
table(hrs_groupmatch_data_nonparents$grandparent, hrs_groupmatch_data_nonparents$valid)
table(hrs_groupmatch_data_nonparents$grandparent, hrs_groupmatch_data_nonparents$female) # exact matching on gender!

hrs_groupmatch_data_nonparents <- left_join(hrs_groupmatch_data_nonparents, hrsimp_nonparents_ps_1,
                                            by = c("HHIDPN", "year", "grandparent", "female"))

# for balance assessment (at the time of matching)
hrs_bal_nonparents_groupmatch <- hrs_groupmatch_data_nonparents %>%
  select(HHIDPN, grandparent, pscore, female, everything(), -time, -year, -valid)

hrs_groupmatch_data_nonparents <- hrs_groupmatch_data_nonparents %>% 
  select(HHIDPN, year, grandparent, time, valid, pscore) %>% 
  rename(match_year = year, time_match = time, valid_match = valid)

# compile analysis sample with all longitudinal observations
# same adjustment (NA -> 0) as for the other matching method (see above)
hrsanalysis_nonparents_groupmatch <- hrslongvalid %>% 
  mutate(grandparent = replace(grandparent, totalnonresidentkids==0 & 
                                 totalresidentkids==0 & is.na(has_ch1) & 
                                 is.na(grandparent), 0)) %>% # adjusting grandparent status
  filter(!is.na(grandparent) & droplater!=T) 

hrsanalysis_nonparents_groupmatch <- left_join(hrs_groupmatch_data_nonparents, 
                                               hrsanalysis_nonparents_groupmatch,
                                               by = c("HHIDPN", "grandparent")) %>% select(-droplater)

# create time variable for controls relative to the time point of matching 
hrsanalysis_nonparents_groupmatch <- hrsanalysis_nonparents_groupmatch %>% mutate(
  time = replace(time, is.na(time) & match_year==year & time_match==-4, -4),
  # variable 'match_year' relates to the controls!
  valid = replace(valid, is.na(valid) & match_year==year, -1)
)
hrsanalysis_nonparents_groupmatch <- hrsanalysis_nonparents_groupmatch %>% mutate(
  time = replace(time, is.na(time) & match_year==year & time_match==-2, -2)
)
table(hrsanalysis_nonparents_groupmatch$time, hrsanalysis_nonparents_groupmatch$grandparent)

hrsanalysis_nonparents_groupmatch <- hrsanalysis_nonparents_groupmatch %>% filter(!is.na(pscore)) %>% 
  mutate(
    time = ifelse(grandparent==0 & is.na(time) & time_match==-4, (year - match_year) - 4, time),
    time = ifelse(grandparent==0 & is.na(time) & time_match==-2, (year - match_year) - 2, time))

hrsanalysis_nonparents_groupmatch <- hrsanalysis_nonparents_groupmatch %>% group_by(HHIDPN) %>% 
  mutate(lastcount = ifelse(grandparent==0 & valid==-1, row_number(), NA)) %>% ungroup()
hrsanalysis_nonparents_groupmatch <- hrsanalysis_nonparents_groupmatch %>% group_by(HHIDPN) %>% 
  mutate(lastcount = max(lastcount, na.rm = T)) %>% ungroup() %>% 
  mutate(lastcount = replace(lastcount, lastcount==-Inf, NA)) # regular NA pls

# finish coding 'valid' for controls
hrsanalysis_nonparents_groupmatch <- hrsanalysis_nonparents_groupmatch %>% group_by(HHIDPN) %>% 
  mutate(helpcount = row_number()) %>% ungroup()
hrsanalysis_nonparents_groupmatch <- hrsanalysis_nonparents_groupmatch %>% 
  mutate(valid = ifelse(is.na(valid) & grandparent==0, helpcount - lastcount - 1, valid)) %>% 
  select(-helpcount, -lastcount)

table(hrsanalysis_nonparents_groupmatch$grandparent, hrsanalysis_nonparents_groupmatch$time)
table(hrsanalysis_nonparents_groupmatch$grandparent, hrsanalysis_nonparents_groupmatch$valid)
table(hrsanalysis_nonparents_groupmatch$grandparent, hrsanalysis_nonparents_groupmatch$year)

hrsanalysis_nonparents_groupmatch <- hrsanalysis_nonparents_groupmatch %>% filter(time %in% c(-6:6)) %>% 
  select(-time_match, -match_year, -valid_match)

#sample comparison - different matching methods
table(hrsanalysis_nonparents$grandparent, hrsanalysis_nonparents$time)
table(hrsanalysis_nonparents_groupmatch$grandparent, hrsanalysis_nonparents_groupmatch$time)

# save .rda 
save(hrsanalysis_nonparents_groupmatch, file = "data/processed/HRS/hrsanalysis_nonparents_groupmatch.rda")


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

# create datasets containing the PSM covariate values at the time of matching
# balance AFTER matching
hrs_bal_parents <- hrsanalysis_parents %>% filter(valid==-1) %>% 
  select(HHIDPN, year, grandparent, pscore)
hrs_bal_parents <- left_join(hrs_bal_parents, hrsimp_parents_ps_1, by=c("HHIDPN", "year", "grandparent")) %>% 
  select(HHIDPN, grandparent, pscore, female, everything(), -year) 
#imputed values of the covariates (or should I use the ones containing missings?)
summary(hrs_bal_parents)

hrs_bal_nonparents <- hrsanalysis_nonparents %>% filter(valid==-1) %>% 
  select(HHIDPN, year, grandparent, pscore)
hrs_bal_nonparents <- left_join(hrs_bal_nonparents, hrsimp_nonparents_ps_1, by=c("HHIDPN", "year", "grandparent")) %>% 
  select(HHIDPN, grandparent, pscore, female, everything(), -year) 
#imputed values of the covariates (or should I use the ones containing missings?)
summary(hrs_bal_nonparents)

# balance BEFORE matching
hrs_bal_parents_before <- hrsimp_parents_ps_2 %>% 
  mutate(pscore = (pscore_m1 + pscore_m2 + pscore_m3 + pscore_m4 + pscore_m5)/5) %>% 
  select(-starts_with("pscore_m"), -year) %>% 
  select(HHIDPN, grandparent, pscore, female, everything())
summary(hrs_bal_parents_before)

names(hrs_bal_parents_before) # column names must be aligned!
names(hrs_bal_parents)
names(hrs_bal_parents_groupmatch)

hrs_bal_nonparents_before <- hrsimp_nonparents_ps_2 %>% 
  mutate(pscore = (pscore_m1 + pscore_m2 + pscore_m3 + pscore_m4 + pscore_m5)/5) %>% 
  select(-starts_with("pscore_m"), -year) %>% 
  select(HHIDPN, grandparent, pscore, female, everything())
summary(hrs_bal_nonparents_before)

names(hrs_bal_nonparents_before) # column names must be aligned!
names(hrs_bal_nonparents)
names(hrs_bal_nonparents_groupmatch)

# evaluate standardized difference in means
# create maxtrix object with empty vectors 'stddiff'
# (1) PARENTS
varnum_parents <-  1:(length(hrs_bal_parents)-2)
covar_parents <-  colnames(hrs_bal_parents[3:paste(length(hrs_bal_parents))])
stddiff_before_parents <- numeric(length = length(hrs_bal_parents_before)-2)     #before matching
stddiff_after1_parents <- numeric(length = length(hrs_bal_parents)-2)            #K&R matching loop
stddiff_after2_parents <- numeric(length = length(hrs_bal_parents_groupmatch)-2) #from 'rollingMatch' (GroupMatch) package  

coln_parents <- c("varnum_parents", "covar_parents", "stddiff_before_parents", 
                  "stddiff_after1_parents", "stddiff_after2_parents") # defining column names 
# creating matrix 
hrs_balance_matrix_parents <- matrix(c(varnum_parents, covar_parents, stddiff_before_parents,
                                       stddiff_after1_parents, stddiff_after2_parents), ncol = 5, 
                                       dimnames = list(varnum_parents, coln_parents))

# use custom function in for-loop to fill matrix vectors 'stddiff'
for (i in seq_along(hrs_balance_matrix_parents[varnum_parents])) {
  hrs_balance_matrix_parents[[i, 3]] <- stdmeandiff(get(hrs_balance_matrix_parents[[i, 2]]), 
                                                    grandparent, hrs_bal_parents_before)     #before matching
  hrs_balance_matrix_parents[[i, 4]] <- stdmeandiff(get(hrs_balance_matrix_parents[[i, 2]]), 
                                                    grandparent, hrs_bal_parents)            #DIY matching loop
  hrs_balance_matrix_parents[[i, 5]] <- stdmeandiff(get(hrs_balance_matrix_parents[[i, 2]]), 
                                                    grandparent, hrs_bal_parents_groupmatch) #from 'rollingMatch' (GroupMatch) package
  }
hrs_balance_matrix_parents[, 3:5] <- round(as.numeric(hrs_balance_matrix_parents[, 3:5]), 3)

kable(hrs_balance_matrix_parents[, 2:5], format="rst", 
      col.names = c("Covariate", "Before Matching",
                    "DIY Matching Loop", "'rollingMatch'"), 
      align = "lccc", digits=2, caption = "Table 1. Covariate Balance")


# (2) NONPARENTS

# create maxtrix object with empty vectors 'stddiff'
varnum_nonparents <-  1:(length(hrs_bal_nonparents)-2)
covar_nonparents <-  colnames(hrs_bal_nonparents[3:paste(length(hrs_bal_nonparents))])
stddiff_before_nonparents <- numeric(length = length(hrs_bal_nonparents_before)-2)     #before matching
stddiff_after1_nonparents <- numeric(length = length(hrs_bal_nonparents)-2)            #K&R matching loop
stddiff_after2_nonparents <- numeric(length = length(hrs_bal_nonparents_groupmatch)-2) #from 'rollingMatch' (GroupMatch) package  

coln_nonparents <- c("varnum_nonparents", "covar_nonparents", "stddiff_before_nonparents", 
                     "stddiff_after1_nonparents", "stddiff_after2_nonparents") # defining column names 
# creating matrix 
hrs_balance_matrix_nonparents <- matrix(c(varnum_nonparents, covar_nonparents, stddiff_before_nonparents,
                                       stddiff_after1_nonparents, stddiff_after2_nonparents), ncol = 5, 
                                       dimnames = list(varnum_nonparents, coln_nonparents))

# use custom function in for-loop to fill matrix vectors 'stddiff'
for (i in seq_along(hrs_balance_matrix_nonparents[varnum_nonparents])) {
  hrs_balance_matrix_nonparents[[i, 3]] <- stdmeandiff(get(hrs_balance_matrix_nonparents[[i, 2]]), 
                                                    grandparent, hrs_bal_nonparents_before)     #before matching
  hrs_balance_matrix_nonparents[[i, 4]] <- stdmeandiff(get(hrs_balance_matrix_nonparents[[i, 2]]), 
                                                    grandparent, hrs_bal_nonparents)            #DIY matching loop
  hrs_balance_matrix_nonparents[[i, 5]] <- stdmeandiff(get(hrs_balance_matrix_nonparents[[i, 2]]), 
                                                    grandparent, hrs_bal_nonparents_groupmatch) #from 'rollingMatch' (GroupMatch) package
}
hrs_balance_matrix_nonparents[, 3:5] <- round(as.numeric(hrs_balance_matrix_nonparents[, 3:5]), 3)

kable(hrs_balance_matrix_nonparents[, 2:5], format="rst", 
      col.names = c("Covariate", "Before Matching",
                    "DIY Matching Loop", "'rollingMatch'"), 
      align = "lccc", digits=2, caption = "Table 1. Covariate Balance")


