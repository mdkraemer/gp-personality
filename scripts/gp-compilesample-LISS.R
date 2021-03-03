### Transition to Grandparenthood Paper - LISS Sample ###

#version.string R version 3.6.3 (2020-02-29)
#nickname       Holding the Windsock        

library(tidyverse)
library(psych)
library(foreign)
library(knitr)

#### LISS data: draw raw data, select variables ####

### Background Variables datasets 2008-2019 (thanks to Ted Schwaba / Jaap Denissen for the basis of this code)

#first, specify the path and the .sav extension
path <- "data/raw/LISS/Background Variables"

#setwd(path)
files <- list.files(path=path, pattern="*.sav")
files <- paste(path, files, sep = "/")

#adds everything together
eve.all = do.call("bind_rows", lapply(files, function(x) read.spss(x, use.value.labels=F, to.data.frame=T)))
length(unique(eve.all$nomem_encr))

#turn waves into months and years
eve.all$year <- as.numeric(substr(eve.all$wave, 1, 4))
eve.all$month <- as.numeric(substr(eve.all$wave, 5,6))
eve.all$timeline <- (eve.all$year-2007)*12 + eve.all$month
 
# get people's demographics from the first month of the year (These things tend to not change over the year, such as gender)
# eve.all <- filter(eve.all, month == 1)
# this does not work if someone misses the Jan wave -> instead select first available wave per year
eve.all <- eve.all %>% group_by(nomem_encr, year) %>% arrange(nomem_encr, year, month) %>% 
  mutate(first = row_number()) %>% ungroup() %>% filter(first==1) %>% select(-first)

eve.all$age <- eve.all$year - eve.all$gebjaar 
eve.all$female <- eve.all$geslacht-1
eve.all$employment_status <- eve.all$belbezig
eve.all$education <- eve.all$oplmet
eve.all$hhmembers <- eve.all$aantalhh
eve.all$totalresidentkids <- eve.all$aantalki
eve.all$marital <- eve.all$burgstat
eve.all$ownrent <- eve.all$woning
eve.all$urban <- eve.all$sted
#eve.all$migback <- eve.all$herkomstgroep # not available in 2008 / 2009 - also too many NAs
# count waves (=years) of participation
eve.all <- eve.all %>% arrange(nomem_encr, year) %>% 
  group_by(nomem_encr) %>% mutate(participation = row_number()) %>% ungroup()
  
eve.all <- eve.all %>% select(nomem_encr, nohouse_encr, year, age, female, 
                              employment_status, education, hhmembers,
                              totalresidentkids, marital, ownrent, urban, 
                              nettoink, participation)

### Personality datasets 2008-2019 (thanks to Ted Schwaba / Jaap Denissen for this code)
per08 <- read.spss('data/raw/LISS/Personality/cp08a_1p_EN.sav', use.value.labels=F, to.data.frame=T)
per09 <- read.spss('data/raw/LISS/Personality/cp09b_1.0p_EN.sav', use.value.labels=F, to.data.frame=T)
per10 <- read.spss('data/raw/LISS/Personality/cp10c_1.0p_EN.sav', use.value.labels=F, to.data.frame=T)
per11 <- read.spss('data/raw/LISS/Personality/cp11d_1.0p_EN.sav', use.value.labels=F, to.data.frame=T)
per12 <- read.spss('data/raw/LISS/Personality/cp12e_1.0p_EN.sav', use.value.labels=F, to.data.frame=T)
per13 <- read.spss('data/raw/LISS/Personality/cp13f_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
per14 <- read.spss('data/raw/LISS/Personality/cp14g_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
per15 <- read.spss('data/raw/LISS/Personality/cp15h_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
#per16 <- read.spss('data/raw/LISS/Personality/cp16i_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
per17 <- read.spss('data/raw/LISS/Personality/cp17i_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
per18 <- read.spss('data/raw/LISS/Personality/cp18j_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
per19 <- read.spss('data/raw/LISS/Personality/cp19k_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)

#Make the names uniform by removing the wave qualifier (e.g., "08a")
names(per08) <- gsub("08a", "", names(per08))
names(per09) <- gsub("09b", "", names(per09))
names(per10) <- gsub("10c", "", names(per10))
names(per11) <- gsub("11d", "", names(per11))
names(per12) <- gsub("12e", "", names(per12))
names(per13) <- gsub("13f", "", names(per13))
names(per14) <- gsub("14g", "", names(per14))
names(per15) <- gsub("15h", "", names(per15))
#names(per16) <- gsub("16i", "", names(per16))
names(per17) <- gsub("17i", "", names(per17))
names(per18) <- gsub("18j", "", names(per18))
names(per19) <- gsub("19k", "", names(per19))

# cp190 "Starting time questionnaire" is factor in some waves and numerical in others
# cp192 "End time questionnaire" --> remove both variables!
remove_timevar <- function(x) { 
  x %>%       
    select(-c(cp190, cp192))
}
listper <- list(per08, per09, per10, per11, per12, per13, per14, per15, per17, per18, per19) %>%
  lapply(remove_timevar)
names(listper) <- c("per08", "per09", "per10", "per11", "per12", "per13", "per14", 
                    "per15", "per17", "per18", "per19")
list2env(listper, .GlobalEnv)

#Merge them all together
per.all <- bind_rows(per08, per09, per10, per11, per12, per13, 
                     per14, per15, per17, per18, per19)
length(unique(per.all$nomem_encr))

rm(per08, per09, per10, per11, per12, per13, 
   per14, per15, per17, per18, per19)

#Rename "c_m" into "wave"
names(per.all)[3] <- c("wave")
per.all$year <- as.numeric(substr(per.all$wave, 1, 4))
per.all$permonth <- as.numeric(substr(per.all$wave,5,6))
per.all$pertimeline <- (per.all$year-2007)*12 + per.all$permonth

#a lot of people didn't do the Big Five personality questionnaire, they just did the basic questions. ELIMINATE THEM :(
#this is why we cannot examine personality in 2010 and 2012
#however, I don't think the LISS people know this error occurred -- the "completed" numbers in the codebook are wrong for these years
per.all %>% filter(!is.na(cp020)) %>% group_by(year) %>% dplyr::summarise(n = n())
#cp020 is the first Big 5 item (cp069 the last); additional missings in later Big 5 items
#but cp020 catches all systematic missings (where the whole scale is missing)
per.all <- per.all[!is.na(per.all$cp020),]

#recode Big 5 variables
per.all$extra1 <- per.all$cp020  #	 cp08a020 Am the life of the party.  
per.all$extra2 <- per.all$cp025 #	 cp08a025 Don't talk a lot.
per.all$extra3 <- per.all$cp030 #	 cp08a030 Feel comfortable around people.   
per.all$extra4 <- per.all$cp035 #	 cp08a035 Keep in the background. 
per.all$extra5 <- per.all$cp040 #	 cp08a040 Start conversations.
per.all$extra6 <- per.all$cp045 #	 cp08a045 Have little to say. 
per.all$extra7 <- per.all$cp050 #	 cp08a050 Talk to a lot of different people at parties.  
per.all$extra8 <- per.all$cp055 #	 cp08a055 Don't like to draw attention to myself. 
per.all$extra9 <- per.all$cp060 #	 cp08a060 Don't mind being the center of attention.   
per.all$extra10 <-per.all$cp065 #	 cp08a065 Am quiet around strangers.

per.all$agree1 <- per.all$cp021 #	  cp08a021 Feel little concern for others. 
per.all$agree2 <- per.all$cp026 #	 cp08a026 Am interested in people. 
per.all$agree3 <- per.all$cp031 #	  cp08a031 Insult people. 
per.all$agree4 <- per.all$cp036 #	 cp08a036 Sympathize with others' feelings. 
per.all$agree5 <- per.all$cp041 #	 cp08a041 Am not interested in other people's problems.
per.all$agree6 <- per.all$cp046 #	 cp08a046 Have a soft heart. 
per.all$agree7 <- per.all$cp051 #	 cp08a051 Am not really interested in others. 
per.all$agree8 <- per.all$cp056 #	 cp08a056 Take time out for others. 
per.all$agree9 <- per.all$cp061 #	 cp08a061 Feel others' emotions. 
per.all$agree10 <- per.all$cp066 #	 cp08a066 Make people feel at ease. 

per.all$con1 <- per.all$cp042 #	 cp08a042 Get chores done right away. 
per.all$con2 <- per.all$cp027 #	 cp08a027 Leave my belongings around.
per.all$con3 <- per.all$cp062 #	 cp08a062 Follow a schedule. 
per.all$con4 <- per.all$cp057 #	 cp08a057 Shirk my duties. 
per.all$con5 <- per.all$cp032 #	 cp08a032 Pay attention to details. 
per.all$con6 <- per.all$cp067 #	 cp08a067 Am exacting in my work. 
per.all$con7 <- per.all$cp047 #	 cp08a047 Often forget to put things back in their proper place. 
per.all$con8 <- per.all$cp022 #	 cp08a022 Am always prepared. 
per.all$con9 <- per.all$cp037 #	 cp08a037 Make a mess of things.
per.all$con10 <- per.all$cp052 #	 cp08a052 Like order. 

per.all$neur1 <- per.all$cp023 #	  cp08a023 Get stressed out easily. 
per.all$neur2 <- per.all$cp028 #	 cp08a028 Am relaxed most of the time. 
per.all$neur3 <- per.all$cp033 #	   cp08a033 Worry about things. 
per.all$neur4 <- per.all$cp038 #	 cp08a038 Seldom feel blue.
per.all$neur5 <- per.all$cp043 #	 cp08a043 Am easily disturbed. 
per.all$neur6 <- per.all$cp048 #	 cp08a048 Get upset easily. 
per.all$neur7 <- per.all$cp053 #	 cp08a053 Change my mood a lot. 
per.all$neur8 <- per.all$cp058 #	 cp08a058 Have frequent mood swings. 
per.all$neur9 <- per.all$cp063 #	  cp08a063 Get irritated easily. 
per.all$neur10 <- per.all$cp068 #	 cp08a068 Often feel blue. 

per.all$open1 <- per.all$cp024 #	 cp08a024 Have a rich vocabulary. 
per.all$open2 <- per.all$cp029 #	 cp08a029 Have difficulty understanding abstract ideas. 
per.all$open3 <- per.all$cp034 #	 cp08a034 Have a vivid imagination.
per.all$open4 <- per.all$cp039 #	 cp08a039 Am not interested in abstract ideas. 
per.all$open5 <- per.all$cp044 #	 cp08a044 Have excellent ideas. 
per.all$open6 <- per.all$cp049 #	 cp08a049 Do not have a good imagination. 
per.all$open7 <- per.all$cp054 #	  cp08a054 Am quick to understand things. 
per.all$open8 <- per.all$cp059 #	 cp08a059 Use difficult words. 
per.all$open9 <- per.all$cp064 #	  cp08a064 Spend time reflecting on things. 
per.all$open10 <- per.all$cp069 #	 cp08a069 Am full of ideas.

#satisfaction with life scale (SWLS, Diener)
per.all$swls1 <- per.all$cp014 # In most ways my life is close to my ideal
per.all$swls2 <- per.all$cp015 # The conditions of my life are excellent
per.all$swls3 <- per.all$cp016 # I am satisfied with my life
per.all$swls4 <- per.all$cp017 # So far I have gotten the important things I want in life
per.all$swls5 <- per.all$cp018 # If I could live my life over, I would change almost nothing

#calculate alphas
alpha.extra <- per.all %>%
  select_if(grepl("extra", names(.))) %>%
  psych::alpha(check.keys = TRUE)

alpha.agree <- per.all %>%
  select_if(grepl("agree", names(.))) %>%
  psych::alpha(check.keys = TRUE)

alpha.con <- per.all %>%
  select_if(grepl("con", names(.))) %>%
  psych::alpha(check.keys = TRUE)

alpha.neur <- per.all %>%
  select_if(grepl("neur", names(.))) %>%
  psych::alpha(check.keys = TRUE)

alpha.open <- per.all %>%
  select_if(grepl("open", names(.))) %>%
  psych::alpha(check.keys = TRUE)

alpha.swls <- per.all %>%
  select_if(grepl("swls", names(.))) %>%
  psych::alpha(check.keys = TRUE)

#score traits 
per.all$extra <- per.all%>%
  select_if(grepl("extra", names(.))) %>%
  reverse.code(keys=alpha.extra$keys, items = .) %>%
  rowMeans(na.rm=T)
per.all$agree <- per.all%>%
  select_if(grepl("agree", names(.))) %>%
  reverse.code(keys=alpha.agree$keys, items = .) %>%
  rowMeans(na.rm=T)
per.all$con <- per.all%>%
  select_if(grepl("con", names(.))) %>%
  reverse.code(keys=alpha.con$keys, items = .) %>%
  rowMeans(na.rm=T)
per.all$neur <- per.all%>%
  select_if(grepl("neur", names(.))) %>%
  reverse.code(keys=alpha.neur$keys, items = .) %>%
  rowMeans(na.rm=T)
per.all$open <- per.all%>%
  select_if(grepl("open", names(.))) %>%
  reverse.code(keys=alpha.open$keys, items = .) %>%
  rowMeans(na.rm=T)
per.all$swls <- per.all%>%
  select_if(grepl("swls", names(.))) %>%
  reverse.code(keys=alpha.swls$keys, items = .) %>%
  rowMeans(na.rm=T)

#select relevant variables: 
#   all Big 5 personality variables
#   SWLS 
per.all <- per.all %>% 
  select(nomem_encr, year, permonth, pertimeline,
         contains("open"), contains("con"), contains("extra"), contains("agree"), contains("neur"),
         contains("swls"))

### Work and Schooling datasets 2008-2019 (thanks to Ted Schwaba / Jaap Denissen for this code)
wed08 <- read.spss('data/raw/LISS/Work and Schooling/cw08a_EN_1.1p.sav', use.value.labels=F, to.data.frame=T)
wed09 <- read.spss('data/raw/LISS/Work and Schooling/cw09b_EN_3.0p.sav', use.value.labels=F, to.data.frame=T)
wed10 <- read.spss('data/raw/LISS/Work and Schooling/cw10c_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
wed11 <- read.spss('data/raw/LISS/Work and Schooling/cw11d_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
wed12 <- read.spss('data/raw/LISS/Work and Schooling/cw12e_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
wed13 <- read.spss('data/raw/LISS/Work and Schooling/cw13f_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
wed14 <- read.spss('data/raw/LISS/Work and Schooling/cw14g_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
wed15 <- read.spss('data/raw/LISS/Work and Schooling/cw15h_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
wed16 <- read.spss('data/raw/LISS/Work and Schooling/cw16i_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
wed17 <- read.spss('data/raw/LISS/Work and Schooling/cw17j_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
wed18 <- read.spss('data/raw/LISS/Work and Schooling/cw18k_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
wed19 <- read.spss('data/raw/LISS/Work and Schooling/cw19l_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)

#Make the names uniform by removing the wave qualifier (e.g., "08a")
names(wed08) <- gsub("08a", "", names(wed08))
names(wed09) <- gsub("09b", "", names(wed09))
names(wed10) <- gsub("10c", "", names(wed10))
names(wed11) <- gsub("11d", "", names(wed11))
names(wed12) <- gsub("12e", "", names(wed12))
names(wed13) <- gsub("13f", "", names(wed13))
names(wed14) <- gsub("14g", "", names(wed14))
names(wed15) <- gsub("15h", "", names(wed15))
names(wed16) <- gsub("16i", "", names(wed16))
names(wed17) <- gsub("17j", "", names(wed17))
names(wed18) <- gsub("18k", "", names(wed18))
names(wed19) <- gsub("19l", "", names(wed19))

# cw502 "Starting time questionnaire" is factor in some waves and numerical in others
# cw504 "End time questionnaire" 
# cw530 "Date of retirement (Dutch: AOW) based on year of birth / 66 years of age"
# --> remove these variables!
remove_timevar <- function(x) { 
  x %>%       
    select(-c(cw502, cw504))
}
listwed <- list(wed08, wed09, wed10, wed11, wed12, wed13, wed14, wed15, wed16, wed17, wed18, wed19) %>%
  lapply(remove_timevar)
names(listwed) <- c("wed08", "wed09", "wed10", "wed11", "wed12", "wed13", "wed14", 
                    "wed15", "wed16", "wed17", "wed18", "wed19")
list2env(listwed, .GlobalEnv)
wed11 <- wed11 %>% select(-cw530)
wed18 <- wed18 %>% select(-cw530)

#Merge them all together
wed.all <- bind_rows(wed08, wed09, wed10, wed11, wed12, wed13, wed14, wed15,
                     wed16, wed17, wed18, wed19)
length(unique(wed.all$nomem_encr))

rm(wed08, wed09, wed10, wed11, wed12, wed13, wed14, wed15,
   wed16, wed17, wed18, wed19)

#Rename "c_m" into "wave"
names(wed.all)[3] <- c("wave")
wed.all$year <- as.numeric(substr(wed.all$wave, 1, 4))
wed.all$wedmonth <- as.numeric(substr(wed.all$wave,5,6))
wed.all$wedtimeline <- (wed.all$year-2007)*12 + wed.all$wedmonth

#rename relevant variables
wed.all$paid_work <- wed.all$cw088 # I perform paid work (even if is it just for one or several hours per week or for a brief period)
wed.all$retire_early <- wed.all$cw098 # I have taken early retirement
wed.all$retirement   <- wed.all$cw099 # I am a pensioner
wed.all$more_paid_work <- wed.all$cw102 # I perform paid work, but am looking for more or other work
wed.all$disabled <- wed.all$cw100 # I am partly or wholly disabled for work

# if cw088=1 (paid work) or cw102=1 (paid work, looking for other job)
# Do you have children and/or grandchildren? (More than one answer permitted)
# Sadly, the grandchildren questions were not administered to the whole sample ...
wed.all$children   <- wed.all$cw436 # children
wed.all$grandchildren   <- wed.all$cw437 # grandchildren
wed.all$no_offspring   <- wed.all$cw438 # no children or grandchildren

wed.all$young_kids   <- wed.all$cw439 # Do you have children younger than 8 years?


# Are you currently working less hours in order to care for your children and/or
# grandchildren?
# If you have children and you are on parental leave, then consider the hours that you
# have additionally started working less (so in addition to the parental leave).
wed.all$work_less   <- wed.all$cw446

# For how many of your children and/or grandchildren do you care during the hours that
# you started working less?
# If a category is not applicable, please enter a 0 (zero).
wed.all$work_less_kids   <- wed.all$cw447 # number of children
wed.all$work_less_grandkids   <- wed.all$cw448 # number of grandchildren

# How many hours per week did you start working less on account of the care for your
# (grand) children? Do not include the hours that you have possibly taken as your parental
# leave.
wed.all$work_less_hours   <- wed.all$cw449

wed.all$informal_care   <- wed.all$cw450 # Do you provide informal care?

wed.all$informal_care_work_less   <- wed.all$cw451 # Are you currently working less hours because you provide informal care?

#select relevant variables: 
wed.all <- wed.all %>% 
  select(nomem_encr, year, wedmonth, wedtimeline,
         retire_early, retirement, paid_work, more_paid_work, disabled, 
         children, grandchildren, no_offspring, young_kids,
         contains("work_less"), contains("informal_care"))

### Income datasets 2008-2019 
inc08 <- read.spss('data/raw/LISS/Income/ci08a_1.0p_EN.sav', use.value.labels=F, to.data.frame=T)
inc09 <- read.spss('data/raw/LISS/Income/ci09b_EN_1.1p.sav', use.value.labels=F, to.data.frame=T)
inc10 <- read.spss('data/raw/LISS/Income/ci10c_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
inc11 <- read.spss('data/raw/LISS/Income/ci11d_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
inc12 <- read.spss('data/raw/LISS/Income/ci12e_1.0p_EN.sav', use.value.labels=F, to.data.frame=T)
inc13 <- read.spss('data/raw/LISS/Income/ci13f_1.1p_EN.sav', use.value.labels=F, to.data.frame=T)
inc14 <- read.spss('data/raw/LISS/Income/ci14g_1.0p_EN.sav', use.value.labels=F, to.data.frame=T)
inc15 <- read.spss('data/raw/LISS/Income/ci15h_EN_2.0p.sav', use.value.labels=F, to.data.frame=T)
inc16 <- read.spss('data/raw/LISS/Income/ci16i_EN_2.0p.sav', use.value.labels=F, to.data.frame=T)
inc17 <- read.spss('data/raw/LISS/Income/ci17j_EN_2.0p.sav', use.value.labels=F, to.data.frame=T)
inc18 <- read.spss('data/raw/LISS/Income/ci18k_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
inc19 <- read.spss('data/raw/LISS/Income/ci19l_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)

#Make the names uniform by removing the wave qualifier (e.g., "08a")
names(inc08) <- gsub("08a", "", names(inc08))
names(inc09) <- gsub("09b", "", names(inc09))
names(inc10) <- gsub("10c", "", names(inc10))
names(inc11) <- gsub("11d", "", names(inc11))
names(inc12) <- gsub("12e", "", names(inc12))
names(inc13) <- gsub("13f", "", names(inc13))
names(inc14) <- gsub("14g", "", names(inc14))
names(inc15) <- gsub("15h", "", names(inc15))
names(inc16) <- gsub("16i", "", names(inc16))
names(inc17) <- gsub("17j", "", names(inc17))
names(inc18) <- gsub("18k", "", names(inc18))
names(inc19) <- gsub("19l", "", names(inc19))

# ci319 "Starting time questionnaire" is factor in some waves and numerical in others
# ci321 "End time questionnaire" 
# --> remove these variables!
remove_timevar <- function(x) { 
  x %>%       
    select(-c(ci319, ci321))
}
listinc <- list(inc08, inc09, inc10, inc11, inc12, inc13, inc14, inc15, inc16, inc17, inc18, inc19) %>%
  lapply(remove_timevar)
names(listinc) <- c("inc08", "inc09", "inc10", "inc11", "inc12", "inc13", "inc14", 
                    "inc15", "inc16", "inc17", "inc18", "inc19")
list2env(listinc, .GlobalEnv)

#Merge them all together
inc.all <- bind_rows(inc08, inc09, inc10, inc11, inc12, inc13, inc14, inc15, 
                     inc16, inc17, inc18, inc19)

rm(inc08, inc09, inc10, inc11, inc12, inc13, inc14, inc15, 
   inc16, inc17, inc18, inc19)

#Rename "c_m" into "wave" to be consistent with "eve.all"
names(inc.all)[3] <- c("wave")

inc.all$year <- as.numeric(substr(inc.all$wave, 1, 4))
inc.all$incmonth <- as.numeric(substr(inc.all$wave,5,6))

# How would you describe the financial situation of your household at this moment?
inc.all$financialsit <- inc.all$ci252 # only Household head / Wedded partner / Unwedded partner
inc.all$difficultybills <- inc.all$ci292 # Were you in arrears on one or more bills on 31 December X? -> 1 = No

inc.all <- select(inc.all, nomem_encr, year, incmonth, financialsit, difficultybills)

### Housing datasets 2008-2019 
hom08 <- read.spss('data/raw/LISS/Housing/cd08a_EN_1.2p.sav', use.value.labels=F, to.data.frame=T)
hom09 <- read.spss('data/raw/LISS/Housing/cd09b_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
hom10 <- read.spss('data/raw/LISS/Housing/cd10c_EN_1.1p.sav', use.value.labels=F, to.data.frame=T)
hom11 <- read.spss('data/raw/LISS/Housing/cd11d_EN_2.0p.sav', use.value.labels=F, to.data.frame=T)
hom12 <- read.spss('data/raw/LISS/Housing/cd12e_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
hom13 <- read.spss('data/raw/LISS/Housing/cd13f_EN_1.1p.sav', use.value.labels=F, to.data.frame=T)
hom14 <- read.spss('data/raw/LISS/Housing/cd14g_EN_1.1p.sav', use.value.labels=F, to.data.frame=T)
hom15 <- read.spss('data/raw/LISS/Housing/cd15h_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
hom16 <- read.spss('data/raw/LISS/Housing/cd16i_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
hom17 <- read.spss('data/raw/LISS/Housing/cd17j_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
hom18 <- read.spss('data/raw/LISS/Housing/cd18k_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
hom19 <- read.spss('data/raw/LISS/Housing/cd19l_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)

#Make the names uniform by removing the wave qualifier (e.g., "08a")
names(hom08) <- gsub("08a", "", names(hom08))
names(hom09) <- gsub("09b", "", names(hom09))
names(hom10) <- gsub("10c", "", names(hom10))
names(hom11) <- gsub("11d", "", names(hom11))
names(hom12) <- gsub("12e", "", names(hom12))
names(hom13) <- gsub("13f", "", names(hom13))
names(hom14) <- gsub("14g", "", names(hom14))
names(hom15) <- gsub("15h", "", names(hom15))
names(hom16) <- gsub("16i", "", names(hom16))
names(hom17) <- gsub("17j", "", names(hom17))
names(hom18) <- gsub("18k", "", names(hom18))
names(hom19) <- gsub("19l", "", names(hom19))

# cd079 "Starting time questionnaire" is factor in some waves and numerical in others
# cd081 "End time questionnaire" 
# --> remove these variables!
remove_timevar <- function(x) { 
  x %>%       
    select(-c(cd079, cd081))
}
listhom <- list(hom08, hom09, hom10, hom11, hom12, hom13, hom14, hom15, hom16, hom17, hom18, hom19) %>%
  lapply(remove_timevar)
names(listhom) <- c("hom08", "hom09", "hom10", "hom11", "hom12", "hom13", "hom14", 
                    "hom15", "hom16", "hom17", "hom18", "hom19")
list2env(listhom, .GlobalEnv)

#Merge them all together
hom.all <- bind_rows(hom08, hom09, hom10, hom11, hom12, hom13, hom14, hom15, 
                     hom16, hom17, hom18, hom19)

rm(hom08, hom09, hom10, hom11, hom12, hom13, hom14, hom15, 
   hom16, hom17, hom18, hom19)

#Rename "c_m" into "wave" to be consistent with "eve.all"
names(hom.all)[3] <- c("wave")

hom.all$year <- as.numeric(substr(hom.all$wave, 1, 4))
hom.all$hommonth <- as.numeric(substr(hom.all$wave,5,6))

hom.all$rooms <- hom.all$cd034 # How many rooms does your dwelling contain?
hom.all$movedinyear <- hom.all$cd036 #In what year did you or your household take up residence in your current dwelling?
hom.all$typedwelling <- hom.all$cd038 # What type of dwelling do you inhabit?
hom.all$secondhouse <- hom.all$cd058 # Do you have (or rent) a second dwelling?

hom.all <- select(hom.all, nomem_encr, year, hommonth, rooms, movedinyear, 
                  typedwelling, secondhouse)

### Religion and Ethnicity datasets 2008-2018
rel08 <- read.spss('data/raw/LISS/Religion/cr08a_1p_EN.sav', use.value.labels=F, to.data.frame=T)
rel09 <- read.spss('data/raw/LISS/Religion/cr09b_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
rel10 <- read.spss('data/raw/LISS/Religion/cr10c_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
rel11 <- read.spss('data/raw/LISS/Religion/cr11d_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
rel12 <- read.spss('data/raw/LISS/Religion/cr12e_1.0p_EN.sav', use.value.labels=F, to.data.frame=T)
rel13 <- read.spss('data/raw/LISS/Religion/cr13f_1.0p_EN.sav', use.value.labels=F, to.data.frame=T)
rel14 <- read.spss('data/raw/LISS/Religion/cr14g_1.0p_EN.sav', use.value.labels=F, to.data.frame=T)
rel15 <- read.spss('data/raw/LISS/Religion/cr15h_1.0p_EN.sav', use.value.labels=F, to.data.frame=T)
rel16 <- read.spss('data/raw/LISS/Religion/cr16i_1.0p_EN.sav', use.value.labels=F, to.data.frame=T)
rel17 <- read.spss('data/raw/LISS/Religion/cr17j_EN_2.0p.sav', use.value.labels=F, to.data.frame=T)
rel18 <- read.spss('data/raw/LISS/Religion/cr18k_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
rel19 <- read.spss('data/raw/LISS/Religion/cr19l_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)

#Make the names uniform by removing the wave qualifier (e.g., "08a")
names(rel08) <- gsub("08a", "", names(rel08))
names(rel09) <- gsub("09b", "", names(rel09))
names(rel10) <- gsub("10c", "", names(rel10))
names(rel11) <- gsub("11d", "", names(rel11))
names(rel12) <- gsub("12e", "", names(rel12))
names(rel13) <- gsub("13f", "", names(rel13))
names(rel14) <- gsub("14g", "", names(rel14))
names(rel15) <- gsub("15h", "", names(rel15))
names(rel16) <- gsub("16i", "", names(rel16))
names(rel17) <- gsub("17j", "", names(rel17))
names(rel18) <- gsub("18k", "", names(rel18))
names(rel19) <- gsub("19l", "", names(rel19))

# cr117 "Starting time questionnaire" is factor in some waves and numerical in others
# cr119 "End time questionnaire" 
# --> remove these variables!
remove_timevar <- function(x) { 
  x %>%       
    select(-c(cr117, cr119))
}
listrel <- list(rel08, rel09, rel10, rel11, rel12, rel13, rel14, rel15, rel16, rel17, rel18, rel19) %>%
  lapply(remove_timevar)
names(listrel) <- c("rel08", "rel09", "rel10", "rel11", "rel12", "rel13", "rel14", 
                    "rel15", "rel16", "rel17", "rel18", "rel19")
list2env(listrel, .GlobalEnv)

#Merge them all together
rel.all <- bind_rows(rel08, rel09, rel10, rel11, rel12, rel13, rel14, rel15, 
                     rel16, rel17, rel18, rel19)

rm(rel08, rel09, rel10, rel11, rel12, rel13, rel14, rel15, 
   rel16, rel17, rel18, rel19)

#Rename "c_m" into "wave" to be consistent with "eve.all"
names(rel.all)[3] <- c("wave")

rel.all$year <- as.numeric(substr(rel.all$wave, 1, 4))
rel.all$relmonth <- as.numeric(substr(rel.all$wave,5,6))

rel.all$religion <- rel.all$cr012 # Do you consider yourself a member of a certain religion or church community? 
rel.all$religion2 <- rel.all$cr143 # Do you see yourself as belonging to a church community or religious group?
rel.all <- rel.all %>% mutate(religion = ifelse(year==2019, religion2, religion))

rel.all$speakdutch <- rel.all$cr089 # At home, do you generally speak Dutch or another language?

rel.all <- select(rel.all, nomem_encr, year, relmonth, religion, speakdutch)

### Family and Household datasets 2008-2019 (thanks to Ted Schwaba / Jaap Denissen for this code)
fam08 <- read.spss('data/raw/LISS/Family and Household/cf08a_2p_EN.sav', use.value.labels=F, to.data.frame=T)
fam09 <- read.spss('data/raw/LISS/Family and Household/cf09b_EN_2.2p.sav', use.value.labels=F, to.data.frame=T)
fam10 <- read.spss('data/raw/LISS/Family and Household/cf10c_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
fam11 <- read.spss('data/raw/LISS/Family and Household/cf11d_EN_1.1p.sav', use.value.labels=F, to.data.frame=T)
fam12 <- read.spss('data/raw/LISS/Family and Household/cf12e_EN_2.1p.sav', use.value.labels=F, to.data.frame=T)
fam13 <- read.spss('data/raw/LISS/Family and Household/cf13f_EN_1.1p.sav', use.value.labels=F, to.data.frame=T)
fam14 <- read.spss('data/raw/LISS/Family and Household/cf14g_EN_1.1p.sav', use.value.labels=F, to.data.frame=T)
fam15 <- read.spss('data/raw/LISS/Family and Household/cf15h_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
fam16 <- read.spss('data/raw/LISS/Family and Household/cf16i_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
fam17 <- read.spss('data/raw/LISS/Family and Household/cf17j_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
fam18 <- read.spss('data/raw/LISS/Family and Household/cf18k_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
#fam19 -> not needed because matching in wave before transition # NOW AVAILABLE

#Make the names uniform by removing the wave qualifier (e.g., "08a")
names(fam08) <- gsub("08a", "", names(fam08))
names(fam09) <- gsub("09b", "", names(fam09))
names(fam10) <- gsub("10c", "", names(fam10))
names(fam11) <- gsub("11d", "", names(fam11))
names(fam12) <- gsub("12e", "", names(fam12))
names(fam13) <- gsub("13f", "", names(fam13))
names(fam14) <- gsub("14g", "", names(fam14))
names(fam15) <- gsub("15h", "", names(fam15))
names(fam16) <- gsub("16i", "", names(fam16))
names(fam17) <- gsub("17j", "", names(fam17))
names(fam18) <- gsub("18k", "", names(fam18))

# cf394 "Starting time questionnaire" is factor in some waves and numerical in others
# cf396 "End time questionnaire" 
# --> remove these variables!
remove_timevar <- function(x) { 
  x %>%       
    select(-c(cf394, cf396))
}
listfam <- list(fam08, fam09, fam10, fam11, fam12, fam13, fam14, fam15, fam16, fam17, fam18) %>%
  lapply(remove_timevar)
names(listfam) <- c("fam08", "fam09", "fam10", "fam11", "fam12", "fam13", "fam14", 
                    "fam15", "fam16", "fam17", "fam18")
list2env(listfam, .GlobalEnv)

#Merge them all together
fam.all <- bind_rows(fam08, fam09, fam10, fam11, fam12, fam13, fam14, fam15, fam16, fam17, fam18)

rm(fam08, fam09, fam10, fam11, fam12, fam13, fam14, fam15, fam16, fam17, fam18)

#Rename "c_m" into "wave" to be consistent with "eve.all"
names(fam.all)[3] <- c("wave")
fam.all$year <- as.numeric(substr(fam.all$wave, 1, 4))
fam.all$fammonth <- as.numeric(substr(fam.all$wave,5,6))

fam.all$currentpartner <- 2-fam.all$cf024 # Do you currently have a partner?
fam.all$livetogether <- 2-fam.all$cf025 # Do you live together with this partner? (filtered!)
fam.all <- fam.all %>% mutate(livetogether = ifelse(is.na(livetogether) & currentpartner==0, 0, livetogether))

#MK: Coding (living) children - Unfortunately, we have to filter out deceased children first, because
#    they were included in the birth biography from 2008-2014.

# These variables can stay the way they are because deceased children are already excluded.
# I'll create copies of these variables for later merge.
fam.all <- fam.all %>% arrange(nomem_encr, year) %>% 
  mutate(
    kidyearnew_1 = cf456, # birth year kids (2015-2018)
    kidyearnew_2 = cf457, 
    kidyearnew_3 = cf458, 
    kidyearnew_4 = cf459, 
    kidyearnew_5 = cf460, 
    kidyearnew_6 = cf461, 
    kidyearnew_7 = cf462, 
    kidyearnew_8 = cf463, 
    kidyearnew_9 = cf464,
    childrennew = cf454, # IF NEW IN PANEL: Did you ever have any children? Please do not include any deceased children. (1=yes, 2=no)
    totalchildrennew = cf455) # How many living children do you have in total? (filtered!)

# Filtering done from 2008 - 2014:
# Last year you indicated that you had [cf011d036] children.
# This concerns biological children (which you had with your partner or someone else) 
# as well as stepchildren, adoptive children and foster children.
# This also includes any deceased children.  <-----PROBLEM
# Has this number changed since last year?

# Handle deceased children 
# filtering variables: cf052, cf406 (2=have a deceased child) - both are used, no overlap
# child deceased=1: cf053, cf054, cf055, cf056, cf057, cf058, cf059, cf060, cf061 
# (cf062-cf067 have all NA)

# Recode for years 2008-2014

# recode birth biography:
# Reshape to a format where every observation (person-year) has 9 rows (one for each potential kid)
fam.reshape <- fam.all %>% arrange(nomem_encr, year) %>% filter(year %in% c(2008:2014)) %>% 
  rename(
  kiddied_1 = cf053, #2008-2014
  kiddied_2 = cf054,
  kiddied_3 = cf055,
  kiddied_4 = cf056,
  kiddied_5 = cf057,
  kiddied_6 = cf058,
  kiddied_7 = cf059,
  kiddied_8 = cf060,
  kiddied_9 = cf061,
  kidyearold_1 = cf037, #2008-2014
  kidyearold_2 = cf038, 
  kidyearold_3 = cf039, 
  kidyearold_4 = cf040, 
  kidyearold_5 = cf041, 
  kidyearold_6 = cf042, 
  kidyearold_7 = cf043, 
  kidyearold_8 = cf044, 
  kidyearold_9 = cf045,
  kidgender_1 = cf068, #2008-2018
  kidgender_2 = cf069, 
  kidgender_3 = cf070, 
  kidgender_4 = cf071, 
  kidgender_5 = cf072, 
  kidgender_6 = cf073, 
  kidgender_7 = cf074, 
  kidgender_8 = cf075, 
  kidgender_9 = cf076, 
  kidhome_1 = cf083, #2008-2018
  kidhome_2 = cf084, 
  kidhome_3 = cf085, 
  kidhome_4 = cf086, 
  kidhome_5 = cf087, 
  kidhome_6 = cf088, 
  kidhome_7 = cf089, 
  kidhome_8 = cf090, 
  kidhome_9 = cf091,
  childrenold = cf035, # Have you had any children? Please include any deceased children as well. (2008-2014)
  totalchildrenold = cf036) %>% # How many children have you had in total? (2008-2014)
  select(nomem_encr, year, childrenold, totalchildrenold, starts_with("kiddied"), 
         starts_with("kidyearold"), starts_with("kidgender"), starts_with("kidhome"))

# Sometimes, there is a package conflict here, and I need to re-install 'tidyverse'
# in order to use the fill() function
#install.packages("tidyverse")

# use fill() to replace missing values for 'kiddied' variables in subsequent years
# https://tidyr.tidyverse.org/reference/fill.html
# I've looked at the cases - filling in both up & down is correct for our situation!
fam.reshape <- fam.reshape %>% group_by(nomem_encr) %>% 
  fill(starts_with("kiddied"), .direction = "downup") %>% ungroup()

# now count number of deceased children per respondent with rowwise() and c_across()
# https://dplyr.tidyverse.org/articles/rowwise.html
# https://dplyr.tidyverse.org/reference/across.html
fam.reshape <- fam.reshape %>% 
  rowwise() %>% mutate(sumdec = sum(c_across(starts_with("kiddied")), na.rm = T)) %>% 
  ungroup()

# adjust 'children' and 'totalchildren'
fam.reshape <- fam.reshape %>% 
  mutate(totalchildrenold = ifelse(!is.na(totalchildrenold), totalchildrenold - sumdec,
                                          totalchildrenold),
                childrenold = ifelse(childrenold==1 & totalchildrenold==0, 2, childrenold))
table(fam.reshape$sumdec) # 2036 deceased kids
# 1504 + (2*177) + (3*38) + (4*16)

# reshape in order to drop deceased children
fam.reshape <- fam.reshape %>% pivot_longer(cols = starts_with("kid"),
                                            names_to = c(".value", "kidnr"),
                                            names_pattern = "(.*)_(.)")
table(fam.reshape$kiddied) # 2036 deceased kids

fam.reshape <- fam.reshape %>% filter(is.na(kiddied)) %>% # drop deceased kids
  group_by(nomem_encr, year) %>% mutate(kidnr = row_number()) %>% # recount kids
  ungroup() %>% select(-kiddied) # all NA

# reshape back to person-year format
fam.reshape <- fam.reshape %>% pivot_wider(names_from = kidnr,
                                           values_from = c(kidyearold, kidgender, kidhome),
                                           names_glue = "{.value}_{kidnr}")
summary(fam.reshape) # 43167 person-years

# merge both datasets 
fam.all <- left_join(fam.all, fam.reshape)
summary(fam.all$sumdec) # 43167 

# create longitudinal variables for 2008-2018

fam.all <- fam.all %>% 
  mutate(children = ifelse(year %in% c(2008:2014), 2-childrenold, 2-childrennew), # children dummy (1=yes)
         totalchildren = ifelse(year %in% c(2008:2014), totalchildrenold, totalchildrennew)) # count children

# both variables still have considerable longitudinal gaps because the questions 
# were not asked if no change in number of kids was reported -> fill in gaps
fam.all <- fam.all %>% group_by(nomem_encr) %>% 
  fill(c(children, totalchildren), .direction = "down") %>% ungroup()

# some cases have children==NA while also totalchildren>=1
# others have children==0 while also totalchildren==NA
fam.all <- fam.all %>% 
  mutate(children = ifelse(is.na(children) & totalchildren>=1, 1, children),
         totalchildren = ifelse(is.na(totalchildren) & children==0, 0, totalchildren))

# birth year children 1-3 (only LIVING children)
# from 2008-2014: cf037, cf038, cf039 -> included deceased children -> corrected versions in kidyearold_
# from 2015-2018: cf456, cf457, cf458 -> renamed to kidyearnew_
fam.all <- fam.all %>% mutate(kid1byear = ifelse(year %in% c(2015:2018), kidyearnew_1, kidyearold_1),
                              kid2byear = ifelse(year %in% c(2015:2018), kidyearnew_2, kidyearold_2),
                              kid3byear = ifelse(year %in% c(2015:2018), kidyearnew_3, kidyearold_3))

fam.all %>% filter(!is.na(kid1byear) & !is.na(kid2byear)) %>% 
  mutate(diff = kid2byear-kid1byear) %>% group_by(diff) %>% 
  summarise(n=n(), N=n_distinct(nomem_encr)) %>% print(n=50)
# mostly positive values (=older kid1), but a few negative ones (kid2 older than kid1)
# possibly other (step) children who entered the household at a later stage
# -> we correct this below by re-ordering!

# gender children 1-3  (1=boy, 2=girl)
# from 2008-2018 cf068, cf069, cf070
# However, values from 2008-2014 were corrected above by removing deceased children. Although
# gender (& living-at-home) was never asked regarding deceased children, the positioning was wrong
# which created (incorrect) missing values.
fam.all <- fam.all %>% mutate(kid1gender = ifelse(year %in% c(2015:2018), cf068, kidgender_1),
                              kid2gender = ifelse(year %in% c(2015:2018), cf069, kidgender_2),
                              kid3gender = ifelse(year %in% c(2015:2018), cf070, kidgender_3))

# living at home children 1-3  (1=living at home, 2=living independently)
# from 2008-2018 cf083, cf084, cf085
# --> see above
fam.all <- fam.all %>% mutate(kid1home = ifelse(year %in% c(2015:2018), cf083, kidhome_1),
                              kid2home = ifelse(year %in% c(2015:2018), cf084, kidhome_2),
                              kid3home = ifelse(year %in% c(2015:2018), cf085, kidhome_3))

fam.all %>% filter(is.na(totalchildren)) %>%
  select(nomem_encr, year, children, totalchildren, 
         contains(c("kid1", "kid2", "kid3"))) %>% print(n=100) # looks good
fam.all %>% filter(totalchildren==0) %>%
  select(nomem_encr, year, children, totalchildren, 
         contains(c("kid1", "kid2", "kid3"))) %>% print(n=200) # also looks good
fam.all %>% filter(totalchildren>=1) %>%
  select(nomem_encr, year, children, totalchildren, 
         contains(c("kid1", "kid2", "kid3"))) %>% print(n=300) # no inconsistencies

# are there any respondents who report kid2 but no kid1?
fam.all %>% arrange(nomem_encr, year) %>% filter(is.na(kid1byear) & !is.na(kid2byear)) %>%
  select(nomem_encr, year, children, totalchildren, 
         contains(c("kid1", "kid2", "kid3"))) %>% print(n=100)
# 1 respondent 896685 -> longitudinal gap that can be corrected
fam.all %>% arrange(nomem_encr, year) %>% filter(nomem_encr==896685) %>% 
  select(nomem_encr, year, children, totalchildren, 
         contains(c("kid1", "kid2", "kid3"))) %>% print(width=Inf)
fam.all <- fam.all %>% mutate(
  kid1byear = ifelse(is.na(kid1byear) & nomem_encr==896685 & year==2014, 1964, kid1byear), 
  kid1gender = ifelse(is.na(kid1gender) & nomem_encr==896685 & year==2014, 2, kid1gender), 
  kid1home = ifelse(is.na(kid1home) & nomem_encr==896685 & year==2014, 2, kid1home), 
)

fam.all %>% filter(totalchildren>=1) %>% group_by(kid1byear) %>% summarise(n=n()) %>% print(n=Inf)
# 279 NAs in kid1byear
fam.all %>% filter(totalchildren>=1 & is.na(kid1byear)) %>%
  select(nomem_encr, year, totalchildren, 
         contains(c("kid1", "kid2", "kid3"))) %>% print(n=100)
# pick a few examples to check longitudinally 
fam.all %>% filter(nomem_encr %in% c(814129, 823611, 862847, 882214)) %>%
  select(nomem_encr, year, totalchildren, 
         contains(c("kid1", "kid2", "kid3"))) %>% print(n=100)
# for some of these missings information on children can be inferred longitudinally 
# but maybe this is in too broad strokes... few missings anyways!

# re-order kid1, kid2, kid3 by birth year
fam.reshape2 <- fam.all %>% select(nomem_encr, year, contains(c("kid1", "kid2", "kid3")))

fam.reshape2 <- fam.reshape2 %>% pivot_longer(cols = starts_with("kid"),
                                              names_to = c("kidnr", ".value"),
                                              names_pattern = "kid(.)(.*)")

fam.reshape2 <- fam.reshape2 %>% mutate(byear = replace(byear, is.na(byear), 9999)) %>% 
  arrange(nomem_encr, year, byear) %>% #reorder
  group_by(nomem_encr, year) %>% mutate(kidnr = row_number()) %>% ungroup() %>% # recount kids
  mutate(byear = replace(byear, byear==9999, NA))

# reshape back to person-year format
fam.reshape2 <- fam.reshape2 %>% pivot_wider(names_from = kidnr,
                                             values_from = c(byear, gender, home),
                                             names_glue = "kid{kidnr}{.value}")

# merge re-ordered variables
fam.all <- fam.all %>% select(-contains(c("kid1", "kid2", "kid3")))
fam.all <- left_join(fam.all, fam.reshape2)

# no older kid2's, anymore (yay)
fam.all %>% filter(!is.na(kid1byear) & !is.na(kid2byear)) %>% 
  mutate(diff = kid2byear-kid1byear) %>% group_by(diff) %>% 
  summarise(n=n(), N=n_distinct(nomem_encr)) %>% print(n=50)

# flag if parent only has 1 or 2 children -> we will use 'totalchildren' to denote parity 
# -> some with totalchildren==0 have a valid value in kid1age -> include as condition for 'nokids'
fam.all <- fam.all %>% mutate(nokids =    ifelse(!is.na(totalchildren), ifelse(totalchildren==0 &
                                                                               is.na(kid1byear) , 1, 0), NA),
                              secondkid = ifelse(!is.na(totalchildren), ifelse(totalchildren>=2, 1, 0), NA),
                              thirdkid  = ifelse(!is.na(totalchildren), ifelse(totalchildren>=3, 1, 0), NA)) %>% 
  mutate_at(vars(contains("kid1")), funs(ifelse(is.na(.) & nokids==1, 0, .))) %>% 
  mutate_at(vars(contains("kid2")), funs(ifelse(is.na(.) & secondkid==0, 0, .))) %>% 
  mutate_at(vars(contains("kid3")), funs(ifelse(is.na(.) & thirdkid==0, 0, .)))

fam.all %>% filter(is.na(kid1gender)) %>%
  select(nomem_encr, year, totalchildren, 
         contains(c("kid1", "kid2", "kid3"))) %>% print(n=100) # look like genuine missings
fam.all %>% filter(is.na(kid1gender)) %>% group_by(year) %>% summarise(n=n())
# no missings due to deceased children, anymore

fam.all <- fam.all %>% select(nomem_encr, year, fammonth, currentpartner, livetogether,
                              totalchildren, contains(c("kid1", "kid2", "kid3")),
                              nokids, secondkid, thirdkid)

### Health datasets 2007-2018
#hel07 <- read.spss('data/raw/LISS/Health/ch07a_2p_EN.sav', use.value.labels=F, to.data.frame=T)
hel08 <- read.spss('data/raw/LISS/Health/ch08b_EN_1.3p.sav', use.value.labels=F, to.data.frame=T)
hel09 <- read.spss('data/raw/LISS/Health/ch09c_EN_1.1p.sav', use.value.labels=F, to.data.frame=T)
hel10 <- read.spss('data/raw/LISS/Health/ch10d_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
hel11 <- read.spss('data/raw/LISS/Health/ch11e_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
hel12 <- read.spss('data/raw/LISS/Health/ch12f_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
hel13 <- read.spss('data/raw/LISS/Health/ch13g_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
#hel14 <- read.spss('data/raw/LISS/Health/ch14g_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
hel15 <- read.spss('data/raw/LISS/Health/ch15h_EN_1.2p.sav', use.value.labels=F, to.data.frame=T)
hel16 <- read.spss('data/raw/LISS/Health/ch16i_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
hel17 <- read.spss('data/raw/LISS/Health/ch17j_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
hel18 <- read.spss('data/raw/LISS/Health/ch18k_EN_1.0p.sav', use.value.labels=F, to.data.frame=T)
#hel19 -> not needed because matching in wave before transition - NOW AVAILABLE

#Make the names uniform by removing the wave qualifier (e.g., "08a")
#names(hel07) <- gsub("07a", "", names(hel07))
names(hel08) <- gsub("08b", "", names(hel08))
names(hel09) <- gsub("09c", "", names(hel09))
names(hel10) <- gsub("10d", "", names(hel10))
names(hel11) <- gsub("11e", "", names(hel11))
names(hel12) <- gsub("12f", "", names(hel12))
names(hel13) <- gsub("13g", "", names(hel13))
#names(hel14) <- gsub("14g", "", names(hel14))
names(hel15) <- gsub("15h", "", names(hel15))
names(hel16) <- gsub("16i", "", names(hel16))
names(hel17) <- gsub("17j", "", names(hel17))
names(hel18) <- gsub("18k", "", names(hel18))

# ch256 "Starting time questionnaire" is factor in some waves and numerical in others
# ch258 "End time questionnaire" 
# --> remove these variables!
remove_timevar <- function(x) { 
  x %>%       
    select(-c(ch256, ch258))
}
listhel <- list(hel08, hel09, hel10, hel11, hel12, hel13, hel15, hel16, hel17, hel18) %>%
  lapply(remove_timevar)
names(listhel) <- c("hel08", "hel09", "hel10", "hel11", "hel12", "hel13", 
                    "hel15", "hel16", "hel17", "hel18")
list2env(listhel, .GlobalEnv)

#Merge them all together
hel.all <- bind_rows(hel08, hel09, hel10, hel11, hel12, 
                     hel13, hel15, hel16, hel17, hel18)

rm(hel08, hel09, hel10, hel11, hel12, 
   hel13, hel15, hel16, hel17, hel18)

#remove list objects, too
rm(listper, listwed, listinc, listrel, listhom, listfam, listhel)

#Rename "c_m" into "wave" to be consistent with "eve.all"
names(hel.all)[3] <- c("wave")
hel.all$year <- as.numeric(substr(hel.all$wave, 1, 4))
hel.all$helmonth <- as.numeric(substr(hel.all$wave,5,6))

hel.all$subjhealth <- hel.all$ch004 # unfiltered

hel.all$bmi <- hel.all$ch017/((hel.all$ch016/100)^2)
hel.all[which(hel.all$bmi > 60 | hel.all$bmi < 5),]$bmi <- NA

hel.all$chronicdisease <- hel.all$ch018  # all unfiltered
hel.all$heartattack <- hel.all$ch081
hel.all$stroke <- hel.all$ch084
hel.all$cancer <- hel.all$ch089
hel.all$diabetes <- hel.all$ch085
hel.all$nodisease <- hel.all$ch098

mobility_items <- c("ch023", "ch027", "ch041")
hel.all$mobility <- rowMeans(hel.all[mobility_items], na.rm = TRUE)

hel.all$dep01 <- hel.all$ch011
hel.all$dep02 <- hel.all$ch012
hel.all$dep03 <- 7-hel.all$ch013
hel.all$dep04 <- hel.all$ch014
hel.all$dep05 <- 7-hel.all$ch015

dep_items <- c("dep01", "dep02", "dep03", "dep04", "dep05")
hel.all$dep <- rowMeans(hel.all[dep_items], na.rm = TRUE)

hel.all <- select(hel.all, nomem_encr, year, helmonth, subjhealth, bmi, chronicdisease,
                  heartattack, stroke, cancer, diabetes, nodisease, mobility, dep)

#### LISS data: merge datasets ####

# start with background variables (no missings in 'nohouse_encr')
merged_liss <- left_join(eve.all, per.all, by=c("nomem_encr", "year"))

# merge other datasets
merged_liss <- left_join(merged_liss, wed.all, by=c("nomem_encr", "year"))
merged_liss <- left_join(merged_liss, inc.all, by=c("nomem_encr", "year"))
merged_liss <- left_join(merged_liss, hom.all, by=c("nomem_encr", "year")) # need to merge first to get 'nohouse_encr'
merged_liss <- left_join(merged_liss, rel.all, by=c("nomem_encr", "year"))
merged_liss <- left_join(merged_liss, fam.all, by=c("nomem_encr", "year"))
merged_liss <- left_join(merged_liss, hel.all, by=c("nomem_encr", "year"))

# these questions were only answered by the hh head and pertain to the whole hh
liss_housing_merge <- merged_liss %>% 
  filter(!is.na(rooms) | !is.na(movedinyear) | !is.na(typedwelling) | !is.na(secondhouse)) %>% 
  select(nomem_encr, nohouse_encr, year, rooms, movedinyear, typedwelling, secondhouse)
# however, some hh have multiple entries which creates come unwanted duplicates later
liss_housing_merge <- liss_housing_merge %>% group_by(nohouse_encr, year) %>% 
  mutate(duplicates = n()) %>% ungroup()
# split into two versions, one identified by 'nohouse_encr' & 'year', and the 
# other by 'nomem_encr' and 'year'
liss_housing_merge_hh <- liss_housing_merge %>% 
  filter(duplicates==1) %>% select(-nomem_encr, -duplicates)
liss_housing_merge_pp <- liss_housing_merge %>% 
  filter(duplicates>1) %>% select(-nohouse_encr, -duplicates)

############# code more elegantly with 'mutate_at'

merged_liss <- merged_liss %>% select(-rooms, -movedinyear, -typedwelling, -secondhouse)
merged_liss <- left_join(merged_liss, liss_housing_merge_hh) #merge by 'nohouse_encr'
merged_liss <- left_join(merged_liss, liss_housing_merge_pp, by=c("nomem_encr", "year"))
merged_liss <- merged_liss %>% mutate(
  rooms = ifelse(!is.na(rooms.x), rooms.x, rooms.y),
  movedinyear = ifelse(!is.na(movedinyear.x), movedinyear.x, movedinyear.y),
  typedwelling = ifelse(!is.na(typedwelling.x), typedwelling.x, typedwelling.y),
  secondhouse = ifelse(!is.na(secondhouse.x), secondhouse.x, secondhouse.y)
) %>% select(-starts_with("rooms."), -starts_with("movedinyear."), 
             -starts_with("typedwelling."), -starts_with("secondhouse."), )

# to determine the final N, two datasets are relevant: 
merged_liss <- merged_liss %>% mutate(validper = ifelse(!is.na(pertimeline), 1, 0),
                                      validwed = ifelse(!is.na(wedtimeline), 1, 0),
                                      validfam = ifelse(!is.na(fammonth), 1, 0))
table(merged_liss$validper, merged_liss$validwed)
table(merged_liss$validper, merged_liss$validfam)
table(merged_liss$validwed, merged_liss$validfam)

# for grandparenthood coding we rely on variables from the Work and Education datasets 
liss_grand <- merged_liss %>% filter(validwed==1) %>% select(nomem_encr, year, grandchildren)
liss_grand %>% group_by(grandchildren) %>% summarise(n()) 

# however, outcome variables are in the Personality datasets
lisslong <- merged_liss %>% filter(validper==1)
lisslong %>% group_by(grandchildren) %>% 
  summarise(n = n(), N = n_distinct(nomem_encr), meanage = mean(age, na.rm=T)) 
lisslong %>% filter(validfam==1) %>% group_by(grandchildren) %>% 
  summarise(n = n(), N = n_distinct(nomem_encr), meanage = mean(age, na.rm=T)) 
# we will loose a few more cases due to missing Family information 

lisslong %>% 
  group_by(year) %>% 
  filter(year!=2016) %>% 
  dplyr::summarise(
    A_count = sum(!is.na(agree)),
    A_meano = mean(agree, na.rm=T),
    A_sd = sd(agree, na.rm=T),
    C_count = sum(!is.na(con)),
    C_meano = mean(con, na.rm=T),
    C_sd = sd(con, na.rm=T),
    N_count = sum(!is.na(neur)),
    N_meano = mean(neur, na.rm=T),
    N_sd = sd(neur, na.rm=T),
    E_count = sum(!is.na(extra)),
    E_meano = mean(extra, na.rm=T),
    E_sd = sd(extra, na.rm=T),
    O_count = sum(!is.na(open)),
    O_meano = mean(open, na.rm=T),
    O_sd = sd(open, na.rm=T),
    swls_count = sum(!is.na(swls)),
    swls_meano = mean(swls, na.rm=T),
    swls_sd = sd(swls, na.rm=T)
  ) %>% 
  pivot_longer(cols = -year, 
               names_to = c(".value", "neurt"),
               names_pattern = "(.*)_(.*)") %>% 
  pivot_longer(cols = (A:swls),
               names_to = "outcome",
               values_to = "val") %>% 
  filter(neurt=="meano") %>% 
  print(n=Inf) %>% 
  ggplot(mapping = aes(y=val, x=outcome, fill=factor(year))) +
  geom_bar(position="dodge", stat="identity")


#### identify transitions to grandparenthood ####
liss_grand %>% group_by(year) %>% 
  dplyr::summarise(mean = mean(grandchildren, na.rm = T))

liss_grand_wide <- liss_grand %>% select(nomem_encr, year, grandchildren) %>% 
  arrange(nomem_encr, year) %>% 
  pivot_wider(names_from = year,
              names_prefix = "grandchildren_",
              values_from = grandchildren
  ) %>% select(sort(tidyselect::peek_vars())) %>% glimpse()

liss_grand_wide %>% select(starts_with("grandchildren")) %>% psych::describe()

# first step: code all the 0 to 1 transitions
liss_grand_wide <- liss_grand_wide %>% mutate(
  transit2009 = ifelse(is.na(grandchildren_2008) | is.na(grandchildren_2009), 0,
                       ifelse(grandchildren_2008==0 & grandchildren_2009==1, 1, 0)),
  transit2010 = ifelse(is.na(grandchildren_2009) | is.na(grandchildren_2010), 0,
                       ifelse(grandchildren_2009==0 & grandchildren_2010==1, 1, 0)),
  transit2011 = ifelse(is.na(grandchildren_2010) | is.na(grandchildren_2011), 0,
                       ifelse(grandchildren_2010==0 & grandchildren_2011==1, 1, 0)),
  transit2012 = ifelse(is.na(grandchildren_2011) | is.na(grandchildren_2012), 0,
                       ifelse(grandchildren_2011==0 & grandchildren_2012==1, 1, 0)),
  transit2013 = ifelse(is.na(grandchildren_2012) | is.na(grandchildren_2013), 0,
                       ifelse(grandchildren_2012==0 & grandchildren_2013==1, 1, 0)),
  transit2014 = ifelse(is.na(grandchildren_2013) | is.na(grandchildren_2014), 0,
                       ifelse(grandchildren_2013==0 & grandchildren_2014==1, 1, 0)),
  transit2015 = ifelse(is.na(grandchildren_2014) | is.na(grandchildren_2015), 0,
                       ifelse(grandchildren_2014==0 & grandchildren_2015==1, 1, 0)),
  transit2016 = ifelse(is.na(grandchildren_2015) | is.na(grandchildren_2016), 0,
                       ifelse(grandchildren_2015==0 & grandchildren_2016==1, 1, 0)),
  transit2017 = ifelse(is.na(grandchildren_2016) | is.na(grandchildren_2017), 0,
                       ifelse(grandchildren_2016==0 & grandchildren_2017==1, 1, 0)),
  transit2018 = ifelse(is.na(grandchildren_2017) | is.na(grandchildren_2018), 0,
                       ifelse(grandchildren_2017==0 & grandchildren_2018==1, 1, 0)),
  transit2019 = ifelse(is.na(grandchildren_2018) | is.na(grandchildren_2019), 0,
                       ifelse(grandchildren_2018==0 & grandchildren_2019==1, 1, 0)))

# second step: code all the 1 to 0 transitions (inconsistent longitudinal data)
liss_grand_wide <- liss_grand_wide %>% mutate(
  goback2009 = ifelse(is.na(grandchildren_2008) | is.na(grandchildren_2009), 0,
                       ifelse(grandchildren_2008==1 & grandchildren_2009==0, 1, 0)),
  goback2010 = ifelse(is.na(grandchildren_2009) | is.na(grandchildren_2010), 0,
                       ifelse(grandchildren_2009==1 & grandchildren_2010==0, 1, 0)),
  goback2011 = ifelse(is.na(grandchildren_2010) | is.na(grandchildren_2011), 0,
                       ifelse(grandchildren_2010==1 & grandchildren_2011==0, 1, 0)),
  goback2012 = ifelse(is.na(grandchildren_2011) | is.na(grandchildren_2012), 0,
                       ifelse(grandchildren_2011==1 & grandchildren_2012==0, 1, 0)),
  goback2013 = ifelse(is.na(grandchildren_2012) | is.na(grandchildren_2013), 0,
                       ifelse(grandchildren_2012==1 & grandchildren_2013==0, 1, 0)),
  goback2014 = ifelse(is.na(grandchildren_2013) | is.na(grandchildren_2014), 0,
                       ifelse(grandchildren_2013==1 & grandchildren_2014==0, 1, 0)),
  goback2015 = ifelse(is.na(grandchildren_2014) | is.na(grandchildren_2015), 0,
                       ifelse(grandchildren_2014==1 & grandchildren_2015==0, 1, 0)),
  goback2016 = ifelse(is.na(grandchildren_2015) | is.na(grandchildren_2016), 0,
                       ifelse(grandchildren_2015==1 & grandchildren_2016==0, 1, 0)),
  goback2017 = ifelse(is.na(grandchildren_2016) | is.na(grandchildren_2017), 0,
                       ifelse(grandchildren_2016==1 & grandchildren_2017==0, 1, 0)),
  goback2018 = ifelse(is.na(grandchildren_2017) | is.na(grandchildren_2018), 0,
                       ifelse(grandchildren_2017==1 & grandchildren_2018==0, 1, 0)),
  goback2019 = ifelse(is.na(grandchildren_2018) | is.na(grandchildren_2019), 0,
                       ifelse(grandchildren_2018==1 & grandchildren_2019==0, 1, 0)))

# third step: count both per person
liss_grand_wide <- liss_grand_wide %>% 
  mutate(
    sum_transit = rowSums(select(., starts_with("transit"))),
    sum_goback  = rowSums(select(., starts_with("goback"))))

# identify eligible grandparents (to-be): respondents with exactly 1 transition to 
# grandparenthood and no subsequent transitions back
liss_grand_wide %>% filter(sum_transit==1) %>% group_by(sum_goback) %>% summarise(n = n()) # N=343

# identify eligible non-grandparents: respondents starting out with no grandchildren 
# with 0 transitions throughout the observation period
liss_grand_wide %>% 
  filter(grandchildren_2008 %in% c(0, NA) & grandchildren_2009 %in% c(0, NA) & 
         grandchildren_2010 %in% c(0, NA) & grandchildren_2011 %in% c(0, NA) & 
         grandchildren_2012 %in% c(0, NA) & grandchildren_2013 %in% c(0, NA) & 
         grandchildren_2014 %in% c(0, NA) & grandchildren_2015 %in% c(0, NA) & 
         grandchildren_2016 %in% c(0, NA) & grandchildren_2017 %in% c(0, NA) & 
         grandchildren_2018 %in% c(0, NA) & grandchildren_2019 %in% c(0, NA)) %>% 
  group_by(sum_transit) %>% summarise(n()) # N=13385

#code variable to denote eligiblity
liss_grand_wide <- liss_grand_wide %>% 
  mutate(
    grandparent = ifelse(sum_transit==1 & sum_goback==0, 1, ifelse(
      (grandchildren_2008 %in% c(0, NA) & grandchildren_2009 %in% c(0, NA) & 
       grandchildren_2010 %in% c(0, NA) & grandchildren_2011 %in% c(0, NA) & 
       grandchildren_2012 %in% c(0, NA) & grandchildren_2013 %in% c(0, NA) & 
       grandchildren_2014 %in% c(0, NA) & grandchildren_2015 %in% c(0, NA) & 
       grandchildren_2016 %in% c(0, NA) & grandchildren_2017 %in% c(0, NA) & 
       grandchildren_2018 %in% c(0, NA) & grandchildren_2019 %in% c(0, NA)),
      0, NA))
    )
table(liss_grand_wide$grandparent)

#problem: we still have a few respondents with missing data who go back to 0 after a 1, 
#e.g. 846987, 848046
liss_grand_wide %>% filter(grandparent==1 & grandchildren_2019==0) %>% 
  select(nomem_encr, starts_with("grandchildren")) %>% print(width=Inf)

#recode for 1-year-gap
liss_grand_wide <- liss_grand_wide %>% mutate(
  goback2010 = replace(goback2010, grandchildren_2008==1 & is.na(grandchildren_2009) &
                         grandchildren_2010==0, 1),
  goback2011 = replace(goback2011, grandchildren_2009==1 & is.na(grandchildren_2010) &
                         grandchildren_2011==0, 1),
  goback2012 = replace(goback2012, grandchildren_2010==1 & is.na(grandchildren_2011) &
                         grandchildren_2012==0, 1),
  goback2013 = replace(goback2013, grandchildren_2011==1 & is.na(grandchildren_2012) &
                         grandchildren_2013==0, 1),
  goback2014 = replace(goback2014, grandchildren_2012==1 & is.na(grandchildren_2013) &
                         grandchildren_2014==0, 1),
  goback2015 = replace(goback2015, grandchildren_2013==1 & is.na(grandchildren_2014) &
                         grandchildren_2015==0, 1),
  goback2016 = replace(goback2016, grandchildren_2014==1 & is.na(grandchildren_2015) &
                         grandchildren_2016==0, 1),
  goback2017 = replace(goback2017, grandchildren_2015==1 & is.na(grandchildren_2016) &
                         grandchildren_2017==0, 1),
  goback2018 = replace(goback2018, grandchildren_2016==1 & is.na(grandchildren_2017) &
                         grandchildren_2018==0, 1),
  goback2019 = replace(goback2019, grandchildren_2017==1 & is.na(grandchildren_2018) &
                         grandchildren_2019==0, 1))
# 815386 846987 848046 855929 896304

#recode for 2-year-gap
liss_grand_wide <- liss_grand_wide %>% mutate(
  goback2011 = replace(goback2011, grandchildren_2008==1 & is.na(grandchildren_2009) &
                         is.na(grandchildren_2010) & grandchildren_2011==0, 1),
  goback2012 = replace(goback2012, grandchildren_2009==1 & is.na(grandchildren_2010) &
                         is.na(grandchildren_2011) & grandchildren_2012==0, 1),
  goback2013 = replace(goback2013, grandchildren_2010==1 & is.na(grandchildren_2011) &
                         is.na(grandchildren_2012) & grandchildren_2013==0, 1),
  goback2014 = replace(goback2014, grandchildren_2011==1 & is.na(grandchildren_2012) &
                         is.na(grandchildren_2013) & grandchildren_2014==0, 1),
  goback2015 = replace(goback2015, grandchildren_2012==1 & is.na(grandchildren_2013) &
                         is.na(grandchildren_2014) & grandchildren_2015==0, 1),
  goback2016 = replace(goback2016, grandchildren_2013==1 & is.na(grandchildren_2014) &
                         is.na(grandchildren_2015) & grandchildren_2016==0, 1),
  goback2017 = replace(goback2017, grandchildren_2014==1 & is.na(grandchildren_2015) &
                         is.na(grandchildren_2016) & grandchildren_2017==0, 1),
  goback2018 = replace(goback2018, grandchildren_2015==1 & is.na(grandchildren_2016) &
                         is.na(grandchildren_2017) & grandchildren_2018==0, 1),
  goback2019 = replace(goback2019, grandchildren_2016==1 & is.na(grandchildren_2017) &
                         is.na(grandchildren_2018) & grandchildren_2019==0, 1))
# plus 888903

#recode for 3-year-gap
liss_grand_wide <- liss_grand_wide %>% mutate(
  goback2012 = replace(goback2012, grandchildren_2008==1 & is.na(grandchildren_2009) & is.na(grandchildren_2010) &
                         is.na(grandchildren_2011) & grandchildren_2012==0, 1),
  goback2013 = replace(goback2013, grandchildren_2009==1 & is.na(grandchildren_2010) & is.na(grandchildren_2011) &
                         is.na(grandchildren_2012) & grandchildren_2013==0, 1),
  goback2014 = replace(goback2014, grandchildren_2010==1 & is.na(grandchildren_2011) & is.na(grandchildren_2012) &
                         is.na(grandchildren_2013) & grandchildren_2014==0, 1),
  goback2015 = replace(goback2015, grandchildren_2011==1 & is.na(grandchildren_2012) & is.na(grandchildren_2013) &
                         is.na(grandchildren_2014) & grandchildren_2015==0, 1),
  goback2016 = replace(goback2016, grandchildren_2012==1 & is.na(grandchildren_2013) & is.na(grandchildren_2014) &
                         is.na(grandchildren_2015) & grandchildren_2016==0, 1),
  goback2017 = replace(goback2017, grandchildren_2013==1 & is.na(grandchildren_2014) & is.na(grandchildren_2015) &
                         is.na(grandchildren_2016) & grandchildren_2017==0, 1),
  goback2018 = replace(goback2018, grandchildren_2014==1 & is.na(grandchildren_2015) & is.na(grandchildren_2016) &
                         is.na(grandchildren_2017) & grandchildren_2018==0, 1),
  goback2019 = replace(goback2019, grandchildren_2015==1 & is.na(grandchildren_2016) & is.na(grandchildren_2017) &
                         is.na(grandchildren_2018) & grandchildren_2019==0, 1))
# no cases with 3-year-gap

#recode for 4-year-gap
liss_grand_wide <- liss_grand_wide %>% mutate(
  goback2013 = replace(goback2013, grandchildren_2008==1 & is.na(grandchildren_2009) & is.na(grandchildren_2010) & is.na(grandchildren_2011) &
                         is.na(grandchildren_2012) & grandchildren_2013==0, 1),
  goback2014 = replace(goback2014, grandchildren_2009==1 & is.na(grandchildren_2010) & is.na(grandchildren_2011) & is.na(grandchildren_2012) &
                         is.na(grandchildren_2013) & grandchildren_2014==0, 1),
  goback2015 = replace(goback2015, grandchildren_2010==1 & is.na(grandchildren_2011) & is.na(grandchildren_2012) & is.na(grandchildren_2013) &
                         is.na(grandchildren_2014) & grandchildren_2015==0, 1),
  goback2016 = replace(goback2016, grandchildren_2011==1 & is.na(grandchildren_2012) & is.na(grandchildren_2013) & is.na(grandchildren_2014) &
                         is.na(grandchildren_2015) & grandchildren_2016==0, 1),
  goback2017 = replace(goback2017, grandchildren_2012==1 & is.na(grandchildren_2013) & is.na(grandchildren_2014) & is.na(grandchildren_2015) &
                         is.na(grandchildren_2016) & grandchildren_2017==0, 1),
  goback2018 = replace(goback2018, grandchildren_2013==1 & is.na(grandchildren_2014) & is.na(grandchildren_2015) & is.na(grandchildren_2016) &
                         is.na(grandchildren_2017) & grandchildren_2018==0, 1),
  goback2019 = replace(goback2019, grandchildren_2014==1 & is.na(grandchildren_2015) & is.na(grandchildren_2016) & is.na(grandchildren_2017) &
                         is.na(grandchildren_2018) & grandchildren_2019==0, 1))
# no cases with 4-year-gap

# update count
liss_grand_wide <- liss_grand_wide %>% 
  mutate(
    sum_goback  = rowSums(select(., starts_with("goback"))))

liss_grand_wide %>% filter(grandparent==1 & sum_goback>=1) %>% 
  select(nomem_encr, starts_with("grandchildren")) %>% print(width=Inf)
# 815386 846987 848046 855929 888903 896304 -> 6 cases where NAs obscured inconsistent 
# longitudinal data 

liss_grand_wide <- liss_grand_wide %>% 
  mutate(grandparent = replace(grandparent, grandparent==1 & sum_goback>=1, NA))
         
table(liss_grand_wide$grandparent) # check count: N=337

# drop non-grandparents where more than half of years are missing
# -> these will not be very useful for matching
liss_grand_wide <- liss_grand_wide %>% 
  mutate(sum_missings = rowSums(is.na(select(., starts_with("grandchildren")))))
liss_grand_wide <- liss_grand_wide %>% filter((grandparent==0 & sum_missings<=6) | grandparent==1) %>% 
  select(-sum_missings)
table(liss_grand_wide$grandparent)

liss_grand_wide_identify <- liss_grand_wide %>% filter(grandparent %in% c(0,1)) %>% 
  select(nomem_encr, grandparent) # for later merge with long data

# reshape again & merge with full dataset
liss_grand_transits <- liss_grand_wide %>% 
  filter(grandparent %in% c(0,1)) %>% 
  select(nomem_encr, starts_with("transit")) %>% 
  pivot_longer(cols = starts_with("transit"), 
               names_to = "year",
               names_prefix = "transit",
               values_to = "transit",
               values_drop_na = T) %>% 
  mutate(year = as.numeric(year))

liss_grand <- left_join(liss_grand, liss_grand_transits)
liss_grand %>% filter(transit==1) %>% group_by(year) %>% summarise(n())

liss_grand <- left_join(liss_grand, liss_grand_wide_identify) # merge group-identifiying variable
liss_grand %>% filter(grandparent==0) %>% group_by(year) %>% summarise(n()) %>% print(n=Inf)
liss_grand %>% filter(grandparent==1) %>% group_by(year) %>% summarise(n()) %>% print(n=Inf)
table(liss_grand$grandparent)

liss_grand %>% group_by(grandparent) %>% summarise(ndist = n_distinct(nomem_encr)) # N = 337 grandparents
liss_grand %>% filter(grandparent==1) %>% group_by(grandchildren) %>% summarise(n())

#### code time in relation to transition ####

# Put all time-person observations into a framework that centers the neighboring observations 
# around the transition

liss_grand <- liss_grand %>% 
  mutate(transityr = ifelse(transit==1, year, NA)) # %>% 
#  group_by(nomem_encr) %>% arrange(nomem_encr, year) %>% 
#  mutate(
#    participation = row_number(),
#    participation_max = n())         # better to count based on Background Variables data (above)

liss_grand_merge <- liss_grand %>% select(nomem_encr, transit, transityr) %>% 
  filter(transit==1) %>% 
  rename(transitever=transit,
         transityear=transityr)

liss_grand <- left_join(liss_grand, liss_grand_merge) %>% 
  select(-(transityr))

liss_grand <- liss_grand %>% 
  mutate(time = year - transityear) %>% #coding time variable
  select(-grandchildren)
liss_grand %>% group_by(time) %>% filter(grandparent==1) %>% summarise(n = n()) %>% 
  print(n=Inf)

# merge with main dataset

lisslong <- left_join(lisslong, liss_grand)

options(pillar.sigfig = 5)
lisslong %>% group_by(grandparent) %>% filter(transit==1 | grandparent==0) %>% 
  summarise(meanage = mean((age), na.rm=T), n = n())
lisslong %>% group_by(grandparent) %>% summarise(n=n(), N=n_distinct(nomem_encr))
  
# save .rda 
save(lisslong, file = "data/processed/LISS/lisslong_cleaned.rda")

# load .rda
load(file = "data/processed/LISS/lisslong_cleaned.rda")

# filter only valid cases and create count of valid observations
# valid = non-missing in at least one of the outcome variables

lisslongvalid <- lisslong  %>% 
  filter(!is.na(extra) | !is.na(con) | !is.na(open) | !is.na(agree) | !is.na(neur) | !is.na(swls))
# only 1 missing remains across all outcomes (in 'agree')
summary(lisslongvalid$agree)

# some respondents have different missing waves in the Personality and 
# Work and Education datasets -> we have some waves with valid personality data, which
# are not in the Waves and Education data
lisslongvalid <- lisslongvalid %>% arrange(nomem_encr, year) %>% group_by(nomem_encr) %>% 
  mutate_at(.vars = vars(grandparent, transityear), ~max(., na.rm = T)) %>% ungroup() %>% 
  mutate_at(.vars = vars(grandparent, transityear), ~ifelse(.==-Inf, NA, .)) %>% 
  mutate(transit = ifelse(is.na(transit) & !is.na(grandparent), 0, transit), 
         time = year - transityear) # recode 'time'
# (could have used fill() function instead)

# For LISS, we need to make sure that we end up selecting only grandparents with a
# valid assessment at time==-2. If we only have 1 year between the time of matching and the
# first report of the transition to GP (time==-1), then we might include respondents whose 
# children are pregnant already. 

# code indicator for last (valid) assessment before the transition to GP (survey year varies)
# -> Revised approach: instead of filter(time<0) I use filter(time < -1). This is to make sure
# that at the time of matching, a respondent's children are not already pregnant with the grandchild
liss_last_time_point <- lisslongvalid %>% filter(time < -1) %>% group_by(nomem_encr) %>% 
  slice(which.max(time)) %>% select(nomem_encr, time) %>% rename(matchtime = time)
table(liss_last_time_point$matchtime)
# N = 270 grandparents (with a valid pre-transition assessment earlier than time==-1)
# -5  -4  -3  -2 
#  1  21  75 173 

lisslongvalid <- left_join(lisslongvalid, liss_last_time_point) %>% arrange(nomem_encr, year)

# N = 335 grandparents (in total) - 65 with no valid assessment at least 2 years before 
# transition to GP -> 270
lisslongvalid %>% group_by(grandparent, matchtime) %>% summarise(n = n(), ndist = n_distinct(nomem_encr))
# Of these 270, all at least appear in Family and Household data
lisslongvalid %>% filter(validfam==1) %>% group_by(grandparent, matchtime) %>% 
  summarise(n = n(), ndist = n_distinct(nomem_encr))

# N = 135 grandparents (with a valid pre-transition assessment AND a t=0 post-treatment assessment)
lisslongvalid %>% filter(transit==1 & !is.na(matchtime)) %>% summarise(ndist = n_distinct(nomem_encr))
# however, there are additional grandparents with valid pre- and post-transition assessments 
# (just not at t=0, but later)

# create 'valid' variable numbering valid assessments before/after the transition
summary(lisslongvalid$time)
lisslongvalid_neg <- lisslongvalid %>% filter(time %in% c(-20:-1)) %>% # for time<0
  select(nomem_encr, year, time) %>% group_by(nomem_encr) %>% mutate(valid = min_rank(desc(time))) %>% 
  mutate(valid = valid - 2*valid) # flip algebraic sign
lisslongvalid_pos <- lisslongvalid %>% filter(time %in% c(0:20)) %>% # for time>=0
  select(nomem_encr, year, time) %>% group_by(nomem_encr) %>% mutate(valid = min_rank(time) - 1)

# merge
lisslongvalid <- left_join(lisslongvalid, bind_rows(lisslongvalid_neg, lisslongvalid_pos))
rm(lisslongvalid_neg, lisslongvalid_pos)

# Variable 'valid' now counts the valid assessments before the transition (negative values), 
# and afterwards with 0 being the first valid assessment after the transition to GP
# This new count is irrespective of the actual timing of the survey years.
lisslongvalid %>% select(nomem_encr, year, time, valid, matchtime, grandparent, transit, transityear) %>% 
  filter(grandparent==1) %>% print(n=50)
table(lisslongvalid$valid, lisslongvalid$matchtime)
(N_GP_liss <- pull(lisslongvalid %>% filter(valid==0 & matchtime %in% c(-5:-2)) %>% 
                   summarise(n = n()))) 

# Restrict sample to those with at least 1 valid pre- and 1 valid post-treatment assessment
# -> ALSO, pre-treatment assessment has to be at time==-2 (or earlier)
lisslongvalid <- lisslongvalid %>% 
  mutate(help = ifelse(grandparent==1, ifelse(valid==0 & !is.na(matchtime), 0, 1), NA)) %>% 
  group_by(nomem_encr) %>% mutate(droplater = min(help, na.rm = T)==1) %>% 
  ungroup() %>% select(-help)
# after imputations: drop 333 obs. (83 resp.) 
# -> no pre-treatment assessment earlier than time==-1 / no post-treatment assessment
lisslongvalid %>% group_by(grandparent, droplater) %>% summarise(n = n(), N=n_distinct(nomem_encr))

table(lisslongvalid$grandparent, lisslongvalid$time)
table(lisslongvalid$grandparent, lisslongvalid$valid)

lisslongvalid <- lisslongvalid %>% 
  mutate_at(vars(nokids, secondkid, thirdkid), funs(replace_na(., 0))) # need a version without NAs

# save .rda 
save(lisslongvalid, file = "data/processed/LISS/lisslong_valid.rda")

# load .rda
load(file = "data/processed/LISS/lisslong_valid.rda")

#### multiple imputations for variables needed for PSM covariates ####

liss_for_implong <- lisslongvalid %>% 
  select(nomem_encr, year, 
         retire_early, retirement, paid_work, more_paid_work, #for covars
         financialsit, difficultybills, rooms, movedinyear, 
         typedwelling, secondhouse, religion, speakdutch, 
         subjhealth, bmi, chronicdisease, 
         currentpartner, livetogether, totalchildren, 
         contains("kid1"), contains("kid2"), contains("kid3"), 
         #nokids, secondkid, thirdkid,
         heartattack, stroke, cancer, diabetes, nodisease, mobility, dep, 
         age, female, employment_status, education, hhmembers, totalresidentkids, marital, ownrent, 
         urban, nettoink, participation, 
         swls, agree, con, extra, neur, open) #for pre-treatment outcome variables

# will not impute the 1st child's birthyear, because this variable will be used
# to filter one of the control groups (and will not be used as a PSM covariate
# in the other second control group)
liss_for_implong <- liss_for_implong %>% select(-kid1byear)

summary(liss_for_implong)

# do multiple imputations 
# (required for pscore matching, although imputed data will not be used in later analyses)
library(mice)
library(lattice)

imp <- 5 # Number of multiple imputations

# not in 2014: subjhealth, bmi, chronicdisease, heartattack, stroke, cancer, diabetes, 
# nodisease, mobility, dep
# -> not included -> separate imputation & PS estimation

# not in 2016: swls, agree, con, extra, neur, open 
# -> Won't include 2016

# seperate imputations for each wave - 2019 not needed because we always match in pre-treat wave
help1 <- NULL;
help2 <- NULL;
help3 <- NULL;
for (yr in c(2008:2013, 2015, 2017:2018)){
    help1 <- paste("liss_for_imp", yr, sep="")
    help2 <- liss_for_implong$year
    help3 <- paste("liss_imp", yr, sep="")
    #create separate datasets for each year
    eval(call("<-", as.name(help1), liss_for_implong %>% filter(help2==yr)))
    #perform imputations
    eval(call("<-", as.name(help3), mice(get(help1), method = "cart", m = imp, maxit = 5, seed = 3000)))
}
# Recent work shows ensemble methods like boosted CART and random
# forests work very well (Setoguchi et al. 2008; Lee et al., 2009).

summary(liss_for_imp2009)
summary(liss_for_imp2017)

summary(liss_imp2009)
summary(liss_imp2017)
#densityplot(liss_imp2009)
#densityplot(liss_imp2017)

# 2014 separately because some variables are missing per design
# (2016 won't be done because the missing outcome variables are too important) (?)
liss_for_imp2014 <- liss_for_implong %>% filter(year==2014) %>% 
  select(-c(subjhealth, bmi, chronicdisease, heartattack, stroke, cancer, diabetes, 
            nodisease, mobility, dep))
liss_imp2014 <- mice(liss_for_imp2014, method = "cart", m = imp, maxit = 5, seed = 3000)
summary(liss_for_imp2014)
summary(liss_imp2014)
#densityplot(liss_imp2014)

# create imp=5 datasets per imp'yr' object (these contain the imputed values, i.e. no missings)
help1 <- NULL;
help2 <- NULL;
for (yr in c(2008:2015, 2017:2018)){
  for(i in 1:imp){
    help1 <- paste("liss_imp", yr, "number", i, sep="_")
    help2 <- paste("liss_imp", yr, sep="")
    eval(call("<-", as.name(help1), complete(data = get(help2), i)))
  }
}

summary(liss_imp_2008_number_1)
summary(liss_imp_2008_number_2)
psych::describe(liss_imp_2008_number_1)
psych::describe(liss_imp_2008_number_2)
#etc. 
detach("package:mice", unload=TRUE)

#build imp=5 long datasets
x <- NULL;
for (i in 1:imp){
  x <- paste("lissimp_matching", i, sep="_")
  d08 <- paste("liss_imp_2008_number", i, sep="_")
  d09 <- paste("liss_imp_2009_number", i, sep="_")
  d10 <- paste("liss_imp_2010_number", i, sep="_")
  d11 <- paste("liss_imp_2011_number", i, sep="_")
  d12 <- paste("liss_imp_2012_number", i, sep="_")
  d13 <- paste("liss_imp_2013_number", i, sep="_")
  d14 <- paste("liss_imp_2014_number", i, sep="_")
  d15 <- paste("liss_imp_2015_number", i, sep="_")
  d17 <- paste("liss_imp_2017_number", i, sep="_")
  d18 <- paste("liss_imp_2018_number", i, sep="_")
  eval(call("<-", as.name(x), bind_rows(get(d08), get(d09), get(d10), get(d11), get(d12), 
                                        get(d13), get(d14), get(d15), get(d17), get(d18))))
}

# merge variables that identify (or describe) the treatment groups and the 
# timing of treatment (including year of birth of 1st child - we don't want 
# imputed values for this variable)
liss_merge_gp <- lisslongvalid %>% 
  select(nomem_encr, year, grandparent, time, matchtime, participation, valid, droplater, 
         kid1byear, nokids, secondkid, thirdkid)
lissimp_matching_1 <- left_join(lissimp_matching_1, liss_merge_gp)
lissimp_matching_2 <- left_join(lissimp_matching_2, liss_merge_gp)
lissimp_matching_3 <- left_join(lissimp_matching_3, liss_merge_gp)
lissimp_matching_4 <- left_join(lissimp_matching_4, liss_merge_gp)
lissimp_matching_5 <- left_join(lissimp_matching_5, liss_merge_gp)

# save .rda 
save(lissimp_matching_1, file = "data/processed/LISS/lissimp_matching_1.rda")
save(lissimp_matching_2, file = "data/processed/LISS/lissimp_matching_2.rda")
save(lissimp_matching_3, file = "data/processed/LISS/lissimp_matching_3.rda")
save(lissimp_matching_4, file = "data/processed/LISS/lissimp_matching_4.rda")
save(lissimp_matching_5, file = "data/processed/LISS/lissimp_matching_5.rda")

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
              poorhealth = ifelse(subjhealth==1, 1, 0), #ref: 3 good
              moderatehealth = ifelse(subjhealth==2, 1, 0), 
              verygoodhealth = ifelse(subjhealth==4, 1, 0), 
              excellenthealth = ifelse(subjhealth==5, 1, 0),
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


# Sadly, some dummy covariates I picked have a very one-sided distribution in the (smaller) GP group where 
# some variables have 0 (or only 1, 2, or 3) cases in one group. Example:
table(lissimp_parents_ps_1$grandparent, lissimp_parents_ps_1$disability)
# Later on, this leads to problems with logistic regression:
# https://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression
# Therefore, I'll remove all these variables here. (see "Overview covariates.xlsx" -> Sheet "Frequencies")
remove_infrequent_liss <- function(x) { 
  x %>% 
    select(-c(retire_early, retirement, heartattack, stroke, cancer, rentfree, businessdwelling, otherdwelling, 
              jobseeker, pensioner, disability, primaryschool, poorhealth, excellenthealth),
           -c(paid_work, more_paid_work, financialsit, difficultybills, secondhouse, # these are not critical (substantively)
              speakdutch, bmi, chronicdisease, diabetes, nodisease, mobility, dep, flatapartment, 
              farmhouse, familybusiness, freelancer, housekeeper, degreeother, moderatehealth, 
              verygoodhealth, moderatehealth, verygoodhealth)) }#,
           #-c(religion, currentpartner, livetogether, hhmembers, rental, degreehighersec,
           #   degreevocational, degreecollege, degreeuniversity, divorced, widowed, single, 
           #   extremelyurban, moderatelyurban, slightlyurban, noturban, logincome, livedhere, rooms)) }

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
# Additionally, the following will be removed only for wave 2014 below:
# more_paid_work, speakdutch, farmhouse, housekeeper, degreeother, widowed, difficultybills, single

#### PSM: compute propensity scores ####

# 1st step -> estimate propensity scores for all obs.
# do 5 times, once for each of m=5 imputations, then compute mean PS (see Mitra 2016)

imp <- 5

# 1) PARENT control group

liss_ps_model_parents <- as.formula("grandparent ~ . - year - nomem_encr - female") # all vars but ...

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
#liss_ps_model_2014 <- as.formula("grandparent ~ . - year - nomem_encr - poorhealth - moderatehealth - 
#                                  verygoodhealth - excellenthealth - bmi - 
#                                  chronicdisease - heartattack - stroke - cancer - diabetes - 
#                                  nodisease - mobility - dep") # all vars but ... # does not work for some reason
liss_ps_model_parents_2014 <- as.formula("grandparent ~ . - year - nomem_encr - female")

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
              select(-c(moderatehealth, verygoodhealth, bmi, # health variables not assessed in 2014 
                        chronicdisease, diabetes, nodisease, 
                        mobility, dep,
                        more_paid_work, speakdutch, farmhouse, # these were too infrequent in 2014 (in GP group)
                        housekeeper, degreeother, widowed, 
                        difficultybills, single))))
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

# 2) NONPARENT control group

liss_ps_model_nonparents <- as.formula("grandparent ~ . - year - nomem_encr - female") # all vars but ...

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
#liss_ps_model_2014 <- as.formula("grandparent ~ . - year - nomem_encr - poorhealth - moderatehealth - 
#                                  verygoodhealth - excellenthealth - bmi - 
#                                  chronicdisease - heartattack - stroke - cancer - diabetes - 
#                                  nodisease - mobility - dep") # all vars but ... # does not work for some reason
liss_ps_model_nonparents_2014 <- as.formula("grandparent ~ . - year - nomem_encr - female")

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
              select(-c(moderatehealth, verygoodhealth, bmi, # health variables not assessed in 2014 
                        chronicdisease, diabetes, nodisease, 
                        mobility, dep,
                        more_paid_work, speakdutch, farmhouse, # these were too infrequent in 2014 (in GP group)
                        housekeeper, degreeother, widowed, 
                        difficultybills, single))))
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

#### PSM: identify possible matches -> (1) parent control group ####

# DIY matching: 2nd step -> identifying all possible PS-matches per parent matching 
#                           exactly on gender
lissimp_matching_parents <- lissimp_matching_parents %>% 
  select(nomem_encr, year, female, grandparent, pscore) 
# sort by random variable in case there are ordering effects
set.seed(123)
rows <- sample(nrow(lissimp_matching_parents))
lissimp_matching_parents <- lissimp_matching_parents[rows, ]
lissimp_matching_parents <- lissimp_matching_parents %>% mutate(new_id = row_number())

lissimp_matching_parents_case <- lissimp_matching_parents %>% filter(grandparent==1) %>% 
  rename(case_id = new_id,
         pid_case = nomem_encr,
         pscore_case = pscore,
         gender_case = female) %>% 
  select(-grandparent, -year) 
# removing 'year' here means that 'year' -> 'match_year' will refer to the controls' survey year
# at the time of matching (see below)

lissimp_matching_parents_control <- lissimp_matching_parents %>% filter(grandparent==0) %>% 
  rename(control_id = new_id,
         pid_control = nomem_encr,
         pscore_control = pscore,
         gender_control = female,
         match_year = year) %>% 
  select(-grandparent)

#matching the two datasets along gender of controls and cases (exact matches)
#equivalent of STATA joinby command (Cartesian product)
lissimp_joined_parents <- crossing(lissimp_matching_parents_case, lissimp_matching_parents_control)
#249*3040 = 756960
lissimp_joined_parents <- lissimp_joined_parents %>% filter(gender_case==gender_control) %>% 
  select(-gender_control) %>% 
  rename(gender = gender_case)

#### PSM: matching loop -> (1) parent control group ####

# DIY matching: 3rd step -> matching the cases and controls according to the similarity of 
#                           their propensity scores (1 to 1 matching; no duplicates)
lissimp_joined_parents <- lissimp_joined_parents %>% mutate(ps_diff = abs(pscore_case - pscore_control))
#no midranks or shared ranks - if PS_diff is the same it can be random which one we pick
lissimp_joined_parents <- lissimp_joined_parents %>% group_by(pid_case) %>% 
  mutate(rank = rank(ps_diff, ties.method = "random"))

#this new numbering makes the following loop easier  
lissimp_joined_parents <- lissimp_joined_parents %>% group_by(case_id) %>%
  mutate(case_id_new = cur_group_id()) %>% 
  arrange(case_id_new, rank) %>% ungroup()

#to help with computation time -> restrict to certain ranks (similar idea to caliper)
#assuming that there will be no valid match according to PS with a higher rank, anyway!
lissimp_joined_parents_reduced <- lissimp_joined_parents %>% filter(rank<=500)
(num_cases <- as.vector(summary(lissimp_joined_parents_reduced$case_id_new)[6]))

liss_matched_parents <- NULL;
picked <- NULL;
for(num in 1:num_cases)
{
  #pick the highest rank of the (remaining) controls each case is matched with
  picked <- lissimp_joined_parents_reduced %>% filter(case_id_new==num) %>% 
    mutate(best = min(rank)) %>% filter(rank==best) %>% select(-best)
  #collect valid matches throughout the loop
  liss_matched_parents <- rbind(liss_matched_parents, picked)
  #track pid of control matched in every iteration of the loop
  picked <- picked %>% mutate(tracked=1) %>% select(pid_control, tracked)
  #drop all other controls with the tracked pid in the remaining pool of available controls
  lissimp_joined_parents_reduced <- left_join(lissimp_joined_parents_reduced, picked)
  lissimp_joined_parents_reduced <- lissimp_joined_parents_reduced %>% 
    filter(is.na(tracked)) %>% select(-tracked)
}
# all 249 grandparents sucessfully matched!
summary(liss_matched_parents$rank)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.00    1.00    9.00   70.61  124.00  327.00 

summary(liss_matched_parents$ps_diff)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000006 0.0002259 0.0227015 0.1162157 0.1825504 0.6875521 

# create dataset for merge with controls
liss_matched_mergecontrols_parents <- liss_matched_parents %>% rename(
  nomem_encr = pid_control,
  pscore = pscore_control,
  nomem_encr_case = pid_case
) %>% select(nomem_encr, nomem_encr_case, match_year, pscore) %>% 
  arrange(nomem_encr)

# create dataset for merge with cases
liss_matched_mergecases_parents <- liss_matched_parents %>% rename(
  nomem_encr = pid_case,
  pscore = pscore_case,
  nomem_encr_control = pid_control
) %>% select(nomem_encr, nomem_encr_control, match_year, pscore) %>% 
  arrange(nomem_encr)


#### compile final analysis sample -> (1) parent control group ####

lissanalysis_parents <- lisslongvalid %>% filter(!is.na(grandparent) & droplater!=T)

lissanalysis_parents <- left_join(lissanalysis_parents, liss_matched_mergecases_parents, by='nomem_encr')
lissanalysis_parents <- left_join(lissanalysis_parents, liss_matched_mergecontrols_parents, by='nomem_encr')

# create dataset for merge containing information on whether a control was 
# at time==-5, time==-4, time==-3, or time==-2 at the time of matching (counterfactual timeframe)
time_for_controls_liss <- lisslongvalid %>% 
  filter(grandparent==1 & nokids==0 & time==matchtime & droplater==F) %>% # this was the condition by which GP observations were chosen for matching (see above)
  select(nomem_encr, matchtime, valid) %>% rename(
    matchtime_controls = matchtime,
    matchvalid_controls = valid) # same procedure for 'valid' (counts valid observations in relation to transition time point for GPs)
table(time_for_controls_liss$matchtime_controls) # different time lags between matching and transition to GP
table(time_for_controls_liss$matchvalid_controls) 
# some GPs were not matched at valid==-1 because we wanted to avoid having only 1 year between matching and 
# transition, because parents might have already been pregnant with the grandchildren at this point.

# match via nomem_encr_case (which gives the ID of each control's match) in order 
# to transfer 'matchtime' value from the cases to controls
lissanalysis_parents <- left_join(lissanalysis_parents, 
                                  time_for_controls_liss, by=c('nomem_encr_case'='nomem_encr')) 

lissanalysis_parents <- lissanalysis_parents %>% mutate( # this just unites information from cases and controls in a single variable
  pscore = ifelse(!is.na(pscore.x), pscore.x, pscore.y),
  match_year = ifelse(!is.na(match_year.x), match_year.x, match_year.y),
  nomem_encr_match = ifelse(!is.na(nomem_encr_case), nomem_encr_case, nomem_encr_control),
  matchtime = ifelse(!is.na(matchtime), matchtime, matchtime_controls), 
) %>% 
  select(-c(pscore.x, pscore.y, match_year.x, match_year.y, nomem_encr_case, nomem_encr_control,
            matchtime_controls)) %>%
  filter(!is.na(pscore)) # only keep cases and controls
# reminder: 'matchtime' is the original var indicating (in years) when matching occurred for grandparents-to-be
#           in relation to the birth of their grandchild. This value was now transferred to the controls via the 
#           var 'matchtime_controls'.
table(lissanalysis_parents$grandparent, lissanalysis_parents$matchtime)
table(lissanalysis_parents$grandparent, lissanalysis_parents$matchvalid_controls)

# coding counterfactual timeframe for controls
# create time variable for controls relative to the time point of matching (& 'valid' variable)
lissanalysis_parents <- lissanalysis_parents %>% mutate(
  time = ifelse(is.na(time) & match_year==year, matchtime, time),
  # variable 'match_year' relates to the controls
  valid = ifelse(is.na(valid) & match_year==year, matchvalid_controls, valid)
)
# reminder: 'time' counts calendar years in relation to transition to GP, 'valid' only valid assessments
# note that we have 1:1 corresponding observations in 'time' and 'valid' between cases and controls only 
# at the time of matching. Earlier or later observations will differ because of different patterns of
# panel attrition and participation.
table(lissanalysis_parents$grandparent, lissanalysis_parents$time)
table(lissanalysis_parents$grandparent, lissanalysis_parents$valid)

lissanalysis_parents <- lissanalysis_parents %>% mutate(
  time = ifelse(is.na(time) & matchtime==-5, (year - match_year) - 5, time),
  time = ifelse(is.na(time) & matchtime==-4, (year - match_year) - 4, time),
  time = ifelse(is.na(time) & matchtime==-3, (year - match_year) - 3, time),
  time = ifelse(is.na(time) & matchtime==-2, (year - match_year) - 2, time))

lissanalysis_parents <- lissanalysis_parents %>% group_by(nomem_encr) %>% 
  mutate(lastcount = ifelse(grandparent==0 & valid==matchvalid_controls, row_number(), NA)) %>% ungroup()
lissanalysis_parents <- lissanalysis_parents %>% group_by(nomem_encr) %>% 
  mutate(lastcount = max(lastcount, na.rm = T)) %>% ungroup() %>% 
  mutate(lastcount = replace(lastcount, lastcount==-Inf, NA))

# finish coding 'valid' for controls
lissanalysis_parents <- lissanalysis_parents %>% group_by(nomem_encr) %>% 
  mutate(helpcount = row_number()) %>% ungroup()
lissanalysis_parents <- lissanalysis_parents %>% 
  mutate(valid = ifelse(is.na(valid) & grandparent==0, helpcount - lastcount + matchvalid_controls, valid)) %>% 
  select(-helpcount, -lastcount)

table(lissanalysis_parents$grandparent, lissanalysis_parents$valid)
table(lissanalysis_parents$grandparent, lissanalysis_parents$time) # cells earlier than '-5' too small
# time==5 also very small....

lissanalysis_parents <- lissanalysis_parents %>% filter(time %in% c(-5:5)) %>% 
  select(-match_year, -droplater, -matchvalid_controls)

# save .rda 
save(lissanalysis_parents, file = "data/processed/LISS/lissanalysis_parents.rda")


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

table(lissimp_matching_parents$grandparent)
table(lissimp_matching_parents$grandparent, lissimp_matching_parents$year)

#we also need the 'time' value for grandparents (-5, -4, -3 or -2) and the 'valid' variable
lissimp_groupmatch_parents <- left_join(lissimp_matching_parents, 
                                       lissimp_matching_1, by=c("nomem_encr", "year")) %>% 
  filter(droplater==F) %>% 
  select(nomem_encr, year, female.x, grandparent.x, pscore, time, valid) %>% 
  rename(female = female.x, grandparent = grandparent.x)

table(lissimp_groupmatch_parents$grandparent, lissimp_groupmatch_parents$time)
table(lissimp_groupmatch_parents$grandparent, lissimp_groupmatch_parents$valid)

# Description
# This is an adaption of fullmatch to allow for restrictions when control observations 
# are "grouped". The motivating use case is when there are multiple observations of control 
# data for each control subject. In this case, the grouping variable is the subject. We may 
# want to place restrictions, for example that only one observation of a subject can be 
# matched, or in the case of one:many matching, a given control subject can only be matched 
# to a given treated subject once.

#matrix of propensity score distances (uses the same 'pscore' variable as the K&R matching loop)
match_on_parents_ps <- as.matrix(match_on(grandparent~pscore, data=lissimp_groupmatch_parents))

#caliper matrix to exact-match on gender
match_on_parents_female <- as.matrix(exactMatch(grandparent ~ female, data=lissimp_groupmatch_parents))

#caliper matrix to match within $500 of income (also possible)
#inc <-  as.matrix(caliper(match_on(treat~income), distance = "Euclidean", width = 500))

#final input to rollingMatch is the sum of these matrices
final_dist_parents <- match_on_parents_ps + match_on_parents_female
# 756960 elements in matrix -> same as in Cartesian product in DIY matching loop

liss_groupmatch_parents <- groupmatch(x=final_dist_parents, 
                                     group = lissimp_groupmatch_parents$nomem_encr, allow_duplicates = T, 
                                     min.controls = 0, max.controls = 1, omit.fraction = NULL, 
                                     mean.controls = NULL, tol = 0.001, data = lissimp_groupmatch_parents)
# allowing duplicates greatly improves cov balance in our situation where we have few available controls

summary(liss_groupmatch_parents) #seems to work for 1:1 matching without replacement despite the warning message!

#matched(liss_groupmatch_parents)
sum(matched(liss_groupmatch_parents))

#unmatched(liss_groupmatch_parents)
sum(unmatched(liss_groupmatch_parents))

#matchfailed(liss_groupmatch_parents)
sum(matchfailed(liss_groupmatch_parents))

liss_groupmatch_data_parents <- cbind(lissimp_groupmatch_parents, matches=liss_groupmatch_parents) %>% 
  filter(!is.na(matches))
liss_groupmatch_data_parents <- liss_groupmatch_data_parents %>% group_by(matches) %>% 
  mutate(time = ifelse(is.na(time), max(time, na.rm = T), time), # same information as 'matchtime' now (but already transferred to controls, too)
         valid = ifelse(is.na(valid), max(valid, na.rm = T), valid)) %>% ungroup %>% 
  select(-matches)

table(liss_groupmatch_data_parents$grandparent, liss_groupmatch_data_parents$year)
table(liss_groupmatch_data_parents$grandparent, liss_groupmatch_data_parents$time)
table(liss_groupmatch_data_parents$grandparent, liss_groupmatch_data_parents$valid)
table(liss_groupmatch_data_parents$grandparent, liss_groupmatch_data_parents$female) # exact matching on gender!

liss_groupmatch_data_parents <- left_join(liss_groupmatch_data_parents, lissimp_parents_ps_1,
                                         by = c("nomem_encr", "year", "grandparent", "female"))

# for balance assessment (at the time of matching - using the variables containing imputed values)
liss_bal_parents_groupmatch <- liss_groupmatch_data_parents %>%
  select(nomem_encr, grandparent, pscore, female, everything(), -time, -year, -valid)

liss_groupmatch_data_parents <- liss_groupmatch_data_parents %>% 
  select(nomem_encr, year, grandparent, time, valid, pscore) %>% 
  rename(match_year = year, time_match = time, valid_match = valid) %>% 
  mutate(match_number = row_number()) # if we allow duplicate matches, we need an unambiguous identifier for later

# compile analysis sample with all longitudinal observations
lissanalysis_parents_groupmatch <- left_join(liss_groupmatch_data_parents, lisslongvalid,
                                            by = c("nomem_encr", "grandparent"))

table(lissanalysis_parents_groupmatch$grandparent, lissanalysis_parents_groupmatch$time_match) # already transferred to controls 
table(lissanalysis_parents_groupmatch$grandparent, lissanalysis_parents_groupmatch$matchtime)

lissanalysis_parents_groupmatch <- lissanalysis_parents_groupmatch %>%
  select(-matchtime) %>% rename(matchtime = time_match) # for more consistency with the matching loop method

table(lissanalysis_parents_groupmatch$grandparent, lissanalysis_parents_groupmatch$valid)
table(lissanalysis_parents_groupmatch$grandparent, lissanalysis_parents_groupmatch$valid_match)

# create time variable for controls relative to the time point of matching (& 'valid' variable)
lissanalysis_parents_groupmatch <- lissanalysis_parents_groupmatch %>% mutate(
  time = ifelse(is.na(time) & match_year==year, matchtime, time),
  # variable 'match_year' relates to the controls!
  valid = ifelse(is.na(valid) & match_year==year, valid_match, valid)
)
#reminder: 'time' counts calendar years in relation to transition to GP, 'valid' only valid assessments
table(lissanalysis_parents_groupmatch$grandparent, lissanalysis_parents_groupmatch$time)
table(lissanalysis_parents_groupmatch$grandparent, lissanalysis_parents_groupmatch$valid)

lissanalysis_parents_groupmatch <- lissanalysis_parents_groupmatch %>% filter(!is.na(pscore)) %>% 
  mutate(
    time = ifelse(grandparent==0 & is.na(time) & matchtime==-5, (year - match_year) - 5, time),
    time = ifelse(grandparent==0 & is.na(time) & matchtime==-4, (year - match_year) - 4, time),
    time = ifelse(grandparent==0 & is.na(time) & matchtime==-3, (year - match_year) - 3, time),
    time = ifelse(grandparent==0 & is.na(time) & matchtime==-2, (year - match_year) - 2, time))

lissanalysis_parents_groupmatch <- lissanalysis_parents_groupmatch %>% group_by(match_number) %>% 
  mutate(lastcount = ifelse(grandparent==0 & valid==valid_match, row_number(), NA)) %>% ungroup()
lissanalysis_parents_groupmatch <- lissanalysis_parents_groupmatch %>% group_by(match_number) %>% 
  mutate(lastcount = max(lastcount, na.rm = T)) %>% ungroup() %>% 
  mutate(lastcount = replace(lastcount, lastcount==-Inf, NA)) # regular NA pls

# finish coding 'valid' for controls 
# grouping by 'match_number' here instead of 'nomem_encr' because we allowed duplicate matches
lissanalysis_parents_groupmatch <- lissanalysis_parents_groupmatch %>% group_by(match_number) %>% 
  mutate(helpcount = row_number()) %>% ungroup()
lissanalysis_parents_groupmatch <- lissanalysis_parents_groupmatch %>% 
  mutate(valid = ifelse(is.na(valid) & grandparent==0, helpcount - lastcount + valid_match, valid)) %>% 
  select(-helpcount, -lastcount)

table(lissanalysis_parents_groupmatch$grandparent, lissanalysis_parents_groupmatch$time)
table(lissanalysis_parents_groupmatch$grandparent, lissanalysis_parents_groupmatch$valid)
table(lissanalysis_parents_groupmatch$grandparent, lissanalysis_parents_groupmatch$year)

lissanalysis_parents_groupmatch <- lissanalysis_parents_groupmatch %>% filter(time %in% c(-5:5)) %>% 
  select(-match_year, -valid_match, -nohouse_encr, -droplater)

#sample comparison - different matching methods
table(lissanalysis_parents$grandparent, lissanalysis_parents$time)
table(lissanalysis_parents_groupmatch$grandparent, lissanalysis_parents_groupmatch$time)

# save .rda 
save(lissanalysis_parents_groupmatch, file = "data/processed/LISS/lissanalysis_parents_groupmatch.rda")


#### PSM: identify possible matches -> (2) nonparent control group ####

# DIY matching: 2nd step -> identifying all possible PS-matches per parent matching 
#                           exactly on gender
lissimp_matching_nonparents <- lissimp_matching_nonparents %>% 
  select(nomem_encr, year, female, grandparent, pscore) 
# sort by random variable in case there are ordering effects
set.seed(123)
rows <- sample(nrow(lissimp_matching_nonparents))
lissimp_matching_nonparents <- lissimp_matching_nonparents[rows, ]
lissimp_matching_nonparents <- lissimp_matching_nonparents %>% mutate(new_id = row_number())

lissimp_matching_nonparents_case <- lissimp_matching_nonparents %>% filter(grandparent==1) %>% 
  rename(case_id = new_id,
         pid_case = nomem_encr,
         pscore_case = pscore,
         gender_case = female) %>% 
  select(-grandparent, -year) 
# removing 'year' here means that 'year' -> 'match_year' will refer to the controls' survey year
# at the time of matching (see below)

lissimp_matching_nonparents_control <- lissimp_matching_nonparents %>% filter(grandparent==0) %>% 
  rename(control_id = new_id,
         pid_control = nomem_encr,
         pscore_control = pscore,
         gender_control = female,
         match_year = year) %>% 
  select(-grandparent)

#matching the two datasets along gender of controls and cases (exact matches)
#equivalent of STATA joinby command (Cartesian product)
lissimp_joined_nonparents <- crossing(lissimp_matching_nonparents_case, lissimp_matching_nonparents_control)
#249*4337 = 1079913
lissimp_joined_nonparents <- lissimp_joined_nonparents %>% filter(gender_case==gender_control) %>% 
  select(-gender_control) %>% 
  rename(gender = gender_case)

#### PSM: matching loop -> (2) nonparent control group ####

# DIY matching: 3rd step -> matching the cases and controls according to the similarity of 
#                           their propensity scores (1 to 1 matching; no duplicates)
lissimp_joined_nonparents <- lissimp_joined_nonparents %>% mutate(ps_diff = abs(pscore_case - pscore_control))
#no midranks or shared ranks - if PS_diff is the same it can be random which one we pick
lissimp_joined_nonparents <- lissimp_joined_nonparents %>% group_by(pid_case) %>% 
  mutate(rank = rank(ps_diff, ties.method = "random"))

#this new numbering makes the following loop easier  
lissimp_joined_nonparents <- lissimp_joined_nonparents %>% group_by(case_id) %>%
  mutate(case_id_new = cur_group_id()) %>% 
  arrange(case_id_new, rank) %>% ungroup()

#to help with computation time -> restrict to certain ranks (similar idea to caliper)
#assuming that there will be no valid match according to PS with a higher rank, anyway!
lissimp_joined_nonparents_reduced <- lissimp_joined_nonparents %>% filter(rank<=600) # 500 not enough
(num_cases <- as.vector(summary(lissimp_joined_nonparents_reduced$case_id_new)[6]))

liss_matched_nonparents <- NULL;
picked <- NULL;
for(num in 1:num_cases)
{
  #pick the highest rank of the (remaining) controls each case is matched with
  picked <- lissimp_joined_nonparents_reduced %>% filter(case_id_new==num) %>% 
    mutate(best = min(rank)) %>% filter(rank==best) %>% select(-best)
  #collect valid matches throughout the loop
  liss_matched_nonparents <- rbind(liss_matched_nonparents, picked)
  #track pid of control matched in every iteration of the loop
  picked <- picked %>% mutate(tracked=1) %>% select(pid_control, tracked)
  #drop all other controls with the tracked pid in the remaining pool of available controls
  lissimp_joined_nonparents_reduced <- left_join(lissimp_joined_nonparents_reduced, picked)
  lissimp_joined_nonparents_reduced <- lissimp_joined_nonparents_reduced %>% 
    filter(is.na(tracked)) %>% select(-tracked)
}
# all 249 grandparents sucessfully matched!
summary(liss_matched_nonparents$rank)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.0    41.0   188.0   198.3   336.0   505.0 

summary(liss_matched_nonparents$ps_diff)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# .0000003 0.0696306 0.3054175 0.3436840 0.5586673 0.9735428 

# create dataset for merge with controls
liss_matched_mergecontrols_nonparents <- liss_matched_nonparents %>% rename(
  nomem_encr = pid_control,
  pscore = pscore_control,
  nomem_encr_case = pid_case
) %>% select(nomem_encr, nomem_encr_case, match_year, pscore) %>% 
  arrange(nomem_encr)

# create dataset for merge with cases
liss_matched_mergecases_nonparents <- liss_matched_nonparents %>% rename(
  nomem_encr = pid_case,
  pscore = pscore_case,
  nomem_encr_control = pid_control
) %>% select(nomem_encr, nomem_encr_control, match_year, pscore) %>% 
  arrange(nomem_encr)


#### compile final analysis sample -> (2) nonparent control group ####

lissanalysis_nonparents <- lisslongvalid %>% filter(!is.na(grandparent) & droplater!=T)

lissanalysis_nonparents <- left_join(lissanalysis_nonparents, liss_matched_mergecases_nonparents, by='nomem_encr')
lissanalysis_nonparents <- left_join(lissanalysis_nonparents, liss_matched_mergecontrols_nonparents, by='nomem_encr')

# create dataset for merge containing information on whether a control was 
# at time==-5, time==-4, time==-3, or time==-2 at the time of matching (counterfactual timeframe)
time_for_controls_liss <- lisslongvalid %>% 
  filter(grandparent==1 & nokids==0 & time==matchtime & droplater==F) %>% # this was the condition by which GP observations were chosen for matching (see above)
  select(nomem_encr, matchtime, valid) %>% rename(
    matchtime_controls = matchtime,
    matchvalid_controls = valid) # same procedure for 'valid' (counts valid observations in relation to transition time point for GPs)
table(time_for_controls_liss$matchtime_controls) # different time lags between matching and transition to GP
table(time_for_controls_liss$matchvalid_controls) 
# some GPs were not matched at valid==-1 because we wanted to avoid having only 1 year between matching and 
# transition, because parents might have already been pregnant with the grandchildren at this point.

# match via nomem_encr_case (which gives the ID of each control's match) in order 
# to transfer 'matchtime' value from the cases to controls
lissanalysis_nonparents <- left_join(lissanalysis_nonparents, 
                                  time_for_controls_liss, by=c('nomem_encr_case'='nomem_encr')) 

lissanalysis_nonparents <- lissanalysis_nonparents %>% mutate( # this just unites information from cases and controls in a single variable
  pscore = ifelse(!is.na(pscore.x), pscore.x, pscore.y),
  match_year = ifelse(!is.na(match_year.x), match_year.x, match_year.y),
  nomem_encr_match = ifelse(!is.na(nomem_encr_case), nomem_encr_case, nomem_encr_control),
  matchtime = ifelse(!is.na(matchtime), matchtime, matchtime_controls), 
) %>% 
  select(-c(pscore.x, pscore.y, match_year.x, match_year.y, nomem_encr_case, nomem_encr_control,
            matchtime_controls)) %>%
  filter(!is.na(pscore)) # only keep cases and controls
# reminder: 'matchtime' is the original var indicating (in years) when matching occurred for grandparents-to-be
#           in relation to the birth of their grandchild. This value was now transferred to the controls via the 
#           var 'matchtime_controls'.
table(lissanalysis_nonparents$grandparent, lissanalysis_nonparents$matchtime)
table(lissanalysis_nonparents$grandparent, lissanalysis_nonparents$matchvalid_controls)

# coding counterfactual timeframe for controls
# create time variable for controls relative to the time point of matching (& 'valid' variable)
lissanalysis_nonparents <- lissanalysis_nonparents %>% mutate(
  time = ifelse(is.na(time) & match_year==year, matchtime, time),
  # variable 'match_year' relates to the controls
  valid = ifelse(is.na(valid) & match_year==year, matchvalid_controls, valid)
)
# reminder: 'time' counts calendar years in relation to transition to GP, 'valid' only valid assessments
# note that we have 1:1 corresponding observations in 'time' and 'valid' between cases and controls only 
# at the time of matching. Earlier or later observations will differ because of different patterns of
# panel attrition and participation.
table(lissanalysis_nonparents$grandparent, lissanalysis_nonparents$time)
table(lissanalysis_nonparents$grandparent, lissanalysis_nonparents$valid)

lissanalysis_nonparents <- lissanalysis_nonparents %>% mutate(
  time = ifelse(is.na(time) & matchtime==-5, (year - match_year) - 5, time),
  time = ifelse(is.na(time) & matchtime==-4, (year - match_year) - 4, time),
  time = ifelse(is.na(time) & matchtime==-3, (year - match_year) - 3, time),
  time = ifelse(is.na(time) & matchtime==-2, (year - match_year) - 2, time))

lissanalysis_nonparents <- lissanalysis_nonparents %>% group_by(nomem_encr) %>% 
  mutate(lastcount = ifelse(grandparent==0 & valid==matchvalid_controls, row_number(), NA)) %>% ungroup()
lissanalysis_nonparents <- lissanalysis_nonparents %>% group_by(nomem_encr) %>% 
  mutate(lastcount = max(lastcount, na.rm = T)) %>% ungroup() %>% 
  mutate(lastcount = replace(lastcount, lastcount==-Inf, NA))

# finish coding 'valid' for controls
lissanalysis_nonparents <- lissanalysis_nonparents %>% group_by(nomem_encr) %>% 
  mutate(helpcount = row_number()) %>% ungroup()
lissanalysis_nonparents <- lissanalysis_nonparents %>% 
  mutate(valid = ifelse(is.na(valid) & grandparent==0, helpcount - lastcount + matchvalid_controls, valid)) %>% 
  select(-helpcount, -lastcount)

table(lissanalysis_nonparents$grandparent, lissanalysis_nonparents$valid)
table(lissanalysis_nonparents$grandparent, lissanalysis_nonparents$time) # cells earlier than '-5' too small
# time==5 also very small....

lissanalysis_nonparents <- lissanalysis_nonparents %>% filter(time %in% c(-5:5)) %>% 
  select(-match_year, -droplater, -matchvalid_controls)

# save .rda 
save(lissanalysis_nonparents, file = "data/processed/LISS/lissanalysis_nonparents.rda")


#### PSM: 'rollingMatch' -> (2) nonparent control group ####

# GroupMatch package (the development version of the package is called 'rollingMatch')

# build/install development version via devtools commands
#library(devtools)
#build("S:/MA/mkraemer/Paper_Grandparenthood/GroupMatch development version/rollingMatch")
#build("/Users/michaelkramer/Documents/Paper_Grandparenthood/GroupMatch development version/rollingMatch")
#install("/Users/michaelkramer/Documents/Paper_Grandparenthood/GroupMatch development version/rollingMatch")

# this did not work:
#devtools::install_github("jgellar/rollingMatch", auth_token = "506524f03641945dc771f973a3d6131ab9edad26")

library(rollingMatch)

table(lissimp_matching_nonparents$grandparent)
table(lissimp_matching_nonparents$grandparent, lissimp_matching_nonparents$year)

#we also need the 'time' value for grandparents (-5, -4, -3 or -2) and the 'valid' variable
lissimp_groupmatch_nonparents <- left_join(lissimp_matching_nonparents, 
                                        lissimp_matching_1, by=c("nomem_encr", "year")) %>% 
  filter(droplater==F) %>% 
  select(nomem_encr, year, female.x, grandparent.x, pscore, time, valid) %>% 
  rename(female = female.x, grandparent = grandparent.x)

table(lissimp_groupmatch_nonparents$grandparent, lissimp_groupmatch_nonparents$time)
table(lissimp_groupmatch_nonparents$grandparent, lissimp_groupmatch_nonparents$valid)

# Description
# This is an adaption of fullmatch to allow for restrictions when control observations 
# are "grouped". The motivating use case is when there are multiple observations of control 
# data for each control subject. In this case, the grouping variable is the subject. We may 
# want to place restrictions, for example that only one observation of a subject can be 
# matched, or in the case of one:many matching, a given control subject can only be matched 
# to a given treated subject once.

#matrix of propensity score distances (uses the same 'pscore' variable as the K&R matching loop)
match_on_nonparents_ps <- as.matrix(match_on(grandparent~pscore, data=lissimp_groupmatch_nonparents))

#caliper matrix to exact-match on gender
match_on_nonparents_female <- as.matrix(exactMatch(grandparent ~ female, data=lissimp_groupmatch_nonparents))

#caliper matrix to match within $500 of income (also possible)
#inc <-  as.matrix(caliper(match_on(treat~income), distance = "Euclidean", width = 500))

#final input to rollingMatch is the sum of these matrices
final_dist_nonparents <- match_on_nonparents_ps + match_on_nonparents_female
# 1079913 elements in matrix -> same as in Cartesian product in DIY matching loop

liss_groupmatch_nonparents <- groupmatch(x=final_dist_nonparents, 
                                      group = lissimp_groupmatch_nonparents$nomem_encr, allow_duplicates = T, 
                                      min.controls = 0, max.controls = 1, omit.fraction = NULL, 
                                      mean.controls = NULL, tol = 0.001, data = lissimp_groupmatch_nonparents)
# allowing duplicates greatly improves cov balance in our situation where we have few available controls

summary(liss_groupmatch_nonparents) #seems to work for 1:1 matching without replacement despite the warning message!

#matched(liss_groupmatch_nonparents)
sum(matched(liss_groupmatch_nonparents))

#unmatched(liss_groupmatch_nonparents)
sum(unmatched(liss_groupmatch_nonparents))

#matchfailed(liss_groupmatch_nonparents)
sum(matchfailed(liss_groupmatch_nonparents))

liss_groupmatch_data_nonparents <- cbind(lissimp_groupmatch_nonparents, matches=liss_groupmatch_nonparents) %>% 
  filter(!is.na(matches))
liss_groupmatch_data_nonparents <- liss_groupmatch_data_nonparents %>% group_by(matches) %>% 
  mutate(time = ifelse(is.na(time), max(time, na.rm = T), time), # same information as 'matchtime' now (but already transferred to controls, too)
         valid = ifelse(is.na(valid), max(valid, na.rm = T), valid)) %>% ungroup %>% 
  select(-matches)

table(liss_groupmatch_data_nonparents$grandparent, liss_groupmatch_data_nonparents$year)
table(liss_groupmatch_data_nonparents$grandparent, liss_groupmatch_data_nonparents$time)
table(liss_groupmatch_data_nonparents$grandparent, liss_groupmatch_data_nonparents$valid)
table(liss_groupmatch_data_nonparents$grandparent, liss_groupmatch_data_nonparents$female) # exact matching on gender!

liss_groupmatch_data_nonparents <- left_join(liss_groupmatch_data_nonparents, lissimp_nonparents_ps_1,
                                          by = c("nomem_encr", "year", "grandparent", "female"))

# for balance assessment (at the time of matching - using the variables containing imputed values)
liss_bal_nonparents_groupmatch <- liss_groupmatch_data_nonparents %>%
  select(nomem_encr, grandparent, pscore, female, everything(), -time, -year, -valid)

liss_groupmatch_data_nonparents <- liss_groupmatch_data_nonparents %>% 
  select(nomem_encr, year, grandparent, time, valid, pscore) %>% 
  rename(match_year = year, time_match = time, valid_match = valid) %>% 
  mutate(match_number = row_number()) # if we allow duplicate matches, we need an unambiguous identifier for later

# compile analysis sample with all longitudinal observations
lissanalysis_nonparents_groupmatch <- left_join(liss_groupmatch_data_nonparents, lisslongvalid,
                                             by = c("nomem_encr", "grandparent"))

table(lissanalysis_nonparents_groupmatch$grandparent, lissanalysis_nonparents_groupmatch$time_match) # already transferred to controls 
table(lissanalysis_nonparents_groupmatch$grandparent, lissanalysis_nonparents_groupmatch$matchtime)

lissanalysis_nonparents_groupmatch <- lissanalysis_nonparents_groupmatch %>%
  select(-matchtime) %>% rename(matchtime = time_match) # for more consistency with the matching loop method

table(lissanalysis_nonparents_groupmatch$grandparent, lissanalysis_nonparents_groupmatch$valid)
table(lissanalysis_nonparents_groupmatch$grandparent, lissanalysis_nonparents_groupmatch$valid_match)

# create time variable for controls relative to the time point of matching (& 'valid' variable)
lissanalysis_nonparents_groupmatch <- lissanalysis_nonparents_groupmatch %>% mutate(
  time = ifelse(is.na(time) & match_year==year, matchtime, time),
  # variable 'match_year' relates to the controls!
  valid = ifelse(is.na(valid) & match_year==year, valid_match, valid)
)
#reminder: 'time' counts calendar years in relation to transition to GP, 'valid' only valid assessments
table(lissanalysis_nonparents_groupmatch$grandparent, lissanalysis_nonparents_groupmatch$time)
table(lissanalysis_nonparents_groupmatch$grandparent, lissanalysis_nonparents_groupmatch$valid)

lissanalysis_nonparents_groupmatch <- lissanalysis_nonparents_groupmatch %>% filter(!is.na(pscore)) %>% 
  mutate(
    time = ifelse(grandparent==0 & is.na(time) & matchtime==-5, (year - match_year) - 5, time),
    time = ifelse(grandparent==0 & is.na(time) & matchtime==-4, (year - match_year) - 4, time),
    time = ifelse(grandparent==0 & is.na(time) & matchtime==-3, (year - match_year) - 3, time),
    time = ifelse(grandparent==0 & is.na(time) & matchtime==-2, (year - match_year) - 2, time))

lissanalysis_nonparents_groupmatch <- lissanalysis_nonparents_groupmatch %>% group_by(match_number) %>% 
  mutate(lastcount = ifelse(grandparent==0 & valid==valid_match, row_number(), NA)) %>% ungroup()
lissanalysis_nonparents_groupmatch <- lissanalysis_nonparents_groupmatch %>% group_by(match_number) %>% 
  mutate(lastcount = max(lastcount, na.rm = T)) %>% ungroup() %>% 
  mutate(lastcount = replace(lastcount, lastcount==-Inf, NA)) # regular NA pls

# finish coding 'valid' for controls 
# grouping by 'match_number' here instead of 'nomem_encr' because we allowed duplicate matches
lissanalysis_nonparents_groupmatch <- lissanalysis_nonparents_groupmatch %>% group_by(match_number) %>% 
  mutate(helpcount = row_number()) %>% ungroup()
lissanalysis_nonparents_groupmatch <- lissanalysis_nonparents_groupmatch %>% 
  mutate(valid = ifelse(is.na(valid) & grandparent==0, helpcount - lastcount + valid_match, valid)) %>% 
  select(-helpcount, -lastcount)

table(lissanalysis_nonparents_groupmatch$grandparent, lissanalysis_nonparents_groupmatch$time)
table(lissanalysis_nonparents_groupmatch$grandparent, lissanalysis_nonparents_groupmatch$valid)
table(lissanalysis_nonparents_groupmatch$grandparent, lissanalysis_nonparents_groupmatch$year)

lissanalysis_nonparents_groupmatch <- lissanalysis_nonparents_groupmatch %>% filter(time %in% c(-5:5)) %>% 
  select(-match_year, -valid_match, -nohouse_encr, -droplater)

#sample comparison - different matching methods
table(lissanalysis_nonparents$grandparent, lissanalysis_nonparents$time)
table(lissanalysis_nonparents_groupmatch$grandparent, lissanalysis_nonparents_groupmatch$time)

# save .rda 
save(lissanalysis_nonparents_groupmatch, file = "data/processed/LISS/lissanalysis_nonparents_groupmatch.rda")


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
liss_bal_parents <- lissanalysis_parents %>% filter(time==matchtime) %>%
  select(nomem_encr, year, grandparent, pscore)
liss_bal_parents <- left_join(liss_bal_parents, lissimp_parents_ps_1, by=c("nomem_encr", "year", "grandparent")) %>% 
  select(nomem_encr, grandparent, pscore, female, everything(), -year) 
#imputed values of the covariates (or should I use the ones containing missings?)
summary(liss_bal_parents)

liss_bal_nonparents <- lissanalysis_nonparents %>% filter(time==matchtime) %>% 
  select(nomem_encr, year, grandparent, pscore)
liss_bal_nonparents <- left_join(liss_bal_nonparents, lissimp_nonparents_ps_1, by=c("nomem_encr", "year", "grandparent")) %>% 
  select(nomem_encr, grandparent, pscore, female, everything(), -year) 
#imputed values of the covariates (or should I use the ones containing missings?)
summary(liss_bal_nonparents)

# balance BEFORE matching
liss_bal_parents_before <- lissimp_matching_parents %>% 
  select(nomem_encr, year, grandparent, pscore) # PS averaged from imp=5
liss_bal_parents_before <- left_join(liss_bal_parents_before, lissimp_matching_1)
liss_bal_parents_before <- liss_bal_parents_before %>% 
  select(nomem_encr, grandparent, pscore, female, everything(), 
         -c(time, year, valid, droplater, matchtime, nokids),
         -c(retire_early, retirement, heartattack, stroke, cancer, rentfree, businessdwelling, otherdwelling,
            jobseeker, pensioner, disability, primaryschool, poorhealth, excellenthealth), # too infrequent, see above
         -c(paid_work, more_paid_work, financialsit, difficultybills, secondhouse, # these are not critical (substantively)
            speakdutch, bmi, chronicdisease, diabetes, nodisease, mobility, dep, flatapartment, 
            farmhouse, familybusiness, freelancer, housekeeper, degreeother, moderatehealth, 
            verygoodhealth, moderatehealth, verygoodhealth)) #,
         #-c(religion, currentpartner, livetogether, hhmembers, rental, degreehighersec,
         #   degreevocational, degreecollege, degreeuniversity, divorced, widowed, single, 
         #   extremelyurban, moderatelyurban, slightlyurban, noturban, logincome, livedhere, rooms))
summary(liss_bal_parents_before)

names(liss_bal_parents_before) # column names must be aligned!
names(liss_bal_parents)
names(liss_bal_parents_groupmatch)

liss_bal_nonparents_before <- lissimp_matching_nonparents %>% 
  select(nomem_encr, year, grandparent, pscore) # PS averaged from imp=5
liss_bal_nonparents_before <- left_join(liss_bal_nonparents_before, lissimp_matching_1)
liss_bal_nonparents_before <- liss_bal_nonparents_before %>% 
  select(nomem_encr, grandparent, pscore, female, everything(), 
         -c(time, year, valid, droplater, matchtime, contains("kid"), totalchildren),
         -c(retire_early, retirement, heartattack, stroke, cancer, rentfree, businessdwelling, otherdwelling,
            jobseeker, pensioner, disability, primaryschool, poorhealth, excellenthealth), # too infrequent, see above
         -c(paid_work, more_paid_work, financialsit, difficultybills, secondhouse, # these are not critical (substantively)
            speakdutch, bmi, chronicdisease, diabetes, nodisease, mobility, dep, flatapartment, 
            farmhouse, familybusiness, freelancer, housekeeper, degreeother, moderatehealth, 
            verygoodhealth, moderatehealth, verygoodhealth)) #,
         #-c(religion, currentpartner, livetogether, hhmembers, rental, degreehighersec,
         #   degreevocational, degreecollege, degreeuniversity, divorced, widowed, single, 
         #   extremelyurban, moderatelyurban, slightlyurban, noturban, logincome, livedhere, rooms))
summary(liss_bal_nonparents_before)

names(liss_bal_nonparents_before) # column names must be aligned!
names(liss_bal_nonparents)
names(liss_bal_nonparents_groupmatch)

# evaluate standardized difference in means
# create maxtrix object with empty vectors 'stddiff'
# (1) PARENTS
varnum_parents <-  1:(length(liss_bal_parents)-2)
covar_parents <-  colnames(liss_bal_parents[3:paste(length(liss_bal_parents))])
stddiff_before_parents <- numeric(length = length(liss_bal_parents_before)-2)     #before matching
stddiff_after1_parents <- numeric(length = length(liss_bal_parents)-2)            #K&R matching loop
stddiff_after2_parents <- numeric(length = length(liss_bal_parents_groupmatch)-2) #from 'rollingMatch' (GroupMatch) package  

coln_parents <- c("varnum_parents", "covar_parents", "stddiff_before_parents", 
                  "stddiff_after1_parents", "stddiff_after2_parents") # defining column names 
# creating matrix 
liss_balance_matrix_parents <- matrix(c(varnum_parents, covar_parents, stddiff_before_parents,
                                       stddiff_after1_parents, stddiff_after2_parents), ncol = 5, 
                                     dimnames = list(varnum_parents, coln_parents))

# use custom function in for-loop to fill matrix vectors 'stddiff'
for (i in seq_along(liss_balance_matrix_parents[varnum_parents])) {
  liss_balance_matrix_parents[[i, 3]] <- stdmeandiff(get(liss_balance_matrix_parents[[i, 2]]), 
                                                    grandparent, liss_bal_parents_before)     #before matching
  liss_balance_matrix_parents[[i, 4]] <- stdmeandiff(get(liss_balance_matrix_parents[[i, 2]]), 
                                                    grandparent, liss_bal_parents)            #DIY matching loop
  liss_balance_matrix_parents[[i, 5]] <- stdmeandiff(get(liss_balance_matrix_parents[[i, 2]]), 
                                                    grandparent, liss_bal_parents_groupmatch) #from 'rollingMatch' (GroupMatch) package
}
liss_balance_matrix_parents[, 3:5] <- round(as.numeric(liss_balance_matrix_parents[, 3:5]), 3)

kable(liss_balance_matrix_parents[, 2:5], format="rst", 
      col.names = c("Covariate", "Before Matching",
                    "DIY Matching Loop", "'rollingMatch'"), 
      align = "lccc", digits=2, caption = "Table 1. Covariate Balance")


# (2) NONPARENTS

# create maxtrix object with empty vectors 'stddiff'
varnum_nonparents <-  1:(length(liss_bal_nonparents)-2)
covar_nonparents <-  colnames(liss_bal_nonparents[3:paste(length(liss_bal_nonparents))])
stddiff_before_nonparents <- numeric(length = length(liss_bal_nonparents_before)-2)     #before matching
stddiff_after1_nonparents <- numeric(length = length(liss_bal_nonparents)-2)            #K&R matching loop
stddiff_after2_nonparents <- numeric(length = length(liss_bal_nonparents_groupmatch)-2) #from 'rollingMatch' (GroupMatch) package  

coln_nonparents <- c("varnum_nonparents", "covar_nonparents", "stddiff_before_nonparents", 
                     "stddiff_after1_nonparents", "stddiff_after2_nonparents") # defining column names 
# creating matrix 
liss_balance_matrix_nonparents <- matrix(c(varnum_nonparents, covar_nonparents, stddiff_before_nonparents,
                                          stddiff_after1_nonparents, stddiff_after2_nonparents), ncol = 5, 
                                        dimnames = list(varnum_nonparents, coln_nonparents))

# use custom function in for-loop to fill matrix vectors 'stddiff'
for (i in seq_along(liss_balance_matrix_nonparents[varnum_nonparents])) {
  liss_balance_matrix_nonparents[[i, 3]] <- stdmeandiff(get(liss_balance_matrix_nonparents[[i, 2]]), 
                                                       grandparent, liss_bal_nonparents_before)     #before matching
  liss_balance_matrix_nonparents[[i, 4]] <- stdmeandiff(get(liss_balance_matrix_nonparents[[i, 2]]), 
                                                       grandparent, liss_bal_nonparents)            #DIY matching loop
  liss_balance_matrix_nonparents[[i, 5]] <- stdmeandiff(get(liss_balance_matrix_nonparents[[i, 2]]), 
                                                       grandparent, liss_bal_nonparents_groupmatch) #from 'rollingMatch' (GroupMatch) package
}
liss_balance_matrix_nonparents[, 3:5] <- round(as.numeric(liss_balance_matrix_nonparents[, 3:5]), 3)

kable(liss_balance_matrix_nonparents[, 2:5], format="rst", 
      col.names = c("Covariate", "Before Matching",
                    "DIY Matching Loop", "'rollingMatch'"), 
      align = "lccc", digits=2, caption = "Table 1. Covariate Balance")
