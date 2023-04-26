library(tidyverse)

## 2021 Data
math <- read.csv("math.csv")
RLA <- read.csv("RLA.csv")

RLA_2021 <- RLA %>%
  filter(STNAM == "CONNECTICUT") %>%
  filter(GRADE == "HS") %>%
  filter(NUMVALID > 50) %>%
  filter(CATEGORY == "ALL")

RLA_2021$LEANM = toupper(RLA_2021$LEANM)

RLA_2021$PCTPROF_2021<- substr(RLA_2021$PCTPROF, 1, 2)

RLA_2021_wide <- RLA_2021 %>%
  mutate(PCTPROF_2021 = as.numeric(PCTPROF_2021))

math_2021 <- math %>%
  filter(STNAM == "CONNECTICUT") %>%
  filter(GRADE == "HS") %>%
  filter(NUMVALID > 50) %>%
  filter(CATEGORY == "ALL") 

math_2021$LEANM = toupper(math_2021$LEANM)

math_2021$PCTPROF_2021<- substr(math_2021$PCTPROF, 1, 2)

math_2021_wide <- math_2021 %>%
  mutate(PCTPROF_2021 = as.numeric(PCTPROF_2021))

## 2019 Data
math_19 <- read.csv("math-achievement-lea-sy2018-19-long.csv")
RLA_19 <- read.csv("rla-achievement-lea-sy2018-19-long.csv")

RLA_2019 <- RLA_19 %>%
  filter(STNAM == "CONNECTICUT") %>%
  filter(GRADE == "HS") %>%
  filter(NUMVALID > 50) %>%
  filter(CATEGORY == "ALL")

RLA_2019$PCTPROF_2019<- substr(RLA_2019$PCTPROF, 1, 2)
RLA_2019_wide <- RLA_2019 %>%
  mutate(PCTPROF_2019 = as.numeric(PCTPROF_2019))


math_2019 <- math19 %>%
  filter(STNAM == "CONNECTICUT") %>%
  filter(GRADE == "HS") %>%
  filter(NUMVALID > 50) %>%
  filter(CATEGORY == "ALL")

math_2019$PCTPROF_2019<- substr(math_2019$PCTPROF, 1, 2)
math_2019_wide <- math_2019 %>%
  mutate(PCTPROF_2019 = as.numeric(PCTPROF_2019))

## Merge math scores

math_scores_wide <- merge(math_2019_wide, math_2021_wide, by.x ="LEANM", by.y = "LEANM")


## Merge RLA Scores

RLA_scores_wide <- merge(RLA_2019_wide, RLA_2021_wide, by.x ="LEANM", by.y = "LEANM")

## Select colums for math wide

math_scores_wide <- math_scores_wide %>%
  select('LEANM', 'PCTPROF_2019', 'PCTPROF_2021')

math_scores_wide$PCTPROF_CHANGE <- math_scores_wide$PCTPROF_2021 - math_scores_wide$PCTPROF_2019

## Select colums for RLA wide


RLA_scores_wide <- RLA_scores_wide %>%
  select('LEANM', 'PCTPROF_2019', 'PCTPROF_2021')
RLA_scores_wide$PCTPROF_CHANGE <- RLA_scores_wide$PCTPROF_2021 - RLA_scores_wide$PCTPROF_2019

## Map Plots

## math long data 

math_2019_long <- math_2019 %>%
  mutate(PCTPROF = as.numeric(PCTPROF_2019))

math_2019_long <- math_2019_long %>%
  select('SCHOOL_YEAR', 'LEANM', 'PCTPROF', 'STNAM')

math_2021_long <- math_2021 %>%
  mutate(PCTPROF = as.numeric(PCTPROF_2021))

math_2021_long <- math_2021_long %>%
  select('SCHOOL_YEAR', 'LEANM', 'PCTPROF', 'STNAM')

math_long <- bind_rows(math_2019_long, math_2021_long)

## RLA long data

RLA_2019_long <- RLA_2019 %>%
  mutate(PCTPROF = as.numeric(PCTPROF_2019))

RLA_2019_long <- RLA_2019_long %>%
  select('SCHOOL_YEAR', 'LEANM', 'PCTPROF')

RLA_2021_long <- RLA_2021 %>%
  mutate(PCTPROF = as.numeric(PCTPROF_2021))

RLA_2021_long <- RLA_2021_long %>%
  select('SCHOOL_YEAR', 'LEANM', 'PCTPROF')

RLA_long <- bind_rows(RLA_2019_long, RLA_2021_long)

## Violin Plots

box_plot_RLA <- ggplot(RLA_long) +
  geom_boxplot(aes(x=PCTPROF, y=SCHOOL_YEAR, fill=SCHOOL_YEAR),
              adjust=1.5) +
  geom_boxplot(aes(x=PCTPROF, y=SCHOOL_YEAR), width=0.25)+
  scale_x_continuous(breaks=c( 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     labels=c( "0", "10%", "20%", "30%", "40%", "50%", "60%", "70%",
                               "80%", "90%", "100%"),
                     limits=c(0, 110)) +
  stat_summary(mapping=aes(x=PCTPROF, y=SCHOOL_YEAR, fill=SCHOOL_YEAR),
               fun="mean", geom="Point", shape=18, size=3.5)

math_Violinplot <- ggplot(math_long) +
  geom_violin(aes(x=PCTPROF, y=SCHOOL_YEAR, fill=SCHOOL_YEAR),
              adjust=1.5) +
  geom_boxplot(aes(x=PCTPROF, y=SCHOOL_YEAR), width=0.25)+
  scale_x_continuous(breaks=c( 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     labels=c( "0", "10%", "20%", "30%", "40%", "50%", "60%", "70%",
                               "80%", "90%", "100%"),
                     limits=c(0, 110)) +
  stat_summary(mapping=aes(x=PCTPROF, y=SCHOOL_YEAR, fill=SCHOOL_YEAR),
               fun="mean", geom="Point", shape=18, size=3.5)

## CT and Nationwide Average
Math_CTAV_2019 <- math_2019_long %>%
  group_by(STNAM) %>%
  summarize(PCT_PROF = mean(PCTPROF))

Math_CTAV_2019$Year <- "2018-2019"

Math_CTAV_2021 <- math_2021_long %>%
  group_by(STNAM) %>%
  summarize(PCT_PROF = mean(PCTPROF))

Math_CTAV_2021$Year <- "2020-2021"

Math_AV <- bind_rows(Math_CTAV_2019, Math_CTAV_2021)

math_AV2019 <- math_19 %>%
  filter(GRADE == "HS") %>%
  filter(NUMVALID > 50) %>%
  filter(CATEGORY == "ALL")

math_AV2019$PCTPROF<- substr(math_AV2019$PCTPROF, 1, 2)
math_AV2019 <- math_AV2019 %>%
  mutate(PCTPROF = as.numeric(PCTPROF)) %>%
  filter(PCTPROF != "NA")

math_AV2019 <- math_AV2019 %>%
  group_by(SCHOOL_YEAR) %>%
  summarize(PCT_PROF = mean(PCTPROF))

math_AV2019 <- math_AV2019 %>%
  select('PCT_PROF') 
math_AV2019$Year <- "2018-2019"
math_AV2019$STNAM <- "All"

Math_AV <- bind_rows(Math_AV, math_AV2019)

math_AV2021 <- math %>%
  filter(GRADE == "HS") %>%
  filter(NUMVALID > 50) %>%
  filter(CATEGORY == "ALL")

math_AV2021$PCTPROF<- substr(math_AV2021$PCTPROF, 1, 2)
math_AV2021 <- math_AV2021 %>%
  mutate(PCTPROF = as.numeric(PCTPROF)) %>%
  filter(PCTPROF != "NA")

math_AV2021 <- math_AV2021 %>%
  group_by(SCHOOL_YEAR) %>%
  summarize(PCT_PROF = mean(PCTPROF))

math_AV2021 <- math_AV2021 %>%
  select('PCT_PROF') 
math_AV2021$Year <- "2020-2021"
math_AV2021$STNAM <- "All"

Math_AV <- bind_rows(Math_AV, math_AV2021)

## Math plot

ggplot(Math_AV) +
  geom_line(aes(x=Year, y=PCT_PROF, group=STNAM, color=STNAM)) +
  scale_y_continuous(breaks=c( 38, 39, 40, 41, 42, 43, 44),
                     labels=c("38%", "39%", "40%", "41%", "42%",
                               "43%", "44%"),
                     limits =c(38, 44))

## Map plot

CT_counties <- map_data("county") %>% 
  filter(region=="connecticut")


