# Gulf of Alaska walleye pollock reproductive potential
# ben.williams@noaa.gov
# 2022-1

# load ----
library(tidyverse)
library(lubridate)
library(mgcv)
library(scales)
library(PBSmapping)
library(funcr) #devtools::install_github("ben-williams/funcr")
theme_set(theme_report())
library(scico)
library(vroom)

# data ----
# fecundity data ----
vroom('data/fecundity.csv') %>%
  mutate(Year = factor(year),
         Month = factor(month),
         # age = ifelse(age > 10, 10, age),
         Age = factor(age),
         date = dmy(date),
         ID = factor(year + month * 0.1 + haul * 0.001),
         length = round(length),
         yc = year - age -1,
         Hist= factor(histocatagory)) %>%
  filter(long > -157, mfec>0) -> ss

# maturity data ----
vroom("data/update_data.csv") %>%
  mutate(Year = factor(year),
         age = ifelse(age>10,10,age),
         Age = factor(age),
         hauls = ifelse(is.na(hauls), cruise, hauls),
         hauls = factor(hauls),
         length = round(length),
         dum = 1,
         mature = case_when(maturity_table==3 & maturity <3 ~ 0,
                            maturity_table==3 & maturity>=3 ~ 1,
                            TRUE ~ mature),
         Mature = factor(mature),
         weight = ifelse(year>2013, weight * 1000, weight)) %>%
  filter(!is.na(weight), !is.na(length), !is.na(maturity), !is.na(age)) -> poll

# pre-spawning mature female data ----
poll %>%
  filter(maturity_table == 11 & maturity %in% c(4, 5) |
           maturity_table == 3  & maturity == 3) %>%
  droplevels() -> poll3

# SAFE biomass and abundance by age and year ----

vroom('data/pollock_abund.csv') %>%
  mutate(Age = factor(age),
         biomass = abundance * weight) %>%
  drop_na() -> wts

# enviro data ----
sst_dat <- vroom("data/sst_dat.csv")

# length-weight ----
# all females - examined using a linear model with log transformed variables
lw <- lm(log(weight) ~ log(length), data = poll)
# lwfit <- exp(fitted(lw) * exp((sigma(lw)^2) / 2))
# lwresid <- poll$weight - lwfit
# plot(lwfit, lwresid)
# abline(h=0, lty=4)
# summary(lw)

# pre-spawning females - examined using a linear model with log transformed variables
lw3 <- lm(log(weight) ~ log(length), data = poll3)
# lwfit3 <- exp(fitted(lw3) * exp((sigma(lw3)^2) / 2))
# lwresid3 <- poll3$weight - lwfit3
# plot(lwfit3, lwresid3)
# abline(h=0, lty=4)
# summary(lw3)

# Body condition Kr based upon predicted weight at length.
# Predict Kr for maturity data
# Body condition Kr based upon predicted weight at length.
# Predict Kr for maturity data
poll %>%
  mutate(w = exp(predict(lw, .)) * exp((sigma(lw)^2) / 2),
         Kr = weight / w) -> poll

poll3 %>%
  mutate(w = exp(predict(lw3, .)) * exp((sigma(lw)^2) / 2),
         Kr = weight / w) -> poll3

# Predict Kr for fecundity data
ss %>%
  mutate(age = ifelse(age>10, 10, age),
         Age = factor(age)) %>%
  mutate(w = exp(predict(lw3, .)) * exp((sigma(lw3)^2) / 2),
         Kr = weight / w) -> ss


# filter data ----
# Subset fecundity data to look at fecundity with complete age/length/weight data
ss %>%
  filter(length>0, weight>0, age>0) %>%
  mutate(dum=1) -> ssawl

# Subset fecundity with completelength/weight data
ss %>%
  filter(length>0, weight>0) %>%
  mutate(dum=1) -> sswl

# Subset fecundity with completelength/weight data
ss %>%
  filter(length>0, age>0) %>%
  mutate(dum=1) -> ssal

# Population abundance ----
# create multiple abundance indices

wts %>%
  group_by(age, year) %>%
  mutate(ay_abund = abundance,
         ay_bio = biomass) %>%
  group_by(year) %>%
  mutate(y_bio = sum(biomass),
         y_abund = sum(abundance)) %>%
  ungroup()  %>%
  mutate(lay_abund = lag(ay_abund),
         ly_abund = lag(y_abund),
         lay_bio  = lag(ay_bio),
         ly_bio  = lag(y_bio)) %>%
  pivot_longer(-c(year, age, abundance, Age, biomass)) %>%
  ungroup() %>%
  mutate(year = ifelse(str_detect(name, "l"), year + 1, year)) %>%
  dplyr::select(-c(abundance, biomass)) %>%
  drop_na() -> pop

# create full datasets

poll3 %>%
  dplyr::select(latitude, longitude, length, weight, age, maturity, maturity_table, mature, year, Year, Mature, Age, Kr) %>%
  left_join(pop) %>%
  left_join(sst_dat) %>%
  arrange(year) -> dat

poll %>%
  dplyr::select(latitude, longitude, length, weight, age, maturity, maturity_table, mature, year, Year, Mature, Age, Kr) %>%
  left_join(pop) %>%
  left_join(sst_dat) %>%
  arrange(year) %>%
  mutate(across(c(length, weight, value, summer, winter), scale)) -> dat2

ssawl %>%
  left_join(pop) %>%
  left_join(sst_dat) %>%
  arrange(year) %>%
  mutate(age = ifelse(age>10, 10, age),
         Age = factor(age)) -> dat3

sswl %>%
  left_join(pop) %>%
  left_join(sst_dat) %>%
  arrange(year) %>%
  mutate(age = ifelse(age>10, 10, age),
         Age = factor(age)) -> dat4
