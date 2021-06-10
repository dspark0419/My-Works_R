# dplyr functions

library(tidyverse)
chicago <- readRDS("chicago.rds")
str(chicago)

# select
S1 <- select(chicago, city:dptp)
S2 <- select(chicago, -(city:dptp))
S3 <- select(chicago, ends_with("2"))
S4 <- select(chicago, starts_with("d"))
S5 <- select(chicago, date, everything()) # put the variable date first and then the rest
S6 <- select(chicago, contains("mean"))

# filter
F1 <- filter(chicago, tmpd < 32 | dptp >70)
F2 <- filter(chicago, pm25tmean2 > 30 & tmpd > 80)
F3 <- select(F2, date, tmpd, pm25tmean2)
F4 <- chicago %>% filter(!is.na(pm25tmean2), !is.na(pm10tmean2))
sum(is.na(F4$pm25tmean2))
summarise(F4, sum(is.na(pm25tmean2))) # almost same as above

# arrange
A1 <- arrange(chicago, date)
A2 <- arrange(chicago, desc(date))
head(select(A2, date, tmpd), 3)
A3 <- arrange(chicago, desc(o3tmean2 - no2tmean2))
head(A3)

# rename
R1 <- rename(chicago, dewpt=dptp, pm25=pm25tmean2)
head(R1[, 1:5],3)

# mutate
M1 <- mutate(R1, pm25detrend = pm25 - mean(pm25, na.rm=TRUE))
head(M1)
M2 <- mutate(M1, year = as.POSIXlt(date)$year + 1900)
head(M2)

# summarize
summarise(chicago, mean(tmpd, na.rm = TRUE))
summarise_at(chicago, vars(tmpd), list(mean = mean), na.rm = TRUE) # same as above
summarise_at(chicago, vars(tmpd, pm25tmean2), 
             list(mean = mean, n = ~n(), sd =sd), na.rm = TRUE) 
# doing above for multi-summaries about multivariables

# group_by
by_year <- group_by(M2, year)
summarise(by_year, 
          pm25 = mean(pm25, na.rm=TRUE), 
          o3=max(o3tmean2, na.rm = TRUE), 
          no2=median(no2tmean2, na.rm = TRUE))
M2 %>% mutate(year = as.POSIXlt(date)$year + 1900) %>% # M2
  group_by(year) %>% # by_group
  summarise(pm25 = mean(pm25, na.rm=TRUE), 
            o3=max(o3tmean2, na.rm = TRUE), 
            no2=median(no2tmean2, na.rm = TRUE))

quintiles <- quantile(M2$pm25, seq(0, 1, length = 6), na.rm = TRUE) # quintiles of pm25
quintiles
M2 %>%
  mutate(pm25.quint = cut(pm25, quintiles)) %>% # pm25 divided into quintiles
  group_by(pm25.quint) %>%
  summarise(o3 = mean(o3tmean2, na.rm = TRUE),
            n02 = mean(no2tmean2, na.rm = TRUE))

# add the variable temp_cat according to # of sd from the mean
sd_temp <- sd(M2$tmpd, na.rm = TRUE)
mean_temp <- mean(M2$tmpd, na.rm = TRUE)
M3 <- M2 %>% 
  mutate(temp_cat = 
           ifelse(tmpd <= mean_temp - 2*sd_temp, "Extreme Cold",
                  ifelse(tmpd <= mean_temp -sd_temp, "Cold",
                         ifelse(tmpd <= mean_temp, "Chilly",
                                ifelse(tmpd <= mean_temp + sd_temp, "Warm",
                                       ifelse(tmpd <= mean_temp + 2*sd_temp, "Hot",
                                              "Extreme Hot"))))))

temp_32_60 <- chicago %>% filter(between(tmpd, 32, 60))
chicago1 <- chicago %>% mutate(tmpd = ifelse(tmpd >= 32 & tmpd <= 60, tmpd, NA)) 
# almost same as above
mean(temp_32_60$tmpd, na.rm = TRUE) == mean(chicago1$tmpd, na.rm = TRUE)
