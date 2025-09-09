library(tidyverse)
library(fingertipsR)

# England data (fingertips) -----------------------------------------------

# download reception indicators from Fingertips using for loop
reception_indicators <- c(90316, 90317, 92464, 90319, 20601)
england_reception_fingertips <- data.frame()

for (id in reception_indicators){
  temp <- fingertips_data(IndicatorID = id, AreaTypeID = 15) |> 
    filter(Sex == "Persons") |> 
    select(Timeperiod, Count, Denominator) |> 
    distinct() |> 
    mutate(School_Year = "Reception",
           indicator = id) |> 
    rename(Year = Timeperiod,
           count = Count,
           denominator = Denominator)
  england_reception_fingertips <- rbind(england_reception_fingertips, temp)
} 

# Add BMI category labels using lookup
reception_indicators_lookup <- data.frame(indicator = reception_indicators,
                                          BMI_Category = c("Underweight", "Healthy Weight", "Overweight", "Obese", "Overweight/Obese"))

england_reception_fingertips <- left_join(england_reception_fingertips,
                                          reception_indicators_lookup,
                                          by = "indicator") |> 
  select(-indicator)

# repeat for y6
# download reception indicators from Fingertips using for loop
y6_indicators <- c(90320, 90321, 92465, 90323, 20602)
england_y6_fingertips <- data.frame()

for (id in y6_indicators){
  temp <- fingertips_data(IndicatorID = id, AreaTypeID = 15) |> 
    filter(Sex == "Persons") |> 
    select(Timeperiod, Count, Denominator) |> 
    distinct() |> 
    mutate(School_Year = "Year 6",
           indicator = id) |> 
    rename(Year = Timeperiod,
           count = Count,
           denominator = Denominator)
  england_y6_fingertips <- rbind(england_y6_fingertips, temp)
} 

# Add BMI category labels using lookup
y6_indicators_lookup <- data.frame(indicator = y6_indicators,
                                   BMI_Category = c("Underweight", "Healthy Weight", "Overweight", "Obese", "Overweight/Obese"))

england_y6_fingertips <- left_join(england_y6_fingertips,
                                   y6_indicators_lookup,
                                   by = "indicator") |> 
  select(-indicator)

# bind year 6 and reception dfs together
england_fingertips <- rbind(england_reception_fingertips,
                            england_y6_fingertips)

# years formatted differently in fingertips data so create new ref vector
years_fingertips <- sub("(20)(.*?)(20)", "\\1\\2", years)

# create df with three years of data (first three years i.e. oldest)
england_data_period_1 <- england_fingertips |> 
  filter(Year == years_fingertips[1]|Year == years_fingertips[2]|Year == years_fingertips[3]) |> 
  mutate(Year = paste0(years_fingertips[1], " to ", years_fingertips[3]))

# same again but with next three year period
england_data_period_2 <- england_fingertips |> 
  filter(Year == years_fingertips[2]|Year == years_fingertips[3]|Year == years_fingertips[4]) |> 
  mutate(Year = paste0(years_fingertips[2], " to ", years_fingertips[4]))

# and so on
england_data_period_3 <- england_fingertips |> 
  filter(Year == years_fingertips[3]|Year == years_fingertips[4]|Year == years_fingertips[5]) |> 
  mutate(Year = paste0(years_fingertips[3], " to ", years_fingertips[5]))

england_data_period_4 <- england_fingertips |> 
  filter(Year == years_fingertips[4]|Year == years_fingertips[5]|Year == years_fingertips[6]) |> 
  mutate(Year = paste0(years_fingertips[4], " to ", years_fingertips[6]))

england_data_period_5 <- england_fingertips |> 
  filter(Year == years_fingertips[5]|Year == years_fingertips[6]|Year == years_fingertips[7]) |> 
  mutate(Year = paste0(years_fingertips[5], " to ", years_fingertips[7]))

england_data_period_6 <- england_fingertips |> 
  filter(Year == years_fingertips[6]|Year == years_fingertips[7]|Year == years_fingertips[8]) |> 
  mutate(Year = paste0(years_fingertips[6], " to ", years_fingertips[8]))

england_data_period_7 <- england_fingertips |> 
  filter(Year == years_fingertips[7]|Year == years_fingertips[8]|Year == years_fingertips[9]) |> 
  mutate(Year = paste0(years_fingertips[7], " to ", years_fingertips[9]))

england_data_period_8 <- england_fingertips |> 
  filter(Year == years_fingertips[8]|Year == years_fingertips[9]|Year == years_fingertips[10]) |> 
  mutate(Year = paste0(years_fingertips[8], " to ", years_fingertips[10]))

england_data_3_years_combined <- bind_rows(england_data_period_1,
                                           england_data_period_2,
                                           england_data_period_3,
                                           england_data_period_4,
                                           england_data_period_5,
                                           england_data_period_6,
                                           england_data_period_7,
                                           england_data_period_8)

# summarise by 3 year group
england_data_3_years_combined <- england_data_3_years_combined |> 
  group_by(Year, School_Year, BMI_Category) |> 
  summarise(count = sum(count),
            denominator = sum(denominator))

# calculate percentage and CIs
england_data_3_years_combined <-wilson_ci(england_data_3_years_combined,
                                          england_data_3_years_combined$count,
                                          england_data_3_years_combined$denominator) |> 
  mutate(geography = "England")

# save as .rda to save quarto having to load every time
save(england_data_3_years_combined, file = "data/england_data_3_years_combined.rda")

# Participation rates (fingertips) ----------------------------------------

# find out how many reception children were measured in most recent 3 year period according to fingertips
fingertips_reception_participants <- fingertips_data(IndicatorID = 90290, AreaTypeID = 502, AreaCode = "E08000025") |> 
  filter(Timeperiod == years_fingertips[8]|Timeperiod == years_fingertips[9]|Timeperiod == years_fingertips[10]) |> 
  summarise(count = sum(Count)) |> 
  pull(count)

# get mean reception participation rate for most recent period from fingertips
fingertips_reception_participation <- fingertips_data(IndicatorID = 90290, AreaTypeID = 502, AreaCode = "E08000025") |> 
  filter(Timeperiod == years_fingertips[8]|Timeperiod == years_fingertips[9]|Timeperiod == years_fingertips[10]) |> 
  summarise(mean_pct = mean(Value)) |> 
  pull(mean_pct)


# find out how many reception children are in the data used (for most recent period
data_reception_participants <- ncmp_data_period_8 |> 
  filter(School_Year == "Reception") |> 
  count() |> 
  pull(n)

# find out how many year 6 children were measured in most recent period according to fingertips
fingertips_y6_participants <- fingertips_data(IndicatorID = 90291, AreaTypeID = 502, AreaCode = "E08000025") |> 
  filter(Timeperiod == years_fingertips[8]|Timeperiod == years_fingertips[9]|Timeperiod == years_fingertips[10]) |> 
  summarise(count = sum(Count)) |> 
  pull(count)

# get mean year 6 participation rate for most recent period from fingertips
fingertips_y6_participation <- fingertips_data(IndicatorID = 90291, AreaTypeID = 502, AreaCode = "E08000025") |> 
  filter(Timeperiod == years_fingertips[8]|Timeperiod == years_fingertips[9]|Timeperiod == years_fingertips[10]) |> 
  summarise(mean_pct = mean(Value)) |> 
  pull(mean_pct)

# find out how many reception children are in the data used (for most recent period)
data_y6_participants <- ncmp_data_period_8 |> 
  filter(School_Year == "Year 6") |> 
  count() |> 
  pull(n)

# save values as .rda to save quarto having to load from fingertips every time
participation_info <- list(fingertips_reception_participants = fingertips_reception_participants,
                           fingertips_reception_participation = fingertips_reception_participation,
                           data_reception_participants = data_reception_participants,
                           fingertips_y6_participants = fingertips_y6_participants,
                           fingertips_y6_participation = fingertips_y6_participation,
                           data_y6_participants = data_y6_participants)

save(participation_info, file = "data/participation_info.rda")