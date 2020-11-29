################################################################################
#' Data Science Specialization - Johns Hopkins / Coursea
#' Reproducible Research - Course Project 2
#' Author: Dan Klemfuss
#' 
#' Description: This section captures some of the exploratory analysis of the 
#' data to answer the following questions: 
#' 1. Acroos the United States, which types of events (as indicated in the EVTYPE
#' variable) are most harmful with respect to population health? 
#' 2. Across the United States, which types of events have the greatest economic
#' consequences?
################################################################################
##
## 0. Load packages
##
library(dplyr)
library(data.table)
library(cacher)
library(R.utils)
library(ggplot2)
library(scales)


##
## 1. Load Data
##

# File loaded in .bz2 format, in 'data' directory: 
raw.data <- fread('./data/repdata_data_StormData.csv.bz2')

# After reading in data, we see that there are 902297 obs. of 37 variables
# To get a sense of data, look at the column names, and structure: 
names(raw.data)
str(raw.data)
summary(raw.data)


##
## Explore Data
##

# Look at first 1000 rows: 
test1 <- raw.data[1:1000]

# We notice that there are certain events that have 0.00 for 'PROPDMGEXP', which
# can be omitted once we look at the second question. After reviewing data it 
# seems appropriate that we split and reduce the data to one data frame containing
# events that had greater than zero fatalities/deaths, and one that has greater 
# than zero property damage.

# For Population Health Impact: 
class(raw.data$FATALITIES) # Numeric
class(raw.data$INJURIES) # Numeric

# Filter out cases where there were zero fatalities and zero injuries:
pop.impact.data <- raw.data %>% filter(FATALITIES > 0 | INJURIES > 0) # 21929 obs

# Define total impact as sum of injuries and fatalities: 
pop.impact.data <- pop.impact.data %>% mutate(
  total_population_impact = FATALITIES + INJURIES
)


# For Economic Consequence: 
class(raw.data$PROPDMG) # Numeric
class(raw.data$CROPDMG) # Numeric
econ.impact.data <- raw.data %>% filter(PROPDMG >0 | CROPDMG >0) # 245031 obs

# Investigate 'PROPDMGEXP' and 'CROPDMGEXP' values: 
unique(econ.impact.data$PROPDMGEXP)
# We see prop damage has: "K" "M" "B" "m" ""  "+" "0" "5" "6" "4" "h" "2" "7" "3" "H" "-"
# Investigating some of the unitutive fields (i.e. +, -, 5, H, etc.)
# (H/h)  = hundreds
# (K/k) = thousands
# (M/m) = millions
# (B/b) = billions
# (+) = 1's (not scaled)
# (-/?) = 0
# (0-8) = tens

# Since the relative scale of the zeros, +/- and tens are tiny compared to the
# Others, only keep the Hundreds, Thousands, Millions, and Billions
econ.impact.data2 <- econ.impact.data[grepl('H|K|M|B', toupper(econ.impact.data$PROPDMGEXP)) |
                                       grepl('H|K|M|B', toupper(econ.impact.data$CROPDMGEXP)),]
# Reduces data from 245031 to 244722 (diff of 309 instances)

# Transform the data to a standard dollar amount: 
econ.impact.data2 <- econ.impact.data2 %>% mutate(
  prop_dmg_impact = case_when(toupper(PROPDMGEXP)=="H" ~ PROPDMG*100,
                              toupper(PROPDMGEXP)=="K" ~ PROPDMG*1000,
                              toupper(PROPDMGEXP)=="M" ~ PROPDMG*1000000,
                              toupper(PROPDMGEXP)=="B" ~ PROPDMG*1000000000,
                              TRUE ~ 0),
  crop_dmg_impact = case_when(toupper(CROPDMGEXP)=="H" ~ CROPDMG*100,
                              toupper(CROPDMGEXP)=="K" ~ CROPDMG*1000,
                              toupper(CROPDMGEXP)=="M" ~ CROPDMG*1000000,
                              toupper(CROPDMGEXP)=="B" ~ CROPDMG*1000000000,
                              TRUE ~ 0),
  total_dmg_impact = prop_dmg_impact + crop_dmg_impact
)

## 
## 2. Determine events that are most harmful to population health
##

# Using the documentation, we assume that population health impact is defined as
# events related to Fatalities/Injuries. From the summary earlier, we see that 
# for the 'FATALITIES' field, the min/1st Quartile/Median are all zero, with the
# Max value being 583. With Injurites, we see the same zero values for min/1st
# Quartile/Median, and see a maximum value of 1700. 

# Since it is not specified, we'll make the determination of population health 
# impact to mean the summation of injuries and deaths. 

# Aggregate by total injuries/deaths by category: 
pop_data <- pop.impact.data %>% group_by(EVTYPE) %>%
  summarize(Total = sum(total_population_impact)) %>% arrange(desc(Total))

# Create Figure showing top 20 events: 
pop_plot <- pop_data %>% slice(1:20) # Include top 20 EVTYPES

d <- transform(pop_plot, EVTYPE=reorder(EVTYPE, -Total)) # Reorder data

g1 <- ggplot(data=d, aes(EVTYPE, Total)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  xlab("Event Type") + 
  ylab("Population Impact (Death + Injuries)") +
  geom_text(aes(label=Total), vjust=-0.5, size=3)


##
## 3. Determine events with greatest economic consequences in U.S.
##

# Using the Summary, we see that there are several fields related to damage. 
# 'PROPDMG' has a max of 5000, 'CROPDMG' has a max value of 990, Note that the 
# 'PROPDMGEXP' contains the unique values '{"","M","K","m","B","?","0","k","2"}

damage_data <- econ.impact.data2 %>% group_by(EVTYPE) %>%
  summarize(Total = sum(total_dmg_impact)) %>% arrange(desc(Total))

# Create Figure showing top 20 events: 
dmg_plot <- damage_data %>% slice(1:20) # Include top 20 EVTYPES

d2 <- transform(dmg_plot, EVTYPE=reorder(EVTYPE, -Total)) # Reorder data

g2 <- ggplot(data=d2, aes(EVTYPE, Total)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=90,hjust=1)) + 
  scale_y_continuous(labels=scales::comma) +
  xlab("Event Type") +
  ylab("Total Damage ($)") +
  geom_text(aes(label=paste0(round(Total/1000000000,0),"B")), vjust=-0.5, size=3)
