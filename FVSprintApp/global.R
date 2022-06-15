library(shiny)
library(shinydashboard)
library(tidyverse)
library(echarts4r)
library(DT)
library(rjson)
library(lubridate)

# Read Data --------------------
dfRaw <- readRDS("data/sprintData")
sTable <- readRDS("data/SummaryTable")
mTable <- readRDS("data/ModelTable")
oTable <- readRDS("data/OutputsTable")
cModel <- readRDS("data/chronModel")
cSummary <- readRDS("data/chronSummary")
cOutputs <- readRDS("data/chronOutputs")

dfSplits <- dfRaw %>%
  # Add repId
  relocate(repId, .before = Total) %>%
  # Arrange by Name, then Sprint Rep
  arrange(Name, Sprint)

repIds <- as.list(dfSplits$repId)


jfile <- "C:/Users/green/R Projects/FVsprintApp/FVSprintApp/GhouseCustom.json"
themeCustom <- fromJSON(file = jfile) 

chronList <- c("Tupac Shakur", "Biggie Smalls", "Big Boi")

scale_this <- function(x){
  n<- (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
  n <- round(n, 2)
  return(n)
}


#v <- as.character(paste0("w",3))

#test <- cModel %>% 
#  filter(Name == "Tupac Shakur") %>%
#  mutate("WeekNo" = paste0("w",as.character(week(Date)))) %>%
#  select(WeekNo, time, pHRZ_rel) %>% 
#  pivot_wider(names_from = WeekNo, values_from = pHRZ_rel) %>%
#  mutate("Hist" = (w2+w3+w4+w5+w6+w7+w8+w9+w10)/9) %>%
#  select(time, Hist, v)


