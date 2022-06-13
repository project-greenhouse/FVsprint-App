library(shiny)
library(shinydashboard)
library(tidyverse)
library(echarts4r)
library(DT)
library(rjson)

# Read Data --------------------
dfRaw <- readRDS("data/sprintData")
sTable <- readRDS("data/SummaryTable")
mTable <- readRDS("data/ModelTable")
oTable <- readRDS("data/OutputsTable")

dfSplits <- dfRaw %>%
  # Add repId
  relocate(repId, .before = Total) %>%
  # Arrange by Name, then Sprint Rep
  arrange(Name, Sprint)

repIds <- as.list(dfSplits$repId)


jfile <- "C:/Users/green/R Projects/FVsprintApp/FVSprintApp/GhouseCustom.json"
themeCustom <- fromJSON(file = jfile) 


