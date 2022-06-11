library(shiny)
library(shinydashboard)
library(tidyverse)
library(echarts4r)
library(DT)
library(rjson)
# Read Data --------------------
dfRaw <- readRDS("data/sprintData.rds")

dfSplits <- dfRaw %>%
  # Add repId
  mutate("repId" = paste0(Name,Sprint)) %>%
  # Rename Columns
  rename("m5"=`5m`, "m10"=`10m`, "m15"=`15m`, "m20"=`20m`, "m25"=`25m`,"m30"=Total)%>%
  # Arrange by Name, then Sprint Rep
  arrange(Name, Sprint)

repIds <- as.list(dfSplits$repId)


jfile <- "C:/Users/green/R Projects/FVsprintApp/FVSprintApp/GhouseCustom.json"
themeCustom <- fromJSON(file = jfile) 


