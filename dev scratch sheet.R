### Sum of Squares Differences ###
# Match Model times to est meter mark for model fit
m5 <- x %>% slice(y$index[1])
m10 <- x %>% slice(y$index[2])
m15 <- x %>% slice(y$index[3])
m20 <- x %>% slice(y$index[4])
m25 <- x %>% slice(y$index[5])
m30 <- x %>% slice(y$index[6])

# Create df for calculating SOS
mod = c(m5$mDist[1], m10$mDist[1], m15$mDist[1], m20$mDist[1], m25$mDist[1], m30$mDist[1])
dist = c(5,10,15,20,25,30)
bind <- bind_cols(dist,mod)
dfSOS <- bind %>% 
  mutate("SqDiff" = (mod-dist)^2)

sos <- sum(dfSOS$SqDiff)

symbol <- if (sos < 0.2) {
  "GOOD"
} else {
  "POOR"
}