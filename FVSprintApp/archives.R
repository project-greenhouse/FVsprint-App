### Rep Summary Filtered Function v1
repSummary <- reactive({
  repFilter <- dfSplits %>% 
    filter(repId == input$profFilter) %>%
    mutate("s1" = m5, "s2" = m10-m5,"s3" = m15-m10,"s4" = m20-m15,"s5" = m25-m20, "s6" = m30-m25) %>%
    mutate("v1" = round(5/s1,3),
           "v2" = round(5/s2,3),
           "v3" = round(5/s3,3),
           "v4" = round(5/s4,3),
           "v5" = round(5/s5,3),
           "v6" = round(5/s6,3),
           "vAVG" = round(30/(s1+s2+s3+s4+s5+s6),3))
  
  # Pivot to Absolute time column
  times <- repFilter %>%
    select(m5, m10, m15, m20, m25, m30) %>%
    pivot_longer(cols = c("m5", "m10", "m15", "m20", "m25", "m30"), names_to = "Dist", values_to = "Time")
  
  # Pivot to Splits time column
  splits <- repFilter %>%
    select(s1, s2, s3, s4, s5, s6) %>%
    rename("m5" = s1, "m10" = s2, "m15" = s3, "m20" = s4, "m25" = s5, "m30" = s6) %>%
    pivot_longer(cols = c("m5", "m10", "m15", "m20", "m25", "m30"), names_to = "Dist", values_to = "Splits")
  
  # Pivot to Velocity column
  velo <- repFilter %>%
    select(v1, v2, v3, v4, v5, v6) %>%
    rename("m5" = v1, "m10" = v2, "m15" = v3, "m20" = v4, "m25" = v5, "m30" = v6) %>%
    pivot_longer(cols = c("m5", "m10", "m15", "m20", "m25", "m30"), names_to = "Dist", values_to = "Velocity")
  
  # Create Distance column
  Dist <- c(5,10,15,20,25,30)
  
  # Bind for Rep Summary Table
  repSummary <- as.data.frame(cbind(Dist, times$Time, splits$Splits, velo$Velocity))
  repSummary <- repSummary %>% transmute("Marker" = Dist, "Times" = V2, "Splits" = V3, "Velocity" = V4)
  
  repSummary
})

### Rep Model Filtered Function v1
repModel <- reactive({
  
  repFilter <- dfSplits %>% 
    filter(repId == input$profFilter) %>%
    mutate("s1" = m5, "s2" = m10-m5,"s3" = m15-m10,"s4" = m20-m15,"s5" = m25-m20, "s6" = m30-m25) %>%
    mutate("v1" = round(5/s1,3),
           "v2" = round(5/s2,3),
           "v3" = round(5/s3,3),
           "v4" = round(5/s4,3),
           "v5" = round(5/s5,3),
           "v6" = round(5/s6,3),
           "vAVG" = round(30/(s1+s2+s3+s4+s5+s6),3))
  
  # Pivot to Absolute time column
  times <- repFilter %>%
    select(m5, m10, m15, m20, m25, m30) %>%
    pivot_longer(cols = c("m5", "m10", "m15", "m20", "m25", "m30"), names_to = "Dist", values_to = "Time")
  
  # Pivot to Splits time column
  splits <- repFilter %>%
    select(s1, s2, s3, s4, s5, s6) %>%
    rename("m5" = s1, "m10" = s2, "m15" = s3, "m20" = s4, "m25" = s5, "m30" = s6) %>%
    pivot_longer(cols = c("m5", "m10", "m15", "m20", "m25", "m30"), names_to = "Dist", values_to = "Splits")
  
  # Pivot to Velocity column
  velo <- repFilter %>%
    select(v1, v2, v3, v4, v5, v6) %>%
    rename("m5" = v1, "m10" = v2, "m15" = v3, "m20" = v4, "m25" = v5, "m30" = v6) %>%
    pivot_longer(cols = c("m5", "m10", "m15", "m20", "m25", "m30"), names_to = "Dist", values_to = "Velocity")
  
  # Create Distance column
  Dist <- c(5,10,15,20,25,30)
  
  # Bind for Rep Summary Table
  repSummary <- as.data.frame(cbind(Dist, times$Time, splits$Splits, velo$Velocity))
  repSummary <- repSummary %>% transmute("Marker" = Dist, "Times" = V2, "Splits" = V3, "Velocity" = V4)
  
  # |----| Variables for model calcs -------------------- 
  # (t) Time variable sequence as length of 30m time x 0.01
  t <- as.vector(seq(0.01, max(repSummary$Times), by = 0.01))
  # MAX VELOCITY
  vHmax <- max(repSummary$Velocity)
  # Tau
  tau <- 0.729052182326299
  # (e) Euler's number 
  e <- 2.7182818284590452353602874713527
  # body mass
  mass <- repFilter$Weight_kg[1]
  # stature
  height <- repFilter$Height_m[1]
  # air pressure
  press <- 2978
  # air temp
  temp <- 22.78
  # solve for pressure
  p <- 1.293 * ( press / 760)  * (273 / ( 273 + temp ))
  # solve for air friction
  Af <- ( 0.2025 * height^0.725 * mass^0.425 ) * 0.266 
  # Drage Coefficient "Cd" = 0.9
  k <- 0.5 * p * Af * 0.9
  
  #|----| Solve for xPos, Velo, aHRZ at (t) --------------------
  
  # Create template table
  time = c(0)
  xPos = c(0)
  velo = c(0)
  aHRZ = c(0)
  Model <- data.frame(time, xPos, velo, aHRZ)  
  
  # Horizontal Velocity at given time (i) #
  for(i in t) {
    # time interval
    time <- i
    
    # Solve for Velocity
    velo <- vHmax*(1-e^(-1*(i/tau)))
    
    # Horizontal Displacement
    xPos <- vHmax *(i + (tau * e^(-1*(i/tau)))) - (vHmax*tau)
    
    # Solve for Acceleration 
    aHRZ <- (vHmax/tau)*e^(-i/tau)
    
    # Create temp df for current values at i
    df <- data.frame(time,xPos,velo,aHRZ)
    
    # Bind temp df to dfMod
    Model <- rbind(Model, df)
  }   
  
  # Trim index row from top
  Model <- slice_tail(Model,n=-1)
  
  #|----| Solve for fAERO, fHRZ, pHRZ, fHRZ_rel, pHRZ_rel --------------------
  
  # Trim index row from top
  dfModel <- Model %>% 
    # Add Calculated column with fAERO data
    mutate(
      # Air Friction
      "fAERO" = k * velo^2,
      # Horizontal Force
      "fHRZ" = fAERO + (mass * aHRZ),
      # Horizontal Power
      "pHRZ" = fHRZ * velo,
      # Relative Horizontal Force
      "fHRZ_rel" = fHRZ/mass, 
      # Relative Horizontal Power
      "pHRZ_rel" = pHRZ/mass
    )
  
  #|----| Solve for RF% --------------------
  sprintRF <- dfModel %>% 
    # calculate for vertical force mass*gravity
    mutate("fVERT" = 9.81*mass) %>%
    # limit time to > 0.5
    filter(time >= 0.3) %>%
    # Calculate for ratio of force percentage
    mutate("RF" = ( fHRZ / sqrt( fHRZ^2 + fVERT^2 )) *100) %>%
    # select just time and RF
    select(time, RF)
  
  #|----| Complete Model Data Frame --------------------
  dfModel <- left_join(x = dfModel, y = sprintRF, by = "time")
  
  return(as.data.frame(dfModel))
  
})