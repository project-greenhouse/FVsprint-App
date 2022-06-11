Server <- function(input, output) {
  
  ####--------------------| Profile  --------------------####
  
  #---------- |-| Reactive Functions --------------------
  # |--| Rep Recorded Summary Data frame --------------------
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
  
  # |--| Kinetics Model Data frame --------------------
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
  
  #---------- |-| Outputs --------------------
  
  # |--| Rep Summary Table  --------------------
  output$tableRepSummary <- renderTable({ repSummary() })
  
  # |--| Info Boxes --------------------
  
  # ~ Peak Force 
  output$infoboxPeakF <- renderInfoBox({
    m <- repModel()
    #linear model of force~velocity
    lmF <- lm( m$fHRZ_rel ~ m$velo, data = m )
    # coefficients
    coef<- coef(lmF)
    # y-intercept of lmF
    peakF <- round(coef[1],2)
    
    # 3 4 red | 5 6  yellow | 7 8 aqua | 9 10 teal | 11 12 green
    color <- if(peakF > 10.99) {
      "green"
    } else if(peakF < 11.00 & peakF > 8.99) {
      "teal" 
    } else if(peakF < 9.00 & peakF > 6.99) {
      "aqua"
    } else if(peakF < 7.00 & peakF > 4.99) {
      "yellow"
    } else {"red"}
    
    
    infoBox(
      title = p(strong(h6("Peak Force"))),
      subtitle = "F0 (N/kg)",
      value = peakF,
      color = color,
      fill = TRUE,
      icon = icon("dumbbell")
    )
  })
  
  # ~ Peak Velocity
  output$infoboxPeakV <- renderInfoBox({
    m <- repModel()
    m <- m %>% filter(xPos <= 30.09)
    #linear model of force~velocity
    lmF <- lm(m$fHRZ_rel ~ m$velo, data = m)
    # coefficients
    coef<- coef(lmF)
    # slope and y-intercept of lmF
    slope <- coef[2]
    yint <- coef[1]
    
    peakV <- round(-yint/slope, 2)
    
    # 3 4 red | 5 6  yellow | 7 8 aqua | 9 10 teal | 11 12 green
    color <- if(peakV > 10.999) {
      "green"
    } else if(peakV < 11.000 & peakV > 8.999) {
      "teal" 
    } else if(peakV < 9.000 & peakV > 6.999) {
      "aqua"
    } else if(peakV < 7.000 & peakV > 4.999) {
      "yellow"
    } else {"red"}
    
    
    infoBox(
      title = p(strong(h6("Peak Velo"))),
      subtitle = "V0 (m/s)",
      value = peakV,
      color = color,
      fill = TRUE,
      icon = icon("tachometer-alt")
    )
  })
  
  # ~ Max Power 
  output$infoboxPmax <- renderInfoBox({
    v <- repModel()
    #linear model of force~velocity
    lmF <- lm(v$fHRZ_rel ~ v$velo, data = v)
    # coefficients
    coef<- coef(lmF)
    # slope and y-intercept of lmF
    slope <- coef[2]
    yint <- coef[1]
    
    f <- yint
    v <- -yint/slope
    
    peakP <- round((f*v)/4, 2)
    
    # 6-10 red | 11-15  yellow | 16-20 aqua | 21-25 teal | 25+green
    color <- if(peakP > 24.999) {
      "green"
    } else if(peakP < 25.000 & peakP > 20.999) {
      "teal" 
    } else if(peakP < 20.000 & peakP > 14.999) {
      "aqua"
    } else if(peakP < 14.000 & peakP > 10.999) {
      "yellow"
    } else {"red"}
    
    infoBox(
      title = p(strong(h6("Peak Power"))),
      subtitle = "Pmax(W/kg)",
      value = peakP,
      color = color,
      fill = TRUE,
      icon = icon("bolt")
    )
  })
  
  # ~ Max Force Velocity Slope 
  output$infoboxFVslope <- renderInfoBox({
    m <- repModel()
    #linear model of force~velocity
    lmF <- lm(m$fHRZ_rel ~ m$velo, data = m)
    # coefficients
    coef<- coef(lmF)
    # slope and y-intercept of lmF
    slope <- round(coef[2], 2)
    
    infoBox(
      title = p(strong(h6("F-V"))),
      subtitle = "Slope",
      value = slope,
      color = "green",
      fill = TRUE,
      icon = icon("expand-alt")
    )
  })
  
  # ~ Max Ratio of Force 
  output$infoboxRFMax <- renderInfoBox({
    df <- repModel()
    n <- round(max(df$RF, na.rm = TRUE), 0)
    
    # 3 4 red | 5 6  yellow | 7 8 aqua | 9 10 teal | 11 12 green
    color <- if(n > 49.99) {
      "green"
    } else if(n < 50.00 & n > 39.99) {
      "teal" 
    } else if(n < 40.00 & n > 29.99) {
      "aqua"
    } else if(n < 30.00 & n > 19.99) {
      "yellow"
    } else {"red"}
    
    infoBox(
      title = p(strong(h6("Force Ratio"))),
      subtitle = "RFmax",
      value = n,
      color = "green",
      fill = TRUE,
      icon = icon("percentage")
    )
  })
  
  # ~ Decrease in Ratio of Force
  output$infoboxDRF <- renderInfoBox({
    df <- repModel()
    
    mod <- lm(RF ~ velo, data = df)
    coefs <- coef(mod)
    drf <- round(coefs[2],2)
    
    # Conditionally format color
    color <- if(drf > -5) {
      "green"
    } else if(drf < -6 & drf > -7) {
      "teal" 
    } else if(drf < -7 & drf > -8) {
      "aqua"
    } else if(drf < -8 & drf > -9) {
      "yellow"
    } else {"red"}
    
    infoBox(
      title = p(strong(h6("RF Decr"))),
      subtitle = "DRF%",
      value = drf,
      color = color,
      fill = TRUE,
      icon = icon("level-down-alt")
    )
  })
  
  # ~ Optimal Velocity
  output$infoboxOptV <- renderInfoBox({
    df <- repModel()
    df <- df %>% filter(pHRZ == max(pHRZ))
    
    infoBox(
      title = p(strong(h6("Optml Velo"))),
      subtitle = "Vopt (m/s)",
      value = round(df$velo, 2),
      color = "green",
      fill = TRUE,
      icon = icon("balance-scale")
    )
  })
  
  # ~ Max Speed 
  output$infoboxMaxSpeed <- renderInfoBox({
    df <- repModel()
    
    vMax <- max(df$velo)
    
    infoBox(
      title = p(strong(h6("Max Velo"))),
      subtitle = "Vmax (m/s)",
      value = round(vMax, 2),
      color = "green",
      fill = TRUE,
      icon = icon("fighter-jet")
    )
  })
  
  
  #---------- |-| Plots --------------------
  
  # |---| Model Fit --------------------
  output$plotRecorded <- renderEcharts4r({
    
    x <- repModel() %>% transmute("mDist" = xPos, "mTime" = time, "index" = time*100)
    y <- repSummary() %>% transmute("rDist" = Marker, "rTime" = Times, "index" = Times*100)
    
    ### Create df for plot by joining Model and Summary ###
    dfPlot <- left_join(x,y, by = "index")
    
    dfsos <- dfPlot %>% 
      filter(!is.na(rTime)) %>% 
      mutate("SqDiff" = (mDist-rDist)^2) %>% 
      summarise("SoS" = cumsum(SqDiff) )
    
    symbol <- if (dfsos$SoS[1] < 0.2) {
      "GOOD"
    } else {
      "POOR"
    }
    
    plot_ModelFit <- dfPlot |>
      e_charts(mTime, name = "Time(s)") |>
      e_scatter(mDist, name = "Modeled") |>
      e_scatter(rDist, name = "Recorded", symbol_size = 8) |>
      e_title(text = "Model Fit", 
              subtext = paste0("                                   SoS Differences: ", round(dfsos$SoS[1],1), " - ", symbol)) |>
      e_color(background = "111111") |>
      e_format_y_axis(suffix = "m") |>
      e_format_x_axis(suffix = "s") |>
      e_grid(bottom = "10%") |>
      e_legend(show = TRUE, left = "right", orient = "horizontal",align = "right")
  
      plot_ModelFit
    
  })
  
  #|---| Force-Velocity-Time --------------------
  output$plotFVT <- renderEcharts4r({
    
    df <- repModel()
    
    # Find FV Slope
    lm <- lm(fHRZ_rel~velo, data = df)
    coef <- coef(lm)
    slope<- round(coef[2],2)
    
    plot_FVT <- df |>
      e_charts(time) |>
      e_scatter(fHRZ_rel, name = "Force", y_index = 0) |>
      e_scatter(velo, name = "Velocity", y_index = 1) |>
      e_lm(fHRZ_rel~xPos, legend = FALSE, name = "FV") |>
      e_legend(show = TRUE, left = "right", orient = "horizontal", align = "right", type = "scroll") |>
      e_color(background = "111111") |>
      e_grid(bottom = "10%") |>
      e_title(text = "Force - Velocity by Time",
              subtext = paste0("                                        FV Slope: ", slope)) |>
      e_axis(serie = time, axis = "x",
             axisLabel = list(margin = 3, fontSize = 12, 
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value} s')
      ) |>
      e_axis(serie = fHRZ_rel, axis = "y", index = 0, name = "Force", 
             nameLocation = "end",
             axisLabel = list(margin = 3, fontSize = 8, 
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value}N/kg')
      ) |>
      e_axis(serie = velo, axis = "y", index = 1, name = "Velocity", 
             nameLocation = "end",
             axisLabel = list(margin = 5, fontSize = 8,
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value}m/s')
      ) |>
      e_tooltip(trigger = "item",
                axisPointer = list(
                  type = "cross"
                ))
    
    plot_FVT
    
  })
  
  #|---| Force-Power-Velocity--------------------
  output$plotFPV <- renderEcharts4r({
    df <- repModel()
    
    peakP <- round(max(df$pHRZ_rel, na.rm = TRUE),2)
    
    plotFPV <- df |>
      e_charts(velo) |>
      e_scatter(fHRZ_rel, name = "Force", y_index = 0) |>
      e_scatter(pHRZ_rel, name = "Power", y_index = 1) |>
      e_legend(show = TRUE, left = "right", orient = "horizontal", align = "right") |>
      e_color(background = "111111") |>
      e_grid(bottom = "10%") |>
      e_title(text = "Force-Power by Velocity",
              subtext = paste0("                              Peak Relative Power: ", peakP," W/kg")) |> 
      e_axis(serie = velo, axis = "x",
             axisLabel = list(margin = 5, fontSize = 12, 
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value} m/s')
      ) |>
      e_axis(serie = fHRZ_rel, axis = "y", index = 0, name = "Force", 
             nameLocation = "end",
             axisLabel = list(margin = 5, fontSize = 8, 
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value}N/kg')
      ) |>
      e_axis(serie = pHRZ_rel, axis = "y", index = 1, name = "Power", 
             nameLocation = "end",
             axisLabel = list(margin = 5, fontSize = 8,
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value}W/kg')
      ) |>
      e_tooltip(trigger = "item",
                axisPointer = list(
                  type = "cross"
                ))
    
    plotFPV
    
  })
  
  #|---| Power-DRF-Position --------------------
  output$plotPDP <- renderEcharts4r({
    df <- repModel()
    
    peakRF <- round(max(df$RF, na.rm = TRUE),0)
    
    m5 <- df %>% filter(xPos > 4.98 & xPos < 5.98 )
    m10 <- df %>% filter(xPos > 9.98 & xPos < 10.98 )
    m15 <- df %>% filter(xPos > 14.98 & xPos < 15.98 )
    
    plotPDP <- df |>
      filter(xPos <= 15.00) |>
      e_charts(xPos) |>
      e_scatter(pHRZ_rel, name = "Power", y_index = 0) |>
      e_scatter(RF, name = "RF%", y_index = 1) |>
      e_mark_line(data = list(xAxis = m5$xPos[1]), title = "5m", title_position = "start") |> 
      e_mark_line(data = list(xAxis = m10$xPos[1]), title = "10m", title_position = "start") |> 
      e_mark_line(data = list(xAxis = m15$xPos[1]), title = "15m", title_position = "start") |> 
      e_color(background = "111111") |>
      e_grid(bottom = "10%") |>
      e_legend(show = TRUE, left = "right", orient = "horizontal", align = "right") |>
      e_title(text = "Power-RF% by Distance", subtext = paste0("                                        Peak RF: ", peakRF, "%")) |> 
      e_axis(serie = xPos, axis = "x", show = FALSE ) |>
      e_axis(serie = pHRZ_rel, axis = "y", index = 0, name = "Power", 
             nameLocation = "end",
             axisLabel = list(margin = 5, fontSize = 8,
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value}W/kg')
      ) |>
      e_axis(serie = RF, axis = "y", index = 1, name = "RF", 
             nameLocation = "end",
             axisLabel = list(margin = 5, fontSize = 8,
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value}%')
      ) |>
      e_tooltip(trigger = "item",
                axisPointer = list(
                  type = "cross"
                ))
    
    plotPDP
    
  })
  
  
  ##########--------------------| Tracking  --------------------##########
  
  
  
  ##########--------------------| Comparisons  --------------------##########
  
  #---------- |-| Reactive Functions --------------------
  # |--| Rep Recorded Summary Data frame --------------------
  # Athlete 1 Aggregate data
  compSummary1 <- reactive({
    repFilter <- dfSplits %>% 
      filter(repId == input$compFilter1) %>%
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
    
    # Pivot to Velocity column
    velo <- repFilter %>%
      select(v1, v2, v3, v4, v5, v6) %>%
      rename("m5" = v1, "m10" = v2, "m15" = v3, "m20" = v4, "m25" = v5, "m30" = v6) %>%
      pivot_longer(cols = c("m5", "m10", "m15", "m20", "m25", "m30"), names_to = "Dist", values_to = "Velocity")
    
    # Create Distance column
    Dist <- c(5,10,15,20,25,30)
    
    # Bind for Rep Summary Table
    repSummary <- as.data.frame(cbind(Dist, times$Time, velo$Velocity))
    repSummary <- repSummary %>% transmute("Split" = Dist, "Times" = V2, "Velocity" = V3)
    
    repSummary
  })
  # Athlete 2 Aggregate data
  compSummary2 <- reactive({
    repFilter <- dfSplits %>% 
      filter(repId == input$compFilter2) %>%
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
    
    # Pivot to Velocity column
    velo <- repFilter %>%
      select(v1, v2, v3, v4, v5, v6) %>%
      rename("m5" = v1, "m10" = v2, "m15" = v3, "m20" = v4, "m25" = v5, "m30" = v6) %>%
      pivot_longer(cols = c("m5", "m10", "m15", "m20", "m25", "m30"), names_to = "Dist", values_to = "Velocity")
    
    # Create Distance column
    Dist <- c(5,10,15,20,25,30)
    
    # Bind for Rep Summary Table
    repSummary <- as.data.frame(cbind(Dist, times$Time, velo$Velocity))
    repSummary <- repSummary %>% transmute("Split" = Dist, "Times" = V2, "Velocity" = V3)
    
    repSummary
  })
  
  # |--| Kinetics Model Data frame --------------------
  # Athlete 1 Model
  compModel1 <- reactive({
    
    # Filter rep data from 'dfSplits' from 'compFilter1'
    repFilter <- dfSplits %>% 
      filter(repId == input$compFilter1) %>%
      mutate("s1" = m5, "s2" = m10-m5,"s3" = m15-m10,"s4" = m20-m15,"s5" = m25-m20, "s6" = m30-m25) %>%
      mutate("v1" = round(5/s1,3),
             "v2" = round(5/s2,3),
             "v3" = round(5/s3,3),
             "v4" = round(5/s4,3),
             "v5" = round(5/s5,3),
             "v6" = round(5/s6,3),
             "vAVG" = round(30/(s1+s2+s3+s4+s5+s6),3))
    
    # Call filtered aggregrate table from 'compSummary1'
    repSummary <- compSummary1()
    
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
  # Athlete 2 Model
  compModel2 <- reactive({
    
    # Filter rep data from 'dfSplits' from 'compFilter1'
    repFilter <- dfSplits %>% 
      filter(repId == input$compFilter2) %>%
      mutate("s1" = m5, "s2" = m10-m5,"s3" = m15-m10,"s4" = m20-m15,"s5" = m25-m20, "s6" = m30-m25) %>%
      mutate("v1" = round(5/s1,3),
             "v2" = round(5/s2,3),
             "v3" = round(5/s3,3),
             "v4" = round(5/s4,3),
             "v5" = round(5/s5,3),
             "v6" = round(5/s6,3),
             "vAVG" = round(30/(s1+s2+s3+s4+s5+s6),3))
    
    # Call filtered aggregrate table from 'compSummary1'
    repSummary <- compSummary2()
    
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
  
  #---------- |-| Outputs --------------------
  
  # |--| Rep Summary Table  --------------------
  # Summary Table 1
  output$tableCompSummary1 <- renderTable({ compSummary1() })
  
  # Summary Table 2
  output$tableCompSummary2 <- renderTable({ compSummary2() })
  
  # |--| Info Boxes --------------------
  
  # ~ Peak Force 
  output$compPeakF1 <- renderInfoBox({
    # Ath 1 model
    m <- compModel1()
    #linear model of force~velocity
    lmF <- lm( m$fHRZ_rel ~ m$velo, data = m )
    # coefficients
    coef<- coef(lmF)
    # y-intercept of lmF
    peakF <- round(coef[1],2)
    
    infoBox(
      title = p(strong(h6("Peak Force"))),
      subtitle = "F0 (N/kg)",
      value = peakF,
      color = "maroon",
      fill = TRUE,
      icon = icon("dumbbell")
    )
  })
  
  # ~ Peak Force 
  output$compPeakF2 <- renderInfoBox({
    # Ath 2 model
    m <- compModel2()
    #linear model of force~velocity
    lmF <- lm( m$fHRZ_rel ~ m$velo, data = m )
    # coefficients
    coef<- coef(lmF)
    # y-intercept of lmF
    peakF <- round(coef[1],2)
    
    infoBox(
      title = p(strong(h6("Peak Force"))),
      subtitle = "F0 (N/kg)",
      value = peakF,
      color = "aqua",
      fill = TRUE,
      icon = icon("dumbbell")
    )
  })
  
  # ~ Peak Velocity
  output$compPeakV1 <- renderInfoBox({
    m <- compModel1()
    m <- m %>% filter(xPos <= 30.09)
    #linear model of force~velocity
    lmF <- lm(m$fHRZ_rel ~ m$velo, data = m)
    # coefficients
    coef<- coef(lmF)
    # slope and y-intercept of lmF
    slope <- coef[2]
    yint <- coef[1]
    
    peakV <- round(-yint/slope, 2)

    infoBox(
      title = p(strong(h6("Peak Velo"))),
      subtitle = "V0 (m/s)",
      value = peakV,
      color = "maroon",
      fill = TRUE,
      icon = icon("tachometer-alt")
    )
  })
  
  # ~ Peak Velocity
  output$compPeakV2 <- renderInfoBox({
    m <- compModel2()
    m <- m %>% filter(xPos <= 30.09)
    #linear model of force~velocity
    lmF <- lm(m$fHRZ_rel ~ m$velo, data = m)
    # coefficients
    coef<- coef(lmF)
    # slope and y-intercept of lmF
    slope <- coef[2]
    yint <- coef[1]
    
    peakV <- round(-yint/slope, 2)
    
    infoBox(
      title = p(strong(h6("Peak Velo"))),
      subtitle = "V0 (m/s)",
      value = peakV,
      color = "aqua",
      fill = TRUE,
      icon = icon("tachometer-alt")
    )
  })
  
  # ~ Max Power 
  output$compPmax1 <- renderInfoBox({
    m <- compModel1()
    #linear model of force~velocity
    lmF <- lm(m$fHRZ_rel ~ m$velo, data = m)
    # coefficients
    coef<- coef(lmF)
    # slope and y-intercept of lmF
    slope <- coef[2]
    yint <- coef[1]
    
    f <- yint
    v <- -yint/slope
    
    peakP <- round((f*v)/4, 2)
    
    infoBox(
      title = p(strong(h6("Peak Power"))),
      subtitle = "Pmax(W/kg)",
      value = peakP,
      color = "maroon",
      fill = TRUE,
      icon = icon("bolt")
    )
  })
  
  # ~ Max Power 
  output$compPmax2 <- renderInfoBox({
    m <- compModel2()
    #linear model of force~velocity
    lmF <- lm(m$fHRZ_rel ~ m$velo, data = m)
    # coefficients
    coef<- coef(lmF)
    # slope and y-intercept of lmF
    slope <- coef[2]
    yint <- coef[1]
    
    f <- yint
    v <- -yint/slope
    
    peakP <- round((f*v)/4, 2)

    infoBox(
      title = p(strong(h6("Peak Power"))),
      subtitle = "Pmax(W/kg)",
      value = peakP,
      color = "aqua",
      fill = TRUE,
      icon = icon("bolt")
    )
  })
  
  #---------- |-| Plots --------------------
  
  #|---| Force-Velocity-Time --------------------
  # Athlete 1 FVT Plot
  output$CompPlotFVT1 <- renderEcharts4r({
    
    # Athlete 1 Data
    df <- compModel1()
    
    # Find FV Slope
    lm <- lm(fHRZ_rel~velo, data = df)
    coef <- coef(lm)
    slope<- round(coef[2],2)
    
    # Plot
    plot_FVT <- df |>
      e_charts(time) |>
      e_scatter(fHRZ_rel, name = "Force", y_index = 0) |>
      e_scatter(velo, name = "Velocity", y_index = 1) |>
      e_lm(fHRZ_rel~xPos, legend = FALSE, name = "FV") |>
      e_legend(show = TRUE, left = "right", orient = "horizontal", align = "right", type = "scroll") |>
      e_color(background = "111111") |>
      e_grid(bottom = "10%") |>
      e_title(text = "Force - Velocity by Time",
              subtext = paste0("                                                  FV Slope: ", slope)) |>
      e_axis(serie = time, axis = "x",
             axisLabel = list(margin = 3, fontSize = 12, 
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value} s')
      ) |>
      e_axis(serie = fHRZ_rel, axis = "y", index = 0, name = "Force", 
             nameLocation = "end", min = 3, max = 13,
             axisLabel = list(margin = 3, fontSize = 8, 
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value}N/kg')
      ) |>
      e_axis(serie = velo, axis = "y", index = 1, name = "Velocity", 
             nameLocation = "end", min = 2, max = 12,
             axisLabel = list(margin = 5, fontSize = 8,
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value}m/s')
      ) |>
      e_tooltip(trigger = "item",
                axisPointer = list(
                  type = "cross"
                ))
    
    plot_FVT
    
  })
  # Athlete 2 FVT Plot
  output$CompPlotFVT2 <- renderEcharts4r({
    
    # Athlete 1 Data
    df <- compModel2()
    
    # Find FV Slope
    lm <- lm(fHRZ_rel~velo, data = df)
    coef <- coef(lm)
    slope<- round(coef[2],2)
    
    # Plot
    plot_FVT <- df |>
      e_charts(time) |>
      e_scatter(fHRZ_rel, name = "Force", y_index = 0) |>
      e_scatter(velo, name = "Velocity", y_index = 1) |>
      e_lm(fHRZ_rel~xPos, legend = FALSE, name = "FV") |>
      e_legend(show = TRUE, left = "right", orient = "horizontal", align = "right", type = "scroll") |>
      e_color(background = "111111") |>
      e_grid(bottom = "10%") |>
      e_title(text = "Force - Velocity by Time",
              subtext = paste0("                                                  FV Slope: ", slope)) |>
      e_axis(serie = time, axis = "x",
             axisLabel = list(margin = 3, fontSize = 12, 
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value} s')
      ) |>
      e_axis(serie = fHRZ_rel, axis = "y", index = 0, name = "Force", 
             nameLocation = "end", min = 3, max = 13,
             axisLabel = list(margin = 3, fontSize = 8, 
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value}N/kg')
      ) |>
      e_axis(serie = velo, axis = "y", index = 1, name = "Velocity", 
             nameLocation = "end", min = 2, max = 12,
             axisLabel = list(margin = 5, fontSize = 8,
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value}m/s')
      ) |>
      e_tooltip(trigger = "item",
                axisPointer = list(
                  type = "cross"
                ))
    
    plot_FVT
    
  })
  
  #|---| Force-Power-Velocity--------------------
  # Athlete 1 FPV Plot
  output$CompPlotFPV1 <- renderEcharts4r({
    
    # Athlete 1 Data
    df <- compModel1()
    
    peakP <- round(max(df$pHRZ_rel, na.rm = TRUE),2)
    
    plotFPV <- df |>
      e_charts(velo) |>
      e_scatter(fHRZ_rel, name = "Force", y_index = 0) |>
      e_scatter(pHRZ_rel, name = "Power", y_index = 1) |>
      e_legend(show = TRUE, left = "right", orient = "horizontal", align = "right") |>
      e_color(background = "111111") |>
      e_grid(bottom = "10%") |>
      e_title(text = "Force-Power by Velocity",
              subtext = paste0("                                        Peak Relative Power: ", peakP," W/kg")) |> 
      e_axis(serie = velo, axis = "x",
             axisLabel = list(margin = 5, fontSize = 12, 
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value} m/s')
      ) |>
      e_axis(serie = fHRZ_rel, axis = "y", index = 0, name = "Force", 
             nameLocation = "end", min = 3, max = 13,
             axisLabel = list(margin = 5, fontSize = 8, 
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value}N/kg')
      ) |>
      e_axis(serie = pHRZ_rel, axis = "y", index = 1, name = "Power", 
             nameLocation = "end", min = 10, max = 40,
             axisLabel = list(margin = 5, fontSize = 8,
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value}W/kg')
      ) |>
      e_tooltip(trigger = "item",
                axisPointer = list(
                  type = "cross"
                ))
    
    plotFPV
    
  })
  # Athlete 2 FPV Plot
  output$CompPlotFPV2 <- renderEcharts4r({
    
    # Athlete 2 Data
    df <- compModel2()
    
    peakP <- round(max(df$pHRZ_rel, na.rm = TRUE),2)
    
    plotFPV <- df |>
      e_charts(velo) |>
      e_scatter(fHRZ_rel, name = "Force", y_index = 0) |>
      e_scatter(pHRZ_rel, name = "Power", y_index = 1) |>
      e_legend(show = TRUE, left = "right", orient = "horizontal", align = "right") |>
      e_color(background = "111111") |>
      e_grid(bottom = "10%") |>
      e_title(text = "Force-Power by Velocity",
              subtext = paste0("                                      Peak Relative Power: ", peakP," W/kg")) |> 
      e_axis(serie = velo, axis = "x",
             axisLabel = list(margin = 5, fontSize = 12, 
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value} m/s')
      ) |>
      e_axis(serie = fHRZ_rel, axis = "y", index = 0, name = "Force", 
             nameLocation = "end", min = 3, max = 13,
             axisLabel = list(margin = 5, fontSize = 8, 
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value}N/kg')
      ) |>
      e_axis(serie = pHRZ_rel, axis = "y", index = 1, name = "Power", 
             nameLocation = "end", min = 10, max = 40,
             axisLabel = list(margin = 5, fontSize = 8,
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value}W/kg')
      ) |>
      e_tooltip(trigger = "item",
                axisPointer = list(
                  type = "cross"
                ))
    
    plotFPV
    
  })
  
  #|---| Power-RF-Distance--------------------
  # Athlete 1 FPV Plot
  output$CompPlotPFD1 <- renderEcharts4r({
    
    # Athlete 1 Data
    df <- compModel1()
    
    peakRF <- round(max(df$RF, na.rm = TRUE),0)
    
    m5 <- df %>% filter(xPos > 4.98 & xPos < 5.98 )
    m10 <- df %>% filter(xPos > 9.98 & xPos < 10.98 )
    m15 <- df %>% filter(xPos > 14.98 & xPos < 15.98 )
    
    
    plotPDP <- df |>
      filter(xPos <= 15) |>
      e_charts(xPos) |>
      e_scatter(pHRZ_rel, name = "Power", y_index = 0) |>
      e_scatter(RF, name = "RF%", y_index = 1) |>
      e_mark_line(data = list(xAxis = m5$xPos[1]), title = "5m", title_position = "start") |> 
      e_mark_line(data = list(xAxis = m10$xPos[1]), title = "10m", title_position = "start") |> 
      e_mark_line(data = list(xAxis = m15$xPos[1]), title = "15m", title_position = "start") |>
      e_color(background = "111111") |>
      e_grid(bottom = "10%") |>
      e_legend(show = TRUE, left = "right", orient = "horizontal", align = "right") |>
      e_title(text = "Power-RF% by Distance", 
              subtext = paste0("                                                  Peak RF: ", peakRF, "%")) |> 
      e_axis(serie = xPos, axis = "x", show = FALSE ) |>
      e_axis(serie = pHRZ_rel, axis = "y", index = 0, name = "Power", 
             nameLocation = "end", min = 10, max = 40,
             axisLabel = list(margin = 5, fontSize = 8,
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value}W/kg')
      ) |>
      e_axis(serie = RF, axis = "y", index = 1, name = "RF", 
             nameLocation = "end", min = 10, max = 70,
             axisLabel = list(margin = 5, fontSize = 8,
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value}%')
      ) |>
      e_tooltip(trigger = "item",
                axisPointer = list(
                  type = "cross"
                ))
    
    plotPDP
    
  })
  # Athlete 1 FPV Plot
  output$CompPlotPFD2 <- renderEcharts4r({
    
    # Athlete 1 Data
    df <- compModel2()
    
    peakRF <- round(max(df$RF, na.rm = TRUE),0)
    
    m5 <- df %>% filter(xPos > 4.98 & xPos < 5.98 )
    m10 <- df %>% filter(xPos > 9.98 & xPos < 10.98 )
    m15 <- df %>% filter(xPos > 14.98 & xPos < 15.98 )
    
    plotPDP <- df |>
      filter(xPos <= 15) |>
      e_charts(xPos) |>
      e_scatter(pHRZ_rel, name = "Power", y_index = 0) |>
      e_scatter(RF, name = "RF%", y_index = 1) |>
      e_mark_line(data = list(xAxis = m5$xPos[1]), title = "5m", title_position = "start") |> 
      e_mark_line(data = list(xAxis = m10$xPos[1]), title = "10m", title_position = "start") |> 
      e_mark_line(data = list(xAxis = m15$xPos[1]), title = "15m", title_position = "start") |> 
      e_color(background = "111111") |>
      e_grid(bottom = "10%") |>
      e_legend(show = TRUE, left = "right", orient = "horizontal", align = "right") |>
      e_title(text = "Power-RF% by Distance", 
              subtext = paste0("                                                  Peak RF: ", peakRF, "%")) |> 
      e_axis(serie = xPos, axis = "x", show = FALSE ) |>
      e_axis(serie = pHRZ_rel, axis = "y", index = 0, name = "Power", 
             nameLocation = "end", min = 10, max = 40,
             axisLabel = list(margin = 5, fontSize = 8,
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value}W/kg')
      ) |>
      e_axis(serie = RF, axis = "y", index = 1, name = "RF", 
             nameLocation = "end", min = 10, max = 70,
             axisLabel = list(margin = 5, fontSize = 8,
                              showMinLabel = FALSE, showMaxLabel = FALSE,
                              formatter = '{value}%')
      ) |>
      e_tooltip(trigger = "item",
                axisPointer = list(
                  type = "cross"
                ))
    
    plotPDP
    
  })
  
}
