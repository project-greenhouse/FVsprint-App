Server <- function(input, output) {
  
  ####--------------------| Profile  --------------------####
  
  #---------- |-| Reactive Functions --------------------
  # |--| Rep Recorded Summary Data frame --------------------
  repSummary <- reactive({
  
    repSummary <- sTable %>% 
      filter(RepId == input$profFilter) %>% 
      select(Meter, Times, Splits, Velocity)
  })
  
  # |--| Kinetics Model Data frame --------------------
  repModel <- reactive({
    
    repModel <- mTable %>% filter(repId == input$profFilter)
    
  })
  
  # |--| Kinetics Model Calculations --------------------
  repOutputs <- reactive({
    
    repOutputs <- oTable %>% filter(RepId == input$profFilter)
    
  })
  
  #---------- |-| Outputs --------------------
  
  # |--| Rep Summary Table  --------------------
  output$tableRepSummary <- renderTable({ 
     repSummary()
    })
  
  # |--| Info Boxes --------------------
  
  # ~ Peak Force 
  output$infoboxPeakF <- renderInfoBox({
    m <- repOutputs()
    peakF <- m$f0
    
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
    m <- repOutputs()
    peakV <- m$v0

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
    m <- repOutputs()
    
    peakP <- m$pMax
    
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
    m <- repOutputs()
    
    slope <- m$fvSlope
    
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
    m <- repOutputs()
    
    n <- m$fRatio
    
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
    m <- repOutputs()
    
    drf <- m$RFD
    
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
    m <- repOutputs()
    
    vOpt <- m$vOpt
    
    infoBox(
      title = p(strong(h6("Optml Velo"))),
      subtitle = "Vopt (m/s)",
      value = vOpt,
      color = "green",
      fill = TRUE,
      icon = icon("balance-scale")
    )
  })
  
  # ~ Max Speed 
  output$infoboxMaxSpeed <- renderInfoBox({
    m <- repOutputs()
    
    vMax <- m$vMax
    
    infoBox(
      title = p(strong(h6("Max Velo"))),
      subtitle = "Vmax (m/s)",
      value = vMax,
      color = "green",
      fill = TRUE,
      icon = icon("fighter-jet")
    )
  })
  
  
  #---------- |-| Plots --------------------
  
  # |---| Model Fit --------------------
  output$plotRecorded <- renderEcharts4r({
    
    x <- repModel() %>% transmute("mDist" = xPos, "mTime" = time, "index" = time*100)
    y <- repSummary() %>% transmute("rDist" = as.numeric(Meter), "rTime" = as.numeric(Times), "index" = as.numeric(Times)*100) 
    
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
    
    df <- df %>%
      transmute("time" = as.numeric(time), 
                "fHRZ_rel" = round(as.numeric(fHRZ_rel),1), 
                "velo" = round(as.numeric(velo),2),
                "xPos" = as.numeric(xPos)
      )
    
    # Find FV Slope
    lm <- lm(fHRZ_rel~velo, data = df)
    coef <- coef(lm)
    slope<- round(coef[2],2)
    
    max <- list(
      name = "Max",
      type = "max"
    )
    
    m5 <- df %>% filter(xPos > 4.98 & xPos < 5.98 )
    m10 <- df %>% filter(xPos > 9.98 & xPos < 10.98 )
    m15 <- df %>% filter(xPos > 14.98 & xPos < 15.98 )
    
    # Plot
    plot_FVT <- df |>
      e_charts(time) |>
      e_bar(velo, name = "Velocity", y_index = 1) |>
      e_bar(fHRZ_rel, name = "Force", y_index = 0) |>
      e_mark_line(data = list(xAxis = m5$time[1]), title = "5m", title_position = "middle", symbol = "none") |> 
      e_mark_line(data = list(xAxis = m10$time[1]), title = "10m", title_position = "middle", symbol = "none") |> 
      e_mark_line(data = list(xAxis = m15$time[1]), title = "15m", title_position = "middle", symbol = "none") |>
      e_mark_point(serie = "Velocity", 
                   data = max, 
                   symbol = "roundRect",
                   symbolSize = 30) |>
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
  output$plotFPV <- renderEcharts4r({
    df <- repModel()
    
    peakP <- round(max(df$pHRZ_rel, na.rm = TRUE),2)
    
    FatpP <- df %>% filter(pHRZ_rel == max(pHRZ_rel)) %>% select(velo, fHRZ_rel)
    
    plotFPV <- df |>
      e_charts(velo) |>
      e_scatter(pHRZ_rel, name = "Power", y_index = 1) |>
      e_bar(fHRZ_rel, name = "Force", y_index = 0) |>
      e_mark_line(data = list(xAxis = FatpP$velo[1]), label = list(show = FALSE), symbol = "none") |>
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
  
  #|---| Power-DRF-Position --------------------
  output$plotPDP <- renderEcharts4r({
    df <- repModel()
    df <- df %>%
      transmute(
        "xPos" = as.numeric(xPos), 
        "pHRZ_rel" = round(as.numeric(pHRZ_rel), 1), 
        "RF" = round(as.numeric(RF),1)
      )
    peakRF <- max(df$RF, na.rm = TRUE)
    
    m5 <- df %>% filter(xPos > 4.98 & xPos < 5.98 )
    m10 <- df %>% filter(xPos > 9.98 & xPos < 10.98 )
    
    max <- list(
      name = "Max",
      type = "max"
    )
    
    plotPDP <- df |>
      filter(xPos <= 15) |>
      e_charts(xPos) |>
      e_scatter(pHRZ_rel, name = "Power", y_index = 0) |>
      e_bar(RF, name = "RF%", y_index = 1) |>
      e_mark_point(serie = "Power", 
                   data = max, 
                   symbol = "roundRect",
                   symbolSize = 30) |>
      e_mark_line(data = list(xAxis = m5$xPos[1]), title = paste0("5m: ", round(m5$RF[1],0),"%"), title_position = "start", symbol = "none") |> 
      e_mark_line(data = list(xAxis = m10$xPos[1]), title = paste0("10m: ", round(m10$RF[1],0),"%"), title_position = "start", symbol = "none") |> 
      e_color(background = "111111") |>
      e_grid(bottom = "10%") |>
      e_legend(show = TRUE, left = "right", orient = "horizontal", align = "right") |>
      e_title(text = "Power-RF% by Distance", 
              subtext = paste0("                                                  Peak RF: ", round(peakRF,0), "%")) |> 
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
  
  
  ##########--------------------| Tracking  --------------------##########
  
  #---------- |-| Reactive Functions --------------------
  
  # |--| Chronological Recorded Summary Data frame --------------------
  chronSummary <- reactive({
    
    cSumm <- cSummary %>% 
      filter(Name == input$chronFilter) %>%
      select(Date, cDate, Meter, Times, Splits, Velocity, pSprint)
    
    return(cSumm)
    
  })

  # |--| Chronological Daily Output Summary Data frame --------------------
  chronOutputs <- reactive({
    req(input$chronFilter)
    
    out <- cOutputs %>%
      filter(Name == input$chronFilter)
    
    return(out)
  })
  
  # |--| Rep Models by Date Data frame --------------------
  chronModel <- reactive({
    
    m <- cModel %>% 
      filter(Name == input$chronFilter) 
    
    return(m)
  })
  
  # |--| Week selection filter --------------------
  chronWeek <- reactive({
    input$chronWeekFilter
  })

  
  #---------- |-| Plots --------------------
  
  # |---| Peak Velocity --------------------
  output$chronV0 <- renderEcharts4rBox({
    co <- chronOutputs()
    co1 <- co %>%
      select(Date, v0) %>%
      mutate("WeekNo" = as.numeric(week(Date)), "vScale" = scale_this(v0))
    
    w <- chronWeek()
    wselect <- co1 %>% filter(WeekNo == w)
    n <- round(wselect$vScale[1],2)
    
    bcolor <- if (n < -0.5 ) {
      "#e48981"
    } else if(n > 0.5) {
      "#84cc33"
    } else {"#f1f3f5"}
    
    tcolor <- if (n < -0.5 ) {
      "#ffffff"
    } else if(n > 0.5) {
      "#ffffff"
    } else {"#121212"}
    
    echarts4rBox(
      data = co1,
      x = WeekNo,
      y = vScale,
      type = "bar",
      step = "middle",
      text = paste0(round(wselect$v0[1],2), "m/s"),
      subtext = "v0",
      background_color = bcolor,
      color = tcolor,
      text_color = tcolor
    )
  })
  
  # |---| Peak Force --------------------
  output$chronF0 <- renderEcharts4rBox({
    fo <- chronOutputs()
    fo1 <- fo %>%
      select(Date, f0) %>%
      mutate("WeekNo" = as.numeric(week(Date)), "fScale" = scale_this(f0))
    
    w <- chronWeek()
    wselect <- fo1 %>% filter(WeekNo == w)
    n <- wselect$fScale[1]
    
    bcolor <- if (n < -0.5 ) {
      "#e48981"
    } else if(n > 0.5) {
      "#84cc33"
    } else {"#f1f3f5"}
    
    tcolor <- if (n < -0.5 ) {
      "#ffffff"
    } else if(n > 0.5) {
      "#ffffff"
    } else {"#121212"}
    
    echarts4rBox(
      data = fo1,
      x = WeekNo,
      y = fScale,
      type = "area",
      step = "middle",
      text = paste0(round(wselect$f0[1],1), "N/kg"),
      subtext = "f0",
      color = tcolor,
      background_color = bcolor,
      text_color = tcolor
    )
  })
  
  # |---| Sprint Momentum --------------------
  output$chronP <- renderEcharts4rBox({
    p <- chronSummary()
    p1 <- p %>%
      filter(Meter == 5) %>%
      select(Date, pSprint) %>%
      mutate("WeekNo" = as.numeric(week(Date)), "pScale" = scale_this(pSprint))
      
    
    w <- chronWeek()
    wselect <- p1 %>% filter(WeekNo == w)
    n <- wselect$pScale[1]
    
    bcolor <- if (n < -0.5 ) {
      "#e48981"
    } else if(n > 0.5) {
      "#84cc33"
    } else {"#f1f3f5"}
    
    tcolor <- if (n < -0.5 ) {
      "#ffffff"
    } else if(n > 0.5) {
      "#ffffff"
    } else {"#121212"}
    
    echarts4rBox(
      data = p1,
      x = WeekNo,
      y = pScale,
      type = "bar",
      step = "middle",
      text = wselect$pSprint[1],
      subtext = "sMomentum",
      color = tcolor,
      background_color = bcolor,
      text_color = tcolor
    )
  })
  
  # |---| Relative Power --------------------
  output$chronpHRZ <- renderEcharts4r({
    df <- chronModel() 
    z <- chronWeek()
    wselect <- as.character(paste0("w", z))
    peakP <- round(max(df$pHRZ_rel),1)
    
    df1 <- df %>% 
      mutate("WeekNo" = paste0("w",as.character(week(Date)))) %>%
      select(WeekNo, time, pHRZ_rel) %>% 
      pivot_wider(names_from = WeekNo, values_from = pHRZ_rel) %>%
      mutate("Hist" = (w2+w3+w4+w5+w6+w7+w8+w9+w10)/9) %>%
      select(time, wselect, Hist)
    
    colnames(df1) <- c("time", "selected", "avg")
    
    df1 |>
      e_charts(time) |>
      e_scatter(selected) |>
      e_area(avg) |>
      e_legend(show = TRUE, 
               left = "right", 
               orient = "horizontal", 
               align = "right"
               ) |>
      e_mark_line(data = list(yAxis = peakP), 
                  title = paste0(peakP,"W/kg"), 
                  title_position = "end",
                  symbol = "none"
                  ) |>
      e_y_axis(name = "W/kg",
              nameLocation = "end",
               min = 10,
               max = 30,
               margin = 5,
               minorTick = list(show = TRUE, splitNumber = 1)
              ) |>
      e_tooltip(trigger = "item",
                axisPointer = list(
                  type = "cross"
                ))
      

  })
  
  
  ##########--------------------| Comparisons  --------------------##########
  
  #---------- |-| Reactive Functions --------------------
  # |--| Rep Recorded Summary Data frame --------------------
  # Athlete 1 Aggregate data
  compSummary1 <- reactive({
    compSummary <- sTable %>% 
      filter(RepId == input$compFilter1) %>% 
      select(Meter, Times, Splits, Velocity)
  })
  # Athlete 2 Aggregate data
  compSummary2 <- reactive({
    compSummary <- sTable %>% 
      filter(RepId == input$compFilter2) %>% 
      select(Meter, Times, Splits, Velocity)
  })
  
  # |--| Kinetics Model Data frame --------------------
  # Athlete 1 Model
  compModel1 <- reactive({
    
    compModel <- mTable %>% filter(repId == input$compFilter1)
    
  })
  # Athlete 2 Model
  compModel2 <- reactive({
    
    compModel <- mTable %>% filter(repId == input$compFilter2)
    
  })
  
  # |--| Kinetics Model Calculations --------------------
  # Athlete 1 Outputs
  comOutupts1 <- reactive({
    
    repOutputs <- oTable %>% filter(RepId == input$compFilter1)
    
  })
  # Athlete 2 Outputs
  comOutupts2 <- reactive({
    
    repOutputs <- oTable %>% filter(RepId == input$compFilter2)
    
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
    m <- comOutupts1()
    peakF <- m$f0
    
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
    m <- comOutupts2()
    peakF <- m$f0
    
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
    m <- comOutupts1()
    
    peakV <- m$v0

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
    m <- comOutupts2()
    
    peakV <- m$v0
    
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
    m <- comOutupts1()
    
    peakP <- m$pMax
    
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
    m <- comOutupts2()
    
    peakP <- m$pMax

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
    
    df <- df %>%
      transmute("time" = as.numeric(time), 
                "fHRZ_rel" = round(as.numeric(fHRZ_rel),1), 
                "velo" = round(as.numeric(velo),2),
                "xPos" = as.numeric(xPos)
                )
    
    # Find FV Slope
    lm <- lm(fHRZ_rel~velo, data = df)
    coef <- coef(lm)
    slope<- round(coef[2],2)
    
    max <- list(
      name = "Max",
      type = "max"
    )
    
    m5 <- df %>% filter(xPos > 4.98 & xPos < 5.98 )
    m10 <- df %>% filter(xPos > 9.98 & xPos < 10.98 )
    m15 <- df %>% filter(xPos > 14.98 & xPos < 15.98 )
    
    # Plot
    plot_FVT <- df |>
      e_charts(time) |>
      e_bar(velo, name = "Velocity", y_index = 1) |>
      e_bar(fHRZ_rel, name = "Force", y_index = 0) |>
      e_mark_line(data = list(xAxis = m5$time[1]), title = "5m", title_position = "middle", symbol = "none") |> 
      e_mark_line(data = list(xAxis = m10$time[1]), title = "10m", title_position = "middle", symbol = "none") |> 
      e_mark_line(data = list(xAxis = m15$time[1]), title = "15m", title_position = "middle", symbol = "none") |>
      e_mark_point(serie = "Velocity", 
                   data = max, 
                   symbol = "roundRect",
                   symbolSize = 30) |>
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
    
    # Athlete 2 Data
    df <- compModel2()
    
    df <- df %>%
      transmute("time" = as.numeric(time), 
                "fHRZ_rel" = round(as.numeric(fHRZ_rel),1), 
                "velo" = round(as.numeric(velo),2),
                "xPos" = as.numeric(xPos)
      )
    
    # Find FV Slope
    lm <- lm(fHRZ_rel~velo, data = df)
    coef <- coef(lm)
    slope<- round(coef[2],2)
    
    max <- list(
      name = "Max",
      type = "max"
    )
    
    m5 <- df %>% filter(xPos > 4.98 & xPos < 5.98 )
    m10 <- df %>% filter(xPos > 9.98 & xPos < 10.98 )
    m15 <- df %>% filter(xPos > 14.98 & xPos < 15.98 )
    
    # Plot
    plot_FVT <- df |>
      e_charts(time) |>
      e_bar(velo, name = "Velocity", y_index = 1) |>
      e_bar(fHRZ_rel, name = "Force", y_index = 0) |>
      e_mark_line(data = list(xAxis = m5$time[1]), title = "5m", title_position = "middle", symbol = "none") |> 
      e_mark_line(data = list(xAxis = m10$time[1]), title = "10m", title_position = "middle", symbol = "none") |> 
      e_mark_line(data = list(xAxis = m15$time[1]), title = "15m", title_position = "middle", symbol = "none") |>
      e_mark_point(serie = "Velocity", 
                   data = max, 
                   symbol = "roundRect",
                   symbolSize = 30) |>
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
    
    FatpP <- df %>% filter(pHRZ_rel == max(pHRZ_rel)) %>% select(velo, fHRZ_rel)
    
    plotFPV <- df |>
      e_charts(velo) |>
      e_scatter(pHRZ_rel, name = "Power", y_index = 1) |>
      e_bar(fHRZ_rel, name = "Force", y_index = 0) |>
      e_mark_line(data = list(xAxis = FatpP$velo[1]), label = list(show = FALSE), symbol = "none") |>
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
  # Athlete 2 FPV Plot
  output$CompPlotFPV2 <- renderEcharts4r({
    
    # Athlete 2 Data
    df <- compModel2()
    
    peakP <- round(max(df$pHRZ_rel, na.rm = TRUE),2)
    
    FatpP <- df %>% filter(pHRZ_rel == max(pHRZ_rel)) %>% select(velo, fHRZ_rel)
    
    plotFPV <- df |>
      e_charts(velo) |>
      e_scatter(pHRZ_rel, name = "Power", y_index = 1) |>
      e_bar(fHRZ_rel, name = "Force", y_index = 0) |>
      e_mark_line(data = list(xAxis = FatpP$velo[1]), label = list(show = FALSE), symbol = "none") |>
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
  # Athlete 1 PRF Plot
  output$CompPlotPFD1 <- renderEcharts4r({
    
    # Athlete 1 Data
    df <- compModel1() %>%
      transmute(
        "xPos" = as.numeric(xPos), 
        "pHRZ_rel" = round(as.numeric(pHRZ_rel), 1), 
        "RF" = round(as.numeric(RF),1)
        )
    
    peakRF <- max(df$RF, na.rm = TRUE)
    
    m5 <- df %>% filter(xPos > 4.98 & xPos < 5.98 )
    m10 <- df %>% filter(xPos > 9.98 & xPos < 10.98 )
    
    max <- list(
      name = "Max",
      type = "max"
    )
    
    plotPDP <- df |>
      filter(xPos <= 15) |>
      e_charts(xPos) |>
      e_scatter(pHRZ_rel, name = "Power", y_index = 0) |>
      e_bar(RF, name = "RF%", y_index = 1) |>
      e_mark_point(serie = "Power", 
                   data = max, 
                   symbol = "roundRect",
                   symbolSize = 30) |>
      e_mark_line(data = list(xAxis = m5$xPos[1]), title = paste0("5m: ", round(m5$RF[1],0),"%"), title_position = "start", symbol = "none") |> 
      e_mark_line(data = list(xAxis = m10$xPos[1]), title = paste0("10m: ", round(m10$RF[1],0),"%"), title_position = "start", symbol = "none") |> 
      e_color(background = "111111") |>
      e_grid(bottom = "10%") |>
      e_legend(show = TRUE, left = "right", orient = "horizontal", align = "right") |>
      e_title(text = "Power-RF% by Distance", 
              subtext = paste0("                                                  Peak RF: ", round(peakRF,0), "%")) |> 
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
  # Athlete 2 PRF Plot
  output$CompPlotPFD2 <- renderEcharts4r({
    
    # Athlete 2 Data
    df <- compModel2()%>%
      transmute(
        "xPos" = as.numeric(xPos), 
        "pHRZ_rel" = round(as.numeric(pHRZ_rel), 1), 
        "RF" =round(as.numeric(RF),1)
        )
    
    peakRF <- max(df$RF, na.rm = TRUE)
    
    m5 <- df %>% filter(xPos > 4.98 & xPos < 5.98 )
    m10 <- df %>% filter(xPos > 9.98 & xPos < 10.98 )
    
    max <- list(
      name = "Max",
      type = "max"
    )
    
    plotPDP <- df |>
      filter(xPos <= 15) |>
      e_charts(xPos) |>
      e_scatter(pHRZ_rel, name = "Power", y_index = 0) |>
      e_bar(RF, name = "RF%", y_index = 1) |>
      e_mark_point(serie = "Power", 
                   data = max, 
                   symbol = "roundRect",
                   symbolSize = 30) |>
      e_mark_line(data = list(xAxis = m5$xPos[1]), title = paste0("5m: ", round(m5$RF[1],0),"%"), title_position = "start", symbol = "none") |> 
      e_mark_line(data = list(xAxis = m10$xPos[1]), title = paste0("10m: ", round(m10$RF[1],0),"%"), title_position = "start", symbol = "none") |> 
      e_color(background = "111111") |>
      e_grid(bottom = "10%") |>
      e_legend(show = TRUE, left = "right", orient = "horizontal", align = "right") |>
      e_title(text = "Power-RF% by Distance", 
              subtext = paste0("                                                  Peak RF: ", round(peakRF,0), "%")) |> 
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
