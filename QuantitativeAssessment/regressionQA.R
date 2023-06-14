

################################################################################
## Combine FlowCam counts vs Microscopy counts

# Get the FlowCam data put together
# NOTE dataForQA comes from flowCountQA.R, my test script for reading in count data for this project
# It's the same as zooplanktonCounts.R, but with a few adjustments for this project
# Need to join with qaID to match up the FlowCam codes from the quantitative assessment to what Julie put together
flowCamData = qaID %>%
  left_join(fcDataForQA, by = c("FlowCamID" = "flowcamCode")) %>%
  # Only select the samples we're interested in
  filter(selectForAnalysis == "Yes") %>%
  group_by(qaSampleID) %>%
  mutate(FCentireSampleCount = sum(count)) %>%
  select(FCentireSampleCount, qaSampleID, waterVolume, monthStart) %>%
  distinct() %>%
  #group_by(qaSampleID) %>%
  #mutate(totVol = sum(waterVolume)) %>%
  # select(-waterVolume) %>%
  #distinct()
  
  # Get total microscopy counts for each sample
  qaDataPerSample = allQAData %>%
    group_by(FlowCamID) %>%
    mutate(QAentireSampleCount = sum(countTot)) %>%
    ungroup() %>%
    select(QAentireSampleCount, qaSampleID, site)%>%
    distinct()
  
  # Join the flowcam data with the microscopy data based on the FlowCamID
  fcAndMicro = flowCamData %>%
    left_join(qaDataPerSample) %>%
    
    
    # Plot them both to see what it looks like
    plot(fcAndMicro$FCentireSampleCount, fcAndMicro$QAentireSampleCount)
  
  x = lm(fcAndMicro$FCentireSampleCount ~ fcAndMicro$QAentireSampleCount)
  summary(x)
  
  
  microAbund = fcAndMicro$QAentireSampleCount*4 / fcAndMicro$totVol
  fcAbund = fcAndMicro$FCentireSampleCount*4/fcAndMicro$totVol
  
  plot(microAbund, fcAbund)
  
  summary(lm(microAbund ~fcAbund))
  summary(lm(fcAbund~microAbund))
  
  summary(lm(fcAndMicro$FCentireSampleCount~fcAndMicro$QAentireSampleCount))
  
  
  # Now try log transforming it
  # I think I will need to do this (based on the literature, and also because the data are skewed)
  plot(log10(fcAndMicro$FCentireSampleCount), log10(fcAndMicro$QAentireSampleCount))
  
  # Run a model 2 regression (need to do this because both variables are independent)
  mod = lmodel2(log10(fcAndMicro$FCentireSampleCount) ~ log10(fcAndMicro$QAentireSampleCount))
  
  # Now "correct" the flowcam data
  # To do this, divide by the slope, and subtract the intercept. This turns it into a 1:1 relationship (see links above for more background)
  flowcamCorLog10 = (log10(fcAndMicro$FCentireSampleCount) - mod$regression.results$Intercept[1])* (1/mod$regression.results$Slope[1])
  
  # Now store all the data in a dataframe
  # Note that I'm log transforming everything
  
  flowcamOrig = fcAndMicro$FCentireSampleCount
  micro = fcAndMicro$QAentireSampleCount
  
  flowcamOrigLog10 = log10(fcAndMicro$FCentireSampleCount)
  microLog10 = log10(fcAndMicro$QAentireSampleCount)
  
  dat = data.frame(flowcamCorLog10, flowcamOrig, micro, flowcamOrigLog10, microLog10)
  
  
  datMeltLog = melt(data = dat,
                    id.vars = c("microLog10"),
                    measure.vars = c("flowcamCorLog10", "flowcamOrigLog10"),
                    variable.name = "flowcamType",
                    value.name = "value")
  
  # datOrig = data.frame(
  #   type = c(rep("Micro", length(micro)), rep("flowcam", length(flowcamOrig))),
  #   value = c(micro, flowcamOrig)
  # )
  
  datOrig = data.frame(
    micro, flowcamOrig
  )
  
  
  ggplot()+
    geom_abline(slope=1, intercept=0, linetype = "dashed", col = "#F8766D", linewidth = 0.8)+
    geom_abline(slope = mod$regression.results$Slope[1], intercept = mod$regression.results$Intercept[1], col = "#00BFC4", linetype = "dotdash", linewidth = 0.8)+
    geom_point(data = datMeltLog, aes(x = microLog10, y = value, fill = flowcamType, pch = flowcamType), size = 4, alpha = 0.9)+
    scale_shape_manual(values = c(21, 22), labels = c("Corrected", "Original"))+
    scale_fill_manual(values = c("white", "#00BFC4"), labels = c("Corrected", "Original"))+
    labs(x = expression(log[10]~"[Microscopy abundance (ind."~ (m^-3) ~"]"),
         y = expression(log[10]~"[FlowCam abundance (ind."~ (m^-3) ~"]")) +
    theme_bw()+
    theme(legend.title = element_blank())
  
  
  # Run model 2 regression for non-transformed data
  
  modOrig = lmodel2(dat$flowcamOrig ~ dat$micro)
  
  
  ggplot()+
    geom_abline(slope=1, intercept=0, linetype = "dashed", col = "#F8766D", linewidth = 0.8)+
    geom_abline(slope = modOrig$regression.results$Slope[1], intercept = modOrig$regression.results$Intercept[1], col = "lightblue", linetype = "dotdash", linewidth = 0.8)+
    
    geom_point(data = datOrig, aes(x = micro, y = flowcamOrig), pch = 21, size = 4, alpha = 0.9, fill = "lightblue")+
    
    labs(x = "Microscopy abundance (ind."~ (m^-3),
         y = "FlowCam abundance (ind."~ (m^-3)) +
    theme_bw()+
    theme(legend.title = element_blank())
  
  
  
  
  
  # also see
  # https://www.eeer.org/upload/eer-2018-266.pdf for more bias corrections
  
  
  
  ################################################################################
  # Test comparison between microscopy and flowcam
  # Perform a correction to the count data from flowcam
  
  
  # Create fake microscopy data
  microscopy = runif(50, min = 0, max = 10)
  # Create fake flowcam data based on the microscopy data
  # Make it obviously different 
  flowcam = microscopy*2 + rnorm(50, mean = 0, sd = 1) + 5
  
  # Plot them both to see how it looks
  plot(microscopy, flowcam)
  
  # Run a model 2 regression 
  model = lmodel2(flowcam ~ microscopy, data = data.frame(microscopy, flowcam))
  
  # Get the slope and intercept (note: not sure which of the options I'm supposed to use)
  # For now, just use the first model output i.e., [1]
  intercept = model$regression.results$Intercept[1]
  slope = model$regression.results$Slope[1]
  
  # Now, correct the flowcam data! 
  # I want the flowcam data to now be at a 1:1 relationship with the microscopy data
  # following y = mx+b, I need to divide by the slope, then subtract the intercept!
  correction_factor = 1/slope
  cor_flowcam = (flowcam - intercept) * correction_factor
  
  # Combine the data
  combData = data.frame(flowcam, microscopy, cor_flowcam)
  
  # Plot the data
  ggplot()+
    geom_point(data = test, aes(x = microscopy, y = flowcam))+ # uncorrected
    geom_point(data = test, aes(x = microscopy, y = cor_flowcam), col = "red")+ # corrected
    geom_abline(slope=1, intercept=0)+ 
    xlim(0, 10)+
    ylim(0,30)
  
  ################################################################################
  # Test interactive plotting to see what the problem samples are 
  x=
    ggplot()+
    geom_point_interactive(data = fcAndMicro, aes(x = QAentireSampleCount, y = FCentireSampleCount), data_id = fcAndMicro$qaSampleID, tooltip = fcAndMicro$qaSampleID, onclick = fcAndMicro$qaSampleID, size = 5)
  girafe(ggobj = x)
  
  
  
  microAbund = fcAndMicro$QAentireSampleCount*4 / fcAndMicro$totVol
  fcAbund = fcAndMicro$FCentireSampleCount*4/fcAndMicro$totVol
  
  x=
    ggplot()+
    geom_point_interactive(data = fcAndMicro, aes(x = microAbund, y = fcAbund), data_id = fcAndMicro$qaSampleID, tooltip = fcAndMicro$qaSampleID, onclick = fcAndMicro$qaSampleID, size = 5)
  girafe(ggobj = x)
  
  
  
  
  
  
  
  
  