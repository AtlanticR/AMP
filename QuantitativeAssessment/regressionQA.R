##############################################################################
### Regression figures 

# Stephen Finnis

# This code compares counts between the FlowCam and Microscopy for AMP
# Only for the 40 samples chosen for this study

# I am not sure if this should be converted to densities (ind m^-3) using the 
# water volume of the tow.

# Note that this uses the total count PER SAMPLE, so there is a conversion based on
# the percent of the sample analyzed

# Also not sure if there should be separate results per region, or all together

################################################################################
### Prep the data

# Run the script that preps the QA and FlowCam data
source("QuantitativeAssessment/QAcodeMatches.R")

## Put together the Quantitative Assessment data
# Summarize it to get the total zooplankton counts per sample
# Combine all dataframes (except qaPac2020)
allQADataReg = bind_rows(qaGulf2021, qaMar2021, qaNL2021, qaNLGulf2020, qaPac2021, qaPac2020) %>%
  select(qaSampleID, countTot, Taxon, taxStage, countSample) %>%
  # Join with the file that matches FlowCam IDs to Quantitative Assessment IDs
  left_join(qaID, by = "qaSampleID") %>%
  # Only select the samples we are interested in
  filter(selectForAnalysis == "Yes") %>%
  # Now replace the old names with my new updated names
  left_join(qaTaxaChanges) %>%
  # Select only the relevant columns again
  select(FlowCamID, countTot, regionYear, qaSampleID, newName) %>%
  # Need to make adjustments: a few taxa names were combined within each sample. Make sure these are added together.
  group_by(FlowCamID, regionYear, qaSampleID) %>%
  summarize(countTot = sum(countTot))

## Get the FlowCam data put together
# Summarize it to get the total zooplankton counts per sample
# NOTE dataForQA comes from flowCountQA.R, my test script for reading in count data for this project
# It's the same as zooplanktonCounts.R, but with a few adjustments for this project
# Need to join with qaID to match up the FlowCam codes from the quantitative assessment to what Julie put together
flowCamDataReg = qaID %>%
  left_join(fcDataForQA, by = c("FlowCamID" = "flowcamCode")) %>%
  # Only select the samples we're interested in
  filter(selectForAnalysis == "Yes") %>%
  group_by(qaSampleID) %>%
  mutate(FCentireSampleCount = sum(abundSample)) %>%
  select(FCentireSampleCount, qaSampleID, waterVolume, monthStart) %>%
  distinct() 

## Now combine both the QA and FC data together in one data frame for easier plotting  
# They are joined based on the same FlowCamID
fcAndMicro = flowCamDataReg %>%
    left_join(allQADataReg) 
    

################################################################################
### Do some analyses

# Run model 2 regression for non-transformed data
# Note, this type of regression is used because neither variable (axis) is controlled 
modOrig = lmodel2(dat$flowcamOrig ~ dat$micro)

# Compare these results against the other type of linear regression
summary(lm(dat$flowcamOrig ~ dat$micro))

# Create figure that plot counts of all flowcam data (y-axis) against all QA data (x-axis)
ggplot()+
  # Add a 1:1 red dashed line that shows a perfect relationship between counts
    geom_abline(slope=1, intercept=0, linetype = "dashed", col = "#F8766D", linewidth = 0.8)+
  # Add true regression line from model 2 regression
    geom_abline(slope = modOrig$regression.results$Slope[1], intercept = modOrig$regression.results$Intercept[1], col = "lightblue", linetype = "dotdash", linewidth = 0.8)+
    geom_point(data = fcAndMicro, aes(x = countTot, y = FCentireSampleCount), pch = 21, size = 4, alpha = 0.9, fill = "lightblue")+
    labs(x = "Microscopy counts (# ind per sample)",
         y = "FlowCam counts (# ind per sample)") +
    theme_bw()+
    theme(legend.title = element_blank())
  

# Show these same relationships broken up by the 4 regions
ggplot()+
  geom_point(data = fcAndMicro, aes(x = countTot, y = FCentireSampleCount), pch = 21, size = 4, alpha = 0.9, fill = "lightblue")+
  facet_wrap(~regionYear, scales = "free")+
  labs(x = "Microscopy counts (# ind per sample)",
       y = "FlowCam counts (# ind per sample)") +
  theme_bw()+
  theme(legend.title = element_blank())


# Create the same plot (without the regression lines) that plots these as abundances in seawater (idk if I'm supposed to do this)
ggplot()+
  #geom_abline(slope=1, intercept=0, linetype = "dashed", col = "#F8766D", linewidth = 0.8)+
  #geom_abline(slope = modOrig$regression.results$Slope[1], intercept = modOrig$regression.results$Intercept[1], col = "lightblue", linetype = "dotdash", linewidth = 0.8)+
  geom_point(data = fcAndMicro, aes(x = countTot*4/waterVolume, y = FCentireSampleCount*4/waterVolume), pch = 21, size = 4, alpha = 0.9, fill = "lightblue")+
  labs(x = "Microscopy counts (# ind per sample)",
       y = "FlowCam counts (# ind per sample)") +
  theme_bw()+
  theme(legend.title = element_blank())




###############################################################################
###############################################################################  
###############################################################################
#### OTHER TESTS 


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
  
  
  
  
  
  
  
  
  