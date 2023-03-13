################################################################################
################################################################################
### EXTRACTING THE DATA FROM THE TESTS I JUST RAN

# For many of the multivariate stats, the results are not easy to copy and paste directly into tables
# The section below does just that: extracts the information and puts it all in a data frame
# Values are also rounded, columns added, rows added, absolute values taken, etc.
# Note the final df may not be perfect (e.g., incorrect column names) , but it is in a "good enough" form 
# since I already have the tables created in Google Docs

# I am creating data frames for 3 of the tests: 
# 1. Dispersion statistics (and the pairwise stats)
# 2. PERMANOVA statistics (and pairwise stats)
# 3. SIMPER (maybe not for bay comparisons. tbd)

################################################################################
### Set up

# Read in the appropriate scripts
source("Figures/permanovaRegions.R") # for permanova, dispersion & simper between regions
source("Figures/permanovaBays.R") # for permanova & dispersion for bays

## DISPERSION

# Create a function that reads in the results of the betadisper test and puts it all into 
# a data frame so it can be exported as a csv
# This includes the main test, but also the pairwise comparisons 
dispCreateTable = function(permDispResults){
  
  # Get the results from the main betadisper test (the table with the degrees of freedom, sum of squares etc.)
  dispDf = data.frame(permDispResults$tab) %>%
    tibble::rownames_to_column(var = "Source") %>%
    # Create a "Total" row that sums a few (but not all) of the columns 
    # I'm confused why the column names have to be written with ` ` when data.frame() gets rid of the spacing. Oh well, it works!
    # It has something to do with the way everything is piped..
    do(bind_rows(., data.frame(Source="Total", Df = sum(permDispResults$tab$Df), `Sum Sq` = sum(permDispResults$tab$`Sum Sq`),
                               `Mean Sq` = sum(permDispResults$tab$`Mean Sq`)))) %>% 
    # round some values to 3 decimal places
    mutate(across(c(Sum.Sq, Mean.Sq, F), round, 3)) %>%
    select(-N.Perm)
  
  
  # Create data frame with t-values and permuted p-values
  # Drop the first value from statistic column since it represents the overall f-value
  dispPairwiseStats = data.frame(tstat = permDispResults$statistic[-1], permP = permDispResults$pairwise$permuted)
  
  # I think I have to break up this step from above or things don't work
  # Make some adjustments to the table
  # The comparisons are already in alphabetical order, yay!
  dispPairwiseStats = dispPairwiseStats %>%
    tibble::rownames_to_column(var = "Comparison") %>%
    # I think I'm supposed to be taking the absolute value?? But maybe not!
    # EDIT: DO NOT MAKE DISPERSION T-STATISTIC NEGATIVE 
    # SEE HERE FOR MY QUESTION: https://stats.stackexchange.com/questions/605558/are-negative-t-values-important-for-pairwise-betadisper-results
    #mutate(tstat = abs(tstat)) %>%
    mutate(across(c(tstat), round, 3))
  
  # Combine the ANOVA-like table with the pairwise comparison values
  # This won't be combined perfectly but that's fine!
  bind_rows(dispDf, dispPairwiseStats)
  
}

# Call the function to get the relevant dispersion test data in a data frame

# For the data from permanovaRegions.R
regDispTable = dispCreateTable(pairRegDisp)
marDispTable = dispCreateTable(pairMarDisp)
gulfDispTable = dispCreateTable(pairGulfDisp)
pacDispTable = dispCreateTable(pairPacDisp)

# For the data from permanovaBays.R
# Note that pairwise comparisons will be included in the table, but I won't be using these due to the low sample size
# Each bay will have two tables: one for tide effects, one for station effects (must be done separately with betadisper)
argDispTable = rbind(dispCreateTable(argTideDispResults),
                         dispCreateTable(argStnDispResults))

stPDispTable = rbind(dispCreateTable(stPTideDispResults),
                     dispCreateTable(stPStnDispResults)) 

aug2020DispTable = rbind(dispCreateTable(aug2020TideDispResults),
                         dispCreateTable(aug2020StnDispResults))

jun2021DispTable = rbind(dispCreateTable(jun2021TideDispResults),
                         dispCreateTable(jun2021StnDispResults))

sept2021DispTable = rbind(dispCreateTable(sept2021TideDispResults),
                          dispCreateTable(sept2021StnDispResults))

nl21DispTable = rbind(dispCreateTable(nl21TideDispResults),
                      dispCreateTable(nl21StnDispResults))


# Write them out as csvs
# write.csv(regDispTable, "regionsDisp.csv")
# write.csv(marDispTable, "marDisp.csv")
# write.csv(gulfDispTable, "gulfDisp.csv")
# write.csv(pacDispTable, "pacDisp.csv")

# write.csv(argDispTable, "argDispTable.csv")
# write.csv(stPDispTable, "stPDispTable.csv")
# write.csv(aug2020DispTable, "aug2020DispTable.csv")
# write.csv(jun2021DispTable, "jun2021DispTable.csv")
# write.csv(sept2021DispTable, "sept2021DispTable.csv")
# write.csv(nl21DispTable, "nl21DispTable.csv")

#################################################################################
## PERMANOVA

# Create a function to combine adonis2, pairwise.adonis2 results in one data frame
# It extracts the relevant data and manipulates data to be in correct format (e.g., round values, add extra columns, etc)
# The resulting dataframe won't be perfect. But it gets it very close so I can just copy and paste the values into the tables already created in Google Docs
# If I want an exact table to be exported (e.g., if using csasdown) it will need a bit of extra work
# Function also passes in the "order" variable which specifies the order (alphabetical) the pairwise comparisons should be listed in
permCreateTable = function(permResults, pairwiseDf, order){
  
  # Get the results from the regular permanova (adonis2 output)
  permResults = permResults %>%
    # Multiply R2 by 100 to convert to percentages
    mutate_at(vars(c(R2)), .funs = funs(.*100)) %>% 
    # Add mean sum of squares (MS) column. Note: this adds a cell in the "total" row. Will need to manually remove this.
    mutate(MS = SumOfSqs/Df, .before = R2) %>%
    # Round most columns to 2 decimal places
    mutate(across(c(SumOfSqs, MS, R2, F), round, 3))
  
  # Create a function to extract values from the pairwise.adonis2 function
  # Pass in the resulting object. It's  a list of lists so it's a bit confusing what's happening
  # I want to create a dataframe of results containing columns with the comparison (e.g., Maritimes vs Gulf), t-value, and p-value
  pairwise.df = 
    # I probably should have used dplyr but I got confused how to do it
    # Create a new dataframe by combining values into unique colums
    as.data.frame(cbind(
      # Get the names of each list element (i.e., comparisons) but drop the first one since it's named "parent_call"
      comparison = names(pairwiseDf)[-1], 
      # 4th value from the list of lists is the F-value. Each test has one F-value followed by multiple NAs just due to how the function reports results
      # I need to turn it into a number, round it to 3 decimal places, due the "format" function to make sure if there aren't 3 decimal places due to zeroes, they get added anyway
      # Then square root the F-value to turn it into a t-value (see "Pairwise Comparisons" p.26 from http://updates.primer-e.com/primer7/manuals/PERMANOVA+_manual.pdf)
      tValue = format(round(sqrt(as.numeric(na.omit(flatten_chr(map(pairwiseDf, 4))))), 3), nsmall = 3),
      # 5th value is the p-value. Convert from character to numeric and round to 4 decimal places 
      pValue = format(round(as.numeric(na.omit(flatten_chr(map(pairwiseDf, 5)))), 4), nsmall = 4))) %>%
    
    # Then rearrange them to be in alphabetical order
    slice(match(order, comparison))
  
  # Combine the full permanova with the pairwise outputs
  # This will not be a perfect table (e.g., columns don't perfectly line up)- I can fix that later! 
  # This is good enough that I can copy and paste values into my existing table in Google Docs
  bind_rows(permResults, pairwise.df)
  
}

# I want the pairwise comparisons to be in alphabetical order. Specify the order I want them to be in
# Note that sometimes it is alphabetical based on the second bay name below (e.g., Argyle is 1st). But that's too much work to change. I already have the row names written out in the Google Doc, I just need the data
order.regions = c("Maritimes_vs_Gulf", "Newfoundland_vs_Gulf", "Pacific_vs_Gulf", "Maritimes_vs_Newfoundland", "Maritimes_vs_Pacific", "Newfoundland_vs_Pacific")
order.marBays = c("Country Harbour_vs_Argyle", "Sober Island_vs_Argyle", "Whitehead_vs_Argyle", "Country Harbour_vs_Sober Island", "Country Harbour_vs_Whitehead", "Whitehead_vs_Sober Island")
order.gulfBays = c("Cocagne_vs_Malpeque", "Cocagne_vs_St. Peters", "Malpeque_vs_St. Peters")
order.pacField = c("September 2021_vs_August 2020", "August 2020_vs_June 2021", "September 2021_vs_June 2021")

# For the bays, I also want the comparisons to be alphabetical
order.arg = c("Central_vs_North", "Central_vs_South", "South_vs_North")
order.stP = c("Mid_vs_Inner", "Outer_vs_Inner", "Outer_vs_Mid")
order.aug2020 = c("Mid_vs_Inner", "Outer_vs_Inner", "Outer_vs_Mid")
order.sept2021 = c("Inner_vs_Mid", "Outer_vs_Inner", "Outer_vs_Mid")

# Make PERMANOVA tables to be exported that combine adonis2 and pairwise.adonis2 results
# Also rearranges the pairwise.adonis2 in alphabetical order

# For the data from permanovaRegions.R
regPNtable = permCreateTable(regPNresults, regPairwisePN, order.regions)
marPNtable = permCreateTable(marPNresults, marPairwisePN, order.marBays)
gulfPNtable = permCreateTable(gulfPNresults, gulfPairwisePN, order.gulfBays)
pacPNtable = permCreateTable(pacPNresults, pacPairwisePN, order.pacField)

# for the data from permanovaBays.R
# Note, no pairwise comparisons were done, so just pass in NAs for the pairwise things
# argPNtableTide = permCreateTable(argPerm, argTidePairwise, NA) # Do not need tide pairwise since only 2 factors
argPNtable = permCreateTable(argPerm, argStnPairwise, order.arg)
stPpermTable = permCreateTable(stPperm, stPStnPairwise, order.stP)
aug2020permTable = permCreateTable(aug2020perm, pacAug2020StnPairwise, order.aug2020)
jun2021permTable = permCreateTable(jun2021perm, pacJun2021StnPairwise, NA) # None were significant. No pairwise comparisons
sept2021permTable = permCreateTable(sept2021perm, pacSept2021StnPairwise, order.sept2021)
nl21permTable = permCreateTable(nl21perm, NA, NA)

# write.csv(regPNtable, "regPNtable.csv")
# write.csv(marPNtable, "marPNtable.csv")
# write.csv(gulfPNtable, "gulfPNtable.csv")
# write.csv(pacPNtable, "pacPNtable.csv")



# write.csv(argPNtable, "argPNtable.csv")
# write.csv(stPpermTable, "stPpermTable.csv")
# write.csv(aug2020permTable, "aug2020permTable.csv")
# write.csv(jun2021permTable, "jun2021permTable.csv")
# write.csv(sept2021permTable, "sept2021permTable.csv")


################################################################################
## SIMPER 

# Making tables of the SIMPER outputs

# Note that I also want the comparisons to be listed in alphabetical order, so that complicates things
# I am manually choosing the order they should be passed into the function. Then rbinding all data together
# Make adjustments to the simper table so I can export it as a csv
# I have to pass in the Summary object, so species are listed in the correct order
# The full simper table (fullObject) has species listed alphabetically. But I need this to get the "Overall average dissimilarity"
# Also pass in the names of groups being compared. First one is "avA" (average abundance from group A) and second is "avB"". It
# is easier to just rearrange these manually in Excel so group names can be listed alphabetically in the final table
simDfMaker = function(summaryObject, fullObject, comparisonNames){
  
  summaryObject %>%
    # Make species names the first column (right now they are rownames)
    tibble::rownames_to_column(var = "Taxa") %>%
    # Turn cumulative contributions into just contributions for each row. Change location of column.
    mutate(cont = diff(c(0, cumsum)), .before = cumsum) %>% 
    # Add new column to show which sites were compared and their order (will be manually moved in Google Doc table)
    mutate(comp = comparisonNames) %>% 
    # Add the overall average dissimilarity (will be manually moved in Google Doc table)
    mutate(overall = fullObject$overall) %>% 
    # Multiply certain columns by 100 to convert to percentages
    mutate_at(vars(c(average, sd, cumsum, cont, overall)), .funs = funs(.*100)) %>% 
    # Round most columns to 2 decimal places
    mutate(across(c(average, sd, ratio, ava, avb, cumsum, cont, overall), round, 2)) %>% 
    # Round p-value to 4 decimal places. Might change this later
    mutate(across(c(p), round, 4)) %>% 
    # Only get the top 5 species
    slice(1:5) 
  
}

# Run the function and pass in the appropriate data
# Remember the order is important because in the end I want the comparisons to be alphabetical

# Regional comparisons

regSimperTable = rbind(
  simDfMaker(summary(simRegion)$Maritimes_Gulf, simRegion$Maritimes_Gulf, "Maritimes_Gulf"),
  simDfMaker(summary(simRegion)$Newfoundland_Gulf, simRegion$Newfoundland_Gulf, "Newfoundland_Gulf"),
  simDfMaker(summary(simRegion)$Pacific_Gulf, simRegion$Pacific_Gulf, "Pacific_Gulf"),
  simDfMaker(summary(simRegion)$Maritimes_Newfoundland, simRegion$Maritimes_Newfoundland, "Maritimes_Newfoundland"),
  simDfMaker(summary(simRegion)$Maritimes_Pacific, simRegion$Maritimes_Pacific, "Maritimes_Pacific"),
  simDfMaker(summary(simRegion)$Newfoundland_Pacific, simRegion$Newfoundland_Pacific, "Newfoundland_Pacific")
)


# Maritimes bay comparisons
marBaySimperTable = rbind(      
  simDfMaker(summary(simMar)$`Country Harbour_Argyle`, simMar$`Country Harbour_Argyle`, "Country Harbour_Argyle"),
  simDfMaker(summary(simMar)$`Sober Island_Argyle`, simMar$`Sober Island_Argyle`, "Sober Island_Argyle"), 
  simDfMaker(summary(simMar)$`Whitehead_Argyle`, simMar$`Whitehead_Argyle`, "Whitehead_Argyle"),
  simDfMaker(summary(simMar)$`Country Harbour_Sober Island`, simMar$`Country Harbour_Sober Island`, "Country Harbour_Sober Island"),
  simDfMaker(summary(simMar)$`Country Harbour_Whitehead`, simMar$`Country Harbour_Whitehead`, "Country Harbour_Whitehead"),
  simDfMaker(summary(simMar)$`Whitehead_Sober Island`, simMar$`Whitehead_Sober Island`, "Whitehead_Sober Island")
)


# Gulf bay comparisons 
gulfBaySimperTable = rbind(
  #simDfMaker(summary(simGulf)$`Cocagne_Malpeque`, simGulf$`Cocagne_Malpeque`, "Cocagne_Malpeque"),
  simDfMaker(summary(simGulf)$`Cocagne_St. Peters`, simGulf$`Cocagne_St. Peters`, "Cocagne_St. Peters") 
  #simDfMaker(summary(simGulf)$`Malpeque_St. Peters`, simGulf$`Malpeque_St. Peters`, "Malpeque_St. Peters")
)


# Pacific field season comparisons
pacFieldSimperTable = rbind(
  simDfMaker(summary(simPac)$`August 2020_March 2021`, simPac$`August 2020_March 2021`, "August 2020_March 2021"),
  simDfMaker(summary(simPac)$`August 2020_June 2021`, simPac$`August 2020_June 2021`, "August 2020_June 2021"), 
  simDfMaker(summary(simPac)$`September 2021_August 2020`, simPac$`September 2021_August 2020`, "September 2021_August 2020"),
  simDfMaker(summary(simPac)$`March 2021_June 2021`, simPac$`March 2021_June 2021`, "March 2021_June 2021"), 
  simDfMaker(summary(simPac)$`September 2021_March 2021`, simPac$`September 2021_March 2021`, "September 2021_March 2021"),
  simDfMaker(summary(simPac)$`September 2021_June 2021`, simPac$`September 2021_June 2021`, "September 2021_June 2021")
)

write.csv(regSimperTable, "regSimperTable.csv")
write.csv(marBaySimperTable, "marSimperTable.csv")
write.csv(gulfBaySimperTable, "gulfSimperTable.csv")
write.csv(pacFieldSimperTable, "pacSimperTable.csv")



### Bay comparisons

# Argyle Station
argyleSimperTableTide = 
  simDfMaker(summary(simArgTide)$Low_High, simArgTide$Low_High, "Low_High"
)

# Argyle Tide
argyleSimperTableStn = rbind(
  #simDfMaker(summary(simArgStn)$Central_North, simArgStn$Central_North, "Central_North"),
  simDfMaker(summary(simArgStn)$Central_South, simArgStn$Central_South, "Central_South")
  #simDfMaker(summary(simArgStn)$South_North, simArgStn$South_North, "South_North")
  )

# St Peters
stPSimperTableStn = rbind(
  #simDfMaker(summary(simStPStn)$Mid_Inner, simStPStn$Mid_Inner, "Mid_Inner"),
  #simDfMaker(summary(simStPStn)$Outer_Inner, simStPStn$Outer_Inner, "Outer_Inner"),
  simDfMaker(summary(simStPStn)$Outer_Mid, simStPStn$Outer_Mid, "Outer_Mid")
)

# Southeast Arm Sept 2021
nl21SimperTableStn = rbind(
  simDfMaker(summary(simnl21Stn)$`Outer_Mid-B`, simnl21Stn$`Outer_Mid-B`, "Outer_Mid-B")
)


# Pacific Aug 2020
aug2020SimperTableStn = rbind(
  simDfMaker(summary(simAug2020Stn)$Mid_Inner, simAug2020Stn$Mid_Inner, "Mid_Inner"),
  simDfMaker(summary(simAug2020Stn)$Outer_Inner, simAug2020Stn$Outer_Inner, "Outer_Inner"),
  simDfMaker(summary(simAug2020Stn)$Outer_Mid, simAug2020Stn$Outer_Mid, "Outer_Mid") 
)

# Pacific June 2021
jun2021SimperTableStn = rbind(
  simDfMaker(summary(simJun2021Stn)$Inner_Mid, simAug2020Stn$Mid_Inner, "Inner_Mid")
  #simDfMaker(summary(simJun2021Stn)$Outer_Inner, simAug2020Stn$Outer_Inner, "Outer_Inner"),
  #simDfMaker(summary(simJun2021Stn)$Outer_Mid, simAug2020Stn$Outer_Mid, "Outer_Mid") 
)


write.csv(argyleSimperTableTide, "argyleSimperTableTide.csv")
write.csv(argyleSimperTableStn, "argyleSimperTableStn.csv")
write.csv(stPSimperTableStn, "stPSimperTableStn.csv")
write.csv(nl21SimperTableStn, "nl21SimperTableStn.csv")
write.csv(aug2020SimperTableStn, "aug2020SimperTableStn.csv")
write.csv(jun2021SimperTableStn, "jun2021SimperTableStn.csv")
# write.csv(sept2021SimperTableStn, "sept2021SimperTableStn.csv")



