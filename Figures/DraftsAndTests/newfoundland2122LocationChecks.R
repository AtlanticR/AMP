# Read in coastline information
source("DataProcessing/studyAreaDataPrep.R")

# Read in Newfoundland metadata 
nlMetaRaw2122 = suppress_warnings(read_excel("../AMPDataFiles/FlowCamMetadata/AMP_Metadata_Plankton_2021_2022_NL_Jan252023.xlsx"))
nlMetaRaw20 = suppress_warnings(read_excel("../AMPDataFiles/FlowCamMetadata/AMP_Metadata_Plankton_2021_NL_Jan132022_OG.xlsx", sheet = "zoo"))




