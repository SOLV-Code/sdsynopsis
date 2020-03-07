#' summarizeData
#'
#' Function to generate various data summaries (number of records by DB, input for heatmap of tag recoveries)
#' Target folders are generated if they don't exist. Uses files generated with matchRecords().
#' @param nuseds.rds.file path to the modified nuSEDS input file (in RDS format). 
#' @param nuseds.pop.info path to the pop/site info file from nuSEDS (in csv format)
#' @param epad.rds.file path to the modified EPAD input file (in RDS format)
#' @param mrp.rds.file path to the modified MRP summary input file (in RDS format)
#' @param epad.rds.details.file path to the modified EPAD details input file (in RDS format)
#' @param out.folder path for storing output (CU summary stored there)
#' @param tracking.folder path for storing tracking files (site mismatches etc. stored there)
#' @keywords record summaries
#' @export
#' @examples
#' \dontrun{summarizeData( nuseds.rds.file = "DATA/RDSFiles/nuSEDS_mod.RDS",
#'         nuseds.pop.info = "DATA/LookupFiles/aopen-data-portaljanuary-2019conservation_unit_system_sites.csv",
#'         epad.rds.file =  "DATA/RDSFiles/EPAD_mod.RDS",
#'         mrp.rds.file =  "DATA/RDSFiles/MRP_mod.RDS",
#'         mrp.rds.details.file =  "DATA/RDSFiles/MRP_Details_mod.RDS",
#'         rosetta.cu = read.csv("DATA/LookupFiles/Generated_RosettaFile_CU.csv",stringsAsFactors = FALSE),
#'         rosetta.pop = read.csv("DATA/LookupFiles/Generated_RosettaFile_Pop.csv",stringsAsFactors = FALSE),
#'         cu.summary = read.csv("OUTPUT/Summary_CU.csv", stringsAsFactors =  FALSE), 
#'         out.folder = "OUTPUT/Summaries",
#'         tracking.folder = "DATA/TrackingFiles")
#'		}



summarizeData <- function(
         nuseds.rds.file = "DATA/RDSFiles/nuSEDS_mod.RDS",
         nuseds.pop.info = "DATA/LookupFiles/aopen-data-portaljanuary-2019conservation_unit_system_sites.csv",
         epad.rds.file =  "DATA/RDSFiles/EPAD_mod.RDS",
         mrp.rds.file =  "DATA/RDSFiles/MRP_mod.RDS",
         mrp.rds.details.file =  "DATA/RDSFiles/MRP_Details_mod.RDS",
         rosetta.cu = read.csv("DATA/LookupFiles/Generated_RosettaFile_CU.csv",stringsAsFactors = FALSE),
         rosetta.pop = read.csv("DATA/LookupFiles/Generated_RosettaFile_Pop.csv",stringsAsFactors = FALSE),
         cu.summary = read.csv("OUTPUT/Summary_CU.csv", stringsAsFactors =  FALSE), 
         out.folder = "OUTPUT/Summaries",
         tracking.folder = "DATA/TrackingFiles"){


# packages set up

library(gridExtra)
library(tidyverse)


# check if the target folder exists
if(!dir.exists(out.folder)){dir.create(out.folder)}
if(!dir.exists(tracking.folder)){dir.create(tracking.folder)}


# read in data
epad.df <-  readRDS(epad.rds.file)
nuseds.df <-  readRDS(nuseds.rds.file)
mrp.df <- readRDS(mrp.rds.file)
mrp.details.df <- readRDS(mrp.rds.details.file)


###################################################################################
# Summary 1:  Number of Records by DB

records.summary <- data.frame(DB= c("nuseds","epad","mrp"),
                              records = c(dim(nuseds.df)[1],dim(epad.df)[1],sum(mrp.df$NumRecords_Total)),
                              sites = c(length(unique(nuseds.df$POP_ID)) ,
                                        length(unique(epad.df$SiteLabel)),
                                        length(unique(mrp.df$EPAD_RELEASE_SITE))),
                              first.year =  c(min(nuseds.df$ANALYSIS_YR ) ,
                                              min(epad.df$RECOVERY_YEAR ),
                                              min(mrp.df$RUN_YEAR)),
                              last.year = c(max(nuseds.df$ANALYSIS_YR ) ,
                                            max(epad.df$RECOVERY_YEAR ),
                                            max(mrp.df$RUN_YEAR))
)
#print(records.summary )
write.csv(records.summary, paste(out.folder,"Summary_Records.csv",sep="/"),row.names=FALSE)




###############################################
# HEATMAP 1:  Years vs. Areas/FisheryGroups

mrp.heatmap1.source <- mrp.details.df %>% 
  mutate(AREA_FISHERY = paste(AREA_SHORT,FISHERY_GROUP,sep="_")) %>%
  group_by(CU_ID_EPAD,RUN_YEAR,AREA_FISHERY) %>%
  summarize(TotalRecoveries = sum(Recoveries),
            TotalEstimated = sum(EstimatedNumber)) 


mrp.heatmap1.rec.df <- mrp.heatmap1.source %>% select(-TotalEstimated) %>%
  pivot_wider(names_from = AREA_FISHERY, values_from = TotalRecoveries, values_fill = list(TotalRecoveries = 0))

mrp.heatmap1.est.df <- mrp.heatmap1.source %>% select(-TotalRecoveries) %>%
  pivot_wider(names_from = AREA_FISHERY, values_from = TotalEstimated, values_fill = list(TotalEstimated = 0))


#print(head(mrp.heatmap1.rec.df))

write.csv(mrp.heatmap1.rec.df, paste(out.folder,"Heatmap1_Recoveries_Inputs.csv",sep="/"),row.names = FALSE)
write.csv(mrp.heatmap1.est.df, paste(out.folder,"Heatmap1_Estimated_Inputs.csv",sep="/"),row.names = FALSE)







} # end summarizeData()
