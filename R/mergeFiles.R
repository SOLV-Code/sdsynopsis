#' mergeFiles
#'
#' Function to merge/match records across databases. Generates various outputs
#' (rosetta files, mismatch tracking). Target folders are generated if they don't exist
#' @param nuseds.rds.file path to the nuSEDS input file (in RDS format)
#' @param nuseds.pop.info path to the pop/site info file from nuSEDS (in csv format)
#' @param epad.rds.file path to the EPAD input file (in RDS format)
#' @param out.folder path for storing output (CU summary stored there)
#' @param tracking.folder path for storing tracking files (site mismatches etc. stored there)
#' @param lookup.folder path for storing generated lookup files ("rosetta" files stored there)
#' @keywords record matching/merging
#' @export
#' @examples
#' \dontrun{mergeFiles(nuseds.rds.file = "DATA/RDSFiles/nuSEDS.RDS",
#'             nuseds.pop.info ="DATA/LookupFiles/aopen-data-portaljanuary-2019conservation_unit_system_sites.csv",
#'            epad.rds.file =  "DATA/RDSFiles/EPAD.RDS")}


mergeFiles <- function(nuseds.rds.file,nuseds.pop.info,epad.rds.file,
                       out.folder = "OUTPUT",
                       tracking.folder = "DATA/TrackingFiles",
                       lookup.folder = "DATA/LookupFiles"){



if(any(is.null(nuseds.rds.file),is.null(nuseds.pop.info),is.null(epad.rds.file))){warning("1 or more inputs missing");stop()}

if(!dir.exists(out.folder)){dir.create(out.folder,recursive = TRUE)}
if(!dir.exists(tracking.folder)){dir.create(tracking.folder,recursive = TRUE)}
if(!dir.exists(lookup.folder)){dir.create(lookup.folder,recursive = TRUE)}


# read in files
nuseds.db <- readRDS(nuseds.rds.file)
nuseds.info <- read.csv(nuseds.pop.info,stringsAsFactors = FALSE, header = TRUE)
epad.db <- readRDS(epad.rds.file)


# NEEDS MORE FILE CHECKING: Required columns etc


# generate the rosetta files
# build everything around the CU list and CU_ID in the current nuSEDS info file
# fix column headers along the way

rosetta.pop <- nuseds.info %>% select(SPECIES_QUALIFIED,POP_ID,SYSTEM_SITE, FULL_CU_IN, CU_NAME,CU_ACRO,
                       	GFE_TYPE, IS_INDICATOR, ISENH,
                       FAZ_ACRO,	MAZ_ACRO,	JAZ_ACRO,
                       OL_GRP_NM,	OL_GRP_N,	AREA) %>%
                   rename(CU_ID = FULL_CU_IN,Site_Type = GFE_TYPE,Species = SPECIES_QUALIFIED )  %>%
                        # remove up to 2 leading zeroes to get nuseds ID
                    mutate(CU_ID_Short = gsub("-0","-",gsub("-0","-",CU_ID) ))  %>%
                    mutate(CU_ID_Min = gsub("-","",CU_ID_Short))  %>%
                    mutate(SiteLabel = paste(CU_ID_Short, tolower(SYSTEM_SITE),sep="_"))  %>%
                    mutate(SiteLabel= gsub("-","",SiteLabel))  %>%       #str_replace was skipping rows?
                    mutate(SiteLabel= gsub(" ","",SiteLabel))  %>%
                    mutate(SiteLabel= gsub("river","r",SiteLabel))  %>%
                    mutate(SiteLabel= gsub("creek","cr",SiteLabel))  %>%
                    mutate(SiteLabel= gsub("upper","up",SiteLabel))


nuseds.db <- nuseds.db %>%
                rename(CU_ID = FULL_CU_IN,Site_Type = GFE_TYPE,Species = SPECIES_QUALIFIED )  %>%
                # remove up to 2 leading zeroes to get nuseds ID
                mutate(CU_ID_Short = gsub("-0","-",gsub("-0","-",CU_ID) ))  %>%
                mutate(CU_ID_Min = gsub("-","",CU_ID_Short))  %>%
                mutate(SiteLabel = paste(CU_ID_Short, tolower(SYSTEM_SITE),sep="_"))  %>%
                mutate(SiteLabel= gsub("-","",SiteLabel))  %>%       #str_replace was skipping rows?
                mutate(SiteLabel= gsub(" ","",SiteLabel))  %>%
                mutate(SiteLabel= gsub("river","r",SiteLabel))  %>%
                mutate(SiteLabel= gsub("creek","cr",SiteLabel))  %>%
                mutate(SiteLabel= gsub("upper","up",SiteLabel))

saveRDS(nuseds.db, gsub(".RDS","_mod.RDS",nuseds.rds.file))


epad.db <- epad.db %>%  mutate(SiteLabel = paste(CU_INDEX, tolower(RETURN_SITE_NAME),sep="_"))  %>%
                        mutate(SiteLabel= gsub("-","",SiteLabel))  %>%
                        mutate(SiteLabel= gsub(" ","",SiteLabel))  %>%
                        mutate(SiteLabel= gsub("river","r",SiteLabel))  %>%
                        mutate(SiteLabel= gsub("creek","cr",SiteLabel))  %>%
                        mutate(SiteLabel= gsub("upper","up",SiteLabel))
                        rename(SYSTEM_SITE_EPAD = RETURN_SITE_NAME,
                               CU_ID_EPAD = CU_INDEX,
                               CU_NAME_EPAD = CU_NAME)  %>%
                        mutate(CU_ID_Short = gsub("-0","-",gsub("-0","-",CU_ID_EPAD) ))  %>%
                        mutate(CU_ID_Min = gsub("-","",CU_ID_Short))  %>%

saveRDS(epad.db, gsub(".RDS","_mod.RDS",epad.rds.file))




# add in CU info
rosetta.cu <- rosetta.pop %>% select(Species,CU_ID_Min,CU_ID_Short,CU_ID, CU_NAME,CU_ACRO,FAZ_ACRO,MAZ_ACRO,JAZ_ACRO,
                                      AREA) %>% distinct()


write.csv(rosetta.cu,paste0(lookup.folder,"/Generated_RosettaFile_CU.csv"),row.names=FALSE)

epad.pop <- epad.db %>%
            select(SYSTEM_SITE_EPAD,CU_ID_EPAD,	CU_NAME_EPAD, RUN_NAME, SiteLabel) %>%
            distinct()


rosetta.pop <- left_join(rosetta.pop, epad.pop,by="SiteLabel")


write.csv(rosetta.pop,paste0(lookup.folder,"/Generated_RosettaFile_Pop.csv"),row.names=FALSE)

# unmatched sitelabels
epad.unmatched.sitelabels <- epad.pop$SiteLabel[!(epad.pop$SiteLabel %in% rosetta.pop$SiteLabel)]

write.csv(epad.pop[!(epad.pop$SiteLabel %in% rosetta.pop$SiteLabel),],
          paste0(tracking.folder,"/EPAD_Unmatched_SiteLabels.csv"),row.names=FALSE)


write.csv(epad.db[epad.db$SiteLabel %in% epad.unmatched.sitelabels,],
          paste0(tracking.folder,"/EPAD_Unmatched_Records.csv"),row.names=FALSE)


# SiteLabel masterlist and cross-check
sitelabel.master <- data.frame(SiteLabel = sort(unique(c(rosetta.pop$SiteLabel,epad.pop$SiteLabel))),
                               nuSEDS = NA, EPAD = NA, MRP = NA) %>%
                    tidyr::separate(SiteLabel,c("CU","SiteName"),sep="_",remove=FALSE) %>%
                            # also works, but would need a second step to label the columns
                            #cbind(str_split_fixed(sitelabel.master$SiteLabel,pattern="_",n=2))
                    mutate(nuSEDS = SiteLabel %in% rosetta.pop$SiteLabel,    # NOTE: Rosetta built from nuseds info, for now
                           EPAD = SiteLabel %in% epad.pop$SiteLabel)

#head(sitelabel.master)

write.csv(sitelabel.master, paste0(tracking.folder,"/SiteLabel_MasterCheck.csv"),row.names=FALSE)


##

cu.summary <- sitelabel.master %>% group_by(CU) %>%
                summarise(total.sites = n(),
                    nuseds.sites = sum(nuSEDS),
                    epad.sites = sum(EPAD),
                    nuseds.only = sum(nuSEDS & !(EPAD)),
                    epad.only = sum(!(nuSEDS) & EPAD)
                    )   %>%
                rename(CU_ID_Min = CU)  %>%
                left_join( rosetta.cu,by="CU_ID_Min") # add in CU info



write.csv(cu.summary, paste0(out.folder,"/Summary_CU.csv"),row.names=FALSE)

return("Merging Complete")

} # end mergeFiles()









