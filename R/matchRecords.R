#' matchRecords
#' 
#' Function to match records across databases. Generates various outputs
#' (rosetta files, mismatch tracking). Target folders are generated if they don't exist
#' @param nuseds.rds.file path to the nuSEDS input file (in RDS format)
#' @param nuseds.pop.info path to the pop/site info file from nuSEDS (in csv format)
#' @param epad.rds.file path to the EPAD input file (in RDS format)
#' @param mrp.rds.file path to the MRP summary input file (in RDS format)
#' @param epad.rds.details.file path to the EPAD details input file (in RDS format)
#' @param out.folder path for storing output (CU summary stored there)
#' @param tracking.folder path for storing tracking files (site mismatches etc. stored there)
#' @param lookup.folder path for storing generated lookup files ("rosetta" files stored there)
#' @param processed.folder folder path for storing modified csv files (MRP summaries with CU match stored there)
#' @keywords record matching/merging
#' @export
#' @examples
#' \dontrun{matchRecords(nuseds.rds.file = "DATA/RDSFiles/nuSEDS.RDS",
#'             nuseds.pop.info ="DATA/LookupFiles/aopen-data-portaljanuary-2019conservation_unit_system_sites.csv",
#'            epad.rds.file =  "DATA/RDSFiles/EPAD.RDS",
#'            mrp.rds.file = "DATA/RDSFiles/MRP.RDS" )}


matchRecords <- function(nuseds.rds.file,nuseds.pop.info,
						epad.rds.file,
						mrp.rds.file,
						mrp.rds.details.file,
                       out.folder = "OUTPUT",
                       tracking.folder = "DATA/TrackingFiles",
                       lookup.folder = "DATA/LookupFiles",
					   processed.folder = "DATA/LargeFiles_Processed"){


warning("Need to build in GFE_ID based match for EPAD")


if(any(is.null(nuseds.rds.file),is.null(nuseds.pop.info),
       is.null(epad.rds.file),is.null(mrp.rds.file))){
  warning("1 or more inputs missing");stop()
  }

if(!dir.exists(out.folder)){dir.create(out.folder,recursive = TRUE)}
if(!dir.exists(tracking.folder)){dir.create(tracking.folder,recursive = TRUE)}
if(!dir.exists(lookup.folder)){dir.create(lookup.folder,recursive = TRUE)}

print("starting file read")

# read in files
nuseds.db <- readRDS(nuseds.rds.file)
nuseds.info <- read.csv(nuseds.pop.info,stringsAsFactors = FALSE, header = TRUE)
epad.db <- readRDS(epad.rds.file)
mrp.db <- readRDS(mrp.rds.file)
mrp.details.db <- readRDS(mrp.rds.details.file)

# NEEDS MORE FILE CHECKING: Required columns etc






# generate the rosetta files
# build everything around the CU list and CU_ID in the current nuSEDS info file
# fix column headers along the way

print("starting file reorg")

rosetta.pop <- nuseds.info %>% select(SPECIES_QUALIFIED,POP_ID,GFE_ID,SYSTEM_SITE, FULL_CU_IN, CU_NAME,CU_ACRO,
                       	GFE_TYPE, IS_INDICATOR, ISENH,
                       FAZ_ACRO,	MAZ_ACRO,	JAZ_ACRO,
                       OL_GRP_NM,	OL_GRP_N,	AREA) %>%
                   rename(CU_ID = FULL_CU_IN,Site_Type = GFE_TYPE,Species = SPECIES_QUALIFIED )  %>%
                        # remove up to 2 leading zeroes to get nuseds ID
                    mutate(CU_ID_Short = gsub("-0","-",gsub("-0","-",CU_ID) ))  %>%
                    mutate(CU_ID_Min = gsub("-","",CU_ID_Short))  %>%
                    mutate(SiteLabel = paste(CU_ID_Short, gsub("[^[:alnum:]]","",tolower(SYSTEM_SITE)),sep="_"))  %>%
                    # the lines below are earlier attempts, keep as ref for now
                    #mutate(SiteLabel= gsub("[^[:alnum:]]","",SiteLabel))  %>%  # this messes up the separate() below
                    #mutate(SiteLabel= gsub("-","",SiteLabel))  %>%       #str_replace was skipping rows?
                    #mutate(SiteLabel= gsub(" ","",SiteLabel))  %>%
                    mutate(SiteLabel= gsub("river","r",SiteLabel))  %>%
                    mutate(SiteLabel= gsub("creek","cr",SiteLabel))  %>%
                    mutate(SiteLabel= gsub("upper","up",SiteLabel))

print("rosetta POP --------------------")
print(head(rosetta.pop))

nuseds.db <- nuseds.db %>%
                rename(Species = SPECIES )  %>%
                left_join(select(rosetta.pop,POP_ID,GFE_ID, CU_ID_Min,SiteLabel),by="POP_ID") %>%
				mutate(EST_QUAL = recode(ESTIMATE_CLASSIFICATION, 
                         "TRUE ABUNDANCE (TYPE-1)"="H",
                         "TRUE ABUNDANCE (TYPE-2)" ="H",
						 "RELATIVE ABUNDANCE (TYPE-3)"  ="M",
						 "RELATIVE ABUNDANCE (TYPE-4)" ="M",
						 "RELATIVE ABUNDANCE (TYPE-5)" ="L",
						 "PRESENCE-ABSENCE (TYPE-6)" ="L",						 
						 "NO SURVEY THIS YEAR"  = "UNK",	
						 "RELATIVE: CONSTANT MULTI-YEAR METHODS"  = "UNK",
						 "RELATIVE: VARYING MULTI-YEAR METHODS"  = "UNK",
						 "UNKNOWN"   = "UNK" 
						 )
						 )

nuseds.db$EST_QUAL[nuseds.db$EST_QUAL == ""] <- "UNK"
 


epad.db <- epad.db %>%  mutate(SiteLabel = paste(CU_INDEX, gsub("[^[:alnum:]]","",tolower(RETURN_SITE_NAME)),sep="_"))  %>%
                        # the lines below are earlier attempts, keep as ref for now
                        #mutate(SiteLabel= gsub("[^[:alnum:]]","",SiteLabel))  %>%  # this should take of all weird symb
                        #mutate(SiteLabel= gsub("-","",SiteLabel))  %>%
                        #mutate(SiteLabel= gsub(" ","",SiteLabel))  %>%
                        mutate(SiteLabel= gsub("river","r",SiteLabel))  %>%
                        mutate(SiteLabel= gsub("creek","cr",SiteLabel))  %>%
                        mutate(SiteLabel= gsub("upper","up",SiteLabel)) %>%
                        rename(SYSTEM_SITE_EPAD = RETURN_SITE_NAME,
                               CU_ID_EPAD = CU_INDEX,
                               CU_NAME_EPAD = CU_NAME)  %>%
                        mutate(CU_ID_Short = gsub("-0","-",gsub("-0","-",CU_ID_EPAD) ))  %>%
                        mutate(CU_ID_Min = gsub("-","",CU_ID_Short))%>%
                        mutate(EPAD2MRP = gsub("[^[:alnum:]]","",paste(SPEC_NAME ,SYSTEM_SITE_EPAD,sep="_"))) 
						
						


epad2mrp.lookup <- unique(select(epad.db,EPAD2MRP,CU_ID_EPAD,CU_NAME_EPAD,CU_ID_Short,CU_ID_Min))
epad2mrp.multiples <- epad2mrp.lookup %>% group_by(EPAD2MRP) %>% filter(n()>1) %>% arrange(EPAD2MRP)



# add in CU info
rosetta.cu <- rosetta.pop %>% select(Species,CU_ID_Min,CU_ID_Short,CU_ID, CU_NAME,CU_ACRO,AREA) %>% distinct()
# FAZ_ACRO,MAZ_ACRO,JAZ_ACRO -> results in replicates because some CUs span multiple FAZ


epad.pop <- epad.db %>%
            select(SYSTEM_SITE_EPAD,CU_ID_EPAD,	CU_NAME_EPAD, RUN_NAME, SiteLabel) %>%
            distinct()


rosetta.pop <- left_join(rosetta.pop, epad.pop,by="SiteLabel")

print("rosetta CU --------------------")
print(head(rosetta.cu))


mrp.db <- mrp.db %>% mutate(EPAD2MRP = gsub("[^[:alnum:]]","",
                              paste(SPECIES_NAME,EPAD_RELEASE_SITE,sep="_"))) %>%
              left_join(epad2mrp.lookup,by="EPAD2MRP")


mrp.details.db <- mrp.details.db %>% mutate(EPAD2MRP = gsub("[^[:alnum:]]","",
                              paste(SPECIES_NAME,EPAD_RELEASE_SITE,sep="_"))) %>%
              left_join(epad2mrp.lookup,by="EPAD2MRP")


print("starting summaries")

# generate a summary
records.summary <- data.frame(DB= c("nuseds","epad","mrp"),
                              records = c(dim(nuseds.db)[1],dim(epad.db)[1],sum(mrp.db$NumRecords_Total)),
                              sites = c(length(unique(nuseds.db$POP_ID)) ,
                                        length(unique(epad.db$SiteLabel)),
                                        length(unique(mrp.db$EPAD_RELEASE_SITE))),
                              first.year =  c(min(nuseds.db$ANALYSIS_YR ) ,
                                              min(epad.db$RECOVERY_YEAR ),
                                              min(mrp.db$RUN_YEAR)),
                              last.year = c(max(nuseds.db$ANALYSIS_YR ) ,
                                            max(epad.db$RECOVERY_YEAR ),
                                            max(mrp.db$RUN_YEAR))
                  )



###################################################

# SiteLabel masterlist and cross-check
sitelabel.master <- data.frame(SiteLabel = sort(unique(c(rosetta.pop$SiteLabel,epad.pop$SiteLabel))),
                               nuSEDS = NA, EPAD = NA, MRP = NA) %>%
  tidyr::separate(SiteLabel,c("CU","SiteName"),sep="_",remove=FALSE) %>%
  # also works, but would need a second step to label the columns
  #cbind(str_split_fixed(sitelabel.master$SiteLabel,pattern="_",n=2))
  mutate(nuSEDS = SiteLabel %in% rosetta.pop$SiteLabel,    # NOTE: Rosetta built from nuseds info, for now
         EPAD = SiteLabel %in% epad.pop$SiteLabel)

# unmatched sitelabels in EPAD
epad.unmatched.sitelabels <- epad.pop$SiteLabel[!(epad.pop$SiteLabel %in% rosetta.pop$SiteLabel)]

# unmatched records in MRP
mrp.unmatched.records <- mrp.db %>% dplyr::filter(is.na(CU_ID_EPAD))



cu.summary <- sitelabel.master %>% group_by(CU) %>%
  summarise(total.sites = n(),
            nuseds.sites = sum(nuSEDS),
            epad.sites = sum(EPAD),
            nuseds.only = sum(nuSEDS & !(EPAD)),
            epad.only = sum(!(nuSEDS) & EPAD)
  )   %>%
  mutate(CU_ID_Min = gsub("-","",CU))  %>%
  select(-CU) %>%
  left_join( rosetta.cu,by="CU_ID_Min") # add in CU info

print(head(cu.summary))

###############################################################################
# OUTPUT

print("starting outputs")


# modified DB files
saveRDS(nuseds.db, gsub("\\.RDS","_mod.RDS",nuseds.rds.file))
saveRDS(epad.db, gsub("\\.RDS","_mod.RDS",epad.rds.file))
saveRDS(mrp.db, gsub("\\.RDS","_mod.RDS",mrp.rds.file))
saveRDS(mrp.details.db, gsub("\\.RDS","_mod.RDS",mrp.rds.details.file))

# modified csv files

write.csv(mrp.db, paste(processed.folder,"MRP_Summary_MOD.csv",sep="/"),row.names =FALSE)
write.csv(mrp.details.db, paste(processed.folder,"MRP_Details_MOD.csv",sep="/"),row.names =FALSE)

# Main summary file
write.csv(cu.summary, paste0(out.folder,"/Summary_CU.csv"),row.names=FALSE)

# CU info and Pop info with label matches across DB
write.csv(rosetta.cu,paste0(lookup.folder,"/Generated_RosettaFile_CU.csv"),row.names=FALSE)
write.csv(rosetta.pop,paste0(lookup.folder,"/Generated_RosettaFile_Pop.csv"),row.names=FALSE)
write.csv(epad2mrp.lookup,paste0(lookup.folder,"/Generated_EPAD2MRP_RosettaFile.csv"),row.names=FALSE)

# short summary of num records by DB
write.csv(records.summary, "OUTPUT/Summary_Records.csv",row.names=FALSE)



# diagnostic / tracking files

write.csv(epad2mrp.multiples, paste0(tracking.folder,"/EPAD2MRP_MISMATCHES.csv"),row.names=FALSE)

write.csv(mrp.unmatched.records, paste0(tracking.folder,"/MRP_UnmatchedRecords.csv"),row.names=FALSE)



write.csv(epad.pop[!(epad.pop$SiteLabel %in% rosetta.pop$SiteLabel),],
          paste0(tracking.folder,"/EPAD_Unmatched_SiteLabels.csv"),row.names=FALSE)

write.csv(epad.db[epad.db$SiteLabel %in% epad.unmatched.sitelabels,],
          paste0(tracking.folder,"/EPAD_Unmatched_Records.csv"),row.names=FALSE)

write.csv(sitelabel.master, paste0(tracking.folder,"/SiteLabel_MasterCheck.csv"),row.names=FALSE)
#


return("Matching Complete")

} # end matchRecords()









