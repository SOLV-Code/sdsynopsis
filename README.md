# sdsynopsis-Package
Package with functions for extracting, merging, and summarizing salmon data from various databases

**THIS IS AN EARLY PROTOTYPE**

## Overview

Focus on 3 database sources:

* nuSEDS: salmon escapement by site
* EPAD: hatchery broodstock takes and releases
* MRP: mark-recapture data


## Functions

Function | Description
-- | --
*xls2csv()* | read in an excel file using the *readxl* package, and produce a cleaned csv. Options for "nuSEDS", "EPAD", or "MRP"
*csv2rds()* | read in a csv file and produce a cleaned data frame stored in an R data file in *rds* format. Options for "nuSEDS", "EPAD", or "MRP"
*mergeData()* | combine records from the 3 databases into 2 separate R data objects (*.rds), one at the population/site level and one at the CU level
*summarizeData() | produce various summaries by population/site and by CU, and generate flat csv files as outputs.
*reportData() | for now, generate a series of pdf summary plots. Once the plot design is finalized, turn this into a full markdown report.
  