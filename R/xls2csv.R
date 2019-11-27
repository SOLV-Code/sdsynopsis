#' xls2csv
#'
#' Function to convert xlsx files with salmon data to csv files. This function uses the {readxl} package which is part of the {tidyverse} package.
#' @param file.in path to the input file
#' @param file.out path to the output file
#' @param db.type one of "nuSEDS","EPAD", or "MRP"
#' @param sheet.read number specifying the tab to read in. Default is 1.
#' @param na.strings vector with strings to be interpreted as NA. Default is c("","NA")
#' @keywords file conversion, xls, csv
#' @export
#' @examples
#' \dontrun{xls2csv("test.xls","test.csv",db.type="nuSEDS")}

xls2csv <- function(file.in, file.out,db.type="nuSEDS",sheet.read=1,na.strings = c("","NA")){

# check inputs
if(is.null(file.in)){warning("need an input file path");stop()}
if(is.null(file.out)){warning("need an output file path");stop()}
if(!(tolower(db.type) %in% c("nuseds","epad","mrp") ) ){warning(paste(db.type,"is not a valid database type"));stop()}

file.read <- readxl(file.in, sheet = sheet.read,na=na.strings)


write.csv(file.read,file.out, row.names=FALSE)




}









