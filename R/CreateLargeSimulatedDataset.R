
#' This is my second function I add to the package package
#'
#'
#' @section ??
#'
#' @author
#' Roel Elbers
#'
#' @docType package
#' @name CreateLargeSimulatedDataset
#' @keywords ??
#' @import data.table

NULL


#' @param Inputfolder path where the csv files are located
#' @param Outputfolder path where the enlarged csv files need to be stored
#' @param N factor of enlargement
#' @param Identifier_name Column name(s) that needs to be extended to ensure unique id's
#' @param Delimiter Define a separator for csv files


#' @return Csv files N times the number of rows as the csv files in the input folder

#' @export





CreateLargeSimulatedDataset <- function(Inputfolder, Outputfolder, N, Identifier_name, Delimiter){
  files <- list.files(Inputfolder, pattern = paste0("*.", "csv"))
  dir.create(Outputfolder, showWarnings = FALSE)

  for(i in files){
    File<-fread(paste0(Inputfolder, '/', i), sep = Delimiter, stringsAsFactors = F)
    if(!any(colnames(File) == Identifier_name)){
      fwrite(File, file = paste0(Outputfolder, "/", i), sep = Delimiter, col.names = F, row.names = F, na = "", append = F)
        next
        }
          for(j in 1:N){
            File_temp <- copy(File)
            File_temp[,eval(Identifier_name) := paste0(get(Identifier_name), "_", j)]
            if(j == 1) fwrite(File_temp, file = paste0(Outputfolder, "/", i), sep = Delimiter, row.names = F, na = "", append = F)
            if(j > 1) fwrite(File_temp, file = paste0(Outputfolder, "/", i), sep = Delimiter, col.names = F, row.names = F, na = "", append = T)
        }

  }
}







