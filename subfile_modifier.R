###############################################################################################
#				Written by Andrew R Sommerlot              									  #
#					November 18th 2014       												  #
# This code is designed to modify all the .sub files in the TxtInout folder to 
#    be set to use the gage records from the station ID identical to the subbasin number      #
#   when used with the write mutiple guages from the cfsr, it set the swat project to use all #
#       downloaded data and allocate the weather data to the proper subbasin                  #
#     observed weather gages, one for each subbasin                                           #
#		working status: code runs as stand alone script when file paths are updated			  #
#																							  #
###############################################################################################

wd = 'C:/Users/Andy/Desktop/reshen/Scenarios/Default/TxtInOut'
outDir = 'C:/Users/Andy/Desktop/reshen/Scenarios/Default/TxtInOut/modfiles'

SWATsubGauge = function(wd, outDir = '', numPars = 5, centroid = FALSE){
  if(outDir == ''){
    outDir = wd
  } 
##set the working directory to the TxtInOut foler (may be an example for now)
setwd(wd)

#list all files with .sub extention
files = list.files(pattern = "sub")

#remove the output.sub file from the list
files = files[! files %in% "output.sub"]

files = files[! files %in% "TxtInOutsub.dat"]

files = files[! files %in% "sub.dat"]

#get total number of files to modify
subnum = length(files)

### test for FOR loop. If works, then set to loop through entire length of files
# get file name
for(i in 1:subnum){
	subfile = files[i]

	#read in file
	sub = readLines(subfile)

	#grab the first line
	headline = sub[1]

	#character gymnastics to get the subbasin number alone
	subvec = strsplit(headline,":")

	subvec2 = subvec[[1]][2]

	subvec3 = strsplit(subvec2," ")

	subnum.file = subvec3[[1]][2]

	#format lead space for gage name change
	subnum.format = formatC(subnum.file, digits = 0, width = 16, flag = ' ', format = 'f')
	if(centroid == FALSE){
	if(numPars == 1){
	##changes flags for just precip and temp to their respective subbasin number
	sub[7] = paste(subnum.format,"    | IRGAGE: precip gage data used in subbasin", sep = "") 
	} else if(numPars == 2){                        
		sub[7] = paste(subnum.format,"    | IRGAGE: precip gage data used in subbasin", sep = "") 
		sub[8] = paste(subnum.format,"    | ITGAGE: temp gage data used in subbasin", sep = "")
	} else if(numPars == 3){
		sub[7] = paste(subnum.format,"    | IRGAGE: precip gage data used in subbasin", sep = "") 
		sub[8] = paste(subnum.format,"    | ITGAGE: temp gage data used in subbasin", sep = "")
		sub[9] =  paste(subnum.format,"    | ISGAGE: solar radiation gage data used in subbasin", sep = "") 
	} else if(numPars == 4){
		sub[7] = paste(subnum.format,"    | IRGAGE: precip gage data used in subbasin", sep = "") 
		sub[8] = paste(subnum.format,"    | ITGAGE: temp gage data used in subbasin", sep = "")
		sub[9] =  paste(subnum.format,"    | ISGAGE: solar radiation gage data used in subbasin", sep = "") 
		sub[10] =  paste(subnum.format,"    | IHGAGE: relative humidity gage data used in subbasin", sep = "")
	} else if(numPars == 5){
		sub[7] = paste(subnum.format,"    | IRGAGE: precip gage data used in subbasin", sep = "") 
		sub[8] = paste(subnum.format,"    | ITGAGE: temp gage data used in subbasin", sep = "")
		sub[9] =  paste(subnum.format,"    | ISGAGE: solar radiation gage data used in subbasin", sep = "") 
		sub[10] =  paste(subnum.format,"    | IHGAGE: relative humidity gage data used in subbasin", sep = "")
		sub[11] =  paste(subnum.format,"    | IWGAGE: wind speed gage data used in subbasin", sep = "")  
	} else(stop(''))
	}
  
	if(centroid == TRUE){
    subnum.format = 1
		if(numPars == 1){
	##changes flags for just precip and temp to their respective subbasin number
	sub[7] = paste(subnum.format,"    | IRGAGE: precip gage data used in subbasin", sep = "") 
	} else if(numPars == 2){                        
		sub[7] = paste(subnum.format,"    | IRGAGE: precip gage data used in subbasin", sep = "") 
		sub[8] = paste(subnum.format,"    | ITGAGE: temp gage data used in subbasin", sep = "")
	} else if(numPars == 3){
		sub[7] = paste(subnum.format,"    | IRGAGE: precip gage data used in subbasin", sep = "") 
		sub[8] = paste(subnum.format,"    | ITGAGE: temp gage data used in subbasin", sep = "")
		sub[9] =  paste(subnum.format,"    | ISGAGE: solar radiation gage data used in subbasin", sep = "") 
	} else if(numPars == 4){
		sub[7] = paste(subnum.format,"    | IRGAGE: precip gage data used in subbasin", sep = "") 
		sub[8] = paste(subnum.format,"    | ITGAGE: temp gage data used in subbasin", sep = "")
		sub[9] =  paste(subnum.format,"    | ISGAGE: solar radiation gage data used in subbasin", sep = "") 
		sub[10] =  paste(subnum.format,"    | IHGAGE: relative humidity gage data used in subbasin", sep = "")
	} else if(numPars == 5){
		sub[7] = paste(subnum.format,"    | IRGAGE: precip gage data used in subbasin", sep = "") 
		sub[8] = paste(subnum.format,"    | ITGAGE: temp gage data used in subbasin", sep = "")
		sub[9] =  paste(subnum.format,"    | ISGAGE: solar radiation gage data used in subbasin", sep = "") 
		sub[10] =  paste(subnum.format,"    | IHGAGE: relative humidity gage data used in subbasin", sep = "")
		sub[11] =  paste(subnum.format,"    | IWGAGE: wind speed gage data used in subbasin", sep = "")  
	}
  }	

	#writes out the changes
	writeLines(sub, paste(outDir, subfile, sep = "/"))
}
	cat(paste('Files written to', outDir, sep = ' '))
}

