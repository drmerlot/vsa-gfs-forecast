
###############################################################################################
# 					           *** Version 1.0.2 *** 										  #
###############################################################################################



################## Bias Correction with qMaps

biasCorrMon = function(obsFile, modFile, inpFile, startDate, endDate, startDatef, endDatef, fileType = 'pcp', tag = 'bias', method = 'QUANT', outDir = getwd()){
	

#REQUIREs qmap 

require(qmap)
require(SWATmodel)

###########################################################################################

setwd(outDir)


###########################################################################################
###########################################################################################
#read in modeled gfsdmo for tmp

if(fileType == 'tmp'){

###########################################################################################
###########################################################################################


###########################################################################################
###########################################################################################

#define widths for file of up to 50 subbasins
tmpw = c(7, rep(5, 30))

#read in all mod temp data 
mod = read.fwf(modFile, widths = tmpw, skip = 4)

#get rid of the NA columns 
mod = mod[,colSums(is.na(mod)) != nrow(mod)]

#get indicies 
modstart = which(mod[,1] == startDate)
modend = which(mod[,1] == endDate)

#separate tmax and tmin temp columns

#get total columns 
modcols = ncol(mod) 

#get tmpmax
modtmax = mod[, c(1, seq(2, modcols, 2))]

#get tmp min
modtmin = mod[, c(1, seq(3, modcols, 2))]

modtmax = modtmax[modstart:modend,] 

modtmin = modtmin[modstart:modend,] 


#to split up all the shit into unique months: 

#first generate regular date series !!!! depends on getDates from hydroMet
regdatesm = getDates(as.character(modtmax[,1]))

# get unique year/month combos. 
yearmonth= substr(regdatesm, 1, 7)

monmodtmax = data.frame(yearmonth, modtmax)

monmodtmin = data.frame(yearmonth, modtmin)

##split the data frames into unique month data frames. 

umodtmax = split( monmodtmax, f = monmodtmax[,1] )

umodtmin = split( monmodtmin, f = monmodtmin[,1] )


###########################################################################################
###########################################################################################
#read in "observed" cfsr for tmp

#define widths for file of up to 50 subbasins
tmpw = c(7, rep(5, 100))

#read in all mod temp data 
obs = read.fwf(obsFile, widths = tmpw, skip = 4)

#get rid of the NA columns 
obs = obs[,colSums(is.na(obs)) != nrow(obs)]

#get indicies 
obsstart = which(obs[,1] == startDate)
obsend = which(obs[,1] == endDate)


#separate tmax and tmin temp columns

#get total columns 
obscols = ncol(obs) 

#get tmpmax
obstmax = obs[, c(1, seq(2, obscols, 2))]

#get tmp min
obstmin = obs[, c(1, seq(3, obscols, 2))]

obstmax = obstmax[obsstart:obsend,] 

obstmin = obstmin[obsstart:obsend,] 



#to split up all the shit into unique months: 

#first generate regular date series !!!! depends on getDates from hydroMet
regdatesm = getDates(as.character(obstmax[,1]))

# get unique year/month combos. 
yearmonth= substr(regdatesm, 1, 7)

monobstmax = data.frame(yearmonth, obstmax)

monobstmin = data.frame(yearmonth, obstmin)

##split the data frames into unique month data frames. 

uobstmax = split( monobstmax, f = monobstmax[,1] )

uobstmin = split( monobstmin, f = monobstmin[,1] )


###########################################################################################
###########################################################################################
#read in the modeled for CDFm forcing 

#define widths for file of up to 50 subbasins
tmpw = c(7, rep(5, 100))

#read in all mod temp data 
inp = read.fwf(inpFile, widths = tmpw, skip = 4)

#get rid of the NA columns 
inp = inp[,colSums(is.na(inp)) != nrow(inp)]

#get indicies 
inpstart = which(inp[,1] == startDatef)
inpend = which(inp[,1] == endDatef)

#separate tmax and tmin temp columns

#get total columns 
inpcols = ncol(inp) 

#get tmpmax
inptmax = inp[, c(1, seq(2, modcols, 2))]

#get tmp min
inptmin = inp[, c(1, seq(3, modcols, 2))]

inptmax = inptmax[inpstart:inpend,] 

inptmin = inptmin[inpstart:inpend,] 


#first generate regular date series !!!! depends on getDates from hydroMet
regdatesm = getDates(as.character(inptmax[,1]))

# get unique year/month combos. 
yearmonth= substr(regdatesm, 1, 7)

monimptmax = data.frame(yearmonth, inptmax)

monimptmin = data.frame(yearmonth, inptmin)

##split the data frames into unique month data frames. 

uinptmax = split( monimptmax, f = monimptmax[,1] )

uinptmin = split( monimptmin, f = monimptmin[,1] )



###########################################################################################
###########################################################################################



#first obs for tmax and tmin

###########################################################################################
###########################################################################################

#convert to kalvin !!!!!


for(i in 1:length(uobstmax)){
	uobstmax[[i]] = uobstmax[[i]][,3:ncol(uobstmax[[i]])] + 273.15
}

for(i in 1:length(uobstmin)){
	uobstmin[[i]] = uobstmin[[i]][,3:ncol(uobstmin[[i]])] + 273.15
}
	

for(i in 1:length(umodtmax)){
	umodtmax[[i]] = umodtmax[[i]][,3:ncol(umodtmax[[i]])] + 273.15
}


for(i in 1:length(umodtmin)){
	umodtmin[[i]] = umodtmin[[i]][,3:ncol(umodtmin[[i]])] + 273.15
}

for(i in 1:length(uinptmax)){
	uinptmax[[i]] = uinptmax[[i]][,3:ncol(uinptmax[[i]])] + 273.15
}

for(i in 1:length(uinptmin)){
	uinptmin[[i]] = uinptmin[[i]][,3:ncol(uinptmin[[i]])] + 273.15
}
		

###########################################################################################	
###########################################################################################
#get the qunatile forcing objects

#tmax
fobjtmax = list()

for(i in 1:length(uobstmax)){
	fobjtmax[[i]] = fitQmap(uobstmax[[i]], umodtmax[[i]], method = method, wet.day = 0)
}

#tmin
fobjtmin = list()

for(i in 1:length(uobstmin)){
	fobjtmin[[i]] = fitQmap(uobstmin[[i]], umodtmin[[i]], method = method, wet.day = 0)
}



###########################################################################################
###########################################################################################	
################apply the forcing to the input data set 	
	
cdfmtmax = list()	
	
for(i in 1:length(uinptmax)){
	cdfmtmax[[i]] = doQmap(uinptmax[[i]], fobjtmax[[i]])
}
	
cdfmtmin = list()	
	
for(i in 1:length(uinptmax)){
	cdfmtmin[[i]] = doQmap(uinptmin[[i]], fobjtmin[[i]])
}
	
###########################################################################################
###########################################################################################	
#convert to deg celcius

for(i in 1:length(cdfmtmax)){
	cdfmtmax[[i]] = cdfmtmax[[i]] - 273.15
}
		
	
for(i in 1:length(cdfmtmin)){
	cdfmtmin[[i]] = cdfmtmin[[i]] - 273.15
}
	
		
###########################################################################################
###########################################################################################	 
#combine tmax and tmin 
tmax = as.matrix(do.call('rbind', cdfmtmax))

tmin = as.matrix(do.call('rbind', cdfmtmin))

#get number of rows to size new matrix 
rows = nrow(tmax)

#get total number of cols for new matrix 
cols = ncol(tmax) + ncol(tmin)

#define new NaN matrix 
tmp = matrix(NA, nrow = rows, ncol = cols)

#fill in the values 
tmp[, seq(1, cols, 2)] <- tmax
tmp[, seq(2, cols, 2)] <- tmin


###############################################################################################
###############################################################################################
#now recombine matrix to df and SWAT format
tmpdf = data.frame(inp[,1][inpstart:inpend] , tmp)

#format the temp DATA  
for(i in 2:ncol(tmpdf)){
	tmpdf[,i] = formatC(tmpdf[,i], digits = 1, width = 5, flag = '0', format ='f')
}

#define the tmpmin header
headtmp = c('Station ID', 'Lati', 'Long', 'Elev') 

writeLines(headtmp, paste(inpFile, tag, '.tmp', sep = '_'))

#writeLines()
write.table(tmpdf, paste(inpFile, tag, '.tmp', sep = '_'), append = TRUE, sep = '', row.names = FALSE, col.names = FALSE, quote = FALSE)

###############################################################################################
###############################################################################################

	
#end of tmp processing 	
}


###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################

#pcp processing 
if(fileType == 'pcp'){
	
###########################################################################################
	
###############################################################################################
###############################################################################################

#define widths for file of up to 50 subbasins
pcpw = c(7, rep(5, 10))

#read in all mod temp data 
mod = read.fwf(modFile, widths = pcpw, skip = 4)

#get rid of the NA columns 
mod = mod[,colSums(is.na(mod)) != nrow(mod)]

#get indicies 
modstart = which(mod[,1] == startDate)
modend = which(mod[,1] == endDate)


#get tmp min
#modpcp = mod[, c(1, seq(3, modcols, 2))]

modpcp = mod[modstart:modend,] 

#first generate regular date series !!!! depends on getDates from hydroMet
regdatesm = getDates(as.character(modpcp[,1]))

# get unique year/month combos. 
yearmonth= substr(regdatesm, 1, 7)

monmodpcp = data.frame(yearmonth, modpcp)

##split the data frames into unique month data frames. 

umodpcp = split( monmodpcp, f = monmodpcp[,1] )



###########################################################################################
###########################################################################################
#read in "observed" cfsr for tmp


#define widths for file of up to 50 subbasins
pcpw = c(7, rep(5, 10))

#read in all obs temp data 
obs = read.fwf(obsFile, widths = pcpw, skip = 4)

#get rid of the NA columns 
obs = obs[,colSums(is.na(obs)) != nrow(obs)]


#get pcps 

#get total columns 
obscols = ncol(obs) 

#get indicies 
obsstart = which(obs[,1] == startDate)
obsend = which(obs[,1] == endDate)


#get tmp min
#obspcp = obs[, c(1, seq(3, obscols, 2))]

obspcp = obs[obsstart:obsend,] 

#first generate regular date series !!!! depends on getDates from hydroMet
regdatesm = getDates(as.character(obspcp[,1]))

# get unique year/month combos. 
yearmonth= substr(regdatesm, 1, 7)

monobspcp = data.frame(yearmonth, obspcp)

##split the data frames into unique month data frames. 

uobspcp = split( monobspcp, f = monobspcp[,1] )



###########################################################################################
###########################################################################################
#read in the modeled for CDFm forcing 

#define widths for file of up to 50 subbasins
pcpw = c(7, rep(5, 10))

#read in all inp temp data 
inp = read.fwf(inpFile, widths = pcpw, skip = 4)

#get rid of the NA columns 
inp = inp[,colSums(is.na(inp)) != nrow(inp)]


#get pcps 

#get total columns 
inpcols = ncol(inp) 

#get indicies 
inpstart = which(inp[,1] == startDatef)
inpend = which(inp[,1] == endDatef)


#get tmp min
#inppcp = inp[, c(1, seq(3, inpcols, 2))]

inppcp = inp[inpstart:inpend,] 

#first generate regular date series !!!! depends on getDates from hydroMet
regdatesm = getDates(as.character(inppcp[,1]))

# get unique year/month combos. 
yearmonth= substr(regdatesm, 1, 7)

moninppcp = data.frame(yearmonth, inppcp)

##split the data frames into unique month data frames. 

uinppcp = split( moninppcp, f = moninppcp[,1] )


###########################################################################################
############################################################################################
#strip extraneaous date columns 


for(i in 1:length(uobspcp)){
	uobspcp[[i]] = uobspcp[[i]][,3:ncol(uobspcp[[i]])]
}


for(i in 1:length(umodpcp)){
	umodpcp[[i]] = umodpcp[[i]][,3:ncol(umodpcp[[i]])]
}


for(i in 1:length(uinppcp)){
	uinppcp[[i]] = uinppcp[[i]][,3:ncol(uinppcp[[i]])]
}

	


###########################################################################################
###########################################################################################

#pcp
fobjpcp = list()

for(i in 1:length(uobspcp)){
	fobjpcp[[i]] = fitQmap(uobspcp[[i]], umodpcp[[i]], method = method, wet.day = 0)
}

###########################################################################################
###########################################################################################	
################apply the forcing to the input data set 	

cdfmpcp = list()	
	
for(i in 1:length(uinppcp)){
	cdfmpcp[[i]] = doQmap(uinppcp[[i]], fobjpcp[[i]])
}

###############################################################################################
###############################################################################################
	
#combine pcp and tmin 
pcp = as.matrix(do.call('rbind', cdfmpcp))

#now recombine matrix to df and SWAT format
pcpdf = data.frame(inp[,1][inpstart:inpend] , pcp)

#format the temp DATA  
for(i in 2:ncol(pcpdf)){
	pcpdf[,i] = formatC(pcpdf[,i], digits = 1, width = 5, flag = '0', format ='f')
}

#define the pcpmin header
headpcp = c('Station ID', 'Lati', 'Long', 'Elev') 

writeLines(headpcp, paste(inpFile, tag, '.pcp', sep = '_'))

#writeLines()
write.table(pcpdf, paste(inpFile, tag, '.pcp', sep = '_'), append = TRUE, sep = '', row.names = FALSE, col.names = FALSE, quote = FALSE)


###############################################################################################
###############################################################################################



#end of pcp processing	
}

#hmd processing 
if(fileType == 'hmd'){
	
###############################################################################################
###############################################################################################

#define widths for file of up to 50 subbasins
hmdw = c(7, rep(8, 10))

#read in all mod temp data 
mod = read.fwf(modFile, widths = hmdw, skip = 1)

#get rid of the NA columns 
mod = mod[,colSums(is.na(mod)) != nrow(mod)]

#get indicies 
modstart = which(mod[,1] == startDate)
modend = which(mod[,1] == endDate)


#get tmp min
#modhmd = mod[, c(1, seq(3, modcols, 2))]

modhmd = mod[modstart:modend,] 

#first generate regular date series !!!! depends on getDates from hydroMet
regdatesm = getDates(as.character(modhmd[,1]))

# get unique year/month combos. 
yearmonth= substr(regdatesm, 1, 7)

monmodhmd = data.frame(yearmonth, modhmd)

##split the data frames into unique month data frames. 

umodhmd = split( monmodhmd, f = monmodhmd[,1] )



###########################################################################################
###########################################################################################
#read in "observed" cfsr for tmp


#define widths for file of up to 50 subbasins
hmdw = c(7, rep(8, 10))

#read in all obs temp data 
obs = read.fwf(obsFile, widths = hmdw, skip = 1)

#get rid of the NA columns 
obs = obs[,colSums(is.na(obs)) != nrow(obs)]


#get hmds 

#get total columns 
obscols = ncol(obs) 

#get indicies 
obsstart = which(obs[,1] == startDate)
obsend = which(obs[,1] == endDate)


#get tmp min
#obshmd = obs[, c(1, seq(3, obscols, 2))]

obshmd = obs[obsstart:obsend,] 

#first generate regular date series !!!! depends on getDates from hydroMet
regdatesm = getDates(as.character(obshmd[,1]))

# get unique year/month combos. 
yearmonth= substr(regdatesm, 1, 7)

monobshmd = data.frame(yearmonth, obshmd)

##split the data frames into unique month data frames. 

uobshmd = split( monobshmd, f = monobshmd[,1] )



###########################################################################################
###########################################################################################
#read in the modeled for CDFm forcing 

#define widths for file of up to 50 subbasins
hmdw = c(7, rep(8, 10))

#read in all inp temp data 
inp = read.fwf(inpFile, widths = hmdw, skip = 1)

#get rid of the NA columns 
inp = inp[,colSums(is.na(inp)) != nrow(inp)]


#get hmds 

#get total columns 
inpcols = ncol(inp) 

#get indicies 
inpstart = which(inp[,1] == startDatef)
inpend = which(inp[,1] == endDatef)


#get tmp min
#inphmd = inp[, c(1, seq(3, inpcols, 2))]

inphmd = inp[inpstart:inpend,] 

#first generate regular date series !!!! depends on getDates from hydroMet
regdatesm = getDates(as.character(inphmd[,1]))

# get unique year/month combos. 
yearmonth= substr(regdatesm, 1, 7)

moninphmd = data.frame(yearmonth, inphmd)

##split the data frames into unique month data frames. 

uinphmd = split( moninphmd, f = moninphmd[,1] )


###########################################################################################
############################################################################################
#strip extraneaous date columns 


for(i in 1:length(uobshmd)){
	uobshmd[[i]] = uobshmd[[i]][,3:ncol(uobshmd[[i]])]
}


for(i in 1:length(umodhmd)){
	umodhmd[[i]] = umodhmd[[i]][,3:ncol(umodhmd[[i]])]
}


for(i in 1:length(uinphmd)){
	uinphmd[[i]] = uinphmd[[i]][,3:ncol(uinphmd[[i]])]
}

	


###########################################################################################
###########################################################################################

#hmd
fobjhmd = list()

for(i in 1:length(uobshmd)){
	fobjhmd[[i]] = fitQmap(uobshmd[[i]], umodhmd[[i]], method = method, wet.day = 0)
}

###########################################################################################
###########################################################################################	
################apply the forcing to the input data set 	

cdfmhmd = list()	
	
for(i in 1:length(uinphmd)){
	cdfmhmd[[i]] = doQmap(uinphmd[[i]], fobjhmd[[i]])
}

###############################################################################################
###############################################################################################
	
#combine hmd and tmin 
hmd = as.matrix(do.call('rbind', cdfmhmd))

#now recombine matrix to df and SWAT format
hmddf = data.frame(inp[,1][inpstart:inpend] , hmd)

#format the temp DATA  
for(i in 2:ncol(hmddf)){
	hmddf[,i] = formatC(hmddf[,i], digits = 3, width = 8, flag = '0', format ='f')
}

#define the hmdmin header
headhmd = 'hmd bias corrected' 

writeLines(headhmd, paste(inpFile, tag, '.hmd', sep = '_'))

#writeLines()
write.table(hmddf, paste(inpFile, tag, '.hmd', sep = '_'), append = TRUE, sep = '', row.names = FALSE, col.names = FALSE, quote = FALSE)


###############################################################################################
###############################################################################################



#end of hmd processing	


}

#pcp processing 
if(fileType == 'slr'){
	
###############################################################################################
###############################################################################################

#define widths for file of up to 50 subbasins
slrw = c(7, rep(8, 10))

#read in all mod temp data 
mod = read.fwf(modFile, widths = slrw, skip = 1)

#get rid of the NA columns 
mod = mod[,colSums(is.na(mod)) != nrow(mod)]

#get indicies 
modstart = which(mod[,1] == startDate)
modend = which(mod[,1] == endDate)


#get tmp min
#modslr = mod[, c(1, seq(3, modcols, 2))]

modslr = mod[modstart:modend,] 

#first generate regular date series !!!! depends on getDates from hydroMet
regdatesm = getDates(as.character(modslr[,1]))

# get unique year/month combos. 
yearmonth= substr(regdatesm, 1, 7)

monmodslr = data.frame(yearmonth, modslr)

##split the data frames into unique month data frames. 

umodslr = split( monmodslr, f = monmodslr[,1] )



###########################################################################################
###########################################################################################
#read in "observed" cfsr for tmp


#define widths for file of up to 50 subbasins
slrw = c(7, rep(8, 10))

#read in all obs temp data 
obs = read.fwf(obsFile, widths = slrw, skip = 1)

#get rid of the NA columns 
obs = obs[,colSums(is.na(obs)) != nrow(obs)]


#get slrs 

#get total columns 
obscols = ncol(obs) 

#get indicies 
obsstart = which(obs[,1] == startDate)
obsend = which(obs[,1] == endDate)


#get tmp min
#obsslr = obs[, c(1, seq(3, obscols, 2))]

obsslr = obs[obsstart:obsend,] 

#first generate regular date series !!!! depends on getDates from hydroMet
regdatesm = getDates(as.character(obsslr[,1]))

# get unique year/month combos. 
yearmonth= substr(regdatesm, 1, 7)

monobsslr = data.frame(yearmonth, obsslr)

##split the data frames into unique month data frames. 

uobsslr = split( monobsslr, f = monobsslr[,1] )



###########################################################################################
###########################################################################################
#read in the modeled for CDFm forcing 

#define widths for file of up to 50 subbasins
slrw = c(7, rep(8, 10))

#read in all inp temp data 
inp = read.fwf(inpFile, widths = slrw, skip = 1)

#get rid of the NA columns 
inp = inp[,colSums(is.na(inp)) != nrow(inp)]


#get slrs 

#get total columns 
inpcols = ncol(inp) 

#get indicies 
inpstart = which(inp[,1] == startDatef)
inpend = which(inp[,1] == endDatef)


#get tmp min
#inpslr = inp[, c(1, seq(3, inpcols, 2))]

inpslr = inp[inpstart:inpend,] 

#first generate regular date series !!!! depends on getDates from hydroMet
regdatesm = getDates(as.character(inpslr[,1]))

# get unique year/month combos. 
yearmonth= substr(regdatesm, 1, 7)

moninpslr = data.frame(yearmonth, inpslr)

##split the data frames into unique month data frames. 

uinpslr = split( moninpslr, f = moninpslr[,1] )


###########################################################################################
############################################################################################
#strip extraneaous date columns 


for(i in 1:length(uobsslr)){
	uobsslr[[i]] = uobsslr[[i]][,3:ncol(uobsslr[[i]])]
}


for(i in 1:length(umodslr)){
	umodslr[[i]] = umodslr[[i]][,3:ncol(umodslr[[i]])]
}


for(i in 1:length(uinpslr)){
	uinpslr[[i]] = uinpslr[[i]][,3:ncol(uinpslr[[i]])]
}

	


###########################################################################################
###########################################################################################

#slr
fobjslr = list()

for(i in 1:length(uobsslr)){
	fobjslr[[i]] = fitQmap(uobsslr[[i]], umodslr[[i]], method = method, wet.day = 0)
}

###########################################################################################
###########################################################################################	
################apply the forcing to the input data set 	

cdfmslr = list()	
	
for(i in 1:length(uinpslr)){
	cdfmslr[[i]] = doQmap(uinpslr[[i]], fobjslr[[i]])
}

###############################################################################################
###############################################################################################
	
#combine slr and tmin 
slr = as.matrix(do.call('rbind', cdfmslr))

#now recombine matrix to df and SWAT format
slrdf = data.frame(inp[,1][inpstart:inpend] , slr)

#format the temp DATA  
for(i in 2:ncol(slrdf)){
	slrdf[,i] = formatC(slrdf[,i], digits = 3, width = 8, flag = '0', format ='f')
}

#define the slrmin header
headslr = 'slr bias corrected'

writeLines(headslr, paste(inpFile, tag, '.slr', sep = '_'))

#writeLines()
write.table(slrdf, paste(inpFile, tag, '.slr', sep = '_'), append = TRUE, sep = '', row.names = FALSE, col.names = FALSE, quote = FALSE)


###############################################################################################
###############################################################################################



#end of slr processing	
}

#pcp processing 
if(fileType == 'wnd'){
	
###############################################################################################
###############################################################################################

#define widths for file of up to 50 subbasins
wndw = c(7, rep(8, 10))

#read in all mod temp data 
mod = read.fwf(modFile, widths = wndw, skip = 1)

#get rid of the NA columns 
mod = mod[,colSums(is.na(mod)) != nrow(mod)]

#get indicies 
modstart = which(mod[,1] == startDate)
modend = which(mod[,1] == endDate)


#get tmp min
#modwnd = mod[, c(1, seq(3, modcols, 2))]

modwnd = mod[modstart:modend,] 

#first generate regular date series !!!! depends on getDates from hydroMet
regdatesm = getDates(as.character(modwnd[,1]))

# get unique year/month combos. 
yearmonth= substr(regdatesm, 1, 7)

monmodwnd = data.frame(yearmonth, modwnd)

##split the data frames into unique month data frames. 

umodwnd = split( monmodwnd, f = monmodwnd[,1] )



###########################################################################################
###########################################################################################
#read in "observed" cfsr for tmp


#define widths for file of up to 50 subbasins
wndw = c(7, rep(8, 10))

#read in all obs temp data 
obs = read.fwf(obsFile, widths = wndw, skip = 1)

#get rid of the NA columns 
obs = obs[,colSums(is.na(obs)) != nrow(obs)]


#get wnds 

#get total columns 
obscols = ncol(obs) 

#get indicies 
obsstart = which(obs[,1] == startDate)
obsend = which(obs[,1] == endDate)


#get tmp min
#obswnd = obs[, c(1, seq(3, obscols, 2))]

obswnd = obs[obsstart:obsend,] 

#first generate regular date series !!!! depends on getDates from hydroMet
regdatesm = getDates(as.character(obswnd[,1]))

# get unique year/month combos. 
yearmonth= substr(regdatesm, 1, 7)

monobswnd = data.frame(yearmonth, obswnd)

##split the data frames into unique month data frames. 

uobswnd = split( monobswnd, f = monobswnd[,1] )



###########################################################################################
###########################################################################################
#read in the modeled for CDFm forcing 

#define widths for file of up to 50 subbasins
wndw = c(7, rep(8, 10))

#read in all inp temp data 
inp = read.fwf(inpFile, widths = wndw, skip = 1)

#get rid of the NA columns 
inp = inp[,colSums(is.na(inp)) != nrow(inp)]


#get wnds 

#get total columns 
inpcols = ncol(inp) 

#get indicies 
inpstart = which(inp[,1] == startDatef)
inpend = which(inp[,1] == endDatef)


#get tmp min
#inpwnd = inp[, c(1, seq(3, inpcols, 2))]

inpwnd = inp[inpstart:inpend,] 

#first generate regular date series !!!! depends on getDates from hydroMet
regdatesm = getDates(as.character(inpwnd[,1]))

# get unique year/month combos. 
yearmonth= substr(regdatesm, 1, 7)

moninpwnd = data.frame(yearmonth, inpwnd)

##split the data frames into unique month data frames. 

uinpwnd = split( moninpwnd, f = moninpwnd[,1] )


###########################################################################################
############################################################################################
#strip extraneaous date columns 


for(i in 1:length(uobswnd)){
	uobswnd[[i]] = uobswnd[[i]][,3:ncol(uobswnd[[i]])]
}


for(i in 1:length(umodwnd)){
	umodwnd[[i]] = umodwnd[[i]][,3:ncol(umodwnd[[i]])]
}


for(i in 1:length(uinpwnd)){
	uinpwnd[[i]] = uinpwnd[[i]][,3:ncol(uinpwnd[[i]])]
}

	


###########################################################################################
###########################################################################################

#wnd
fobjwnd = list()

for(i in 1:length(uobswnd)){
	fobjwnd[[i]] = fitQmap(uobswnd[[i]], umodwnd[[i]], method = method, wet.day = 0)
}

###########################################################################################
###########################################################################################	
################apply the forcing to the input data set 	

cdfmwnd = list()	
	
for(i in 1:length(uinpwnd)){
	cdfmwnd[[i]] = doQmap(uinpwnd[[i]], fobjwnd[[i]])
}

###############################################################################################
###############################################################################################
	
#combine wnd and tmin 
wnd = as.matrix(do.call('rbind', cdfmwnd))

#now recombine matrix to df and SWAT format
wnddf = data.frame(inp[,1][inpstart:inpend] , wnd)

#format the temp DATA  
for(i in 2:ncol(wnddf)){
	wnddf[,i] = formatC(wnddf[,i], digits = 3, width = 8, flag = '0', format ='f')
}

#define the wndmin header
headwnd = 'wnd bias corrected'

writeLines(headwnd, paste(inpFile, tag, '.wnd', sep = '_'))

#writeLines()
write.table(wnddf, paste(inpFile, tag, '.wnd', sep = '_'), append = TRUE, sep = '', row.names = FALSE, col.names = FALSE, quote = FALSE)


###############################################################################################
###############################################################################################



#end of wnd processing	

}

###############################################################################################
###############################################################################################


}


