#read hindcast outputs

library(swatmodel)


wd = 'C:/Users/Andy/Desktop/testrch/'

nsubs = 5

wantsub = 1

setwd(wd)

r24 = list()
r48 = list()
r72 = list()
r96 = list()
r120 = list()
r144= list()
r168 = list()
r192 = list()

date24 = as.Date('2013-05-20')
date48 = as.Date('2013-05-21')
date72 = as.Date('2013-05-22')
date96 = as.Date('2013-05-23')
date120 = as.Date('2013-05-24')
date144 = as.Date('2013-05-25')
date168 = as.Date('2013-05-26')
date192 = as.Date('2013-05-27')

is = seq(1, 462, 1)
readins = paste(wd, is, '/', 'output.rch', sep = '')

for(i in 1:462){
  input = readLines(readins[i])
  r24[[i]] = as.numeric(substr(input[length(input) - 8*(nsubs - wantsub)], 51, 61))
  r48[[i]] = as.numeric(substr(input[length(input) - 7*(nsubs - wantsub)], 51, 61))
  r72[[i]] = as.numeric(substr(input[length(input) - 6*(nsubs - wantsub)], 51, 61))
  r96[[i]] = as.numeric(substr(input[length(input) - 5*(nsubs - wantsub)], 51, 61))
  r120[[i]] = as.numeric(substr(input[length(input) - 4*(nsubs - wantsub)], 51, 61))
  r144[[i]] = as.numeric(substr(input[length(input) - 3*(nsubs - wantsub)], 51, 61))
  r168[[i]] = as.numeric(substr(input[length(input) - 2*(nsubs - wantsub)], 51, 61))
  r192[[i]] = as.numeric(substr(input[length(input) - 1*(nsubs - wantsub)], 51, 61))
}


df24 = cbind(r24)
df48 = cbind(r48)
df72 = cbind(r72)
df96 = cbind(r96)
df120 = cbind(r120)
df144 = cbind(r144)
df168 = cbind(r168)
df192 = cbind(r192)

hind24 = data.frame(date = seq(date24, date24 + nrow(df24) - 1, by = 'day'), m3s = df24)
hind48 = data.frame(date = seq(date48, date48 + nrow(df48) - 1, by = 'day'), m3s = df48)
hind72 = data.frame(date = seq(date72, date72 + nrow(df72) - 1, by = 'day'), m3s = df72)
hind96 = data.frame(date = seq(date96, date96 + nrow(df96) - 1, by = 'day'), m3s = df96)
hind120 = data.frame(date = seq(date120, date120 + nrow(df120) - 1, by = 'day'), m3s = df120)
hind144 = data.frame(date = seq(date144, date144 + nrow(df144) - 1, by = 'day'), m3s = df144)
hind168 = data.frame(date = seq(date168, date168 + nrow(df168) - 1, by = 'day'), m3s = df168)
hind192 = data.frame(date = seq(date192, date192 + nrow(df192) - 1, by = 'day'), m3s = df192)

## check the forecasts 
###for the observed values. 
meas = get_usgs_gage("01628500", end_date ='2014-08-31' )

measdf = meas[[3]]

calstart = hind24[1,1]
calend = hind24[nrow(hind24),1]

# for calibration set 
st = which(measdf$mdate == calstart)
ed = which(measdf$mdate == calend)

calobs = measdf$flow[st:ed]/24/3600


