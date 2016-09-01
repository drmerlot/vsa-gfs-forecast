#re write the file.cio to run 4 days ahead of current day 

##depends on reshape

#library(reshape)

#get system date
date = Sys.Date()

#test date run if testing is needed. otherwise, use SysDate to get current date as above
#date = as.character("207-04-16")

dates = date

dates.c = as.character(dates)

#The dates must be converted to the proper format and added to the 

dates = as.data.frame(dates.c)

#separtate dates out and leave time behind
dates.split = colsplit(dates$dates.c, '-', names = c('year', 'month', 'day'))

#Create the 2nd to last data frame for dates 
dates.format = data.frame(matrix(ncol = 2, nrow = 1))
colnames(dates.format) = c('year', 'days')

#create the add days matrix. this defines how many extra days are added given the month 
add.days = data.frame(matrix(ncol = 2, nrow = 12))
colnames(add.days) = c('month', 'days')

#define the months and corresponing extra days to add. ex.days is calculated by placing the amount of days from the ith-1 month. 
ex.days = c(0,31,59,90,120,151,181,212,243,273,304,334)
months = c(1,2,3,4,5,6,7,8,9,10,11,12)

#To take care of the leap year change: 
leap.years = vector()
leap.years = as.numeric(leap.years)
year = 1900
for(i in 1:100){
		year = year+ (1*4)
		leap.years[i] = year
}

ln.leap.years = length(leap.years)

for(i in 1:ln.leap.years){
	if(dates.split$year[1] == leap.years[i]){
		ex.days[3] = 29
	}
}

#fill in add.days
add.days$month = months
add.days$days = ex.days



extra.days = vector()
extra.days = as.numeric(extra.days)
ex.day = 1

for(j in 1:12){
		if(dates.split$month == add.days$month[j]){
			ex.day = add.days$days[j]
		}
	}


#fill out the day column of dates.format 
dates.format$days = dates.split$day + ex.day
dates.format$year = dates.split$year


#get number of years

start.year = dates.format$year - 5

year.num = dates.format$year - start.year

start.day = dates.format$day + 1


maxdays = 361

for(i in 1:ln.leap.years){
	if(dates.format$year[1] == leap.years[i]){
		maxdays = 362
	}
}



if(dates.format$day <= maxdays){
	end.day = dates.format$day + 4
}

if(dates.format$day == maxdays + 1){
	end.day = 1
}

if(dates.format$day == maxdays + 2){
	end.day = 2
}

if(dates.format$day == maxdays + 3){
	end.day = 3
}

if(dates.format$day == maxdays + 4){
	end.day = 4
}


##### modify file.cio to run current simulation 


file.copy('/Users/labadmin/Desktop/fileCIO/fileCIOstart.txt', '/Users/labadmin/Desktop/fileCIO/file.cio', overwrite = TRUE)



simlength.format = formatC(year.num, width = 16, flag = ' ')

simlength = as.character(paste(simlength.format,"    | NBYR : Number of years simulated"))

startyear.format = formatC(start.year, width = 16, flag = ' ')

startyear = as.character(paste(startyear.format, "    | IYR : Beginning year of simulation"))

startday.format = formatC(start.day, width = 16, flag = ' ')

startday = as.character(paste(startday.format, "    | IDAF : Beginning julian day of simulation"))

endday.format = formatC(end.day, width = 16, flag = ' ')

endday = as.character(paste(endday.format, "    | IDAL : Ending julian day of simulation"))

filecon = file('/Users/labadmin/Desktop/fileCIO/fileCIOmid.txt')

write(c(simlength, startyear, startday, endday), filecon, append = TRUE)


close(filecon)

###finishes the new file.cio dependent on the current date. 

file.append('/Users/labadmin/Desktop/fileCIO/file.cio', c('/Users/labadmin/Desktop/fileCIO/fileCIOmid.txt', '/Users/labadmin/Desktop/fileCIO/fileCIOend.txt'))


#### copies file.cio to the running TxtInOut folder
file.copy('/Users/labadmin/Desktop/fileCIO/file.cio','/Users/labadmin/Desktop/TxtInOut/file.cio', overwrite = TRUE)






