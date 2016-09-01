### runswat and save 


library(SWATmodel)

runs =  seq(1, 462)

swatpath = 'C:/Users/Andy/Desktop/cmdcenter/TxtInOut300'
incios = paste('C:/Users/Andy/Desktop/cios/', runs, sep = '')
inmets = paste('C:/Users/Andy/Desktop/runs/', runs, sep = '')
outpaths = paste('C:/Users/Andy/Desktop/rchouts/', runs, sep = '')

setwd(swatpath)

for(i in 1:length(runs)){
  flist1 <- list.files(inmets[i], full.names = TRUE)
  flist2 <- list.files(incios[i], full.names = TRUE)
  
  file.copy(flist1, swatpath, overwrite = TRUE)
  file.copy(flist2, swatpath, overwrite = TRUE)
  #closeAllConnections()
  
  runSWAT2012()
  
  flist3 = list.files(swatpath, 'output.rch')
  dir.create(outpaths[i])
  file.copy(flist3, outpaths[i])
  #closeAllConnections()

}
