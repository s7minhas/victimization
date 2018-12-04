if(Sys.info()['user'] %in% c('s7m', 'janus829')){ source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('maxgallop')){ source('~/Documents/victimization/R/setup.R') }

# abm path
abmPath = paste0(pathDrop, 'abm/')
abmPath = paste0(pathGit, "python/")
# load in file
abmData = read.csv(paste0(abmPath, 'abmViz.csv'), header=FALSE)
# V12: who victimized and where
# V13 is what attacks took place
# V14 is groups + their territory per period
# V15 = adjacency matrix