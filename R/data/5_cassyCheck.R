# cassy check 
if(Sys.info()['user'] %in% c('s7m', 'janus829')){ source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ source('~/ProjectsGit/victimization/R/setup.R') }

loadPkg('readr')

### ACLED DATA
acled = read_csv(paste0(pathData, "ACLED-Version-7-All-Africa-1997-2016_csv_dyadic-file.csv"))
acled = acled[acled$YEAR>=1997,]

### UCDP DATA
load(paste0(pathData, 'ged171.Rdata'))
ged=data.frame(ged171, stringsAsFactors = FALSE) ; rm(ged171)

ged = ged[,c(
	'id','year','type_of_vi','conflict_n',
	'dyad_new_i','dyad_name',
	'side_a_new','side_a',
	'side_b_new','side_b',
	'country',
	'best', 'deaths_a', 'deaths_b', 'deaths_civ'
	)]

gedCiv = ged[ged$type_of_vi==2,]
gedCiv = gedCiv[gedCiv$year>=1997,]

# acled unique country years
acledCheck<-as.data.frame(acled[c("COUNTRY", "YEAR")])
acledCheck<- unique(df1[order(df1$YEAR),])

ucdpCheck<-as.data.frame(gedCiv[c("country", "year")])
ucdpCheck<- unique(df2[order(df2$year),])

