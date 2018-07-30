### cassy's check, compare UCDP and ACLED data

if(Sys.info()['user'] %in% c('s7m', 'janus829')){ source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ source('~/ProjectsGit/victimization/R/setup.R') }

loadPkg('readr')

### ACLED DATA
acled = read_csv(paste0(pathData, "ACLED-Version-7-All-Africa-1997-2016_csv_dyadic-file.csv"))
acled = acled[acled$YEAR>=1997,]

# subset on battles
toKeep = c(
	# 'Remote violence', 'Headquarters or base established', 
	'Battle-No change of territory', 
	'Battle-Government regains territory', 
	'Battle-Non-state actor overtakes territory')

acled = acled[which(acled$EVENT_TYPE %in% toKeep),]

# stdz country names using panel
acled$cname = cname(acled$COUNTRY) #FAIL
acled$id = with(acled, paste0(cname, '_', YEAR))
acled$ccode=panel$ccode[match(acled$cname,panel$cname)]
acled$cyear=paste(acled$ccode, acled$YEAR, sep='')

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

# convert side a to countryname format
gedCiv$cname = cname(gedCiv$side_a)

# get rid of side_a obs that are not countries
gedCiv = gedCiv[!is.na(gedCiv$cname),]

# fix country name
gedCiv$cname[gedCiv$cname=='Yugoslavia'] = 'SERBIA'

# add in countrycode
gedCiv$ccode=panel$ccode[match(gedCiv$cname,panel$cname)]

### Compare acled and ucdp cases
# acled unique country years (wrote this before fixing countries so need to update)
acledCheck<-as.data.frame(acled[c("COUNTRY", "YEAR")])
acledCheck<- unique(acledCheck[order(acledCheck$YEAR),])

ucdpCheck<-as.data.frame(gedCiv[c("country", "year")])
ucdpCheck<- unique(ucdpCheck[order(ucdpCheck$year),])

