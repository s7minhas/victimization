####
if(Sys.info()['user'] %in% c('s7m', 'janus829')){ source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ source('~/ProjectsGit/victimization/R/setup.R') }

# load libs
loadPkg('readr')
####

############################
### ACLED DATA
if(!file.exists(paste0(pathData, 'acledTest.rda'))){
	acled = read_csv(paste0(pathData, "ACLED-Version-7-All-Africa-1997-2016_csv_dyadic-file.csv"))
	save(acled, file=paste0(pathData, 'acledTest.rda')) }
load(paste0(pathData, 'acledTest.rda'))

# year threshold
acled = acled[acled$YEAR>=1997,]

# subset on battles
toKeep = c(
	# 'Remote violence', 'Headquarters or base established', 
	'Battle-No change of territory', 
	'Battle-Government regains territory', 
	'Battle-Non-state actor overtakes territory')

acled = acled[which(acled$EVENT_TYPE %in% toKeep),]

# stdz country names using panel
acled$cname = cname(acled$COUNTRY) 
acled$id = with(acled, paste0(cname, '_', YEAR))
acled$ccode=panel$ccode[match(acled$cname,panel$cname)]
acled$cyear=paste(acled$ccode, acled$YEAR, sep='_')
############################

############################
### UCDP DATA
load(paste0(pathData, 'ged171.Rdata'))
ged=data.frame(ged171, stringsAsFactors = FALSE) ; rm(ged171)

# vars to keep
ged = ged[,c(
	'id','year','type_of_vi','conflict_n',
	'dyad_new_i','dyad_name',
	'side_a_new','side_a',
	'side_b_new','side_b',
	'country',
	'best', 'deaths_a', 'deaths_b', 'deaths_civ'
	)]

# subset to relev type
ged = ged[ged$type_of_vi %in% c(1:2),]
ged = ged[ged$year>=1997,]

# convert side a to countryname format
ged$cname = cname(ged$country)

# get rid of side_a obs that are not countries
ged = ged[!is.na(ged$cname),]

# fix country name
ged$cname[ged$cname=='Yugoslavia'] = 'SERBIA'
ged$cname[ged$country=='DR Congo (Zaire)'] = cname('Democratic Republic of Congo')

# add in countrycode
ged$ccode=panel$ccode[match(ged$cname,panel$cname)]
ged$cyear=with(ged, paste(ccode, year, sep='_') )

# only keep countries that are in acled
ged = ged[ged$cname %in% acled$cname,]
############################

############################
### Compare acled and ucdp cases
# preserve raw files
gedRaw = ged
acledRaw = acled

# dimensions of data at acountry year level
ged = unique(ged[,c('cname','year','cyear')])
acled = unique(acled[,c('cname','YEAR','cyear')])

# number of unique cyear cases
nrow(acled)
nrow(ged)

# stats comparing diffs
ged_eq_acled = intersect(ged$cyear, acled$cyear)
ged_v_acled = setdiff(ged$cyear, acled$cyear)
acled_v_ged = setdiff(acled$cyear, ged$cyear)

length(ged_eq_acled)
length(ged_v_acled)
length(acled_v_ged)

#####
# cases where stuff shows up in ucdp not in acled
ged[ged$cyear %in% ged_v_acled,]

# what is the stuff
summary(gedRaw[gedRaw$cyear %in% ged_v_acled,'best'])
gedRaw[gedRaw$cyear %in% ged_v_acled,]
#####

#####
# cases where stuff shows up in ucdp not in acled
data.frame(acled[acled$cyear %in% acled_v_ged,])

# what is the stuff
summary(acledRaw[acledRaw$cyear %in% acled_v_ged,'FATALITIES'])
acledRaw[acledRaw$cyear %in% acled_v_ged,]

# how many deaths in a year
stats = acledRaw[acledRaw$cyear %in% acled_v_ged,] %>%
	group_by(cyear) %>%
	summarize(deaths = sum(FATALITIES)) %>%
	data.frame()
sum(stats$deaths>=25)
sum(stats$deaths<25)
#####
############################