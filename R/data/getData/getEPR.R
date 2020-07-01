####
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){
	source('~/ProjectsGit/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
####

############################
# load data
epr = read_csv(paste0(pathData, 'growup/data.csv'))
############################

############################
# Process epr data

# include only post 1993 observations
epr = epr[epr$year >= 1993, ]

# Match country names with panel dataset
epr$country = char(epr$countryname) %>% trim()

# fix drc roc
epr$country[epr$country=='Congo, DRC'] = 'Democratic Republic of Congo'

# fix korea
epr$country[epr$country=="Democratic People's Republic of Korea"] = 'North Korea'

# fix serbia
epr = epr[-which(epr$country=='Serbia' & epr$year==2006),]

# Convert to matching countrycodes
epr$cname=cname(epr$country)

# Construct id from year + name
epr$cnameYear=paste0(epr$cname, '_', epr$year)

# Check for Dupes
names(table(epr$cnameYear)[table(epr$cnameYear)>1]) # Dupe check

# Adding in codes from panel
epr$ccode=panel$ccode[match(epr$cname,panel$cname)]
slice = unique(epr[is.na(epr$ccode),c('country','cname')]) # check for NAs
nrow(slice)
epr$cyear=paste(epr$ccode, epr$year, sep='_')
table(epr$cyear)[table(epr$cyear)>1] # Dupe check
############################

############################
# Save cleaned epr data
save(epr, file=paste0(pathData, 'growup/epr.rda'))
############################
