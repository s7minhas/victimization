####
if(Sys.info()['user'] %in% c('s7m', 'janus829')){ 
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ 
	source('~/ProjectsGit/victimization/R/setup.R') }

####

############################

load(paste0(pathData, 'ucdp-dyadic-171.Rdata'))

# greater than 1993
ucdp.dyad = ucdp.dyad[ucdp.dyad$year>=1993,]

# for UCDP we could count # of countries with "stake"
# we could also take primary actor intensity score? not sure how to ID primary actor 
# stdz country names
ucdp.dyad$cname = countrycode(ucdp.dyad$gwnoa, 'cowc', 'country.name')


############################
############################
# Download file from ICOW site
woodURL = 'http://file.prio.no/Journals/JPR/2012/49/5/Wood%20et%20al%202012%20replication.zip'
woodName = paste0(pathData, 'woodData/wood.zip')
if(!file.exists(woodName)) { download.file(woodURL, woodName) }

wood = unzip(woodName, 
	'WKG JPR Govt Replication.dta') %>% foreign::read.dta()
############################

############################
# greater than 1999
wood = wood[wood$year>=1993,]
wood$cname = countrycode(wood$ccode, 'cowc', 'country.name')

