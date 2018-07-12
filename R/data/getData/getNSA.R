####
if(Sys.info()['user'] %in% c('s7m', 'janus829')){ 
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ 
	source('~/ProjectsGit/victimization/R/setup.R') }
####

############################
# load data
if(!file.exists(paste0(pathData, 'nsa/nsaDirty.rda'))){
	nsa = read.csv(
		'http://privatewww.essex.ac.uk/~ksg/data/nsa_v3.4_21November2013.asc', 
		sep='\t')
	save(nsa, file=paste0(pathData, 'nsa/nsaDirty.rda'))
}  ; load(paste0(pathData, 'nsa/nsaDirty.rda'))
############################

############################
# remove extraterr obs
nsa = nsa[nsa$extraterritorial!=1,]
############################

############################
# break every row that has multiple side_a
# actors into its own separate row
nsa = nsa[!is.na(nsa$side_a),]
nsa$side_a = char(nsa$side_a)
nsa2 = NULL
for(i in 1:nrow(nsa)){
	check = grepl(',', nsa[i,'side_a'], fixed=TRUE)
	if(check){
		slice = nsa[i,]
		actors = unlist(strsplit(slice$side_a, ','))
		out = do.call('rbind', lapply(1:length(actors), function(k){
			slice[,'side_a'] = actors[k]
			return(slice) }))
		nsa2 = rbind(nsa2, out)
	} else {
		nsa2 = rbind(nsa2, nsa[i,])
	}
}
nsa = nsa2

# trim extra spaces
nsa$side_a = trim(nsa$side_a)
############################

############################
# convert side a to countryname format
nsa$cname = cname(nsa$side_a)

# get rid of side_a obs that are not countries
nsa = nsa[!is.na(nsa$cname),]

# fix country name
nsa$cname[nsa$cname=='Yugoslavia'] = 'SERBIA'

# add in countrycode
nsa$ccode=panel$ccode[match(nsa$cname,panel$cname)]
############################

############################
# go from date level to year level
nsa$startdate = char(nsa$startdate) ; nsa$enddate = char(nsa$enddate)
nsa$startYear = num(unlist(lapply(strsplit(nsa$startdate, '-'), function(x){x[1]})))
nsa$endYear = num(unlist(lapply(strsplit(nsa$enddate, '-'), function(x){x[1]})))

# drop cases that dont contain post 1993 data in interval
nsa = nsa[nsa$endYear>=1993,]
nsa$startYear[nsa$startYear<1993] = 1993
years = 1993:max(nsa$endYear)

# expand using same approach
nsa$year = nsa$startYear
nsa2 = NULL
for(i in 1:nrow(nsa)){
	check = (nsa$startYear[i] - nsa$endYear[i]==0)
	if(check){
		nsa2 = rbind(nsa2, nsa[i,])
	} else {
		genSeq = nsa$startYear[i]:nsa$endYear[i]
		slice = nsa[i,]
		out = do.call('rbind', lapply(genSeq, function(t){
			slice$year = t ; return(slice) }))
		nsa2 = rbind(nsa2, out)
	}
}
nsa = nsa2
############################

############################
# add year ids
# Construct id from year + name
nsa$cnameYear=paste0(nsa$cname, '_', nsa$year)

# Adding in codes from panel
nsa$cyear=paste(nsa$ccode, nsa$year, sep='_')
############################

############################
### now we have all this data on rebel groups in country years
### we need make a country year aggregation strategy
############################

############################
# Save cleaned nsa data
save(nsa, file=paste0(pathData, 'nsa/nsa.rda'))
############################