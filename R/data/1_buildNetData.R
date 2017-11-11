if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/intraConfNetDyn/R/setup.R') }

#
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

#
slice = ged[ged$country=='Somalia',]