# 7/17

empirical results hold with fixed or random effects on countries

to maximimize the amount of data we have for every model we break our models with controls into two pieces, the first includes polity, world bank vars, and excl pop. the max year for this dataset is 2018 because of epr sample limitations. the other adds in kathman's peacekeeper variable and rebel v gov strength from gleditsch's nsa dataset. max year for this is 2012 because of nsa limitations.

# 7/16

we count "number of conflicts" in two ways. First is nConf which is generated in 5_buildData and is simply a count of the number of battle events faced by a country year. Second is nEvents which is generated in 4_calcNetStats.R and is a sum of the number of conflictual interactions between the actors that we subsetted our analysis to. Our empirical models are robust to using either of these approaches.

# 7/13

changed around control variables a bit, couldnt find the ethnic frac measure in the most recent version of the epr data so we switched to using exclpop
