## actor inclusion rules (6/22/20):

- aggregate military/police into gov
- violence of any kind that led to at least 10 fatalities
  - so just do a group by actor and then sum of fatalities
- include actors in the range of their event min and max year
- not a peacekeeper or election observer
- remove unidentified groups based on grepl searches looking for the words unidentified, opposition
- create adjacency matrices such that any actor in the list and active (within their min-max) gets included ... calc net stats

these inclusion rules are used in the following file(s):

- 1_caseSelecACLED.R


## sample of countries to choose (6/25/20):

we limit the countries we pull from ACLED based on the number of actors and dyadic pairs

sample of countries is chosen in:

- 1_caseSelecACLED.R 
