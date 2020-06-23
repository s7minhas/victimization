specifically, in the case of the actor counts, we:

combined military/police into just a single gov actor for that country
removed unidentified actors
did a spot check for general labels, and ended up removing anything that was just like opposition parties
last we only included actors that were involved in events that led to 10 fatalities from 1997 to 2020

in the context of our empirical data, we limit actors by:

removing unidentified groups
remove groups that are IGOs or international actors (this coding was done manually by you guys)
aggregate miliatry/police into gov
and then only keep an actor if they were active for one year

modified:

- aggregate military/police into gov
- violence of any kind that led to at least 10 fatalities
  - so just do a group by actor and then sum of fatalities
- include actors in the range of their event min and max year
- not a peacekeeper or election observer
- remove unidentified groups based on grepl searches looking for the words unidentified, opposition
- create adjacency matrices such that any actor in the list and active (within their min-max) gets included ... calc net stats
