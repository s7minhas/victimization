from VicForViz import *
import csv

with open('abmGovIdeoRandom2.csv', 'w') as csvfile:
  abmwriter = csv.writer(csvfile, delimiter=',')
  for i in range(3000):
    actors = np.random.poisson(10)
    territory = max(np.random.poisson(13), actors + 1)
    conn = np.random.uniform(low = .2, high = .75)
    civ = np.random.poisson(45)
    VicPenalty = np.random.uniform(low = .05, high = .3)
    CoerceMob = np.random.uniform(low = .1, high = .5)
    DisloyalPenalty = np.random.uniform(low = .25, high = .75)
    delta = np.random.uniform(low = .1, high = .75)
    battledeaths = np.random.poisson(1) + 1
    growthrate = .1
    victimerror = np.random.uniform(low = 0, high = .5)
    turnlimit = np.random.poisson(10) + 1
    govIdeo = 0
    abm = Country("abm", territory, conn, actors, civ)
    try:
      abm.Game()
      abmwriter.writerow([actors, territory, conn, civ, VicPenalty, CoerceMob, delta, battledeaths, growthrate, victimerror, turnlimit, abm.victimhist, abm.attackhist, abm.actorlists])
    except ZeroDivisionError:
      print("next one")
    if i%100 == 0:
      print(i)