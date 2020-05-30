from random import *
import numpy as np
from string import *
import operator
import networkx as nx
import copy
from math import exp, floor
###Game Parameters, we Can Change These
#How much less people like you when you kill your supporters
VicPenalty = .05
#How many resources you get for non-supporters
CoerceMob = .3
#How much worse fighting is for you for each supporter of your enemy in your territory
DisloyalPenalty = .5
##Discount of resources over space
delta = .2
#Number of civilians in each battlefield territory
battledeaths = 2
#Rate that the population grows in a turn
growthrate = .1
#How likely you are, with 1 supporter, to incorrectly kill a supporter
###victimerror = .1
#If no one wins, how many turns should the game last
turnlimit = 10
govIdeo = 0

def sketchmultinom(probs):
  try:
    return(np.random.multinomial(1, list(probs)))
  except ValueError:
    for i in range(len(probs)):
      probs[i] -= 1e-3
    sketchmultinom(probs)


def terrin(prov, list):
  for i in list:
    if i.__repr__ == prov.__repr__:
      return(True)
  return(False)

###Create the Object Class Territory
class Territory(object):
  def __init__(self, name):
    self.name = name
    self.neighbors = []
    self.control = 0
    self.civilians = []
    self.country = 0
    self.resources = 0
    self.attack = []
    self.exsupp = 0
    self.exstr = {}
    self.newterritory = 1
    self.borders = 0
    self.target = 0
###Check whether each civilian in a territory supports the dominant group, then also get an expected level of support for future period calculations
  def SupportCheck(self):
    self.exsupp = 0
    for i in self.civilians:
      i.SupportDecisions()
##Get expected support for future calculations
      self.exsupp += (i.support == i.territory.control)
###Wrapper that determines what type of decisions civilians are making
  def SupportDecisions(self):
    for i in self.civilians:
      if len(self.attack) == 0:
        i.InteriorSupport()
      else:
        i.BattleSupport()
###Get resources from a territory
  def Mobilize(self):
    self.SupportDecisions()
    resources = 0
    for i in self.civilians:
      if i.support == self.control:
        resources += 1
      if len(self.attack) == 0 and i.support != self.control:
        resources += CoerceMob
##Apply penalty to the territory for supporters of the opposing groups its part of the battlefield
      if len(self.attack) != 0 and i.support != self.control:
        if i.support in [j.control for j in self.attack]:
          resources -= DisloyalPenalty
        if i.support not in [j.control for j in self.attack]:
          resources += CoerceMob
    self.resources = resources
##Calculate expected resources you could apply in this territory
  def ExpectedStrength(self):
    if self.exsupp == 0:
      self.exsupp = (1 - self.control.ideo**2/2 - (1 - self.control.ideo)**2/2)*len(self.civilians)
    for i in self.country.armedactors:
      exst = 0
      for j in i.territory:
###Strength is discounted by distance
        if int(self.__repr__()) in self.country.dists[int(j.__repr__())].keys():
          dist = max(self.country.dists[int(j.__repr__())][int(self.__repr__())] - 1, 0)
          exst += delta**dist*(CoerceMob*len(j.civilians) + (1 - CoerceMob)*j.exsupp)
      self.exstr[i] = exst
###Assign territories on the battlefield to fight eachother
  def Attack(self, target):
###can only attack your neighbors
    if target not in self.neighbors:
      raise ValueError('This attack cant happen, water is too wet.')
    if self.control != target.control:
      self.attack.append(target)
      target.attack.append(self)
      self.country.attackhist[self.country.turn].append((self.control, target.control))
      target.target = 1
##Resolve a battle involving this territory
  def Battle(self):
    rsrs = {}
    if len(self.attack) > 0:
      ###Make a list of the actors fighting
      combatants = [self.control]
      for i in self.attack:
        combatants.append(i.control)
      ##Calculate their resources
      for i in combatants:
        res = 0
        trr = i.territory
        for j in trr:
          j.Mobilize()
          if int(self.__repr__()) in self.country.dists[int(j.__repr__())].keys():
            dist = max(self.country.dists[int(j.__repr__())][int(self.__repr__())] - 1, 0)
            res += delta**dist*j.resources
        rsrs[i] = res
    #Transform to probability
    probs = rsrs
    denom = sum(rsrs.values())
    for i in probs.keys():
      probs[i] /= denom
    #Who won?
    outcome = sketchmultinom(probs.values())
    winner = 0
    #Winner takes the territory
    for i in range(len(probs.keys())):
      if outcome[i] == 1:
        winner = list(probs.keys())[i]
        old = copy.deepcopy(self.control)
        if self.target == 1:
          self.control = list(probs.keys())[i]
        if self.control != old:
          newsupp = 0
          for i in self.country.armedactors:
            i.territory = []
          for i in self.country.provinces:
            i.control.territory.append(i)
          self.newterritory = 0 #cant victimize in the period you acquire a territory
          for j in self.civilians:
            newsupp += j.support == self.control
    #People died, it sucks
    self.target = 0 
    self.attack.append(self)
    for j in self.attack:
      dmax = min(len(j.civilians), battledeaths)
      for i in range(dmax):
        shuffle(j.civilians)
        j.civilians.pop()
###Ok, battle over, people not fighting anymore
    for i in self.attack:
      i.attack = []
    #All these people dying, who are we gonna mobilize next time?
  def Growth(self,rate = .1):
      add = round(len(self.civilians)*rate)
      for i in range(add):
        newbie = Civilian(ascii_lowercase[int((len(self.civilians) + i + 1)/26/26)] + ascii_lowercase[int((len(self.civilians) + i + 1)/26)%26] + ascii_lowercase[(len(self.civilians) + i + 1)%26] + self.name, self)
        self.civilians.append(newbie)
        self.country.civilians.append(newbie)
  ##Some people arent supporting you, that also sucks, maybe we can kill them?
  def Victimize(self):
    if len(self.civilians) > 0:
      supps = []
      nosupp = []
      number = 0 
  ##List of supporters and opponents in the territory
      for i in self.civilians:
        if i.support == self.control:
          supps.append(i)
          number += 1
        else:
          nosupp.append(i)
      if len(nosupp) > 0:
    #Based on how many supporters, you get a probability of selective vs indiscriminate violence
        selectprob = 1 - (((len(self.civilians) - len(supps))*(len(supps)))/len(self.civilians)**2)
        selective = np.random.binomial(1, selectprob, 1)
    ##If it works, you kill an opponent, everyone loves you!
        if selective == 1:
          nosupp.pop()
          self.control.VioHist += 1
          self.exsupp -= (1 + VicPenalty*len(nosupp))
    ##If it doesnt, you kill a supporter, man, supporting you seems like a bad idea
        if selective == 0 and len(supps) > 0:
          supps.pop()
          self.control.VioHist -= 1
          self.exsupp += VicPenalty*len(nosupp)
        self.civilians = supps + nosupp
        self.country.victimhist[self.country.turn].append((self.control,self))
###Does it make sense for the controller of this territory to attack a neighbor
  def ConsiderAttack(self, target):
###you dont attack yourself, thats stupid
    if self.control == target.control:
      return(False)
    self.ExpectedStrength()
    target.ExpectedStrength()
###Given expected strength, what is the probability of victory
    pwin = target.exstr[self.control]/(target.exstr[self.control] + target.exstr[target.control] + 0.00000001)
###How many resources are at stake
    R = len(target.civilians)
###What is the cost of war
    c = battledeaths
###How much do you benefit from good things happening to the owner of the territory
    sq = (.5 - abs(self.control.preference - target.control.preference))*2*self.control.ideo*R
    attack = (.5 - abs(self.control.preference - target.control.preference))*2*self.control.ideo*(R - c)*(1 - pwin) + pwin*(R - c) - c 
###Ok, is the cost of war higher than the benefit of war?
    return(attack > sq)
###Do any of your neighbors want to attack you?
  def BorderCheck(self):
    for i in self.neighbors:
      if i.ConsiderAttack(self):
        self.borders = 1
##Does the owner of the territory victimize
  def VicChoice(self):
    if len(self.civilians) > 0:
      self.BorderCheck()
  ###Cant do it when you just took over the territory, because you dont have supporters or opponents yet
      if self.newterritory == 0:
        return()
  ###What if this is interior territory
      if self.borders == 0: 
  ###Number of supporters and non-supporters
        suppnum = 0 
        for i in self.civilians:
          if i.support == 1:
            suppnum += 1
        nsuppnum = len(self.civilians) - suppnum
  ###How likely are you to kill the right people
        selectprob  = 1 - (((nsuppnum)*(suppnum))/len(self.civilians)**2)
        if nsuppnum > 0:
    ###What are the ranges of preferences for which people will support or oppose you
          if suppnum > 0:
            supprange = ((suppnum -1)/(suppnum + nsuppnum - 1 ) + self.control.VioHist*VicPenalty)*2
            victUtility = selectprob*((2*VicPenalty/(1 - supprange)*nsuppnum)*(1 - CoerceMob) - CoerceMob) - (1 - selectprob)*(VicPenalty/supprange * suppnum * (1 - CoerceMob) - 1)
          if suppnum == 0:
            victUtility = 2*VicPenalty*nsuppnum*(1 - CoerceMob) - CoerceMob         
    ###Ok, how does victimization help our ability to mobilize in the future? 
    ###If its a good idea, do it
          if victUtility > 0:
            self.Victimize()
  ###What if we're in a border territory
      if self.borders == 1:
        shouldVic = 0
  ###Look at all the potential threats
        for j in self.neighbors:
          if j.ConsiderAttack(self):
            suppnum = 0 
            for i in self.civilians:
              if i.support == 1:
                suppnum += 1
            nsuppnum = len(self.civilians) - suppnum
            if nsuppnum > 0:
              suppInt = ((suppnum -1 + 0.0001)/(suppnum + nsuppnum - 1+ 0.0001) + self.control.VioHist*VicPenalty)
    ###Who's likely to win the fight
              pwin = self.exstr[self.control]/(self.exstr[self.control] + self.exstr[j.control] + 0.000001)
    ###Between you and the attacker, civilians choose based on who's likely to win, so this is the cutpoint
              suppborder = self.control.preference*pwin + j.control.preference*(1 - pwin)
    ###Which side of the cutpoint depends on your orientation
              if self.control.preference > j.control.preference:
                loyalzone = 1 - suppborder
              if self.control.preference <= j.control.preference:
                loyalzone = suppborder
    ###Number of loyal civilians
              loyal = len(self.civilians)*loyalzone
    ###Opponent supporters
              nonloyal = len(self.civilians) - loyal
              nsuppnum = len(self.civilians) - suppnum
              selectprob  = 1 - ((nsuppnum)*(suppnum))/(len(self.civilians)**2)
    ###How victimization effects the resource balance
              if loyalzone == 0:
                lossFunction = selectprob*nonloyal*VicPenalty*(1 + DisloyalPenalty) 
              elif loyalzone == 1:
                lossFunction = -1
              else:
                lossFunction = selectprob*(nonloyal*VicPenalty/(1 - loyalzone)*(1 + DisloyalPenalty) + DisloyalPenalty) - (1 - selectprob)*(loyal*VicPenalty/(loyalzone)*(1 + DisloyalPenalty) - 1)
    ###Do it if its a positive effect 
              #print(lossFunction)
              if lossFunction > 0:
                self.Victimize()
                return("victimized")  
  def __repr__(self):
    return self.name


##Create an armed actor object
class ArmedActor(object):
  def __init__(self, name, gov):
    self.gov = gov
    ##Government doesnt share (ideo = 0) and has extreme preferences. Alternatively we could have them have moderate prefs.
    if self.gov == 1:
      self.preference = (1- govIdeo)*0.5 + govIdeo*uniform(0,1)
      self.ideo = 0.0
    else:
      self.preference = uniform(0,1)
      self.ideo = uniform(0,1)
    self.name = name
    self.territory = []
    self.VioHist = 0
    self.country = 0
  def AttackChoice(self):
    ###List of possible targets based on contiguity
    targets = {}
    for i in self.territory:
      poss = i.neighbors
      for j in poss:
    ###Cant attack yourself
        if j.control != self:
          targets[i] = j
    ###Parameters
    payoffs = []
    phi = self.ideo
    x1 = self.preference
    ####Check the utility of not attacking
    for i in targets.keys():
      R = len(targets[i].civilians)
      c = battledeaths
      combatants = [targets[i]] + targets[i].attack
      caps = []
      for q in combatants:
        caps.append(targets[i].exstr[q.control])
      if len(combatants) == 1:
        sq = (.5 - abs(x1 - targets[i].control.preference))*2*phi*R
      else:
        pwins = [x/(sum(caps) + 0.0000000000001) for x in caps]
        sq = 0
        for k in range(len(combatants)):
          sq += (.5 - abs(x1 - combatants[k].control.preference))*2*phi*(R*pwins[k] -c)
      ###Check the utility of attacking
      combatants.append(i)
      caps.append(targets[i].exstr[self])
      pwins = [x/(sum(caps) + 0.00000000001) for x in caps]
      attackUtil = 0
      for k in range(len(combatants)):
        attackUtil += (.5 - abs(x1 - combatants[k].control.preference))*2*phi*(R*pwins[k] -c)*(k != len(combatants)) + (R*pwins[k] -c)*(k == len(combatants))
      payoffs.append((i, targets[i], attackUtil - sq))
    ###Choose the best target
    if len(payoffs) > 0:
      besttarget = max(payoffs, key = lambda x:x[2])
    ###If attacking is better than doing nothing, attack
      if besttarget[2] > 0:
        i.Attack(targets[i])
 ###Go through all your territories deciding whether to victimize
  def ChooseVictimize(self):
    for i in self.territory:
      i.VicChoice()
  def __repr__(self):
    return self.name


###Create a civilian object, note: I'm shitty at object oriented programming and have had no inheritances...
class Civilian(object):
  def __init__(self, name, Territory):
###Civilians have preferences and locations
    self.preference = uniform(0, 1)
    self.name = name
    self.support = 0
    self.territory = Territory
    self.country = 0
  #Calculate whether civilians will support the armed actor in a non-battle territory
  def InteriorSupport(self):
    IncSupp = 0
    IncPref = self.territory.control.preference
    #Get a probability the other civilians are supporting the actor?
    for i in self.territory.civilians:
      if i != self:
        IncSupp += max(min(1 - abs(IncPref - i.preference) + self.territory.control.VioHist*VicPenalty,1),0)
    IncSupp /= len(self.territory.civilians)
    #Compare that to your ideological distance
    if IncSupp/2 > abs(IncPref - self.preference) - self.territory.control.VioHist*VicPenalty:
      self.support = self.territory.control
    else: 
      self.support = 0
##Who will civilians support during a battle
  def BattleSupport(self):
##Enumerate the options
    combatants = [self.territory.control]
    for i in self.territory.attack:
      combatants.append(i.control)
##What resources can each combatant bring to bear on the location
    rsr = {}
    for i in combatants:
      res = 0
      for j in i.territory:
        if j not in self.territory.attack:
          for k in j.civilians:
            res += max(min(1 - abs(i.preference - k.preference) + i.VioHist*VicPenalty,1),0)*(1 - CoerceMob) + CoerceMob
        if j in self.territory.attack:
          for k in j.civilians:
            diffs = 0
            for l in combatants:
              diffs += abs(k.preference - l.preference)
            es = max(min(1 - abs(i.preference - k.preference)/diffs + i.VioHist*VicPenalty,1),0)
            res += es - (CoerceMob -DisloyalPenalty)*(1-es)
      rsr[i] = res
      utils = rsr
##Calculate combatants ideology*preference distance
    for c in combatants:
      rsr[c] /= sum(rsr.values())
      utils[c]  = rsr[c]*(1 - abs(c.preference - self.preference) + c.VioHist*VicPenalty)
###Choose who to support
    max_key = max(utils, key=lambda k: utils[k])
    max_value = max(utils.values()) 
    choices = [key for key, value in utils.items() if value == max_value]
    if len(choices) > 0:
      self.support = 0
    else:
      self.support = shuffle(choices)[0]
  #Civilians can leave... Might want to have a risk of it going really badly and them getting killed tho
  def Flee(self, target):
    if self in self.territory.civilians:
      self.territory.civilians.remove(self)
    self.territory = target
    target.civilians.append(self)
  #Calculate if civilians want to flee
  def CheckFlee(self):
  #Need to not have supported the incumbent
    if self.support != self.territory.control:
    #Make a list of options, neighboring territories
      opt = {}
      current = abs(self.preference - self.territory.control.preference) - self.territory.control.VioHist*VicPenalty
      for i in self.territory.neighbors:
        opt[i] = abs(self.preference - i.control.preference) - i.control.VioHist*VicPenalty
      #Is the best alternative better than the SQ?
      try:
        turn = self.country.turn
      except AttributeError:
        turn = 0
      if min(opt.items(), key=operator.itemgetter(1))[1]*exp(3 - turn*3/turnlimit) < current:
        self.Flee(min(opt.items(), key = operator.itemgetter(1))[0])
  def __repr__(self):
    return self.name



##Object containing the other objects, used for populating everything
#Size = number of provinces, connectedness is how dense the adjacency matrix is
#Nactors is number of armed actors
#Population is average population of a territory


class Country(object):
  def __init__(self, name, size, connectedness, nactors, population):
    self.name = name
    adj = np.zeros(shape = (size, size))
    terr = []
    ##Generate a random adjacency matrix of appropiate size and create territories
    for i in range(size):
      adj[i] = np.append([0]*(i + 1),np.array(np.random.binomial(1, connectedness, size - i - 1)))
      terr.append(Territory(str(i)))
      terr[-1].country = self
    tadj = adj.transpose()
    adj = adj + tadj
    #Its symmetrical
    ##Oh no, isolates are no good, lets give them one random link.
    check = adj.sum(axis = 0)
    for i in range(len(check)):
      if check[i] == 0:
        fix = np.random.randint(size - 1)
        if fix >= i:
          fix += 1
        adj[i][fix] = adj[fix][i] = 1
    ##Make sure each territory has a record of their neighbors
    for i in range(size):
      nbrs = np.where(adj[i] > 0)[0].tolist()
      for j in nbrs:
        terr[i].neighbors.append(terr[j])
    self.adj = adj
    ##Calculate distances using networkx
    self.dists = nx.shortest_path_length(nx.from_numpy_matrix(self.adj))
    self.provinces = terr
    ##Now lets generate armed actors, the last one will be the government, each actor controls one territory
    actrs = []
    for i in range(nactors):
      actrs.append(ArmedActor(ascii_uppercase[i], i == (nactors - 1)))
      ##Make sure can reference country level stuff using an armed actor
      actrs[-1].country = self
      actrs[i].territory.append(terr[i])
      terr[i].control = actrs[i]
    ##Generate civilians in each territory
    civs = []
    for i in terr:
      pop = np.random.poisson(population)
      for j in range(pop):
        newbie = Civilian(ascii_lowercase[int(j/26/26)] + ascii_lowercase[int(j/26)%26] + ascii_lowercase[j%26] + i.name, i)
        i.civilians.append(newbie)
        civs.append(newbie)
        ##Make sure civilians also have a country
        civs[-1].country = self
      if i.control == 0:
      ##Except the government who controls all the other territories
        i.control = actrs[nactors - 1]
        actrs[nactors - 1].territory.append(i)
    self.civilians = civs
    self.armedactors = actrs
    self.actorlists = {}
    self.activeactors = self.armedactors
    self.turn = 0
    self.victimhist = {0:[]}
    self.attackhist = {0:[]}
  def ActiveActors(self):
      active = []
      for i in self.armedactors:
        if len(i.territory) > 0:
          active.append((i, i.territory))
      self.activeactors = active
      ####run one turn of the game
  def OneTurn(self):
      self.turn += 1
      self.victimhist[self.turn] = []
      self.attackhist[self.turn] = []
      self.ActiveActors()
      self.actorlists[self.turn] = self.activeactors
      ###Beliefs about the strength of each actor in each location
      for i in self.provinces:
        i.newterritory = 1
        i.ExpectedStrength()
      ###Actors go in a random order
      shuffle(self.armedactors)
      ###Each armed actor chooses who to attack
      for i in self.armedactors:
        i.AttackChoice()
      ###Civilians choose who to support
      for i in self.provinces:
        i.Mobilize()
      ###Battles resolve
      for i in self.provinces:
        i.Battle()
      ####Armed Actors choose who to victimize
      for i in self.armedactors:
        i.ChooseVictimize()
      ###Civilians choose whether to flee, and if so, where
      for i in self.civilians:
        i.CheckFlee()
      for i in self.provinces:
        if len(i.civilians) < 250:
          i.Growth(rate = .1)
      ####Game advances a turn
  def Game(self):
      gov = 0
      for i in self.armedactors:
        if i.gov:
          gov = i
      govterr = len(gov.territory)
      ###Game stops if government wins (has all the territory), loses (has no territory), or stalemate (certain number of turns with no winner)
      while self.turn <= turnlimit and govterr > 0 and govterr < len(self.provinces):
      ###Check governments territory
        govterr = 0
        for i in self.provinces:
          govterr += i.control.gov
      ###Run one turn of the game
        self.OneTurn()
  def __repr__(self):
      return self.name


##Playing around to see if stuff works


##Create a country with 10 territories, average connectivity of .2, 5 armed actors, 15 civilians on average per territory
