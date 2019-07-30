import json
import random
from copy import deepcopy
import numpy as np
import pandas as pd
import math
import os
# import matplotlib.pyplot as plt

with open('dishMap.json') as menu_Data:
    menuData = json.load(menu_Data)

with open('genInfo.json') as gen_Info:
    genInfo = json.load(gen_Info)

## This part creates the chromosome

# NOT USING THIS FUNCTION CURRENTLY
# This function limits to starting dishes more so I am not using it currently
def randSeq(n, a, b, sum):
    """
    Generate the random sequence of quantities for the dishes
    """
    found = False
    while not found:
        totalNum = 0
        numSum = 0
        sequence = []
        while totalNum < n and numSum < sum:
            r = random.randint(a,b)
            numSum += r
            totalNum += 1
            sequence.append(r)
        if numSum == sum:
            while totalNum < n:
                sequence.append(0)
                totalNum+=1
            found = True 
    return sequence

# This function gives a more distributed result so using this currently
def randSeq2(n, sum):
    """
    Generate the random sequence of quantities for the dishes

    n -> total number of dishes in menu
    sum -> total number of dishes that can be ordered

    returns a list containing quantity of dish to be ordered
    with each index matching the index of corresponding dish 
    in menu database
    """
    sequence = [0]*n
    i=0
    while i < sum:
        r = random.randint(1,n)
        sequence[r-1]+=1
        i+=1
    return sequence


def createChromosome( totQty ):
    """
    Creates the chromosome with Qty assigned to Each Dish such that
    sum of all Qty equals to the number of dishes to be ordered

    totQty = Number of Dishes to be Ordered

    returns chromosome of dish id and corresponding quantity
    """
    chromosome = []
    qtySeq = randSeq2(genInfo["totalDishes"],totQty)
    i=0
    for key in menuData:
        chromosome.append(Dish(key,qtySeq[i]))
        i+=1
    return chromosome

## This part does the mutation

def mutate(individual, mutationRate):
    """
    This function performs scramble mutation in an individual

    individual -> chromosome 
    mutationRate -> the rate at which mutation is supposed to occur

    returns chromosome after mutation if mutation occurs, else returns
    original chromosome
    """
    chromosomeLength = len(individual)
    k = random.random()
    # print(k)
    if(k <= mutationRate):
        startPtr = random.randint(0, chromosomeLength-1)
        # print (startPtr)
        endPtr = random.randint(0, chromosomeLength-1)
        # print (endPtr)
        if(endPtr < startPtr):
            temp = endPtr
            endPtr = startPtr
            startPtr = temp
        tempArrQty = []
        for i in range(startPtr,endPtr+1):
            tempArrQty.append(individual[i].qty)
        tempArrQty = random.sample(tempArrQty,len(tempArrQty))
        for i in range(startPtr,endPtr+1):
            individual[i].qty = tempArrQty[i-startPtr]
    return individual

def mutatePopulation ( population, noOfElite, mutationRate):
    """
    This function performs mutation on entire population

    population -> pool of chromosomes of current generation
    mutationRate -> rate at which mutation is supposed to occur

    returns new population pool after mutation of original pool
    """
    popuLen = len(population)
    newPopu = []
    for i in range(0,noOfElite):
        newPopu.append(deepcopy(population[i]))
    for i in range(noOfElite,popuLen):
        mutantIndi = mutate(population[i],mutationRate)
        newPopu.append(mutantIndi)
    return newPopu

## This part does the selection

def selection(population, noOfElite):
    """
    This selection algorithm uses Roulette Wheel Algorithm to
    determine mating pool for selection

    population -> pool of population of chromosomes in sorted order
                  (max fitness chromosome at 0th index)
    noOfElite -> Number of chromosomes that we want to persist in
                 next generation due to elitism

    returns mating pool consisting of array of indices of chromosomes
    belonging to current generation
    """
    matingPool = []
    popuPd = pd.DataFrame(np.array(population), columns=["Index", "Fitness"])
    popuPd["cum_sum"] = popuPd.Fitness.cumsum()
    popuPd["cum_perc"] = 100*popuPd.cum_sum/popuPd.Fitness.sum()

    for i in range(0, noOfElite):
        matingPool.append(population[i][0])
    for i in range(0, len(population) - noOfElite):
        pieToBePickedInCircle = 100*random.random()
        for j in range(0, len(population)):
            if pieToBePickedInCircle <= popuPd.iat[j, 3]:
                matingPool.append(population[j][0])
                break
    return matingPool


## Coming up with the next generation

def nextGeneration(currentPopulation, cuisineScore, maxQtyToBeOrdered, noOfElite, mutationRate, graphPoints, answer):
    """
    This function applies genetic operators over current generation to produce 
    new generation

    currentPopulation -> current pool of chromosomes of current generation
    cuisineScore -> Object consisting of cuisine score as rated by the users
    noOfElite -> no of Elite chromosomes that are to be persisted in next generation
    mutationRate -> rate at which mutation is to take place

    returns new generation of chromosomes
    """
    populationRanked = rankDishes(currentPopulation, cuisineScore, maxQtyToBeOrdered)
    graphPoints.append(populationRanked[0][1])
    
    if(answer.fitness<populationRanked[0][1]):
        answer.ans = deepcopy(currentPopulation[populationRanked[0][0]])
        answer.fitness = populationRanked[0][1]

    selectedPopulationPool = selection(populationRanked, noOfElite)
    populationAfterCrossover = crossover(
        selectedPopulationPool, currentPopulation, noOfElite)
    nextGen = mutatePopulation(populationAfterCrossover, noOfElite, mutationRate)
    return nextGen



## Computing Fitness

class Fitness:
    """
    Define the characteristics required to calculate the fitness function
    and calculate the fitness of chromosome
    """

    def __init__(self, dishes, cuisineScore, MaxQtyToBeOrdered):
        """
        dishes = list of dishes i.e. chromosome
        cuisineScore  = the score obtained per cuisine using 
                        preferences of the group members     
        totCuisineScore = sum of all cuisine scores
        ratings = sum of all ratings for which qty > 0
        cost = cost of dishes for which qty > 0
        fitness = value of fitness for this chromosome
        """
        self.dishes = dishes
        self.ratings = 0
        self.cost = 0
        self.fitness = 0
        self.cuisineScore = cuisineScore
        self.MaxQtyToBeOrdered = MaxQtyToBeOrdered
        self.totCuisineScore = 0
        self.fitnessSet = False
        for i in cuisineScore:
            self.totCuisineScore += cuisineScore[i]

    def calcFitness(self):
        """
        Calculate Fitness for a Chromosome
        cuisineQty = object holding individual qty per cuisine
        cuisineRating = object holding individual ratings per cuisine
        cuisineCost = object holding individual cost per cuisine
        """
        cuisineQty = {}
        cuisineRating = {}
        cuisineCost = {}
        for i in genInfo["cuisines"]:
            cuisineQty[i] = 0
            cuisineRating[i] = 0
            cuisineCost[i] = 0

        totalQty = 0

        for i in range(0, len(self.dishes)):
            dishId = self.dishes[i].id
            quantity = self.dishes[i].qty
            totalQty += quantity
            if quantity > 0:
                # Copy values of this dish from menuData
                tempCuisine = menuData[dishId]["cuisine"]
                tempRating = menuData[dishId]["rating"]
                tempPrice = menuData[dishId]["price"]

                # Add quantity to respective Cuisine
                cuisineQty[tempCuisine] += quantity

                # Add rating to respective Cuisine as well as total Rating sum
                self.ratings += tempRating*quantity
                cuisineRating[tempCuisine] += tempRating*quantity

                # Add cost to respective Cuisine as well as total Rating sum
                self.cost += tempPrice*quantity
                cuisineCost[tempCuisine] += tempPrice*quantity

        if (totalQty == 0):
            self.fitness = 0
            return self.fitness

        # for i in self.dishes:
        #     if (i.qty>0):
        #         PopuWithFitness.write("%s " %str(i))

        for i in genInfo["cuisines"]:
            tempQtyFit = float(1/float(1+math.exp(-1*(cuisineQty[i]-1))))
            tempRatingFit = float(cuisineRating[i]/self.ratings)
            tempCostFit = float(cuisineCost[i]/self.cost)
            tempCuisineScoreFit = float(
                self.cuisineScore[i]/self.totCuisineScore)
            # self.fitness += (tempQtyFit*(2*tempRatingFit -
            #                              tempCostFit)*tempCuisineScoreFit)
            tempCuisineFitness = tempCuisineScoreFit*(-2*tempCostFit+3*tempRatingFit+2*tempQtyFit)
            # PopuWithFitness.write("%s %s \n" %(i,str(tempCuisineFitness)))
            self.fitness += tempCuisineFitness

        if (totalQty > self.MaxQtyToBeOrdered):
            self.fitness = -1*self.fitness
        
        # PopuWithFitness.write("Fitness: %s\n\n**\n" %self.fitness)

        return self.fitness

    def getFitness(self):
        if not self.fitnessSet:
            self.fitness = float(self.calcFitness())
            self.fitnessSet = True
        return self.fitness


## Build a dish
class Dish:
    """
    Allele in chromosome consists of this Dish which has
    id -> Dish ID
    qty -> Quantity of this Dish
    """
    # Assign the Allele with DishID and Quantity in suggestion
    def __init__(self, id, qty):
        self.id = id
        self.qty = qty

    # Return Quantity of current Dish
    def Quantity(self):
        return self.qty

    # Return the information of the dish
    def __repr__(self):
        # dishId = "Dish Id:"+str(self.id)
        # dishname = "Dish Name:"+str(menuData[self.id]["dishName"])
        # restname = "Restaurant Name:"+str(menuData[self.id]["restName"])
        # price = "Price:"+str(menuData[self.id]["price"])
        # rating = "Rating:"+str(menuData[self.id]["rating"])
        # quantity = "Quantity:" + str(self.qty)
        # totCost = "Total Cost:" + str(menuData[self.id]["price"]*self.qty)
        # return '%s %s %s %s %s %s %s'%(dishId,dishname,restname,price,rating,quantity,totCost)

        dishId = "Dish Id:"+str(self.id)
        quantity = "Quantity:" + str(self.qty)
        # totCost = "Total Cost:" + str(menuData[self.id]["price"]*self.qty)
        # return '%s %s %s'%(dishId,quantity,totCost)
        return '%s %s'%(dishId,quantity)

## Perform crossover

def breed(chromosome1, chromosome2):
    """
    This crossover function performs uniform crossover between
    two chromosomes
    """
    for i in range(0,len(chromosome1)):
        k = random.random()
        # print (k)
        if(k>=0.5):
            temp = chromosome1[i]
            chromosome1[i] = chromosome2[i]
            chromosome2[i] = temp
    # print (chromosome1)
    # print (chromosome2)
    

def crossover( matingPool, origChromosomes, noOfElite):
    """
    This crossover function performs uniform crossover between
    two chromosomes

    matingPool -> list consisting of indices from original Chromosomes
    origChromosomes -> consists of list of original Chromosomes
    noOfElite -> number of times we need to perform breed function

    returns list containing chromosome pool of new generation
    """
    sizeOfMatingPool = len(matingPool)
    sizeOfChromosome = len(origChromosomes[0])
    newChromosome = []
    # Copy the elite chromosomes and persist them to next generation
    for i in range(0, noOfElite):
        newChromosome.append(deepcopy(origChromosomes[matingPool[i]]))
    LowerPtrForCrossover = noOfElite
    UpperPtrForCrossover = sizeOfMatingPool-1
    while (LowerPtrForCrossover<UpperPtrForCrossover):
        ind1 = matingPool[LowerPtrForCrossover]
        ind2 = matingPool[UpperPtrForCrossover]
        tempChromo1 = deepcopy(origChromosomes[ind1])
        tempChromo2 = deepcopy(origChromosomes[ind2])
        breed(tempChromo1,tempChromo2)
        newChromosome.append(tempChromo1)
        newChromosome.append(tempChromo2)
        LowerPtrForCrossover+=1
        UpperPtrForCrossover-=1
    if(LowerPtrForCrossover==UpperPtrForCrossover):
        ind1 = matingPool[LowerPtrForCrossover]
        tempChromo1 = deepcopy(origChromosomes[ind1])
        newChromosome.append(tempChromo1)

    return newChromosome


## create initial population
def createInitialPopu(maxDishes, initialPopuSize):
    """
    Creates initial set of population

    maxDishes -> total number of dishes that are to be ordered

    initialPopuSize -> Size of initial population pool
    """
    population = []
    for i in range(1, initialPopuSize+1):
        population.append(createChromosome(maxDishes))
    return population


## rank the population
def rankDishes(population, cuisineScore, MaxQtyToBeOrdered):
    """
    Ranks the population using the Fitness class
    and the method to calculate fitness

    population -> initial population of chromosomes
    cuisineScore -> object consisting of score that is rated
                    by users with key as cuisine name

    returns array consisting of (chromosome index,fitness) pairs
    in sorted order (chromosome with max fitness at 0th index)
    """
    fitnessResults = {}
    for i in range(0,len(population)):
        fitnessResults[i] = Fitness(population[i], cuisineScore, MaxQtyToBeOrdered).calcFitness()
    return sorted(fitnessResults.items(), key=lambda x: x[1], reverse=True)


class DotDict(dict):
    def __getattr__(self, key):
        return self[key]
    def __setattr__(self, key, val):
        if key in self.__dict__:
            self.__dict__[key] = val
        else:
            self[key] = val

def geneticAlgorithm( maxDishes, initialPopulationSize, cuisineScore, noOfElite, mutationRate, generations ):
    popu = createInitialPopu( maxDishes, initialPopulationSize)

    graphPoints = []
    answer = DotDict([("ans",-1),("fitness",0)])
    for i in range(0, generations):
        popu = nextGeneration( popu, cuisineScore , maxDishes, noOfElite, mutationRate, graphPoints, answer ) 

    lastGenRanked = rankDishes(popu, cuisineScore, maxDishes)
    graphPoints.append(lastGenRanked[0][1])

    if(answer.fitness<lastGenRanked[0][1]):
        answer.ans = popu[lastGenRanked[0][0]].copy()
        answer.fitness = lastGenRanked[0][1]

    # plt.plot(graphPoints)
    # plt.ylabel('Best Fitness')
    # plt.xlabel('Generation')
    # plt.show()
    print( answer.ans )
    print("Fitness: %s" %str(answer.fitness))
    return answer.ans