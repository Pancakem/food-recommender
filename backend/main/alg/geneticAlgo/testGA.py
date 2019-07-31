initialPopu = open('initPopu.txt','w')
PopuWithRank = open('popuWithFit.txt','w')
matingPoolAfterFirstSelection = open('afterFirstSelection.txt','w')
popuNewGen = open("populationAfterCrossover.txt",'w')
popuNewGenMutate = open("populationAfterMutation.txt",'w')

cuisineScore = {"indian":0.20,"italian":0.15,"afghani":0.08,"chinese":0.14, "Kenyan":0, "Nigerian":0.20, "ugandan":0.10, "Tanzanian":0.13}

temp = createInitialPopu(4, 10)
for item in temp:
    initialPopu.write("%s\n" %item)
temp1 = rankDishes(temp, cuisineScore)
for item in temp1:
    PopuWithRank.write("%s\n" %str(item))

matePool = selection(temp1,2)
for item in matePool:
    matingPoolAfterFirstSelection.write("%s\n" %str(item))

newGen = crossover(matePool,temp,5)
for item in newGen:
    popuNewGen.write("%s\n" %str(item))

newGenAfterMutation = mutatePopulation(newGen, 0.67)
for item in newGenAfterMutation:
    popuNewGenMutate.write("%s\n" %str(item))
# p1 = indian, italian, chinese
# p2 = italian, indian, chinese
# p3 = italian, indian, chinese
# p4 = italian, afghani, indian

# indian = 8+2*5+3=21
# italian = 8*3+5 = 29
# afghani = 5
# chinese = 3*3 = 9

# total = (8+5+3)*4 = 64


