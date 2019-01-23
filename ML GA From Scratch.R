rm(list=ls(all=TRUE))

#if package not installer 
# install.packages("rlist")

library(rlist)
set.seed(1)

# Data
Gems <- data.frame(
        Color = c("Red", "Blue", "Purple", "Orange", "Green", "Pink", "White", "Black", "Yellow"),
        Weight = round(runif(9,0.5,5),2),
        Value = round(abs(rnorm(9,0,5))+0.5,2)
)

# Task: Gem selection. 
# Aim: Get highest combined value.
# Restriction: Max weight of the gem combined = 10. 


InitilzePop <- function(size){
  #pop<- lapply(1:size, function(pop) round(runif(nrow(Gems),0,1),0))
  pop <- (t(sapply(1:size, function(pop) round(runif(nrow(Gems),0,1),0))))
  colnames(pop)<- Gems$Color
  return(pop)
}

# fitness function
fitness_function <- function(population, weightRestriction){
  
  score<-0
  for (i in 1:nrow(population)) {
    temp <-apply(Gems[which(population[i,1:nrow(Gems)]==1),2:3],2, sum)
    
    
    score[i]<- ifelse(temp[1]>weightRestriction, 0 , temp[2])
    
  }
   pop<- cbind(population,score)
   pop<-pop[order(score, decreasing = T),]
  return(pop)

}


#Selection, Breeding, crossover, mutation

#mutation function
mutate <- function(dna){
  #inverses one random genome in the chromosome
 ind <- sample(1:length(dna),1)
 dna[ind] <- ifelse(dna[ind]==1,0,1)
  return(dna) 
}

#crossover function
crossover <- function(dna1, dna2){
  len <- 1:ceiling(length(dna1)/2)
  offspring <-c(dna1[len],dna2[-len])
  return(offspring)
}


# Making a function to combine genetic information from parents to create offspring
Breed <- function(parent1, parent2, population){
  dna1 <- population[parent1,-ncol(population)]
  dna2 <- population[parent2,-ncol(population)]
  
  H <- 0; W<- 0; S<- 0
  # crossover 95% of the time 
  if(runif(1,0,100)>10) {
    offspring<-crossover(dna1,dna2)
    
  }else{
    # 5% chance for mutation
    offspring <-  mutate(dna1)
  }
  
  return(offspring)
}


# Function to breed the next generation
BreedPopulation <- function(population){
  population<-  fitness_function(population, 10)
  NewPopulation <- list()
  #only top x% of the list allowed the breed.
  len <- floor(nrow(population)/4)
  
  for (i in 1:(len-1)) {
    
    NewPopulation<- list.append(NewPopulation,Breed(i,i+1,population ))
    NewPopulation<- list.append(NewPopulation,Breed(i+1,i,population ))
    NewPopulation<- list.append(NewPopulation,Breed((len+1-i),i,population ))
    NewPopulation<- list.append(NewPopulation,Breed(i,(len+1-i),population ))
    
  }
  NewPopulation<- list.append(NewPopulation, InitilzePop(1))
  NewPopulation<- list.append(NewPopulation, InitilzePop(1))
  NewPopulation<- list.append(NewPopulation, InitilzePop(1))
  NewPopulation<- list.append(NewPopulation, InitilzePop(1))
  
  pop<-Reduce(rbind, NewPopulation)
  
  return(pop)
}

# Create one function to handle the whole generational process
Genetic_Algorithm <- function(popsize,Gen,BreakCond){
  
  #initilize the population
  population <-InitilzePop(popsize)
  
  #initialize a matrix to store performance data in
  Performance <- matrix(NA, nrow=Gen,ncol=2)
  colnames(Performance) <- c("Average","Best")
  rownames(Performance) <- paste("Generation",seq(nrow(Performance)))
  
  breakCond <- 0
  
  #creating the frame for the plot
  plot(NULL, xlim =c(1,Gen), ylim = c(0,45), ylab = "No. of stones", xlab = "Generation")
  legend("bottomright", c("Best individual", "Average individual"), pch = 20, col = c(1, 2),bty = "n")
  
  #Starting the Generational process
  for (j in 1:Gen) {
    # create the new population
    population <-  BreedPopulation(population)
    
    #In this example, weightRestriction is set to 10 
    score <- fitness_function(population,10)[,nrow(Gems)+1]
    
    #store the performance of the jth generation
    Performance[j,] <- c(mean(score),max(score))
    
    # update plot 
    if(j %in% seq(1,Gen,Gen/20)) {
    Sys.sleep(0.1)
    lines(Performance[,2])
    lines(Performance[,1], col = "red")
    }
    Sys.sleep(0)
    
    
    #Stops the process if the result has not improved the last x generations
    if(Performance[j,2] > max(Performance[1:(j-1),2])){
      #if the performance of this generation is better than the previous generations, the breakCond reset to zero
      breakCond <- 0
    }else
    {
      #if the performace of this generation is not better. Add 1 to the breakCond. 
      breakCond <- breakCond+1
    }
    # if breakCond reaches the Break condition, the process stops.
    if(breakCond >= BreakCond){ break}
    
  }
  
  #Organizing all data into a list
  result <- list(GenerationPerformance = Performance, Solution = list(Genes=population[1,],Result=apply(Gems[which(population[1,] ==1),2:3],2,sum)))
  
  return(result)
  
}


Genetic_Algorithm(popsize = 20, Gen = 200,BreakCond = 150)




