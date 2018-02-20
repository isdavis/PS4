#PS4

#1: This is code that we are supposed to fix. The problem, just from a quick glance, is that it sets "doorthing1" equal to "doorthing2". 
#This will cause every iteration of it to return "true". Also, the names are very strange. 
myFunction<-function(doorthing, doorthing2, x){
  doorthing1<-doorthing2<-sample(1:3, 1)
  if (doorthing1==doorthing2){ x<-TRUE } else { x==FALSE }
  x
}
myFunction(sample(1:3, 1), sample(1:3, 1))

#This is an edited version. I renamed the function, took out the x input, and renamed the variables. 
#I also  return the actual values that doorChoice and carDoor take on, as a form of debugging. 
comparison<-function(doorChoice, carDoor){
  x<-FALSE
  if (doorChoice==carDoor){ x<-TRUE }
  print(carDoor)
  print(doorChoice)
  return(x)
}
comparison(sample(1:3, 1), sample(1:3, 1))

# This works! Yay! 

#Now, to simulate the Monty Hall Problem. First, I create a validity checker called checkdoor. This was used back in the 
#previous problem set, and it stays the same, as all doors can only take on the values 1, 2, or 3. I add ways to check the 
#carDoor slot as well, since it can also only take on values of 1, 2, or 3. 

checkDoor<-function(object) {
  errors<-character()
  d<-object@doorChoice
  c<-object@carDoor
  s<-object@switch
  if(d!=1 & d!=2 & d!=3) {errors<-"The contestant may only choose doors 1-3."}
  if(c!=1 & c!=2 & c!=3) {errors<-"The car can only be behind doors 1-3."}
  if(length(errors)==0) return(TRUE) else return(errors)
}

#Then the setclass to specify what constitutes a "Door"

setClass("Door", representation(doorChoice="numeric", carDoor="numeric", switch="logical"), validity=checkDoor)

#To test the condition, I put this in. These incorrect new doors will throw an error

Incorrect<-new("Door", doorChoice=1, carDoor=4, switch=5)
Incorrect<-new("Door", doorChoice=1, carDoor=4, switch=TRUE)
Incorrect<-new("Door", doorChoice=0, carDoor=2, switch=TRUE)

#This one is correct and will not throw an error. Also, I win a car :) 

Ian<-new("Door", doorChoice=1, carDoor=1, switch=T)

#Now, we create the generic function

setGeneric("playGame", function(object="Door"){
  standardGeneric("playGame")
})

#Specifying what the generic method "playGame" actually does. 
#There are several browser locations that I included to show where I checked the function to ensure the values were correct.

#By checking in those places, I can see that I am removing a valid door all the time (there was a point at which I accidentally
#repeated "removed" instead of "finalchoice" in line 75, and my browser pauses were able to show me that "removed" changed to 
#an invalid value during the running of the function. My earlier commits should have shown this, but it looks like they didn't 
#go through :( 

setMethod("playGame", "Door", function(object){
  car<-sample(1:3, 1)
  initialChoice<-sample(1:3, 1)
  object@doorChoice<-initialChoice
  object@carDoor<-car
  #browser()
  if(object@switch==TRUE){
    removed<-sample(1:3, 1)
    while(removed==car | removed==initialChoice){removed<-sample(1:3, 1)}
    #browser()
    finalChoice<-sample(1:3, 1)
    while(finalChoice==removed){finalChoice<-sample(1:3, 1)} 
    #browser()
    object@doorChoice<-finalChoice
  }
  winner<-FALSE
  #browser()
  if(object@doorChoice==object@carDoor) { winner<-TRUE}
  return(winner)
})

#I also tried debugging using the debug function, but I think I prefer placing specific "browser()" lines. It helps me to see
#exactly where I am looking and it is a little quicker. 

debug(playGame)
playGame(Ian)
undebug(playGame)

#I created two new doors, one where switch=TRUE, one where switch=F. The one who does not switch is, obviously, a chump. 

switcher<-new("Door", doorChoice=1, carDoor=1, switch=T)
chump<-new("Door", doorChoice=1, carDoor=1, switch=F)

s<-playGame(switcher)
c<-playGame(chump)

#These are 2 for loops that I included to test the ratios of wins to losses for each strategy. I don't know if my earlier 
#commits went through, but the first time I did the function I did it incorrectly and was able to see that when each strategy
#returned the same proportion of successes in these for loops. 

wins<-0
for(i in 1:1000) {
  v<-playGame(switcher)
  if(v==TRUE){wins<-wins+1}
}
wins

wins<-0
for(i in 1:1000) {
  v<-playGame(chump)
  if(v==TRUE){wins<-wins+1}
}
wins

#Those for loops work, but since the assignment does not allow them, I will now do the same thing but with apply functions. 
#I first create functions that will take in a Door and output a Door with random door choice/car door and a specific switch value. 
#"Switchify" makes it so that the output Door has switch=TRUE

setGeneric("switchify", function(object="Door"){
  standardGeneric("switchify")
})
setMethod("switchify", "Door", function(object){
  object@switch<-TRUE
  object@doorChoice<-sample(1:3, 1)
  object@carDoor<-sample(1:3, 1)
  return(object)
})

#UNswitchify does the opposite: it takes in a Door and outputs a Door with switch=FALSE. 

setGeneric("UNswitchify", function(object="Door"){
  standardGeneric("UNswitchify")
})
setMethod("UNswitchify", "Door", function(object){
  object@switch<-FALSE
  object@doorChoice<-sample(1:3, 1)
  object@carDoor<-sample(1:3, 1)
  return(object)
})

#I now install plyr so that I can later use the laply function

install.packages("plyr")
library(plyr)

#A vector of new, blank Doors that is 1000 doors long. 
doors <- lapply( rep("Door", 1000), new)

#By iterating switchify over my vector of 1000 doors, I now have a list of 1000 doors that all have switch=TRUE
switchList<-lapply(Doors, switchify)

#Using laply over my list of 1000 switch=TRUE doors, I play the game 1000 times and switch=TRUE for all of them. 
#By using laply, the output is a vector of logicals, where each entry is TRUE if the contestant won the car on that iteration
#and FALSE if the contestant failed to win the car on that iteration
winsSwitch<-laply(switchList, playGame)

#By summarizing it, we can see the total number of TRUE and FALSE values in the vector, and thus the number of times 
#the contestant won the car using this strategy. 
summary(winsSwitch)

#Now, I create a new vector of 1000 doors, except this time I use the UNswitchify function instead of the switchify function
#This makes a vector of 1000 doors with switch=FALSE, and using the same laply process, I can look at the summary to see 
#how many times the contestand won the car using this strategy. 
doors <- lapply( rep("Door", 1000), new)
NOswitchList<-lapply(doors, UNswitchify)
winsNOswitch<-laply(NOswitchList, playGame)
summary(winsNOswitch)

#It works! The switch list has around 500 TRUE values, and the NO switch list has around 350.
#Thus, switching is better! We have solved the Monty Hall problem. 