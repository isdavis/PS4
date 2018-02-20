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
  x
}
comparison(sample(1:3, 1), sample(1:3, 1))

# This works! Yay! 

#Now, to simulate the Monty Hall Problem. First, I create a validity checker called checkdoor. This was used back in the 
#previous problem set, and it stays the same, as all doors can only take on the values 1, 2, or 3. I add ways to check the other 
#slots as well; including the switch slot. 

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

#To test the condition, I put this in. This incorrect new door choice will throw an error

Incorrect<-new("Door", doorChoice=1, carDoor=4, switch=5)
Incorrect<-new("Door", doorChoice=1, carDoor=4, switch=TRUE)
Incorrect<-new("Door", doorChoice=0, carDoor=2, switch=TRUE)

#This one is correct and will not throw an error 

Ian<-new("Door", doorChoice=1, carDoor=1, switch=T)

#Now, we create the generic function

setGeneric("playGame", function(object="Door"){
  standardGeneric("playGame")
})

#Specifying what the generic method "playGame" actually does

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

switcher<-new("Door", doorChoice=1, carDoor=1, switch=T)
chump<-new("Door", doorChoice=1, carDoor=1, switch=F)

playGame(switcher)
playGame(chump)

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

#Play the game! 

playGame(Ian)