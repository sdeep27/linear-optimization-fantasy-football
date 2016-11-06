library(lpSolve)
library(lpSolveAPI)

getSquad = function(csvfile, delPlayer = NULL, minSalary = 0){
  rawTable = read.csv(csvfile, stringsAsFactors = F)
  #creating binary variables
  nfl <-cbind(rawTable, model.matrix(~ Pos + 0, rawTable))
  #creating flex binary column
  nfl$PosFlex <- 0
  for (row in 1:length(nfl$PosRB)) {
    if (nfl$PosRB[row] > 0 || nfl$PosWR[row] > 0 || nfl$PosTE[row] > 0){
      nfl$PosFlex[row] <- 1
    }
  }
  #set any player we don't want to include in analysis points to 0. 
  for (player in delPlayer){
    index = which(nfl$Name == player)
    nfl[index, 9] <- 0
  }
  #create constraints
  cons<- c(nfl$PosQB, nfl$PosRB, nfl$PosWR, nfl$PosTE, nfl$PosFlex, nfl$PosDef, nfl$DK.salary)
  #create matrix and double it for both sides of constraints
  con <- matrix(cons, nrow=7, byrow=TRUE)
  allcons <- rbind(con, con)
  #set right hand side coefficients for both max and min
  maxrhs <- c(1,3,4,2,7,1,"50000") #max salary,qb,rb,wr,te,flex,def
  minrhs <- c(1,2,3,1,7,1, minSalary) #minsalary, #qb, rb, wr, te, flex, def
  maxrel <- c("<=","<=","<=","<=","<=","<=","<=")
  minrel <- c(">=", ">=",">=",">=",">=",">=",">=")
  #all final variables
  obj <- nfl$DK.points
  rel <- c(maxrel,minrel)
  rhs <- c(maxrhs, minrhs)
  mylp<- lp("max",obj,allcons,rel,rhs,all.bin=TRUE)
  #creating table just for the players that are in optimal solution
  solindex <- which(mylp$solution==1)
  optsolution <- nfl[solindex,]
  #cleaning up table and adding sums
  optsolution <- optsolution[,c(-1,-2,-3,-7,-11:-16)]
  optsolution[10,] <- c("","","","", sum(optsolution$DK.points), sum(optsolution$DK.salary))
  #write to csv or View in R
  View(optsolution)
  write.csv(optsolution, paste("Optimized", csvfile), row.names = F)
}
