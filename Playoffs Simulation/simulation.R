##  Lyle Paskowitz, JHU
#   NBA GATE REVENUE SIMULATOR.
#   Full simulation of entire NBA playoff series
#   and resulting revenue using win probability
#   and gate revenue data provided by the application.

library("xlsx")
win_probabilities <- read.xlsx("win_probabilities.xlsx", 1, header=TRUE)
revenues <- read.xlsx("gate_revenues.xlsx",1,header=TRUE)

## Create revenue, conference and team objects for entire playoff series.

totalRevenue <- "numeric"

# Team Object
Team <- setClass("Team", slots=c(
  name = "character",
  seed = "numeric",
  conference = "character"
))

# East or West Conference
Conference <- setClass("Conference", slots = c(
  active = "vector"
))

#returns list of team objects initialized name and seed
makeTeamList <- function(conferenceName) {
  East <- c("East1", "East2", "East3", "East4", "East5", "East6", "East7", "East8")
  West <- c("West1", "West2", "West3", "West4", "West5", "West6", "West7", "West8")
  
  if (conferenceName == "East") conference = East
  if (conferenceName == "West") conference = West
  
  sapply(conference, function(x) {
    newTeam <- new("Team", name=x, seed=match(x, conference), conference = conferenceName)
  })
}

##HOME AND AWAY TEAMS
#Determines which team is initally at home, and which team is initially away
findHome <- function(team1,team2) {
  if (team1@seed < team2@seed) {
    home <- team1@name
    away <- team2@name
  } else if (team1@seed > team2@seed) {
    home <- team2@name
    away <- team1@name
  } else {
    testConference <- team1@conference
    if (testConference == "West") {
      home <- team1@name
      away <- team2@name
    } else {
      home <- team2@name
      away <- team1@name
    }
  }
  return(c(home,away))
}

##WIN PROBABILITY
#returns a list <probLoc> of probabilities for home team winning at (home,away) 
matchProb <- function(team1,team2,home,away) {
  if (team1@seed == team2@seed) {
    if (team1@conference == "West") {
      #West at home first if against equal seed
      probLoc <- which(c(win_probabilities$Team1 == team2@name & win_probabilities$Team2 == team1@name), arr.ind = TRUE)
    } else {
      probLoc <- which(c(win_probabilities$Team1 == team1@name & win_probabilities$Team2 == team2@name), arr.ind = TRUE)
    }
    #reverse order found in xlsx file (East is listed first)
    winProbHome <- (1 - win_probabilities$Prob1WinsAway[probLoc])
    winProbAway <- (1 - win_probabilities$Prob1WinsHome[probLoc])
    winProb <- c(winProbHome,winProbAway)
    return(winProb)
  } else if(team1@seed < team2@seed) {
    if(team1@conference == "West" & team2@conference == "East") {
      probLoc <- which(c(win_probabilities$Team1 == team2@name & win_probabilities$Team2 == team1@name), arr.ind = TRUE)
      winProbHome <- (1 - win_probabilities$Prob1WinsAway[probLoc])
      winProbAway <- (1 - win_probabilities$Prob1WinsHome[probLoc])
      winProb <- c(winProbHome,winProbAway)
      return(winProb)
    } else {
      probLoc <- which(c(win_probabilities$Team1 == home & win_probabilities$Team2 == away), arr.ind = TRUE)
    }
  } else if(team2@seed < team1@seed) {
    if(team2@conference == "West" & team1@conference == "East") {
      probLoc <- which(c(win_probabilities$Team1 == team1@name & win_probabilities$Team2 == team2@name), arr.ind = TRUE)
      winProbHome <- (1 - win_probabilities$Prob1WinsAway[probLoc])
      winProbAway <- (1 - win_probabilities$Prob1WinsHome[probLoc])
      winProb <- c(winProbHome,winProbAway)
      return(winProb)
    } else {
      probLoc <- which(c(win_probabilities$Team1 == home & win_probabilities$Team2 == away), arr.ind = TRUE)
    }
  }
  winProbHome <- win_probabilities$Prob1WinsHome[probLoc]
  winProbAway <- win_probabilities$Prob1WinsAway[probLoc]
  winProb <- c(winProbHome,winProbAway)
  return(winProb)
}

##LOCATION OF CURRENT GAME
#returns list of two teams in order (home, away)
findLoc <- function(currentLocation,numGames) {
  reverse <- switch(numGames,FALSE,FALSE,TRUE,TRUE,FALSE,TRUE,FALSE)
  if(reverse == TRUE) {
    return((rev(currentLocation)))
  }
  return((currentLocation))
}

#GATE REVENUE
#returns location-based revenue (e.g. round1-4)
gateRevenue <- function(location,homeTeam,numGames) {
  home <- location[[1]]
  gaterow <- which(c(revenues$HomeTeam == home), arr.ind = TRUE)
  if(location[[1]] == homeTeam[[1]]) {
    gateRound <- switch(numGames,1,2,99,99,3,99,4)
  } else {
    gateRound <- switch(numGames,99,99,1,2,99,3,99)
  }
  if(gateRound == 1) {
    return(revenues$Round1[gaterow])
  } else if(gateRound == 2) {
    return(revenues$Round2[gaterow])
  } else if(gateRound == 3) {
    return(revenues$Round3[gaterow])
  } else {
    return(revenues$Round4[gaterow])
  }
}

##GAME OUTCOME
#returns outcome from location-based win probability of home team
matchDay <- function(location,homeTeam,winProbs) {
  if(location[[1]] == homeTeam[[1]]) {
    probability <- winProbs[[1]]
  } else {
    probability <- winProbs[[2]]
  }
  outcome <- rbinom(1,1,probability)
  return(outcome)
}

##SERIES SIMULATION
#returns 4-game winner of series between two teams
series <- function(team1, team2) {
  seriesRevenue <- 0
  wins1 <- 0
  wins2 <- 0
  numGames <- 1
  
  homeLoc <- findHome(team1, team2)
  
  home <- homeLoc[[1]]
  away <- homeLoc[[2]]
  winProbs <- matchProb(team1,team2,home,away)

  while(wins1<4 & wins2<4) {
    location <- findLoc(homeLoc, numGames)
    revenue <- gateRevenue(location,homeLoc,numGames)
    seriesRevenue <- seriesRevenue + revenue
    outcome <- matchDay(location,homeLoc,winProbs)
    if(outcome == 1) {
      wins1 <- wins1 + 1
    } else {
      wins2 <- wins2 + 1
    }
    numGames <- numGames + 1
  }
  #assign to global object
  assign("totalRevenue", totalRevenue + seriesRevenue, envir = .GlobalEnv)
  
  if(wins1 == 4) {
    return(team1)
  }
  return(team2)
}

##Returns winner of each conference. 
#Seeded strong vs weak (e.g. first round 1vs8, 2vs7, etc.)
conferencePlayoffs <- function(conference) {
  numTeams <- length(conference@active)
  while(numTeams > 1) {
    currentActive = c()
    for(i in 1:(numTeams/2)) {
      team1 <- conference@active[[i]]
      team2 <- conference@active[[numTeams-i+1]]
      winner <- series(team1,team2)
      currentActive <- c(currentActive,winner)
    }
    conference@active <- currentActive
    numTeams <- length(conference@active)
  }
  return(conference@active[[1]])
}

##Playoff function. 
#Generates winning team ("champion") and returns total revenue.
playoff <- function() {
  assign("totalRevenue", 0, envir = .GlobalEnv)
  
  East = new("Conference", active=makeTeamList("East"))
  West = new("Conference", active=makeTeamList("West"))
  
  winnerEast <- conferencePlayoffs(East)
  winnerWest <- conferencePlayoffs(West)
  champion <- series(winnerEast,winnerWest)
  
  return(totalRevenue)
}


##Plotting. After sourcing the file, enter p to view the graph
# or visit http://rpubs.com/lyl_pask/gaterevenue.
library(plotly)

x <- c(playoff())
for (i in 2:3000) {
  x <- c(x,playoff())
}

stddev <- sd(x)

f <- list(
  family = "Helvetica Neue",
  size = 18,
  color = "#822433"
)

xlabel <- list(
  title = "Revenue",
  titlefont = f
)

ylabel <- list(
  title="Occurences/3000 Trials",
  titleFont = f
)

p <- plot_ly(x = ~x, type = "histogram") %>%
  layout(title="Predicted Gate Revenue of NBA Playoffs", xaxis = xlabel, yaxis = ylabel)

###Enter "p" into the console to view a plot of the gate revenues.