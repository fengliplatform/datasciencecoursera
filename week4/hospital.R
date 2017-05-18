# week 4
# hospital

setwd("~/course/ds_coursera/week4")

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
dim(outcome)
str(outcome)

names(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
#hist(outcome[, 11])

valid_states <- unique(outcome[,"State"])
valid_outcomes <- c("heart attack", "heart failure", "pneumonia")

given_state <- "TX"
given_outcome <- "heart attack"

if (!given_state %in% valid_states) {
  stop("invalid state", call. = TRUE)
}

if (!given_outcome %in% valid_outcomes) {
  stop("invalid outcome", call. = TRUE)
}

if (given_outcome == "heart attack") {
  outcome_col <- 11
  outcome_col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
} else if (given_outcome == "heart failure") {
  outcome_col <- 17
} else if (given_outcome == "pneumonia") {
  outcome_col <- 23
}

state_outcome <- outcome[outcome$State == given_state,]
state_outcome
state_outcome_rm_na_for_given_outcome <- state_outcome[complete.cases(state_outcome[,outcome_col]),]
names(state_outcome)
names(state_outcome_rm_na_for_given_outcome)
min_outcome <- min(state_outcome_rm_na_for_given_outcome[outcome_col])
min_outcome
best_hospitals <- state_outcome_rm_na_for_given_outcome[state_outcome_rm_na_for_given_outcome[outcome_col] == min_outcome,]
best_hospitals
dim(best_hospitals)
best_hospitals_names <- best_hospitals$Hospital.Name
sort(best_hospitals_names)




best <- function(state, outcome) {
  given_state <- state
  given_outcome <- outcome
  
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  valid_states <- unique(outcomes[,"State"])
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  
  if (!given_state %in% valid_states) {
    stop("invalid state", call. = TRUE)
    return()
  }
  
  if (!given_outcome %in% valid_outcomes) {
    stop("invalid outcome", call. = TRUE)
    return()
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  if (given_outcome == "heart attack") {
    outcome_col <- 11
  } else if (given_outcome == "heart failure") {
    outcome_col <- 17
  } else if (given_outcome == "pneumonia") {
    outcome_col <- 23
  }

  state_outcome <- outcomes[outcomes$State == given_state,]

  state_outcome[,outcome_col] <- suppressWarnings(as.numeric(state_outcome[,outcome_col]))
  
  state_outcome_rm_na_for_given_outcome <- state_outcome[complete.cases(state_outcome[,outcome_col]),]

  min_outcome <- min(state_outcome_rm_na_for_given_outcome[,outcome_col])

  best_hospitals <- state_outcome_rm_na_for_given_outcome[state_outcome_rm_na_for_given_outcome[outcome_col] == min_outcome,]
  best_hospitals_names <- best_hospitals$Hospital.Name
  sort(best_hospitals_names)
}

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")







