# week 4
# hospital part 1

setwd("~/course/ds_coursera/week4")

best <- function(state, outcome) {
  given_state <- state
  given_outcome <- outcome
  
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomes <- outcomes[,c(2,7,11,17,23)]
  
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
    outcome_col <- 3
  } else if (given_outcome == "heart failure") {
    outcome_col <- 4
  } else if (given_outcome == "pneumonia") {
    outcome_col <- 5
  }
  
  # subset to get hospitals in given state
  state_outcome <- outcomes[outcomes$State == given_state,]
  # convert given column to be numeric
  state_outcome[,outcome_col] <- suppressWarnings(as.numeric(state_outcome[,outcome_col]))
  # remove records that have NA on given outcome
  state_outcome_rm_na_for_given_outcome <- state_outcome[complete.cases(state_outcome[,outcome_col]),]
  # get best/lowest outcome
  min_outcome <- min(state_outcome_rm_na_for_given_outcome[,outcome_col])
  # get hospitals with best outcome
  best_hospitals <- state_outcome_rm_na_for_given_outcome[state_outcome_rm_na_for_given_outcome[outcome_col] == min_outcome,]
  # get hospital name
  best_hospitals_names <- best_hospitals$Hospital.Name
  # sort hospital name and return
  sort(best_hospitals_names)
}

#best("TX", "heart attack")
#best("TX", "heart failure")
#best("MD", "heart attack")
#best("MD", "pneumonia")

best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")



