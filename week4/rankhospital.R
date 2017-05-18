# week 4
# hospital part 2

setwd("~/course/ds_coursera/week4")

rankhospital <- function(state, outcome, num = "best") {
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
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  if (given_outcome == "heart attack") {
    outcome_col <- 3
  } else if (given_outcome == "heart failure") {
    outcome_col <- 4
  } else if (given_outcome == "pneumonia") {
    outcome_col <- 5
  }
  outcome_col
  
  # subset to get hospitals in given state
  state_outcome <- outcomes[outcomes$State == given_state,]
  # convert given column to be numeric
  state_outcome[,outcome_col] <- suppressWarnings(as.numeric(state_outcome[,outcome_col]))
  # remove records that have NA on given outcome
  state_outcome_rm_na_for_given_outcome <- state_outcome[complete.cases(state_outcome[,outcome_col]),]
  state_outcome_rm_na_for_given_outcome
  # sort hospitals by given outcome
  total <- nrow(state_outcome_rm_na_for_given_outcome)
  total
  
  state_outcome_rm_na_for_given_outcome_order <- state_outcome_rm_na_for_given_outcome[order(state_outcome_rm_na_for_given_outcome[,outcome_col], state_outcome_rm_na_for_given_outcome[,1]),]
  state_outcome_rm_na_for_given_outcome_order[,c(1, outcome_col)]
  
  if (num == "best") {
    state_outcome_rm_na_for_given_outcome_order[1, "Hospital.Name"]
  } else if(num =="worst") {
    state_outcome_rm_na_for_given_outcome_order[total, "Hospital.Name"]
  } else {
    if (num > total) {
      NA
    } else {
      state_outcome_rm_na_for_given_outcome_order[num, "Hospital.Name"]
    }
  }
}

#rankhospital("TX", "heart failure",4)
#rankhospital("MD", "heart attack", "worst")
#rankhospital("MN", "heart attack", 5000)


rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)




