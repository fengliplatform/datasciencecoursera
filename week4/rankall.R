# week 4
# hospital part 3

setwd("~/course/ds_coursera/week4")

rankall <- function(outcome, num = "best") {
  given_outcome <- outcome
  
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomes <- outcomes[,c(2,7,11,17,23)]
  
  ## Check if outcome is valid
  valid_states <- unique(outcomes[,"State"])
  valid_states <- sort(valid_states)
  
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!given_outcome %in% valid_outcomes) {
    stop("invalid outcome", call. = TRUE)
    return()
  }
  
  ## For each state, find the hospital of the given rank
    if (given_outcome == "heart attack") {
    outcome_col <- 3
  } else if (given_outcome == "heart failure") {
    outcome_col <- 4
  } else if (given_outcome == "pneumonia") {
    outcome_col <- 5
  }

  hospital_names <- character()
  
  for (given_state in valid_states) {
    # subset to get hospitals in given state
    state_outcome <- outcomes[outcomes$State == given_state,]
    # convert given column to be numeric
    state_outcome[,outcome_col] <- suppressWarnings(as.numeric(state_outcome[,outcome_col]))
    # remove records that have NA on given outcome
    state_outcome_rm_na_for_given_outcome <- state_outcome[complete.cases(state_outcome[,outcome_col]),]
    state_outcome_rm_na_for_given_outcome
    # sort hospitals by given outcome
    total <- nrow(state_outcome_rm_na_for_given_outcome)

    state_outcome_rm_na_for_given_outcome_order <- state_outcome_rm_na_for_given_outcome[order(state_outcome_rm_na_for_given_outcome[,outcome_col], state_outcome_rm_na_for_given_outcome[,1]),]
    state_outcome_rm_na_for_given_outcome_order[,c(1, outcome_col)]
  
    if (num == "best") {
      hospital_name <- state_outcome_rm_na_for_given_outcome_order[1, "Hospital.Name"]
    } else if(num =="worst") {
      hospital_name <- state_outcome_rm_na_for_given_outcome_order[total, "Hospital.Name"]
    } else {
      if (num > total) {
        hospital_name <- NA
      } else {
        hospital_name <- state_outcome_rm_na_for_given_outcome_order[num, "Hospital.Name"]
      }
    }
    hospital_names <- c(hospital_names,hospital_name)
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  df <- data.frame(hospital=hospital_names, state=valid_states)
  df
}

#head(rankall("heart attack", 20), 10)
#tail(rankall("pneumonia", "worst"), 3)
#tail(rankall("heart failure"), 10)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)





