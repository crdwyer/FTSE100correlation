predict_BAE <- function(lse, newdata){
  # Carry out any transformations prior to fitting you model
  # Add transformed variables to both lse and newdata. E.g.:
  
  # this is the part that fits your linear model
  BAE.lm <- lm(BA ~ STJ + PRU + AHT + CPG + RR + DLG + LLOY + BATS + TSCO + CNA*EZJ + ANTO, data = lse)
  # this is the part that produces predictions using your linear model
  predictions <- predict(BAE.lm, newdata = newdata)
  return(predictions)
}


