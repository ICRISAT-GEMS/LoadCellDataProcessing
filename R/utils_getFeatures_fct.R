#################################
# functions to extract features #
#################################

ETmax_fct <- function(x){
  
  if (length(unique(x)) > 1) {
    max(x, na.rm = TRUE)
  } else {NA}
  
}

slope_t1_t2 <- function(x, t1, t2){
  
  X <- 1:((t2 + 1) - t1)
  Y <- x[t1:t2]
  tryCatch(lm(Y ~ X)$coefficient[2], error = function(x) NA)
  
}

slope_7_maxET <- function(x, t1 = 29){
  
  if (length(unique(x)) > 1) {
  
  maxET <- unique(max(x, na.rm = TRUE))
  
  if(maxET > 0){
    
    t2 <- min(which.max(x))
    
    if(t2 <= t1){
      
      0
      
    } else {
    
    X <- 1:((t2 + 1) - t1)
    Y <- x[t1:t2]
    tryCatch(lm(Y ~ X)$coefficient[2], error = function(x) NA)
    
    }
    
  } else {
    
    0
    
  }
  
  } else {NA}
  
}

slope_min6_maxET <- function(x){
  
  if (length(unique(x)) > 1) {
  
  maxET <- unique(max(x, na.rm = TRUE))
  
  if(maxET > 0){
    
    t2 <- min(which.max(x))
    
    if(t2 <=5){
      
      0
      
    } else {
      
      t1 <- t2 - 5
      X <- 1:((t2 + 1) - t1)
      Y <- x[t1:t2]
      tryCatch(lm(Y ~ X)$coefficient[2], error = function(x) NA)
      
    }
    
  } else {
    
    0
    
  }
  
  } else {NA}
  
}

curvature_fct <- function(x) {
  
  if (length(unique(x)) > 1) {
    tsf <- tsfeatures(x)
    tsf$curvature
  } else {0}
  
}

AUC_fct <- function(x, t1 = 1, t2 = 96){
  
  sum(x[t1]/2, x[(t1+1) : (t2 - 1)], x[t2]/2)
  
}

SD_t1_t2 <- function(x, t1, t2){
  
  sd(x[t1:t2])
  
}