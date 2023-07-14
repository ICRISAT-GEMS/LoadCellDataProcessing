curateRawLC <- function(x, y, col_names) {
  
  base.d <- x[,-1]
  
  data.RmOut <- base.d 
  
  for ( i in 1:(ncol(base.d)) ) {
    data <- base.d[, i]
    data[data < 0] <- NA # if less than 0, it is wrong.
    data <- as.numeric(unlist(data))
    
    ol <- boxplot(data, plot = FALSE)$out
    tf <- which(data %in% ol)
    
    data.RmOut[tf, i] <- NA
    
  }
  
  rmOut <- as.data.frame(t(data.RmOut))
  
  rmOut.DF <- as.data.frame(cbind(y, rmOut))
  colnames(rmOut.DF) <- col_names
  
  ####### Imputation #######
  
  # fill the eventual missing values at the beginning (first observed value)
  first_NA_val <- is.na(data.RmOut[1, ])
  
  if(any(first_NA_val)){
    
    NA_pos <- which(first_NA_val)
    
    for(i in 1:length(NA_pos)){
      
      val_i <- data.RmOut[, NA_pos[i]]
      first_non_na <- min(which(!is.na(val_i)))
      data.RmOut[1:(first_non_na-1), NA_pos[i]] <- val_i[first_non_na] 
      
    }
    
  }
  
  data.Imp <- data.frame(apply(data.RmOut, 2, na.approx, na.rm = F))
  
  # fill the eventual missing values at the end
  last_NA_val <- is.na(data.Imp[nrow(data.Imp), ])
  
  if(any(last_NA_val)){
    
    NA_pos <- which(last_NA_val)
    
    for(i in 1:length(NA_pos)){
      
      val_i <- data.RmOut[, NA_pos[i]]
      last_non_na <- max(which(!is.na(val_i)))
      data.Imp[(last_non_na + 1):nrow(data.Imp), NA_pos[i]] <- val_i[last_non_na] 
      
    }
    
  }
  
  data.Imp.df <- as.data.frame(t(data.Imp))
  
  imputed.DF <- as.data.frame(cbind(y, data.Imp.df))
  colnames(imputed.DF) <- col_names
  
  # write.csv(imputed.DF, paste0(opPATH, "LC.MAT.BOX.OL_s1_IMPUTED.csv"))
  
  
  # There still could remain columns with maximum missing, hence run the below script
  na.list <- as.numeric(apply(imputed.DF[,-c(1:5)], 1, FUN = function(x) {sum(is.na(x))}))
  
  # keep sectors which have more than 30% of values
  na.list.G.Locs <- which(na.list > ceiling(0.3*dim(imputed.DF[,-c(1:5)])[2])) 
  
  # replace 'na.list.G.Locs' only with 0 if NA so that ETr can be extracted
  if(length(na.list.G.Locs) > 0)
  {
    imputed.DF.tmp <- imputed.DF
    imputed.DF.tmp <- imputed.DF.tmp[-(na.list.G.Locs+5), ]
  } else {imputed.DF.tmp <- imputed.DF}
  
  imputed.DF.final <- imputed.DF.tmp
  
  interp.ip <- imputed.DF.final[ ,6:ncol(imputed.DF.final)]
  
  interp.df <- as.data.frame(t(interp.ip))
  
  interp.df.op <- as.data.frame(apply(interp.df, 1, na.aggregate.default))
  
  imputed.DF.final[ ,6:ncol(imputed.DF.final)] <- interp.df.op
  
  return(imputed.DF.final)
}