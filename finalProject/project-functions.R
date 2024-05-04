get.total.N <- function(x){
  
  dims <- 2:length(dim(x))
  return(apply(x, dims, sum))
  
} 


extinct.prob <- function(N, limit, time = NULL){
  
  
  if (length(dim(N)) == 3){
    
    if (is.null(time)) time <- dim(N)[2] - 1
    
    N <- apply(N[,1:(time+1),], 2:3, sum)
    
  } else {
    
    if (is.null(time)) time <- dim(N)[1] - 1
    
    N <- N[1:(time+1),]
  }
  
  ext <- sum(apply(N,2,function(x){any(x < limit)}))
  
  return(ext / dim(N)[2])
  
}


remove.extinct <- function(N, limit, time = NULL, replace = NA_real_){
  
  if (length(dim(N)) == 3){
    
    if (is.null(time)) time <- dim(N)[2] - 1
    
    N <- apply(N, 2:3, sum)
    
  } else {
    
    if (is.null(time)) time <- dim(N)[1] - 1
    
  }
  
  
  for (it in 1:ncol(N)){
    
    i <- which(N[1:(time+1),it] < limit)
    
    if (length(i) > 0) N[min(i):dim(N)[1],it] <- replace
    
  }
  
  return(N)
  
}


median.N <- function(N, time = NULL){
  
  if (length(dim(N)) == 3) N <- apply(N, 2:3, sum)
  
  if (is.null(time)){
    
    return(apply(N, 1, median, na.rm = TRUE))
    
  } else {
    
    return(median(N[time+1,], na.rm = TRUE))
  }
  
}


N.start <- function(A, Total.N, round = FALSE){
  
  w <- as.numeric(eigen(A)$vectors[,1]/sum(eigen(A)$vectors[,1]))
  
  w <- w*Total.N
  
  if (round) w <- round(w)
  
  return(w)
  
}


get.lam <- function(A) as.numeric(eigen(A)$values[1])


mat.dstoch <- function(A, Nt, repro.rows = 1, repro.cols = 2:ncol(A), post.breed = TRUE){
  
  # # For testing
  # repro.rows <- 1:2  #default = 1
  # repro.cols <- 2:ncol(A)
  # Nt <- rep(100, nrow(A))
  # post.breed = TRUE
  
  
  N.out <- Nt*0
  
  A.surv <- A
  A.surv[repro.rows, repro.cols] <- 0
  
  A.repro <- A*0
  A.repro[repro.rows, repro.cols] <- A[repro.rows, repro.cols]
  
  
  for (j in 1:ncol(A)){
    
    #sample next year's class for each individual (0 = death)
    Next.Year <- sample(0:nrow(A), Nt[j], replace = TRUE,
                        prob = c(1 - sum(A.surv[,j]), A.surv[,j]))
    
    #Only survivors
    Next.Surv <- Next.Year[Next.Year > 0]
    
    #Total survivors going into each class
    Next.Nums <- as.numeric(table(Next.Surv))
    
    #Corresponding class (index) values
    Next.Class <- as.numeric(names(table(Next.Surv)))
    
    #Add survivors in
    N.out[Next.Class] <- N.out[Next.Class] + Next.Nums
    
    #Adult survival in reproduction elements?
    if (post.breed){
      Num.Repro <- sum(Next.Nums) #only survivors reproduce
      A.repro[,j] <- A.repro[,j] / sum(A.surv[,j]) #adjust repro elements higher (remove survival vital rate)
    } else {
      Num.Repro <- Nt[j] #all reproduce
    }
    
    #loop for each reproduction row
    for (i in repro.rows){
      
      N.out[i] <- N.out[i] + sum(rpois(Num.Repro, A.repro[i,j]))
      
    }
  }
  
  return(N.out)  
  
}