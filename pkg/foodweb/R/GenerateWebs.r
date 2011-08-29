## =============================================================================
## Generate a random food web with S species and L links
## =============================================================================

generate.random <- function (S, L) {
 web <- matrix(c(rep(1, L), rep(0, S^2-L))[order(runif(S^2))],
                               S, S)
 dimnames(web) <- list(1:S, 1:S)
 web
}

## =============================================================================
## Generate a cascade food web with S species and L links
## =============================================================================

generate.cascade <- function(S, L) {
  web <- matrix(0, S, S)
  web[upper.tri(web)] <- c(rep(1, L), rep(0, (S^2-S)/2-L))[order(runif((S^2-S)/2))]
  dimnames(web) <- list(1:S, 1:S)
  web
}

## =============================================================================
## Make a generalized niche model with S species and number of links L
## If (c == 0) this reduces to generalized cascade models (see Stouffer et al. 2006) 
## If (c == 1) this reduces to the Niche model of Williams and Martinez
## Includes a check for desired connectance
## =============================================================================

generate.niche <- function(S, L, c = 1, Ctol = 0.1, toliter = 1000){
  C <- L/S^2   # directed connectance
  beta <- (1 - 2 * C) / (2 * C)   # parameter for beta distribution

  Creal <- C - Ctol -1
  loopcounter <- 0

  Strue <- TRUE     # Karline -> Yangchin: to check if all species are connected
  
  while(loopcounter <= toliter & (Creal < (C - Ctol) | Creal > (C + Ctol)) | !Strue) {
    if (loopcounter  == 1) print("Searching for models within Ctol")
  
    n <- sort(runif(S))
    r0 <- n*(1 - (1 - runif(S))^(1/beta))
    r <- c*r0
    deltak <- round(S*(r0 - r))
    mid <- r/2 + runif(S) * (n - r/2)
    web <- matrix(0, S, S)
    min.n <- mid - r/2
    max.n <- mid + r/2
    for(i in 1:S){
      Select <- c(which(n > min.n[i]), which(n < max.n[i]))
      diet <- c(1:S)[Select[duplicated(Select)]]
              
     ## add to diet: deltak species (see Details in help file)
     if (n[i] >= max.n[i])              
        k <- i
     else 
        k <- max(which(n <= max.n[i]))
     
     if (deltak[i] > length(setdiff(1:k, diet))) 	#due to rounding of deltak to nearest integer
     		deltak[i] <- length(setdiff(1:k, diet))

     satellite <- resample(setdiff(1:k,diet), deltak[i])
     diet <- c(diet, satellite)

     web[diet,i] <- 1
  }
          
  ## check whether connectance is within +/- Ctol; 
  ## if not, repeat until it is or until toliter unsuccessful models are generated, whichever comes first
  Creal <- length(which(web == 1))/S^2
  loopcounter <- loopcounter + 1
  Strue <- !any(colSums(web) +rowSums(web) ==0)
  
  }
  if (loopcounter == (toliter + 1) & (Creal < (C - Ctol) | Creal > (C + Ctol))) 
          	stop("Failed to generate models within Ctol")

  dimnames(web) <- list(1:S, 1:S)
  return(web)
}

