## =============================================================================
##
## CALCULATES FOOD WEB INTERVALITY AS FORMULATED BY 
## STOUFFER et al. 2006 (PNAS 103:19015)
##
## Implemented: Yangchen Lin
## Department of Zoology, University of Cambridge
## linyangchen@gmail.com
## 28 July 2011
##
## =============================================================================


## -----------------------------------------------------------------------------
## SUBSIDIARY FUNCTIONS
## -----------------------------------------------------------------------------

## circumvents problem with sample() when only one item to choose from
## from R documentation of sample(). R >= 2.11.0 only
resample <- function(x, ...) x[sample.int(length(x), ...)]


dietgap <- function (Flow) {
  ## total length of diet gaps of one species (one column)
    gap <- function(x) {
  	# count number of 0s between the first and last positive element
	  pos <- which(x>0)
  	if (length(pos) > 0) 	{
      x <- x[pos[1]:pos[length(pos)]]
		  return(length(which(x == 0)))
    } else return(0)
  }

  return(sum(apply(Flow, 2, gap)))
}  

## -----------------------------------------------------------------------------
## INTERVALITY
## -----------------------------------------------------------------------------

Interval <- function(Flow, 
                     verbose = FALSE, full = FALSE, 
                     control = list()) {	

# ------------------------
# Check validity of input
# ------------------------

  useDLL <- TRUE ######### TESTING: Set to false to use R-code

  if (useDLL) {
    return(Interval_DLL(Flow, full, verbose, control))
  }
  if(is.null(rownames(Flow)) | is.null(colnames(Flow)))
		stop("Error: 'Flow' has no row names and/or column names")

  # simulated annealing parameters: similar code as in optim()
  con <- list(StartTemp = 0.2, Cool = 0.99, 
              Numiter = 1000, EndTemp = 0.1)   
  nmsC <- names(con)
  con[(namc <- names(control))] <- control
  if (length(noNms <- namc[!namc %in% nmsC])) {
     warning("unknown names in 'control': ", paste(noNms, collapse = ", "))
     warning("See 'details' in Interval help file")
  } 
# ------------------------
# Initialising SANN
# ------------------------

  # initial index
  StartIntInd <- dietgap(Flow)
  
	IntInd  <- StartIntInd
  IntMat  <- Flow
  
# keep 'best' set; sometimes the simmulated annealing forgets the best

	BestInd <- StartIntInd
  BestMat <- Flow

	##SIMULATED ANNEALING TO MINIMIZE IntInd

	IntIndMat <- NULL        
	T <- con$StartTemp
	loopcounter <- 0
	is.finished <- FALSE
	if (StartIntInd == 0) is.finished = TRUE
	
	while(T >= con$EndTemp & !is.finished)	{
		loopcounter <- loopcounter+1
		IntIndMat   <- cbind(IntIndMat, rep(NA, con$Numiter))   

		for(counter in 1:con$Numiter) {
			
			##swap two random species (rows)
			swap <- resample(1:nrow(IntMat), 2)
			newIntMat <- IntMat
			newIntMat[swap,] <- newIntMat[rev(swap),]
			rownames(newIntMat)[swap] <- rownames(newIntMat)[rev(swap)]
		
			## calculate new IntInd
			newIntInd <- dietgap(newIntMat)
		
			accept <- FALSE
      if (newIntInd <= IntInd)
			   accept <- TRUE
			else { ##probability of acceptance as defined by Stouffer et al.
				Paccept <- exp(-(newIntInd-IntInd)/T)
				draw <- runif(1)
				if(draw < Paccept) 
          accept <- TRUE
			}   
      if (accept) {
				IntInd <- newIntInd
				IntMat <- newIntMat
				if (IntInd < BestInd) {
  				BestMat <- IntMat
				  BestInd  <- IntInd
  			}	
			} 				
			IntIndMat[counter, loopcounter] <- IntInd
  		if(IntInd == 0) {
          is.finished <- TRUE
          break
      } 			
		}
		if (verbose) print(paste('annealing temperature', T,
			              'current best index', BestInd))

		T <- T*con$Cool

	}
	if (full)
  	return(list(StartTemp = con$StartTemp, Cool = con$Cool, Numiter = con$Numiter,
              EndTemp = con$EndTemp, SANNIntIndex = IntIndMat,
              StartIntIndex = StartIntInd,
              IntIndex = IntInd, IntMatrix = IntMat))
  else
  	return(list(IntIndex = IntInd, IntMatrix = IntMat))
  
}


##EXAMPLE
