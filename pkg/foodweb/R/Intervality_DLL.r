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

Resample <- function(size, n,...) sample.int(size= size, n = n,  ...) 

# we do resampling for all simm ann steps.

## -----------------------------------------------------------------------------
## INTERVALITY
## -----------------------------------------------------------------------------

Interval_DLL <- function(Flow, 
                     full = FALSE, verbose = TRUE,
                     control = list()) {	

# ------------------------
# Check validity of input
# ------------------------

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
# Initialising SANN: generate permutations
# ------------------------
  # Estimate number of loops, Nloops (StartTemp * Cool^Nloops < EndTemp)

  Tratio <- con$EndTemp / con$StartTemp
  Nloops <- round(log(Tratio) / log(con$Cool) + 0.5)
  Nswaps <- Nloops * con$Numiter      # could 

  # Generate the swap indices
  Swapindices <- matrix (ncol = 2, byrow = TRUE, 
                         unlist(lapply(rep(2, Nswaps), FUN = Resample,
                         n = nrow(Flow))))
  Nrow <- nrow(Flow)

#  DietGap <- .Fortran("dietgap",Flow = as.double(Flow), 
#            Nrow = Nrow, Ncol = ncol(Flow), index = as.integer(1:Nrow), 
#            work = as.double(1:Nrow),sumgap = as.integer(0))
#  DietGap$sumgap
  
#  RR <- sort.int(runif(Nrow),index.return = TRUE)$ix
#  DietGap <- .Fortran("dietgap",Flow = as.double(Flow), 
#            Nrow = Nrow, Ncol = ncol(Flow), index = as.integer(RR), 
#            work = as.double(1:Nrow),sumgap = as.integer(0))
#  DietGap$sumgap
                         
  res <- .Fortran("intervality",Flow = as.double(Flow), 
            Nrow = nrow(Flow), Ncol = ncol(Flow),
            Swapindices = Swapindices, Nswaps = as.integer(Nswaps),
            Nloops = as.integer(Nloops), Niter = as.integer(con$Numiter),
            verbose = as.integer(verbose), StartTemp = as.double(con$StartTemp), 
            Cool = as.double (con$Cool), runif = runif(Nswaps),
            work = as.double(1:Nrow),
            intervalSteps = matrix(ncol = Nloops, nrow = con$Numiter, as.integer(0)),
            index = as.integer(1:Nrow), bestindex = as.integer(1:Nrow), 
            workindex = as.integer(1:Nrow), bestinterval = as.integer(1e6)) 
            
   IntMat <- Flow[res$bestindex, ]

if (full)                                                                   
         return(list(StartTemp = con$StartTemp, Cool = con$Cool,                 
             Numiter = con$Numiter, EndTemp = con$EndTemp, SANNIntIndex = res$intervalSteps,   
             StartIntIndex = res$intervalSteps[1,1], IntIndex = res$bestinterval, IntMatrix = IntMat))
     else return(list(IntIndex = res$bestinterval, IntMatrix = IntMat)) 
}

