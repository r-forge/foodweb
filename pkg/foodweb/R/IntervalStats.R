## =============================================================================
##
## RUNS STATISTICAL TESTS OF FOOD WEB INTERVALITY AS FORMULATED BY 
## STOUFFER et al. 2006 (PNAS 103:19015)
##
## Implemented:
## 
## Yangchen Lin
## Department of Zoology, University of Cambridge
## linyangchen@gmail.com
##
## Owen L. Petchey
## Institute for Evolutionary Biology and Environmental Studies
## University of Zurich
## owen.petchey@ieu.uzh.ch
##
## 10 August 2011
##
## Karline -> Yangchen: made exported functions of generated webs
## called generate.random, generate.niche, generate.cascade
## =============================================================================

## -----------------------------------------------------------------------------
## AUXILIARY FUNCTIONS
## -----------------------------------------------------------------------------

## circumvents problem with sample() when only one item to choose from
## from R documentation of sample(). R >= 2.11.0 only
resample <- function(x, ...) x[sample.int(length(x), ...)]

## Make N completely random food webs with S species and L links
## The output is a list of matrices
Random.model <- function(S, L, N){
   
        web <- list()
        for(j in 1:N){
            web[[j]] <- generate.random(S,L) 
        }
    
    web
}

## Make N generalized niche models (reduces to generalized cascade models if c==0; see Stouffer et al. 2006) with S species and number of links L
## The output is a list of matrices
Niche.model <- function(S, L, N, c, Ctol, toliter){
  web <- list()
  for(j in 1:N)
    web[[j]] <- generate.niche(S,L, c, Ctol, toliter) 
  return(web)
}



## -----------------------------------------------------------------------------
## INTERVALITY STATISTICS
## -----------------------------------------------------------------------------


IntervalStats<-function(Flow, verbose = FALSE, full = FALSE, 
                     control = list(), web = list()) {
  # web parameters: similar code as in optim()

  con <- list(N = 100, Ctol = 0.1, toliter = 100000)   
  nmsC <- names(con)
  con[(namc <- names(web))] <- web
  if (length(noNms <- namc[!namc %in% nmsC])) {
     warning("unknown names in 'web': ", paste(noNms, collapse = ", "))
     warning("See 'details' in Interval help file")
  }
   
  N <- con$N
  Ctol <- con$Ctol
  toliter <- con$toliter



	S <- ncol(Flow)
	L <- length(which(Flow != 0))
	C <- L/S^2
	if (C >= 0.5) {
		stop("procedure not valid for connectance >= 0.5")	#see Details in help file
	}
	
	## Get min G of empirical web
	EmpInd <- Interval(Flow, verbose = verbose, full = full, control=control)
	
	# ------------------------
	# Generate models and get min G's
	# ------------------------

	## Random models
	RFlow <- Random.model(S, L, N)
	RInt <- NULL
	for(counter in 1:N) {
		int <- Interval(RFlow[[counter]], verbose = verbose, control=control)$IntIndex
		RInt <- c(RInt, int)
	}

	## Cascade models
	CFlow <- Niche.model(S, L, N, 0, Ctol, toliter)
	CInt <- NULL
	for(counter in 1:N) {
		int <- Interval(CFlow[[counter]], verbose = verbose, control=control)$IntIndex
		CInt <- c(CInt, int)
	}

	## Niche models
	c <- seq(0.5, 1, by=0.025)
	NInt <- matrix(NA, N, length(c))
	for(c.counter in 1:length(c)) {
		NFlow <- Niche.model(S, L, N, c[c.counter], Ctol, toliter)
		for(counter in 1:N) {
			NInt[counter,c.counter] <- Interval(NFlow[[counter]], verbose = verbose, control=control)$IntIndex
		}
	}

	# ------------------------
	# Test normality of model G's
	# ------------------------

	KSp <- list()
	KSp[['Random.models']] <- ks.test(RInt, pnorm, mean(RInt), sd(RInt))$p
	KSp[['Cascade.models']] <- ks.test(CInt, pnorm, mean(CInt), sd(CInt))$p
	
	KSn <- NULL
	for(c.counter in 1:length(c)) {
		ks <- ks.test(NInt[,c.counter], pnorm, mean(NInt[,c.counter]), sd(NInt[,c.counter]))$p
		KSn <- c(KSn, ks)
	}
	KSp[['Niche.models']] <- KSn

	# ------------------------
	# Null hypothesis testing
	# ------------------------

	NH <- list()
	NH[['Random.models']] <- pnorm(EmpInd$IntIndex, mean(RInt), sd(RInt))
	NH[['Cascade.models']] <- pnorm(EmpInd$IntIndex, mean(CInt), sd(CInt))

	NHz <- NULL
	for(c.counter in 1:length(c)) {	#z scores of Stouffer et al.
		z <- (EmpInd$IntIndex - mean(NInt[,c.counter]))/sd(NInt[,c.counter])
		NHz <- c(NHz, z)
	}
	NH[['Niche.models']] <- NHz
	
	## upper bound of 95% confidence interval of c
	I <- c[max(which(NHz <= 1.645))]

	if (full) {
		return(list(StartTemp = EmpInd$StartTemp, Cool = EmpInd$Cool, Numiter = EmpInd$Numiter, N = N, Ctol = Ctol, toliter = toliter,
              EndTemp = EmpInd$EndTemp, SANNIntIndex = EmpInd$IntIndMat,
              StartIntIndex = EmpInd$StartIntInd,
              IntIndex = EmpInd$IntIndex, IntMatrix = EmpInd$IntMatrix,
              RInt = RInt, CInt = CInt, NInt = NInt,
              KSp = KSp, NH = NH, I = I)) }
     else {
		return(list(IntIndex = EmpInd$IntIndex, IntMatrix = EmpInd$IntMatrix, 
                RInt = RInt, CInt = CInt, NInt = NInt, KSp = KSp, NH = NH, I = I)) }
}
