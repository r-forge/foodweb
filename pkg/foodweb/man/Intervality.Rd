\name{Interval}
\alias{Interval}
\alias{IntervalStats}
\title{The food web intervality index}
\description{
  Function Interval calculates the intervality index of an entire food web, e.g. the level of diet discontiguity.

  Function IntervalStats performs Null hypothesis testing of empirical intervality indices against random, cascade and niche models
}

\usage{
Interval      (Flow, verbose = FALSE, full = FALSE, control = list()) 
IntervalStats (Flow, verbose = FALSE, full = FALSE, control = list(), 
               web = list())
}
\arguments{
  \item{Flow }{a matrix with named rows and columns of dimensions no. of 
     species by no. of species, with a given element being some positive real number if
		 column species eats row species, \code{0} otherwise
  }
  \item{verbose }{If \code{TRUE}, will print to screen the steps of the simulated
    annealing algorithm
  }
  \item{full }{If \code{TRUE}, will also output the details of the simulated
    annealing algorithm
  }
  \item{control }{A list of control parameters for the simulated
    annealing algorithm; defaults are \code{list(StartTemp = 0.2, Cool = 0.99, 
              Numiter = 1000, EndTemp = 0.1)} - see 'details'.
  }
  \item{web }{A list of parameters that control the generation of 
   random webs. Defaults are: \code{list(N = 100, Ctol = 0.1, toliter = 100000)} - see 'details'
   }
  
}
\value{
If \code{full} = \code{FALSE}, a named list containing the following:
  \item{IntIndex }{The intervality index, i.e. the minimum 
    of the total number of gaps in the food web.
  }
  \item{IntMatrix }{The reordered flow matrix giving the lowest
    number of gaps in the food web
  }

If \code{full} = \code{TRUE} then in addition to the above, also returns the following:  
  \item{Cool, Numiter, StartTemp, EndTemp }{The input parameters for the simulated
   annealing algorithm.
  }
  \item{SANNIntIndex  }{a matrix of IntIndex values obtained over iterations 
    (columns) and temperatures (rows).
  }
  \item{StartIntIndex }{The intervality index, measured on the original flow matrix.
  }

In addition to the above, the function \code{IntervalStats} also returns:
  \item{RInt }{Vector of minimum G of every random model.
  }
  \item{CInt }{Vector of minimum G of every cascade model.
  }
  \item{NInt }{Matrix containing minimum G of every niche model, each column corresponding to one value of the parameter c.
  }
  \item{KSp }{A named list of p-values from the Kolmogorov-Smirnov normality tests as in Stouffer et al., the first two items in the list being results from the random and cascade models respectively, the third item being a vector of results from the niche models by different values of the parameter c in Stouffer et al.
  }
  \item{NH }{A named list of results from the null hypothesis tests as in Stouffer et al., the first two items in the list being the p-value for the random and cascade models respectively, the third item being a vector of z scores for the niche models with increasing values of the parameter c
  }
  \item{I }{Upper bound of 95\% confidence interval of parameter \code{c}; this is the intervality \emph{I} of Stouffer et al.
  }

}
\author{
  Yangchen Lin  <linyangchen@gmail.com>,
  
  Owen Petchey <owen.petchey@ieu.uzh.ch>

  Karline Soetaert <k.soetaert@nioo.knaw.nl>
}
\examples{

# ------------------------------------------------------------------------------
# The takapoto atoll network - intervality = 0; no iterations
# ------------------------------------------------------------------------------

predmatrix <- Takapoto[1:5, 1:5]
intervality <- Interval(Flow = predmatrix)
intervality$IntMatrix
intervality$IntIndex

# ------------------------------------------------------------------------------
# The Benguela ecosystem network 
# ------------------------------------------------------------------------------

intervality <- Interval(Flow = Benguela, 
          control = list(StartTemp = 0.25, Numiter = 200, EndTemp = 0.15))

par (mfrow = c(1,2))
BB <- Benguela ; BB[BB > 0] <- 1
image (BB, col = greycol(2), main = "Original web")

B2 <- intervality$IntMatrix ; B2[B2 > 0] <- 1
image (B2, col = greycol(2), main = "Interval web")

# IF it has not converged, we may do a second step:
# intervality <- Interval(Flow = intervality$IntMatrix,
#          control = list(StartTemp = 0.25, Numiter = 100, EndTemp = 0.15))

# Note: the intervality should be = 27 for this problem...


# ------------------------------------------------------------------------------
# Artificial network
# ------------------------------------------------------------------------------

nR   <- 10
base <- 1
predmatrix <- matrix(nrow = nR, ncol = nR, data = round(runif(nR^2)*base))
colnames(predmatrix) <- rownames(predmatrix) <- as.character(1:nR)

intervality <- Interval(Flow = predmatrix, full = TRUE, verbose = FALSE,
                        control = list(StartTemp = 0.25, Numiter = 100, 
                        EndTemp = 0.2))
                        
# look at simulating annealing steps
plot(as.vector(intervality$SANNIntIndex))
 

# ------------------------------------------------------------------------------
# Intervality statistics
# ------------------------------------------------------------------------------
predmatrix <- generate.random(S = 10, L = 20)

Stats <- IntervalStats(Flow = predmatrix, web = list(N = 20), 
          verbose = FALSE,
          control = list(Numiter = 50, EndTemp = 0.15))

}

\references{
Stouffer, D.B., Camacho, J. and L.A. Nunes Amaral, 2006
A robust measure of food web intervality.
PNAS 103 (50), \url{www.pnas.org/cgi/reprintframed/103/50/19015}

Stouffer, D.B., Camacho, J., Guimer\`{a}, R., Ng, C.A. and L.A. Nunes Amaral, 2005
Quantitative patterns in the structure of model and empirical food webs.
Ecology 86(5), 1301--1311.

}

\details{
***** The Intervality index *****

The intervality index is estimated by first generating the most 'interval'
ordering of the food web (as returned in item \code{IntMatrix}).

The best ordering is found by simulated annealing, which minimises the sum 
of the gaps of all consumer's diets:

G = min (sum(i)(sum(j) gij))

The intervality index is the minimum of the total number of gaps in the food web.

The smaller the intervality index, the more interval the food web is.

The minimum is located by a simulated annealing algorithm, as given in Stouffer et al. (2006). 
The method uses the Metropolis function for the acceptance probability p. 
p = 1 if G_proposal <= G_current and p = exp( - (G_proposal - G_current) / temperature) if G_proposal > G_current.

Simulated annealing belongs to the class of stochastic global optimization methods, and is not guaranteed to converge to a minimum. Therefore it is recommended to experiment with parameters as in list \code{control}

This control argument is a list that can supply any of the following parameters 
of the simulated annealing algorithm.

\describe{
  \item{StartTemp	}{starting temperature for the cooling scheme. Defaults to 0.2. 
     The higher this value, the more the matrix will change in the initial phase
     of the SANN.}
   \item{Cool }{cooling exponent (defaults to 0.99 as in Stouffer et al.)}
   \item{Numiter }{number of iterations at each temperature; default = 1000;
     note that Stouffer et al. use Numiter = q*(no. of species)^2 where q >= 250}
   \item{EndTemp	}{cut-off annealing temperature at which the algorithm stops;
     defaults to 0.1}
  }

***** Function IntervalStats *****

Implements the procedures of Stouffer et al. 2006 to test whether the minimum sum of diet gaps in an empirical food web departs significantly from models without bias (random and generalized cascade models) or with varying bias (generalized niche models) towards prey contiguity.

The models are generated with the same number of species as the empirical web.

Cascade models are according to the generalized form of Stouffer et al. 2005, as used by Stouffer et al. 2006, generated with the same number of links as the empirical web.

Niche models are according to the generalized form of Stouffer et al. 2006, with one modification. If the upper bound of a predator's prey range exceeds the predator's niche value, delta k (Stouffer et al. p. 19017, last paragraph) are selected from species ranging from smallest niche value up to the species with niche value <= the aforementioned upper bound, that are not already prey. The models are generated with connectance within +/- Ctol of that of the empirical web.

Stouffer's procedure is not applicable if connectance >= 0.5. This is because the parameter beta of the beta distribution is defined as beta = 1/(2*connectance) - 1, and beta > 0.

For niche models, N models are generated for each value of the parameter c from 0.5 to 1 in increments of 0.025 as in Stouffer et al. No checking or correction is done for totally disconnected species, trophically identical species or isolated web clusters, since only the statistical properties of the models are required for analysis.

Stouffer et al. recommend making a Bonferroni correction of the significance level for null hypothesis testing with the random and cascade models.

Warning: the results will not be statistically meaningful with small food webs. In addition, p-values should always be interpreted judiciously, especially for the Kolmogorov-Smirnov tests where there are ties in the data.

The model food webs are controlled with argument \code{web}; this is a list that can supply any of the following parameters:

\describe{
     \item{N }{Number of model webs to generate for each model type and, in the case of the niche model, for each value of the parameter c in Stouffer et al. Defaults to 100 as in Stouffer et al.
  }
  \item{Ctol }{Connectance tolerance for niche model generation. Models with connectance beyond +/- Ctol of the empirical web connectance will be replaced until all models are within tolerance. Defaults to 0.1
  }
   \item{toliter }{For each instance of \code{N}, the maximum number of times to try generating a model which conforms to the connectance tolerance. Defaults to 100000.
  }
}
}

\seealso{
  \code{\link{generate.random}}, \code{\link{generate.cascade}}, \code{\link{generate.niche}}, 
  to generate random, cascade and niche food webs
  
  \code{\link{Benguela}}, the Benguela pelagic food web.
}

\keyword{utilities}

