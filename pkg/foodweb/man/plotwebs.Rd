\name{imageweb}
\alias{imageweb}

\title{Image plot of a food web}
\description{
 Creates an image plot from a food web
}

\usage{
imageweb (Flow, names = FALSE, ...)
}
\arguments{
  \item{Flow }{a matrix with named rows and columns of dimensions no. of 
     species by no. of species, with a given element being some positive real number if
		 column species eats row species, \code{0} otherwise
  }
  \item{names }{if \code{TRUE} will label the third and fourth axis with names of components
  }
  \item{... }{extra arguments passed to R-function \link{image}
  }
}


\details{

Function \code{imageweb} creates an image plot with consumers 
(columns of the \code{Flow} matrix) on the x-axis and 
resources (rows of the \code{Flow} matrix) on the y-axis. 

By default, the colors chosen are \code{grey(100)}
}

\author{
  Karline Soetaert <k.soetaert@nioo.knaw.nl>
}
\examples{

## -----------------------------------------------------------------------------
## Plotting the Benguela food web:
## -----------------------------------------------------------------------------

BB <- (Benguela) ; BB[BB > 0] <- 1
imageweb(BB, main = "Benguela food web")
imageweb(log(Benguela), col = femmecol(100), ylab = "FOOD", xlab = "CONSUMER")
imageweb(log(Benguela), names = TRUE)
plotweb(Benguela)     # from R-package diagram


## -----------------------------------------------------------------------------
## Niche webs
## -----------------------------------------------------------------------------

nicheweba <- generate.niche (S = 10, L = 30, c = 0)
nichewebb <- generate.niche (S = 10, L = 30, c = 0.3)

pm <- par (mfrow = c(1,2))
imageweb(nicheweba, main = "niche web, c = 0") 
imageweb(nichewebb, main = "niche web, c = 0.3") 

}



\seealso{
  \code{\link{plotweb}}, for different type of plotting
  
  \code{\link{Benguela}}, the Benguela pelagic food web.
}
\keyword{utilities}

