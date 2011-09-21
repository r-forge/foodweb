\name{generate.webs}
\alias{generate.random}
\alias{generate.cascade}
\alias{generate.niche}
\title{Generates food webs with S species and L links}
\description{
Generates random, cascade and niche food webs with S species and L links
}

\usage{
generate.random (S, L)

generate.cascade (S, L) 

generate.niche (S, L, c = 1, Ctol = 0.1, toliter = 1000)
}
\arguments{
  \item{S }{The number of species in the food web
  }
  \item{L }{The number of links in the food web
  }
  \item{Ctol }{Connectance tolerance for niche model generation. 
     Models with connectance beyond +/- Ctol of the empirical web connectance will be replaced until within tolerance. Defaults to 0.1
  }
  \item{c }{parameter for niche food web generation; If (\code{c == 0}) 
    this reduces to generalized cascade models (see Stouffer et al. 2006); 
    If (\code{c == 1}) this reduces to the Niche model of Williams and Martinez
  }
   \item{toliter }{The maximum number of times to try generating a model which conforms to the connectance tolerance. Defaults to 1000.
  }
}

\value{

 a Flow matrix

}
\details{
Function \code{generate.random} generates a random food web with \code{S} species and \code{L} links

Function \code{generate.cascade} generates a cascade food web with \code{S} species and \code{L} links

Function \code{generate.niche} generates a generalized niche model with \code{S} species and \code{L} links.
 Includes a check for desired connectance

}

\author{
  Yangchen Lin  <linyangchen@gmail.com>,
  
  Karline Soetaert <k.soetaert@nioo.knaw.nl>,
  
  Owen Petchey <owen.petchey@ieu.uzh.ch>
}
\examples{
## -----------------------------------------------------------------------------
## Characteristics of the Benguela food web:
## -----------------------------------------------------------------------------
L <- 203
S <- 29

randomweb <- generate.random (S, L)

cascadeweb <- generate.cascade (S, L) 

nicheweb <- generate.niche (S, L)

# image plots
par (mfrow = c(2,2))
imageweb(randomweb,  main = "random web")
imageweb(cascadeweb, main = "cascade web") 
imageweb(nicheweb,   main = "niche web") 

BB <- (Benguela) ; BB[BB > 0] <- 1
imageweb(BB,  main = "Benguela food web") 

## -----------------------------------------------------------------------------
## Niche webs
## -----------------------------------------------------------------------------
nicheweba <- generate.niche (S, L, c = 0)
nichewebb <- generate.niche (S, L, c = 0.3)
nichewebc <- generate.niche (S, L, c = 0.6)
nichewebd <- generate.niche (S, L, c = 1.0)

par (mfrow = c(2,2))
imageweb(nicheweba, main = "niche web, c = 0") 
imageweb(nichewebb, main = "niche web, c = 0.3") 
imageweb(nichewebc, main = "niche web, c = 0.6") 
imageweb(nichewebd, main = "niche web, c = 1") 

}



\references{

Stouffer, D.B., Camacho, J., Guimer\`{a}, R., Ng, C.A. and L.A. Nunes Amaral, 2005
Quantitative patterns in the structure of model and empirical food webs.
Ecology 86(5), 1301--1311.

Stouffer, D.B., Camacho, J. and L.A. Nunes Amaral, 2006
A robust measure of food web intervality.
PNAS 103 (50), \url{www.pnas.org/cgi/reprintframed/103/50/19015}



}
\seealso{
  \code{\link{Interval}}, the food web intervality index.
  
  \code{\link{Benguela}}, the Benguela pelagic food web.
}
\keyword{utilities}

