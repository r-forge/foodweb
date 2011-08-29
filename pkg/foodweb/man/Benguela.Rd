\name{Benguela}
\docType{data}
\alias{Benguela}
\title{
  Benguela Marine Ecosystem.
}
\description{
The Benguela Marine Ecosystem, up to Marine Mammals and Fisheries, as in 
Yodzis (1998).   


  The food web comprises 29 functional compartments, comprising
  phytoplankton, benthic filter-feeders, bacteria,
  benthic carnivores, microzooplankton, mesozooplankton,
  macrozooplankton, gelatinous zooplankton, anchovy,
  pilchard, round herring, lightfish, lanternfish, goby,
  other pelagics, horse mackerel, chub mackerel,
  other groundfish, hakes, squid, tunas, snoek,
  kob, yellowtail, geelbek, whales & dolphins, birds,
  seals and sharks.

  There are no export, or import compartments
}

\usage{Benguela}

\format{
  matrix with flow values, where element (i,j) denotes flow from
  compartment i to j

  rownames and columnames are the components.
}

\examples{
unlist(GenInd(Flow = Benguela))

TrophInd(Flow = Benguela)

plotweb(Benguela)
}

\references{
 Yodzis, P., 1988. Local Trophodynamics and the Interaction of Marine 
 Mammals and Fisheries in the Benguela Ecosystem.
 Journal of Animal Ecology, Vol. 67, No. 4, pp. 635-658
}
\author{Karline Soetaert <k.soetaert@nioo.knaw.nl>}

\seealso{
  \code{\link{generate.random}}, \code{\link{generate.cascade}}, \code{\link{generate.niche}}, 
  to generate random, cascade and niche food webs
  
  \code{\link{Interval}}, the food web intervality index.
}

\keyword{datasets}

