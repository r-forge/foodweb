####################################################
#   foodwebs package - the foodweb analytics
#
#   last edit: 28.11.2011
#   
#	implemented: 
#
#	todo:   summary.foodweb() 
#			analytic functions: L, C, TL, gen, vul, ...
#
#	notes: 	the analytic functions should use as.linklist() to apply for all types of input
#####################################################







## functions calculating food web properties for food webs (from Encyclopedia of Complex Systems, Food Webs (Dunne) ) 
# if possible, the functions should work for binary webs and quantitative webs (with link strengths) 

# fundamental properties number of nodes in a food web, x should be a foodweb object OR a linklist OR a webmatrix
speciesnumber <- function(x)    as.linklist(length(unique(c(x$pred, x$prey))))


links <- function(x)  			# number of links in a food web
linkdensity <- function(x)  		# links per species
connectance <- function(x)        # connectance, or the proportion of possible links that are realized
# types of taxa
top <- function(x) 				# percentage of top taxa (taxa with no consumers)
inter <- function(x) 				# percentage of intermediate taxa (taxa with both consumers and resources)
basal <- function(x) 				# percentage of basal taxa (taxa without resources)
herbivores <- function(x)			# percentage of herbivores plus detritivores (taxa that feed on autotrophs or detritus)
cannibals <- function(x)			# percentage of cannibals (taxa that feed on their own taxa)
omnivores <- function(x) 			# percentage of omnivores (taxa that feed that feed on taxa at different trophic levels)
loops <- function(x) 				# percentage of taxa that are in loops, food chains in which a taxon occur twice (e. g., A>B>C>A)

# species specific properties
trophiclevel <- function(x, type = "min" or "average")    # tl of each species within foodweb
trophiclevel <-  function(x)
generality <-  function(x)
vulnerability <-  function(x)



# network structure
meantrophiclevel <- function(x)   # trophic level averaged across taxa. Trophic level represents how many steps energy must take to get from an energy source to a taxon.
chainlength <- function(x)   		# mean food chain length, averaged over all species
sdchainlength <- function(x)		# standard deviation of chainlength
chains <- function(x) 			# log number of food chains
sdlinks <- function(x) 			# normalized standard deviation of links (# links per taxon)
sdvulnerability <- function(x) 	# normalized standard deviation of vulnerability (#	consumers per taxon)
sdgenerality <- function(x)		# normalized standard deviation of generality (#resources per taxon)
maxsimilarity <- function(x)		# mean across taxa of the maximum trophic similarity of each taxon to other taxa
dietdiscontinuity <- function(x)  # the level of diet discontinuity—the proportion of triplets of taxa with an irreducible gap in feeding links over the number of possible triplets
clusterincoef <- function(x)		# clustering coefficient (probability that two taxa linked to the same taxon are linked)
pathlength <- function(x)			# characteristic path length, the mean shortest set of links between species pairs


summary.foodweb <- function(x)
summary.linklist <- function(x)
summary.webmatrix <- function(x)


