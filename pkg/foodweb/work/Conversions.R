#===============================================================================
# Test functions - karline
#===============================================================================



library(foodweb)

# In 'foodweb', there is a data set called "Benguela" (>?Benguela)
# This is still in matrix format (row = from, col = to).
# I use this as a test data set for the conversion functions

#===============================================================================
#===============================================================================
# Function to convert a foodweb matrix to a foodweb "list"
#===============================================================================
#===============================================================================

mat2foodweb <- function(x, ...) {
  # check input
  if (! "matrix" %in% class(x))
    stop ("'x' should be of class 'matrix'")
  
  # create indices to existing links in x (values > 0)
  CC <- which(x > 0, arr.ind = TRUE)          

  # foodweb list
  Web <- list(links = data.frame(from = CC[,1], to = CC[,2]), 
              nodes = data.frame(nr = 1:nrow(x), name = rownames(x)))

  # this is not very nice coding... sometimes we need an extra column in 'links'
  if (diff(range(x[CC])) != 0)    # check if not all same values
     Web$links$strength <- x[CC]

  # Everything in the ellipsis becomes a separate element of the Web list
  Web <- c(Web, list(...))

  class(Web) <- c("foodweb", "list")
  return(Web)
}


# test the function with the benguela food web. 
benglist <- mat2foodweb(Benguela) 
names(benglist)

# We add the bibtext entry:

Bib <- bibentry (
   bibtype = "article",
   title = "Local Trophodynamics and the Interaction of Marine Mammals and Fisheries in the Benguela Ecosystem",
   author = "Yodzis, P.",
   year = "1988",
   journal = "Journal of Animal Ecology",
   volume = "67", 
   pages = "635-658"
)

benglist <- mat2foodweb(Benguela, bibentry = Bib) 
names(benglist)

# Adding still other information...
benglist <- mat2foodweb(Benguela, bibentry = Bib, input_by = "Karline Soetaert") 
names(benglist)
            
head(Benguela)
head(benglist$links)
head(benglist$nodes)

#===============================================================================
#===============================================================================
# Function to convert from a foodweb list to a flowmatrix
# This can be changed to a more general function called "as.matrix"
# if we agree upon the name
#===============================================================================
#===============================================================================

as.matrix.foodweb <- function(x, select = NULL, ...) {

  # test input
  if (! "foodweb" %in% class(x))
    stop ("'x' should be of class 'foodweb'")

  nnode <- nrow(x$nodes)  # number of nodes  
  Flow <- matrix(nrow = nnode, ncol = nnode, data = 0,
                 dimnames = list(x$nodes$name, x$nodes$name))

  if (is.null(select))                  # binary foordweb matrix
    Flow[as.matrix(x$links[,1:2])] <- 1  
  else                                  # use column named "select" instead
    Flow[as.matrix(x$links[,1:2])] <- x$links[,select]  

  return(Flow)
}

B  <- as.matrix.foodweb(benglist)                    # binary web
BB <- as.matrix.foodweb(benglist, select = "strength")   # quantified web

head(B)
head(BB)
