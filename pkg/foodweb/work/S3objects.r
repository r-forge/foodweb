####################################################
#   foodwebs package - the foodweb S3 object classes
#
#   last edit: 26.11.2011
#   
#	implemented: classes metadata, nodes, linklist, foodweb
#			generic function as.linklist and as.foodweb
#			
#
#	todo: 	as.linklist.matrix should convert matrix with numerical entries to a linklist with 3 columns
#			generic function as.webmatrix and methods to create valid webmatrix objects
#			print methods for "foodweb" and "metadata" objects
#			checking functions is.linklist(); is.foodweb(); is.webmatrix()
#
#####################################################


	
as.linklist <- function(x, ...) UseMethod("as.linklist")   #generic function


# S3 method generates new linklist object from two numeric vectors(plus optional vectors for link information)  
as.linklist.numeric <- function(pred, prey, ...) {
	if(! is.vector(pred)) stop("could not create 'linklist' object: 'pred' has to be of class 'vector' !")
	if(! is.vector(prey)) stop("could not create 'linklist' object: 'prey' has to be of class 'vector' !")
	if(length(pred) != length(prey)) stop("could not create 'linklist' object: 'pred' and 'prey' must be of same length !")
	
	as.linklist(data.frame(pred, prey, ...))	
}



# S3 method to convert objects of class 'data.frame' into 'linklist'
as.linklist.data.frame <- function(x, pred.first = TRUE, ...) {		#converts data.frame to linklist object, checks for valid data
		cols <- length(x[1,])	#number of columns
		
		if(pred.first == TRUE) names(x)[1:2] <- c("pred", "prey")  #if object has predators as first column (default)
		else names(x)[1:2] <- c("prey", "pred") 				#if object has prey as first column

		# check if data.frame fits conditions
		if( FALSE %in% (round(x$pred, digits= 0) == x$pred) ) stop("no valid data.frame to create linklist object: 'pred' contains non-integer")
		else if( FALSE %in% (round(x$prey, digits= 0) == x$prey) ) stop("no valid data.frame to create linklist object: 'prey' contains non-integer ")
		# here, other conditions for this class can be defined
		# e.g. the foodweb needs to be cohesive (all species are indirectly linked to each other)
		
		else if(cols == 2) object <- x[,c("pred", "prey")]      #sorts predators in first column
		else if(cols > 2) object <- x[,c("pred", "prey",  names(x)[-c(1,2)]   )]  #sorts predators in first column, other columns follow
		else stop("no valid data.frame to create linklist object")
		
		class(object) <- c("linklist", "data.frame")   #sets new class
		return(object)
	}

	
# S3 method converts objects of class 'matrix' to 'linklist', checks for validity of matrix 
# creates binary linklist only!!!
as.linklist.matrix <- function(x, pred.rows = TRUE, ...) {
		# check if matrix fits conditions
		if( length(x[,1]) != length(x[1,])) stop("no valid matrix to create linklist: number of columns and number of rows differ!")
		# here, other conditions for this class can be defined
		
		# use indices to existing links in x (values > 0), creates matrix with two columns: row, col
		pred <- as.vector(which(x > 0, arr.ind = TRUE)[,"col"])
		prey <- as.vector(which(x > 0, arr.ind = TRUE)[,"row"])
		
		linklist <- data.frame(pred,prey)
		
		#create linklist object
		object <- as.linklist(as.data.frame(linklist), pred.first = "TRUE")

		return(object)
}

# S3 method converts objects of class 'webmatrix' to 'linklist'. no need to check for validity! 
# (no generic function and methods to create "webmatrix" objects defined, yet! )
as.linklist.webmatrix <- function(x) {
		# use indices to existing links in x (values > 0), creates matrix with two columns: row, col
		pred <- as.vector(which(webmatrix > 0, arr.ind = TRUE)[,"col"])
		prey <- as.vector(which(webmatrix > 0, arr.ind = TRUE)[,"row"])
		
		linklist <- data.frame(pred,prey)
		
		#create linklist object
		object <- as.linklist(as.data.frame(linklist), pred.first = "TRUE")

		return(object)
}

# S3 method to extract linklist from 'foodweb' object
as.linklist.foodweb <- function(x) x$linklist




as.webmatrix <- function(x, ...) UseMethod("as.webmatrix")   #generic function


# generates a webmatrix object out of a linklist object
as.webmatrix.linklist <- function(x, ...) {
S <- sort(unique(c(x$pred, x$prey)))

webmatrix <- matrix(0, nrow = length(S), ncol = length(S), dimnames = list(S,S))

for(i in 1:length(x$pred)) webmatrix[x$prey[i],x$pred[i]] <- 1
class(webmatrix) <- c("webmatrix", "matrix")  
return(webmatrix)
}


as.webmatrix.foodweb <- function(x, ...)  as.webmatrix(x$linklist)

as.webmatrix.matrix <- function(x, ...)  as.webmatrix(as.linklist(x, ...))

as.webmatrix.data.frame <- function(x, ...)  as.webmatrix(as.linklist(x, ...))



# creates metadata object, only parameter 'name' is mandatory. 
metadata <- function(
		name, 
		type = NULL, 							# empirical webs can be "terrestrial", "marine", "grassland" and generated webs can be "niche", "nestedhierarchy", "adbm" etc.
		source = NULL, 							# generator functions should create metadata with source = 'model' and type = 'model.type'
		author = NULL, 							
		year = NULL, 
		country = NULL, 
		reference = NULL, 					  	#object of class bibentry
		lat = NULL, lon = NULL, alt = NULL,   	#latitude, longitude, altitude
		description = NULL) {				  	#contains detailed information on the empirical web
	
		#checking conditions for metadata object
		if(class(source) != "NULL")  if(! source %in% c("empirical", "model")) stop(cat("could not create metadata object: '", source ,"' is no valid value for parameter 'source'!/n", sep = ""))
		if(! class(reference) %in% c("NULL", "bibentry") ) stop(cat("could not create metadata object: please provide an object of class 'bibentry' for 'reference'!/n Type '?bibentry' for further information on that object class!/n", sep = "") )
		# here, other conditions for this class can be defined
		
		# if a reference is provided, author and year could be extracted automatically
		
		object <- list(name = name, type = type , source = source , author = author, year = year, country = country, reference = reference, lat = lat, lon = lon, alt = alt, description = description)
		class(object) <- c("metadata", "list")
		return(object)
}

#print.metadata <- function()



#function creates object of class 'nodes'
nodes <- function(x, ...) {
		object <- data.frame(nodes = x, ...)
		class(object) <- c("nodes", "data.frame")
		return(object)
}


# Generic S3 function to create foodweb object
as.foodweb <- function(x, metadata, nodes = NULL, ...) UseMethod("as.foodweb")   

# S3 method to create foodweb from linklist, metadata, and (optional) nodes
as.foodweb.linklist <- function(x, metadata, nodes = NULL, ...) {
		if(! "metadata" %in% class(metadata)) stop("invalid entry for 'metadata'!")
		if(class(nodes) == "NULL")  nodes <- nodes(sort(unique(c(x$pred, x$prey))))
		
		object <- list(linklist = x, metadata = metadata, nodes = nodes)
		class(object) <- c("foodweb", "list")
		return(object)
}

# S3 method to create foodweb from 'data.frame', metadata, and (optional) nodes
as.foodweb.data.frame <- function(x, metadata, nodes = NULL) {
		as.foodweb(as.linklist(x), metadata, nodes)
}

# S3 method to create foodweb from 'matrix', metadata, and (optional) nodes
as.foodweb.matrix <- function(x, metadata, nodes = NULL)  {
		as.foodweb(as.linklist(x), metadata, nodes)
}

