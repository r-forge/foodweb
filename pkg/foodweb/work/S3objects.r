####################################################
#   foodwebs package - the foodweb S3 object classes
#
#   last edit: 03.10.2011
#   
#	implemented: classes reference, metadata, nodes, linklist, foodweb
#			generic function as.linklist and as.foodweb
#			
#
#	todo: 	as.linklist.matrix should convert matrix with numerical entries to a linklist with 3 columns
#			generic function as.webmatrix and methods to create valid webmatrix objects
#			a print method for foodweb objects
#
#
#####################################################
	

	
as.linklist <- function(x, ...) UseMethod("as.linklist")   #generic function

# S3 method to convert objects of class 'data.frame' into 'linklist'
as.linklist.data.frame <- function(x, pred.first = TRUE) {		#converts data.frame to linklist object, checks for valid data
		cols <- length(x[1,])	#number of columns
		
		if(pred.first == TRUE) names(x)[1:2] <- c("pred", "prey")  #if object has predators as first column (default)
		else names(x)[1:2] <- c("prey", "pred") 				#if object has prey as first column

		# check if data.frame fits conditions
		if(class(x$pred) != "integer") stop("no valid data.frame to create linklist object: 'pred' contains non-integer")
		else if(class(x$prey) != "integer") stop("no valid data.frame to create linklist object: 'prey' contains non-integer ")
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
as.linklist.matrix <- function(x, pred.rows = TRUE) {
		# check if matrix fits conditions
		if( length(x[,1]) != length(x[1,])) stop("no valid matrix to create linklist: number of columns and number of rows differ!")
		# here, other conditions for this class can be defined
		
		# use indices to existing links in x (values > 0), creates matrix with two columns: row, col
		CC <- which(x > 0, arr.ind = TRUE)
		
		#create linklist object
		object <- as.linklist(as.data.frame(CC), pred.first = !pred.rows)

		return(object)
}

# S3 method converts objects of class 'webmatrix' to 'linklist'. no need to check for validity! 
# (no generic function and methods to create "webmatrix" objects defined, yet! )
as.linklist.webmatrix <- function(x) {
		# use indices to existing links in x (values > 0)
		CC <- which(x > 0, arr.ind = TRUE)
		
		#create linklist object
		object <- as.linklist(as.data.frame(CC), pred.first = !pred.rows)
		return(object)
}

# S3 method to extract linklist from 'foodweb' object
as.linklist.foodweb <- function(x) x$linklist


# function generates new linklist object from two (or more) vectors 
linklist <- function(pred, prey, ...) {
	if(! "vector" %in% class(pred)) stop("could not create 'linklist' object: 'pred' has to be of class 'vector' !")
	if(! "vector" %in% class(prey)) stop("could not create 'linklist' object: 'prey' has to be of class 'vector' !")
	if(length(pred) != length(prey)) stop("could not create 'linklist' object: 'pred' and 'prey' must be of same length !")
	
	as.linklist(data.frame(pred, prey, ...))	
}


# function creates list object of class 'reference'. mandatory parameters are title, author, year. default bibtype is 'article'. 
reference <- function(
					title, 
					author, 
					year, 
					journal = NULL, 
					volume = NULL, 
					pages = NULL, 
					publisher = NULL, 
					issue = NULL, 
					bibtype = "article") {
					
				if(bibtype != "article" && bibtype != "book") stop(cat(bibtype, " is no valid entry for parameter 'bibtype'!", sep = ""))  
				
				object <- list(
					title = title, 
					author = author, 
					year = year, 
					journal = journal,
					volume = volume, 	
					pages = pages,
					publisher = publisher, 
					issue = issue,   
					bibtype = bibtype)
				class(object) <- c("reference")
				return(object)
	}

# print method for reference-objects
print.reference <- function(object) {
		if(object$bibtype == "article") out <- cat(object$author, " (", object$year, ") ", object$title, ", ", object$journal, ", ", object$volume, ":", object$pages, "\n", sep = "") 
		else if(object$bibtype == "book") out <-  cat(object$author, " (", object$year, ") ", object$title, ", ", object$publisher, ", ", object$issue, "\n", sep = "") 
		#else if(object@bibtype == "bookchapter") out <- ...
	
		else stop(cat(object$bibtype, " is no valid entry for parameter 'bibtype'!", sep = ""))
		return(out)
}

# creates metadata object, only parameter 'name' is mandatory. 
metadata <- function(
		name, 
		type = NULL, 
		source = NULL, 							# generator functions should create metadata with source = 'generator' and type = 'model.type'
		author = NULL, 							
		year = NULL, 
		country = NULL, 
		reference = NULL, 					  	#reference object
		lat = NULL, lon = NULL, alt = NULL,   	#latitude, longitude, altitude
		description = NULL) {				  	#contains detailed information on the empirical web
	
		#checking conditions for metadata object
		if(source != "empirical" && source != "model") stop(cat("could not create metadata object: '", source ,"' is no valid value for parameter 'source'!"))
		if(! "NULL" %in% class(reference) && ! "reference" %in% class(reference) ) stop(cat("could not create metadata object: '", reference , "' is no valid object for 'reference'! ") )
		# here, other conditions for this class can be defined
		
		# if reference is provided, author and year could be extracted automatically
		
		object <- list(name = name, type = type , source = source , author = author, year = year, country = country, reference = reference, lat = lat, lon = lon, alt = alt, description = description)
		class(object) <- c("metadata", "list")
		return(object)
}

#function creates object of class 'nodes'
nodes <- function(x, ...) {
		object <- data.frame(nodes = x, ...)
		class(object) <- c("nodes", "data.frame")
		return(object)
}


# Generic S3 function to create foodweb object
as.foodweb <- function(x, metadata, nodes = NULL) UseMethod("as.foodweb")   

# S3 method to create foodweb from linklist, metadata, and (optional) nodes
as.foodweb.linklist <- function(x, metadata, nodes = NULL) {
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

