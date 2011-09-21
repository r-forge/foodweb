imageweb <- function (Flow, names = FALSE, ...)  {
  
  pm <- par("mar")
  pl <- par("las")
  
  ll <- list(...)
  Flowmat <- t(Flow[nrow(Flow):1, ncol(Flow):1])

  dd <- dim(Flowmat)
  xnames <- colnames(Flow)
  ynames <- rownames(Flow)
  
  if (is.null(ll$col))  ll$col <- greycol(100)
  if (is.null(ll$xlab)) ll$xlab <- "consumer"
  if (is.null(ll$ylab)) ll$ylab <- "resource"
  if (is.null(ll[["x"]])) ll$x <- 1:dd[1]      # need [[]] to avoid partial matching..
  if (is.null(ll[["y"]])) ll$y <- 1:dd[2]
  
  if (names) {
    ll$axes <- FALSE
    pm <- par("mar")
    par(mar = pm + c(0, 0, 4, 4) )
  }

  do.call ("image", c(alist(Flowmat), ll))
  box() 
  if (names){
    par(las = 2)
    axis(3, at = ll$x, labels = rev(xnames))
    axis(4, at = ll$y, labels = rev(ynames))
  }
  par("mar" = pm )
  par("las" = pl)
  
}

