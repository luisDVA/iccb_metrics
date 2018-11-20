# custom barplot function to alter the appearance of terminal nodes
# modified from code provided by Achim Zeileis to the R-help mailing list 
# source the function and assign it as a grapcon_generator object
node_barplot2 <- function(ctreeobj,
                          col = "black",
                          fill = c("#40A4D8", "white"),
                          beside = NULL,
                          ymax = NULL,
                          ylines = NULL,
                          widths = 1,
                          gap = NULL,
                          reverse = NULL,
                          id = TRUE)
{
  getMaxPred <- function(x) {
    mp <- max(x$prediction)
    mpl <- ifelse(x$terminal, 0, getMaxPred(x$left))
    mpr <- ifelse(x$terminal, 0, getMaxPred(x$right))
    return(max(c(mp, mpl, mpr)))
  }
  
  y <- response(ctreeobj)[[1]]
  
  if(is.factor(y) || class(y) == "was_ordered") {
    ylevels <- levels(y)
    if(is.null(beside)) beside <- if(length(ylevels) < 3) FALSE else TRUE
    if(is.null(ymax)) ymax <- if(beside) 1.1 else 1
    if(is.null(gap)) gap <- if(beside) 0.1 else 0
  } else {
    if(is.null(beside)) beside <- FALSE
    if(is.null(ymax)) ymax <- getMaxPred(ctreeobj @ tree) * 1.1
    ylevels <- seq(along = ctreeobj @ tree$prediction)
    if(length(ylevels) < 2) ylevels <- ""
    if(is.null(gap)) gap <- 1
  }
  if(is.null(reverse)) reverse <- !beside
  if(is.null(fill)) fill <- gray.colors(length(ylevels))
  if(is.null(ylines)) ylines <- if(beside) c(3, 4) else c(1.5, 2.5)
  
  ### panel function for barplots in nodes
  rval <- function(node) {
    
    ## parameter setup
    pred <- node$prediction
    if(reverse) {
      pred <- rev(pred)
      ylevels <- rev(ylevels)
    }
    np <- length(pred)
    nc <- if(beside) np else 1
    
    fill <- rep(fill, length.out = np)
    widths <- rep(widths, length.out = nc)
    col <- rep(col, length.out = nc)
    ylines <- rep(ylines, length.out = 2)
    
    gap <- gap * sum(widths)
    yscale <- c(0, ymax)
    xscale <- c(0, sum(widths) + (nc+1)*gap)
    
    top_vp <- viewport(layout = grid.layout(nrow = 2, ncol = 3,
                                            widths = unit(c(ylines[1], 1, ylines[2]), c("lines", "null", "lines")),
                                            heights = unit(c(3.5,3.5), c("lines", "null"))),
                       width = unit(1, "npc"),
                       height = unit(1, "npc") - unit(2, "lines"),
                       name = paste("node_barplot", node$nodeID, sep = ""))
    
    pushViewport(top_vp)
    grid.rect(gp = gpar(fill = "white", col = 0))
    
    ## main title
    top <- viewport(layout.pos.col=2, layout.pos.row=1)
    pushViewport(top)
    mainlab <- paste(ifelse(id, paste("Node", node$nodeID,"\n", "(n = "), "n = "),
                     sum(node$weights), ifelse(id, ")", ""), sep = "")
    grid.text(mainlab)
    popViewport()
    
    plot <- viewport(layout.pos.col=2, layout.pos.row=2,
                     xscale=xscale, yscale=yscale,
                     name = paste("node_barplot", node$nodeID, "plot",
                                  sep = ""))
    
    pushViewport(plot)
    
    if(beside) {
      xcenter <- cumsum(widths+gap) - widths/2
      for (i in 1:np) {
        grid.rect(x = xcenter[i], y = 0, height = pred[i],
                  width = widths[i],
                  just = c("center", "bottom"), default.units = "native",
                  gp = gpar(col = col[i], fill = fill[i]))
      }
      if(length(xcenter) > 1) grid.xaxis(at = xcenter, label = FALSE)
      grid.text(ylevels, x = xcenter, y = unit(-1, "lines"),
                default.units = "native",
                just= c("center","bottom"),
                check.overlap = TRUE)
      grid.yaxis()
    } else {
      ycenter <- cumsum(pred) - pred
      
      for (i in 1:np) {
        grid.rect(x = xscale[2]/2, y = ycenter[i], height = min(pred[i], ymax - ycenter[i]),
                  width = widths[1],
                  just = c("center", "bottom"), default.units = "native",
                  gp = gpar(col = col[i], fill = fill[i]))
      }
      
      grid.yaxis(at = round(1 - pred[i], digits = 2), main = FALSE)
    }
    
    grid.rect(gp = gpar(fill = "transparent"))
    upViewport(2)
  }
  
  return(rval)
}
class(node_barplot2) <- "grapcon_generator"

# custom function by user "agstudy"
# draws a white circle with the node name and the number of obs.
innerWeights <- function(node){
  grid.circle(r=0.36,gp = gpar(fill = "White",col="White"))
  mainlab <- paste( node$psplit$variableName, "\n(n = ")
  mainlab <- paste(mainlab, sum(node$weights),")" , sep = "")
  grid.text(mainlab,gp = gpar(col='black'))
}