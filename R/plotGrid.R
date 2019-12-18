#' plotGrid
#'
#' Function to plot a heatmap based on a dataframe/matrix
#' @param grid.plot dataframe with numerical values
#' @param x.label  columns of the grid
#' @param y.label  rows of the grid
#' @param plot.title title for the plot
#' @param 
#' @keywords heatmap
#' @export
#' @examples
	

plotGrid <- function(grid.plot,
				x.label = "X Label", # columns of the grid
				y.label = "Y Label", # rows of the grid
				plot.title = "Title"){
				
			
#warning("I think the color palette is still switched? But it seems to work right?")

			
# extract position vectors
#y.vec <- as.numeric(str_split(dimnames(grid.plot)[[1]]," ",simplify=TRUE)[,2])
#x.vec <- as.numeric(str_split(dimnames(grid.plot)[[2]]," ",simplify=TRUE)[,2])

y.vec <- as.numeric(str_split(dimnames(grid.plot)[[1]]," ",simplify=TRUE)[,2])
x.vec <- seq(-50,30,by=10)
#1:length(dimnames(grid.plot)[[2]])  # TEMPORARY PATCH FOR A2 OUTPUT
#as.numeric(str_split(dimnames(grid.plot)[[2]]," ",simplify=TRUE)[,2])

			breaks.use <- seq(min(grid.plot,na.rm=TRUE),max(grid.plot,na.rm=TRUE),length.out=80)
			cols.use <- heat.colors(79)


# plot heatmap
image(x.vec , y.vec , t(grid.plot),
      xlab=x.label,ylab=y.label,
      breaks = breaks.use,
      col= cols.use,
      bty="n",axes=FALSE
)


# add grid
abline(h=y.vec + (y.vec[2]-y.vec[1])/2,col="lightgrey",lty=3)
abline(v=x.vec + (x.vec[2]-x.vec[1])/2,col="lightgrey",lty=3)

# add axes
axis(1,at= x.vec,labels=x.vec,las=1,tick=FALSE)
axis(2,at= y.vec,labels=y.vec,las=1,tick=FALSE)

# add values
text.pos <- expand.grid(x.vec,y.vec)
text(text.pos[,1],text.pos[,2],round(as.vector(t(grid.plot))*100),cex=0.7)

# add title
title(main=plot.title )


} # end plotGrid
