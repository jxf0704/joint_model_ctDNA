

#Figure 1A (oncoprint): many were modified in adobe illustrator

col=c('clonal_missense_variant'='#0000CD','clonal_loss'='#FF1493','subclonal_frameshift_variant'='#CDB38B','subclonal_inframe_deletion'='#B0E0E6','subclonal_missense_variant'='#0000CD','subclonal_gain'='cyan3','clonal_gain'='cyan3','subclonal_loss'='#FF1493','clonal_inframe_deletion'='#B0E0E6','clonal_splice_acceptor_variant'='#FFE4C4','clonal_splice_donor_variant'='#CDC1C5','clonal_stop_gained'='#CDC9C9','subclonal_stop_gained'='#CDC9C9','clonal_frameshift_variant'='#CDB38B','both_missense_variant'='#0000CD','clonal_inframe_insertion'='#778899','subclonal_inframe_insertion'='#778899','subclonal_splice_donor_variant'='#CDC1C5','subclonal_splice_acceptor_variant'='#FFE4C4','subclonal_stop_lost'='#7FFF00')

alter_fun = list(background = function(x, y, w, h) {grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), gp = gpar(fill = "white", col = NA))},clonal_splice_acceptor_variant = function(x, y, w, h) {grid.polygon(unit.c(x - 0.5*w, x-0.5*w, x + 0.5*w, x + 0.5*w), unit.c(y - 0.5*h, y+0.5*h, y + 0.5*h, y - 0.5*h),gp = gpar(fill = col['clonal_splice_acceptor_variant'], col = 'white'))},clonal_inframe_insertion = function(x, y, w, h) {grid.polygon(unit.c(x - 0.5*w, x-0.5*w, x + 0.5*w, x + 0.5*w), unit.c(y - 0.5*h, y+0.5*h, y + 0.5*h, y - 0.5*h),gp = gpar(fill = col['clonal_inframe_insertion'], col = 'white'))},clonal_inframe_deletion = function(x, y, w, h) {grid.polygon(unit.c(x - 0.5*w, x-0.5*w, x + 0.5*w, x + 0.5*w), unit.c(y - 0.5*h, y+0.5*h, y + 0.5*h, y - 0.5*h),gp = gpar(fill = col['clonal_inframe_deletion'], col = 'white'))},clonal_gain = function(x, y, w, h) {grid.polygon(unit.c(x - 0.5*w, x-0.5*w, x + 0.5*w, x + 0.5*w), unit.c(y - 0.5*h, y+0.5*h, y + 0.5*h, y - 0.5*h),gp = gpar(fill = col['clonal_gain'], col = 'white'))},clonal_loss = function(x, y, w, h) {grid.polygon(unit.c(x - 0.5*w, x-0.5*w, x + 0.5*w, x + 0.5*w), unit.c(y - 0.5*h, y+0.5*h, y + 0.5*h, y - 0.5*h),gp = gpar(fill = col['clonal_loss'], col = 'white'))},clonal_stop_gained = function(x, y, w, h) {grid.polygon(unit.c(x - 0.5*w, x-0.5*w, x + 0.5*w, x + 0.5*w), unit.c(y - 0.5*h, y+0.5*h, y + 0.5*h, y - 0.5*h),gp = gpar(fill = col['clonal_stop_gained'], col = 'white'))},clonal_frameshift_variant = function(x, y, w, h) {grid.polygon(unit.c(x - 0.5*w, x-0.5*w, x + 0.5*w, x + 0.5*w), unit.c(y - 0.5*h, y+0.5*h, y + 0.5*h, y - 0.5*h),gp = gpar(fill = col['clonal_frameshift_variant'], col = 'white'))},clonal_splice_donor_variant = function(x, y, w, h) {grid.polygon(unit.c(x - 0.5*w, x-0.5*w, x + 0.5*w, x + 0.5*w), unit.c(y - 0.5*h, y+0.5*h, y + 0.5*h, y - 0.5*h),gp = gpar(fill = col['clonal_splice_donor_variant'], col = 'white'))},clonal_missense_variant = function(x, y, w, h) {grid.polygon(unit.c(x - 0.5*w, x-0.5*w, x + 0.5*w, x + 0.5*w), unit.c(y - 0.5*h, y+0.5*h, y + 0.5*h, y - 0.5*h),gp = gpar(fill = col['clonal_missense_variant'], col = 'white'))},subclonal_splice_acceptor_variant = function(x, y, w, h) {grid.polygon(unit.c(x - 0.5*w,  x + 0.5*w, x + 0.5*w), unit.c(y - 0.5*h,  y + 0.5*h, y - 0.5*h),gp = gpar(fill = col['subclonal_splice_acceptor_variant'], col = 'white'))},subclonal_inframe_insertion = function(x, y, w, h) {grid.polygon(unit.c(x - 0.5*w,  x + 0.5*w, x + 0.5*w), unit.c(y - 0.5*h,  y + 0.5*h, y - 0.5*h),gp = gpar(fill = col['subclonal_inframe_insertion'], col = 'white'))},subclonal_inframe_deletion = function(x, y, w, h) {grid.polygon(unit.c(x - 0.5*w,  x + 0.5*w, x + 0.5*w), unit.c(y - 0.5*h,  y + 0.5*h, y - 0.5*h),gp = gpar(fill = col['subclonal_inframe_deletion'], col = 'white'))},subclonal_stop_lost = function(x, y, w, h) {grid.polygon(unit.c(x - 0.5*w,  x + 0.5*w, x + 0.5*w), unit.c(y - 0.5*h,  y + 0.5*h, y - 0.5*h),gp = gpar(fill = col['subclonal_stop_lost'], col = 'white'))},subclonal_gain = function(x, y, w, h) {grid.polygon(unit.c(x - 0.5*w,  x + 0.5*w, x + 0.5*w), unit.c(y - 0.5*h,  y + 0.5*h, y - 0.5*h),gp = gpar(fill = col['subclonal_gain'], col = 'white'))},subclonal_loss = function(x, y, w, h) {grid.polygon(unit.c(x - 0.5*w,  x + 0.5*w, x + 0.5*w), unit.c(y - 0.5*h,  y + 0.5*h, y - 0.5*h),gp = gpar(fill = col['subclonal_loss'], col = 'white'))},subclonal_stop_gained = function(x, y, w, h) {grid.polygon(unit.c(x - 0.5*w,  x + 0.5*w, x + 0.5*w), unit.c(y - 0.5*h,  y + 0.5*h, y - 0.5*h),gp = gpar(fill = col['subclonal_stop_gained'], col = 'white'))},subclonal_frameshift_variant = function(x, y, w, h) {grid.polygon(unit.c(x - 0.5*w,  x + 0.5*w, x + 0.5*w), unit.c(y - 0.5*h,  y + 0.5*h, y - 0.5*h),gp = gpar(fill = col['subclonal_frameshift_variant'], col = 'white'))})

fun2<-list(subclonal_splice_donor_variant = function(x, y, w, h) {grid.polygon(unit.c(x - 0.5*w,  x + 0.5*w, x + 0.5*w), unit.c(y - 0.5*h,  y + 0.5*h, y - 0.5*h),gp = gpar(fill = col['subclonal_splice_donor_variant'], col = 'white'))},subclonal_missense_variant = function(x, y, w, h) {grid.polygon(unit.c(x - 0.5*w,  x + 0.5*w, x + 0.5*w), unit.c(y - 0.5*h,  y + 0.5*h, y - 0.5*h),gp = gpar(fill = col['subclonal_missense_variant'], col = 'white'))},both_missense_variant = function(x, y, w, h) {grid.polygon(unit.c(y - 0.5*h,  y + 0.5*h, y - 0.5*h), unit.c(x - 0.5*w,  x + 0.5*w, x + 0.5*w),gp = gpar(fill = col['both_missense_variant'], col = "white"))})

#library(rgl)
#install.packages("digest")
#library(reshape2)


#' Title
#'
#' @param x
#' @param y
#' @param z
#' @param alpha
#' @param topcol
#' @param sidecol
#'
#' @return
#' @export
#'
#' @examples
stackplot.3d<-function(x,y,z,alpha=1,topcol="#078E53",sidecol="#aaaaaa"){

  ## These lines allow the active rgl device to be updated with multiple changes
  ## This is necessary to draw the sides and ends of the column separately
  save <- par3d(skipRedraw=TRUE)
  on.exit(par3d(save))

  ## Determine the coordinates of each surface of the column and its edges
  x1=c(rep(c(x[1],x[2],x[2],x[1]),3),rep(x[1],4),rep(x[2],4))
  z1=c(rep(0,4),rep(c(0,0,z,z),4))
  y1=c(y[1],y[1],y[2],y[2],rep(y[1],4),rep(y[2],4),rep(c(y[1],y[2],y[2],y[1]),2))
  x2=c(rep(c(x[1],x[1],x[2],x[2]),2),rep(c(x[1],x[2],rep(x[1],3),rep(x[2],3)),2))
  z2=c(rep(c(0,z),4),rep(0,8),rep(z,8) )
  y2=c(rep(y[1],4),rep(y[2],4),rep(c(rep(y[1],3),rep(y[2],3),y[1],y[2]),2) )

  ## These lines create the sides of the column and its coloured top surface
  rgl.quads(x1,z1,y1,col=rep(sidecol,each=4),alpha=alpha,lit=FALSE)
  rgl.quads(c(x[1],x[2],x[2],x[1]),rep(z,4),c(y[1],y[1],y[2],y[2]),
            col=rep(topcol,each=4),alpha=1,lit=FALSE)
  ## This line adds black edges to the column
  rgl.lines(x2,z2,y2,col="#000000",lit=FALSE)
}
# Example:
# stackplot.3d(c(0,1),c(0,1),3,alpha=0.6)

## Calls stackplot.3d repeatedly to create a barplot
## z is the heights of the columns and must be an appropriately named vector
#' Title
#'
#' @param z
#' @param alpha
#' @param scalexy
#' @param scalez
#' @param gap
#'
#' @return
#' @export
#'
#' @examples
context3d<-function(z,alpha=1,scalexy=10,scalez=1,gap=0.2){
  ## These lines allow the active rgl device to be updated with multiple changes
  ## This is necessary to add each column sequentially
  save <- par3d(skipRedraw=TRUE)
  on.exit(par3d(save))

  ## Recreate Broad order
  types=c("C.G.G.C","T.A.A.T","C.A.G.T","T.G.A.C","C.T.G.A","T.C.A.G")
  contexts=c("TxT","CxT","AxT","GxT","TxC","CxC","AxC","GxC",
             "TxA","CxA","AxA","GxA","TxG","CxG","AxG","GxG")
  typeorder=c()
  for(type in types){
    typeorder=c(typeorder,paste(type,contexts,sep="_"))
  }
  z=z[typeorder]

  ## Reorder data into 6 regions
  set1=c(1:4,17:20,5:8,21:24,9:12,25:28,13:16,29:32)
  set2=set1+32
  set3=set1+64
  neworder=c(set1,set2,set3)

  ## Define dimensions of the plot
  dimensions=c(12,8)

  ## Scale column area and the gap between columns
  y=seq(1,dimensions[1])*scalexy
  x=seq(1,dimensions[2])*scalexy
  gap=gap*scalexy

  ## Scale z coordinate
  z=z*scalez

  ## Set up colour palette
  broadcolors=c("red","darkviolet","#5EAFB2","#3F4F9D","#F2EC3C","#74B655")
  #broadcolors=c("#000000","#CAC9C9","#03BCEE","#EBC6C4","#E32926","#A1CE63")
  colors=as.vector(sapply(broadcolors,rep,16))

  i<-1
  j<-2
  ## Plot each of the columns
  for(i in 1:dimensions[1]){
    for(j in 1:dimensions[2]){
      it=(i-1)*dimensions[2]+j # Variable to work out which column to plot; counts from 1:96
      stackplot.3d(c(gap+x[j],x[j]+scalexy),
                   c(-gap-y[i],-y[i]-scalexy),
                   z[neworder[it]],
                   alpha=alpha,
                   topcol=colors[neworder[it]],
                   sidecol=colors[neworder[it]])
    }
  }
  ## Set the viewpoint and add axes and labels
  rgl.viewpoint(theta=50,phi=40,fov=0)
  axes3d("y-+",labels=TRUE)
}
# Example:
# context3d(counts)

#### END OF FUNCTIONS

## Read in example data and cast to an appropriate vector
#rawdata=read.table("snvspermegabase.txt",header=TRUE)
#counts=as.numeric(rawdata)
#names(counts)=colnames(rawdata)

## Example plots
#context3d(counts)

#context3d(counts,alpha=0.5)






