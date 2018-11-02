#' Calculate average values of flows and plot them
#'
#' @import graphics
#' @param flow a dataframe which is to draw half-circles. The data should consist of node of origin, node of destination, and magnitude of the flow on the first three columns.
#' @param node a dataframe which contains names of node on the first column. Nodes on the center line of a circle are drawn by the order of the data.
#' @param dir if 'horizontal', nodes are drawn along the X-axis. If 'vertical', nodes are drawn along the Y-axis.
#' @return A list containing calculated average values c(x-coordinate of weighted mean center, y-coordinate of weighted mean center, weighted average radius,x-coordinate of unweighted mean center, y-coordinate of unweighted mean center, unweighted average radius)
#' @details This function is to get values of mean centers and average radius of flows. One of values of mean centers is weighted by the magnitude of flow and the other one is unweighted. If flows are normally distributed or all combinations of flows between nodes are made, the mean center should be located in the center of a circle, that is (0,0) on the xy-coordinates, and average radius should be 0.5. If the mean center fall in a certain quadrant, a user can evaluate the skewedness.
#' @author Sohyun Park <park.2627@osu.edu>, Ningchuan Xiao
#' @examples
#' data(ex_flow)
#' flow <- subset(ex_flow, ex_flow$veget>5000)
#' data(ex_node)
#' node <- ex_node[c(order(-ex_node$gdpc)),]
#' halfmeancenter(flow, node, dir="vertical")
#' @export
halfmeancenter<-function(flow,node,dir="horizontal"){

  plot.new()
  plot.window(xlim=c(-1.1,1.1),ylim=c(-1.1,1.1),asp=1)
  circle=seq(0,2*pi,length=100)
  p=cos(circle);q=sin(circle)
  polygon(1.1*p,1.1*q,col="lightgray",border="lightgray")
  polygon(p,q,col="white",border="lightgray")

  nodes<-NULL
  nodes$fid<-rep(1:length(node[,c(1)]))
  nodes$x<-2*(nodes$fid-1)/(length(node[,c(1)])-1)-1
  nodes$y<-0
  nodes<-as.data.frame(nodes)
  nodes$name<-as.character(node[,c(1)])

  flow<-flow[,c(1,2,3)]
  colnames(flow)<-c("O","D","volume")
  flow$O<-as.character(flow$O); flow$D<-as.character(flow$D)
  x<-0;y<-0;x_center<-0;y_center<-0;volume<-0;radius_sum<-0;radius_center<-0
  x_un<-0;y_un<-0;x_unweighted<-0;y_unweighted<-0;radius_un<-0;radius_unweighted<-0
  if(dir=="vertical"){
    polygon(c(0.04,-0.12,-0.07),c(1.05,1.2,0.95),col="white",border="white")
    polygon(c(0,-0.15,-0.12),c(1.05,1.12,0.95),col="lightgray",border="lightgray")
    polygon(c(-0.04,0.12,0.07),c(-1.05,-1.2,-0.95),col="white",border="white")
    polygon(c(0,0.15,0.12),c(-1.05,-1.12,-0.95),col="lightgray",border="lightgray")
    for(i in 1:nrow(flow)) {
      node1 <- nodes[nodes$name == flow[i,]$O,]
      node2 <- nodes[nodes$name == flow[i,]$D,]
      radius <- (-node1$x+node2$x)/2
      if(radius!=0){
        width <- flow[i,]$volume
        x<-width*(radius*4)/(3*pi) + x
        y<-width*(-node1$x-node2$x)/2 + y
        radius_sum<-width*radius+radius_sum
        x_un<-(radius*4)/(3*pi) + x_un
        y_un<-(-node1$x-node2$x)/2 + y_un
        radius_un<-radius+radius_un
        volume<-volume+width
      }}}
  else{
      polygon(c(1.05,1.2,0.95),c(-0.04,0.12,0.07),col="white",border="white")
      polygon(c(1.05,1.12,0.95),c(0,0.15,0.12),col="lightgray",border="lightgray")
      polygon(c(-1.05,-1.2,-0.95),c(0.04,-0.12,-0.07),col="white",border="white")
      polygon(c(-1.05,-1.12,-0.95),c(0,-0.15,-0.12),col="lightgray",border="lightgray")
      for(i in 1:nrow(flow)) {
        node1 <- nodes[nodes$name == flow[i,]$O,]
        node2 <- nodes[nodes$name == flow[i,]$D,]
        radius <- (-node1$x+node2$x)/2
        if(radius!=0){
          width <- flow[i,]$volume
          x<-width*(node1$x+node2$x)/2 + x
          y<-width*(radius*4)/(3*pi) + y
          radius_sum<-width*radius+radius_sum
          x_un<-(node1$x+node2$x)/2 + x_un
          y_un<-(radius*4)/(3*pi) + y_un
          radius_un<-radius+radius_un
          volume<-volume+width
          }}}
  x_center<-x/volume
  y_center<-y/volume
  radius_center<-radius_sum/volume
  x_unweighted<-x_un/nrow(flow)
  y_unweighted<-y_un/nrow(flow)
  radius_unweighted<-radius_un/nrow(flow)
  points(x_center,y_center,pch=19)
  points(x_unweighted,y_unweighted,pch=1)
  points(0,0,pch=4,col="lightgray")
  text(x_center+0.15,y_center,"weighted",cex=0.5)
  text(x_unweighted+0.15,y_unweighted,"unweighted",cex=0.5)
  print(paste("weighted mean center (",round(x_center,3),",",round(y_center,3),")",", r.weighted=",round(radius_center,3)))
  print(paste("unweighted mean center (",round(x_unweighted,3),",",round(y_unweighted,3),")",", r.unweighted=",round(radius_unweighted,3)))
  half.meancircle<-c(x.w=x_center,y.w=y_center,r.w=radius_center,x.u=x_unweighted,y.u=y_unweighted,r.u=radius_unweighted)
  half.meancircle
}
