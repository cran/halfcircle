#' Visualization method for flow data using halfcircle diagram
#'
#' halfcircle function draws flows between nodes creating halfcircle diagram.
#' @import scales, graphics
#' @param flow a dataframe which is to draw half-circles. The data should be in the form of an edge list containing node of origin, node of destination, and magnitude of the flow on the first three columns.
#' @param node a dataframe which contains names of node on the first column. Nodes on the center line of a circle are drawn by the order of the data. Every node presented in flow data must be contained.
#' @param dir if 'horizontal' (the default), nodes are drawn along the X-axis. If 'vertical', nodes are drawn along the Y-axis.
#' @param circle.col color of background circle
#' @param circle.trans transparency of color of background circle
#' @param flow.col flow color. flow.col can be a list of color vectors, the vectors are then used per flow.
#' @param flow.trans transparency of color of flows
#' @param flow.width width of flows. if 'proportional' (the default), each width is calculated to be proportional to the maximum volume of flows. Maximum width is set to be 10. Otherwise, a list of width vectors can be used per flow.
#' @param node.color node color. It can be a list of color vectors, and the vectors are then used per node.
#' @param node.size node size
#' @param node.pch node type. see ?points for more options.
#' @param node.trans transparency of color of flows
#' @param label the first column of node, names, is represented (the default). a list of vector an be used per node. if NULL, no label is drawn.
#' @param label.size label size
#' @param label.col label color
#' @param label.gap gap between the node and the respective label
#' @details This function is a low-level graphical function, and you will create a halfcircle diagram. To create the diagram, nodes are placed as a set of points on a straight line segment in the center of a circle. The flow between two nodes is represented using a half cicle drawn from the origin to the destination in a clockwise direction. It is virtually drawn on xy-coordinates where both x and y range from -1 to 1. Flows between the same nodes are not drawn.
#' @references Xiao and Chun (2009) <doi:10.1559/152304009788188763>
#' @author Sohyun Park <park.2627@osu.edu>, Ningchuan Xiao
#' @examples
#' # load flow data
#' data(ex_flow)
#' flow <- ex_flow[,c(1,2,3)] # select veget column as volume
#' flow <- subset(flow,flow$vegetable>5000)
#' data(ex_node) # load node data
#' node <- ex_node[c(order(-ex_node$gdpc)),] # sort nodes in descending order of gdpc values
#' halfcircle(flow, node, dir="vertical", circle.col="gray", flow.col="black",label=NULL)
#'
#' # legend
#' max <- max(flow[,c(3)]); median <- median(flow[,c(3)]); min <- min(flow[,c(3)])
#' max_w <- 10; median_w <- round(10*median/max); min_w <- round(10*min/max)
#' legend(x=-1.2, y=-0.8, legend=c(paste(round(max)), paste(round(median)), paste(round(min))),
#'     lty=1, lwd=c(max_w, median_w, min_w), cex=0.7)
#'
#' # customize colors
#' node$color <- c("#22abcb","#4eb6ad","#86c388","#adcd6c","#dad84f")[node$income_level]
#' flow2 <- data.frame(flow, node[match(flow[,"O"], node[,"country"]),])
#' halfcircle(flow2, node, dir="vertical", flow.col=flow2$color, node.color=node$color, label=NULL)
#'
#' # highlight one node
#' flow3 <- flow
#' flow3$color <- "gray"
#' flow3$color[flow3$O=="China"|flow3$D=="China"] <- "blue"
#' flow3 <- flow3[c(order(flow3$color,decreasing=TRUE)),]
#' node$label <- ""
#' node$label[node$country=="China"] <- "China"
#' halfcircle(flow3, node, dir="vertical", flow.col=flow3$color, label=node$label, label.size=0.7)
#' @export
halfcircle<-function(flow, node, dir="horizontal",
                     circle.col="lightgray",circle.trans=0.5,
                     flow.col="black",flow.trans=0.5,flow.width="proportional",
                     node.color="black",node.size=0.1,node.pch=20,node.trans=0.7,
                     label=node[,c(1)],label.size=0.5,label.col="black",label.gap=0.1){

  plot.new()
  plot.window(xlim=c(-1.1,1.1),ylim=c(-1.1,1.1),asp=1)
  circle=seq(0,2*pi,length=100)
  p=cos(circle);q=sin(circle)
  polygon(p,q,col=scales::alpha(circle.col,circle.trans),border=scales::alpha(circle.col,circle.trans))

  nodes<-NULL
  nodes$fid<-rep(1:length(node[,c(1)]))
  nodes$x<-2*(nodes$fid-1)/(length(node[,c(1)])-1)-1
  nodes$y<-0
  nodes<-as.data.frame(nodes)
  nodes$name<-as.character(node[,c(1)])

  flow<-flow[,c(1,2,3)]
  colnames(flow)<-c("O","D","volume")
  flow$O<-as.character(flow$O); flow$D<-as.character(flow$D)

  if(dir=="vertical"){
    for(i in 1:nrow(flow)) {
      node1 <- nodes[nodes$name == flow[i,]$O,]
      node2 <- nodes[nodes$name == flow[i,]$D,]
      radius <- (node1$x-node2$x)/2
      ifelse(flow.width=="proportional",width<-round(10*flow[i,]$volume/max(flow$volume)),width<-flow.width[i])
      flow$color<-flow.col
      if (radius>0) {
        theta=seq(0,-pi,length=5000)
        p<-radius*cos(theta)+node1$x-radius
        q<-radius*sin(theta)
        lines(q,-p,lwd=width,col=scales::alpha(flow$color[i],flow.trans))
      } else if(radius<0){
        theta=seq(0,pi,length=5000)
        p<-abs(radius)*cos(theta)+node1$x-radius
        q<-abs(radius)*sin(theta)
        lines(q,-p,lwd=width,col=scales::alpha(flow$color[i],flow.trans))
      }}
    points(nodes$y,-1*nodes$x,col=scales::alpha(node.color,node.trans),pch=node.pch, cex=node.size)
    if(is.null(label)==FALSE){text(nodes$y+label.gap,-nodes$x,label,cex=label.size,col=label.col)}

  }  else{
    for(i in 1:nrow(flow)) {
      node1 <- nodes[nodes$name == flow[i,]$O,]
      node2 <- nodes[nodes$name == flow[i,]$D,]
      radius <- (node1$x-node2$x)/2
      ifelse(flow.width=="proportional",width<-round(10*flow[i,]$volume/max(flow$volume)),width<-flow.width[i])
      flow$color<-flow.col
      if(radius>0) {
        theta=seq(0,-pi,length=5000)
        p<-radius*cos(theta)+node1$x-radius
        q<-radius*sin(theta)
        lines(p,q,lwd=width,col=scales::alpha(flow$color[i],flow.trans))
      } else if(radius<0){
        theta=seq(0,pi,length=5000)
        p<-abs(radius)*cos(theta)+node1$x-radius
        q<-abs(radius)*sin(theta)
        lines(p,q,lwd=width,col=scales::alpha(flow$color[i],flow.trans))
      }}
    points(nodes$x,nodes$y,col=scales::alpha(node.color,node.trans),pch=node.pch,cex=node.size)
    if(is.null(label)==FALSE){text(nodes$y+label.gap,-nodes$x,label,cex=label.size,col=label.col)}
  }
}
