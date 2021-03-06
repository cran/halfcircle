% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/halfmeancenter.R
\name{halfmeancenter}
\alias{halfmeancenter}
\title{Calculate average values of flows and plot them}
\usage{
halfmeancenter(flow, node, dir = "horizontal")
}
\arguments{
\item{flow}{a dataframe which is to draw half-circles. The data should consist of node of origin, node of destination, and magnitude of the flow on the first three columns.}

\item{node}{a dataframe which contains names of node on the first column. Nodes on the center line of a circle are drawn by the order of the data.}

\item{dir}{if 'horizontal', nodes are drawn along the X-axis. If 'vertical', nodes are drawn along the Y-axis.}
}
\value{
A list containing calculated average values c(x-coordinate of weighted mean center, y-coordinate of weighted mean center, weighted average radius,x-coordinate of unweighted mean center, y-coordinate of unweighted mean center, unweighted average radius)
}
\description{
Calculate average values of flows and plot them
}
\details{
This function is to get values of mean centers and average radius of flows. One of values of mean centers is weighted by the magnitude of flow and the other one is unweighted. If flows are normally distributed or all combinations of flows between nodes are made, the mean center should be located in the center of a circle, that is (0,0) on the xy-coordinates, and average radius should be 0.5. If the mean center fall in a certain quadrant, a user can evaluate the skewedness.
}
\examples{
data(ex_flow)
flow <- subset(ex_flow, ex_flow$veget>5000)
data(ex_node)
node <- ex_node[c(order(-ex_node$gdpc)),]
halfmeancenter(flow, node, dir="vertical")
}
\author{
Sohyun Park <park.2627@osu.edu>, Ningchuan Xiao
}
