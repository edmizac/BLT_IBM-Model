library(nlrx)
library(here)
modelpath <- here("Model_Mayara2020", "BLT_model_2020.nlogo")
nw <- nldoc_network(modelpath) 

# Determine communities within the network and plot using Igraph package:
library(igraph)
com <- walktrap.community(nw)
V(nw)$community <- com$membership
rain <- rainbow(14, alpha=.5)
V(nw)$color <- rain[V(nw)$community]

plot(nw,
     edge.arrow.size=.2,
     vertex.label.color="black",
     vertex.label.dist=1,
     vertex.size=5,
     edge.curved=0,
     vertex.label.cex=.5,
     layout=layout_with_fr(nw, niter = 2000))

# Interactive plot using igraph::tkplot
tkplot(nw, layout=layout_with_fr(nw, niter = 2000))
