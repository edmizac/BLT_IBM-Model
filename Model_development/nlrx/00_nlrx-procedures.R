library("nlrx")
library("here")

modelpath <- here("Model_simulations", "BLT_model_v1.2.nlogo") # Last version with stored-energy
nw <- nldoc_network(modelpath) 

# Determine communities within the network and plot using Igraph package:
library("igraph")
com <- walktrap.community(nw)
V(nw)$community <- com$membership
rain <- rainbow(14, alpha=.5)
V(nw)$color <- rain[V(nw)$community]

plot.igraph(nw,
     edge.arrow.size=.5,
     vertex.label.color="black",
     vertex.label.dist=1.5,
     vertex.size=12,
     edge.curved=0,
     vertex.label.cex=.8,
     layout=layout_with_fr(nw, niter = 2000)
     # layout=layout_with_kk
     )

# par(mfrow=c(1,2), mar=c(2,1,2,1))
plot(g, layout=LO, frame=TRUE)

# Save png
# ggsave("figure2.png", plot(g, layout=layout_with_kk))
png(filename=here("Model_development", "nlrx", "00_nlrx_procedures_BLT_model_v1.2.png"), 
    height=800, width=600, res = 300) #call the png writer
#run the plot
dev.off() #dont forget to close the device


# Interactive plot using igraph::tkplot
tkplot(nw, layout=layout_with_fr(nw, niter = 2000))


# Save html
library(visNetwork)
library(htmlwidgets)
saveWidget(visIgraph(nw), file = here("Model_development", "nlrx", "00_nlrx_procedures_BLT_model_v1.2.html"))
