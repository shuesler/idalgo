library(idalgo)




G     <- parse.graphml("C:\\Users\\shs\\Desktop\\hernan_book_graphs\\19.1.graphml")
gml   <- read.gml("C:\\Users\\shs\\Desktop\\hernan_book_graphs\\19.1.gml", coords = T)

G     <- parse.graphml("C:\\Users\\shs\\Desktop\\hernan_book_graphs\\20.3.graphml")
gml   <- read.gml("C:\\Users\\shs\\Desktop\\hernan_book_graphs\\20.3.gml", coords = T)


G     <- set_graph_attr(G, "layout", mat_from_gml(gml))

plot(G)


G.lat <- latent.projection(G, "U1")

plot(G.lat)

plot(mark_U(G.lat))



