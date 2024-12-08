library(idalgo)

gml   <- read.gml(here::here("labor", "hernan_book_graphs", "20.3.gml"), coords = T)

G     <- parse.graphml(here::here("labor", "hernan_book_graphs", "20.3.graphml"))

G     <- set_graph_attr(G, "layout", mat_from_gml(gml))

G.lat <- latent.projection(G, "U1")

plot(G)
plot(G.lat)
plot(mark_U(G.lat))
