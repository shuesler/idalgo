

gml          <- idalgo::read.gml("C:/Users/shs/Desktop/pearl2009_119.gml", coords = T)
G            <- causaleffect::parse.graphml("C:/Users/shs/Desktop/pearl2009_119.graphml")

mat          <- idalgo::mat_from_gml(gml)
G            <- igraph::set_graph_attr(G, "layout", mat)

G.lat        <- idalgo::latent.projection(G, c("U1", "U2"))

G.lat.marked <- idalgo::mark_U(G.lat)

plot(G)
plot(G.lat.marked)

# ------------------------------------------------------------------------------

rm(list=ls())
rstudioapi::restartSession()

# ------------------------------------------------------------------------------

G            <- causaleffect::parse.graphml("C:/Users/shs/Desktop/pearl2009_119.graphml")
G.lat        <- causaleffect:::latent.projection(G, c("U1", "U2"))
parse.joint  <- causaleffect:::parse.joint

result <- causaleffect::causal.effect(y = "Y", x = c("X1", "X2"), z = NULL, G = G.lat, expr = TRUE, steps = TRUE, primes = T)
katexR::katex(result$P)
