
G            <- causaleffect::parse.graphml("C:/Users/shs/Desktop/2b.graphml")

igraph::E(G)

G <- igraph::set.edge.attribute(graph = G, name = "description", index = c(9,11,12,13), value = "U")



result <- causaleffect::causal.effect(y = "Y", x = c("A"), z = NULL, G = G, expr = TRUE, steps = TRUE, primes = T)
katexR::katex(result$P)



