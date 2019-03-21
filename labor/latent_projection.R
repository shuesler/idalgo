library(magrittr)
library(causaleffect)
library(igraph)
#library(googledrive)
#library(rlang)
#library(tidyverse)
#library(data.tree)
#library(collapsibleTree)
#library(lobstr)

doll  <- magrittr::use_series
options(tibble.print_min = 100)

# --------------------------------------------
# Smoking:
G     <- parse.graphml("C:\\Users\\shs\\Desktop\\graph_ed\\2.graphml")

sils  <- causaleffect:::latent.projection(G, "gene")

ce1   <- causal.effect(y = "Y", x = "X", z = NULL, G = sils, expr = TRUE, steps = TRUE, primes = T)

ce1$P

ce1$P %>% katexR::katex()

# --------------------------------------------
# Smoking - Kontrolle
fig1    <- igraph::graph.formula(X -+ Z, Z -+ Y, X -+ Y, Y -+ X,simplify = FALSE)
fig1    <- igraph::set.edge.attribute(graph = fig1, name  = "description", index = c(3,4), value = "U")

ce1     <- causal.effect(y = "Y", x = "X", z = NULL, G = fig1, expr = TRUE, steps = TRUE, primes = T)

ce1$P

ce1$P %>% katexR::katex()


# --------------------------------------------
# Pearl 2009 S 119: Figure 4.4

G     <- parse.graphml("C:\\Users\\shs\\Desktop\\graph_ed\\pearl2009_119.graphml")

l     <- c("U1", "U2")


sils <- causaleffect:::latent.projection(G, l)


G.obs <- causaleffect:::observed.graph(sils)
topo  <- topological.sort(G.obs)
topo  <- get.vertex.attribute(G, "name")[topo]
causaleffect:::c.components(sils, topo)



ce1   <- causal.effect(y = "Y", x = c("X1", "X2"), z = NULL, G = sils, expr = TRUE, steps = TRUE, primes = T)
causaleffect:::c.components(G, topo)
ce1$P %>% katexR::katex()

# ---------
# neu
#X1->Y
#X1->X2
#X1->Z
#
#X2->Y
#Z ->X2
#
#X1->Y
#X1->Z
#
#X1->Z
#Z ->X1
#
#Y ->Z
#Z ->Y

# ---------
# alt
#X1->Y
#X1->X2
#X1->Z
#
#X2->Y
#Z ->X2
#
#U2->Y
#X1->U2
#U2->Z
#U1->X1
#U1->Z
# ---------


latent.projection <- function(G, l) {

  to            <- NULL
  from          <- NULL
  description   <- NULL

  for (i in 1:length(l)) {

    e           <- E(G)
    v           <- get.vertex.attribute(G, "name")

    inc.edges   <- e[to(l[i])   & (is.na(description) | description != "U")]
    out.edges   <- e[from(l[i]) & (is.na(description) | description != "U")]

    unobs.edges <- e[to(l[i])   & description == "U" & !is.na(description)]

    inc.ind     <- get.edges(G, inc.edges)[ ,1]
    out.ind     <- get.edges(G, out.edges)[ ,2]

    unobs.ind   <- setdiff(get.edges(G, unobs.edges)[ ,1], out.ind)
    inc.len     <- length(inc.ind)
    out.len     <- length(out.ind)
    unobs.len   <- length(unobs.ind)

    if (inc.len > 0 & out.len > 0) {
      obs.new   <- t(as.matrix(expand.grid(inc.ind, out.ind)))
      G         <- G + edges(v[c(obs.new)]) # replace path v_1 -> L -> v_2 with v_1 -> v_2
    }
    if (out.len > 1) {
      unobs.new <- combn(out.ind, 2)
      G         <- G + edges(v[c(unobs.new, unobs.new[2:1, ])], description = rep("U", 2 * ncol(unobs.new))) # replace path v_1 <- L -> v_2 with v_1 <-> v_2
    }
    if (unobs.len > 0 & out.len > 0) {
      unobs.old <- t(as.matrix(expand.grid(unobs.ind, out.ind)))
      G         <- G + edges(v[c(unobs.old, unobs.old[2:1, ])], description = rep("U", 2 * ncol(unobs.old))) # replace path v_1 <-> L -> v_2 with v_1 <-> v_2
    }
    G           <- induced.subgraph(G, base::setdiff(v, l[i]))
    e.dat       <- as.data.frame(get.edges(G, E(G)))
    e.dat[ ,3]  <- edge.attributes(G)
    G           <- subgraph.edges(G, which(!duplicated(e.dat)))
  }
  return(G)
}



e[to(l[i]) & (is.na(description) | description != "U")]

e[from("gene")]
