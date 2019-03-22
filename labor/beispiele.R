# ref(alles)

##########################################################################################################

# Smoking - Beispiel

#fig1    <- igraph::graph.formula(X -+ Z, Z -+ Y, X -+ Y, Y -+ X,simplify = FALSE)
#fig1    <- igraph::set.edge.attribute(graph = fig1, name  = "description", index = c(3,4), value = "U")
#
#NODES   <- names(V(fig1))
#
#ce1     <- causal.effect(y = "Y", x = "X", z = NULL, G = fig1, expr = TRUE, steps = TRUE, primes = T)
#
#ce1$P %>% katexR::katex()
#
#alles   <- ce1$steps

##########################################################################################################

# Hernan 20.3

# Auf Seite 19 (Teil III) steht:
# Table 20.1 shows data from a sequentially randomized trial with treatmentconfounder
# feedback, as represented by the causal diagram in Figure 20.3.

#fig1  <- graph.formula(
#  L0 -+ A0,
#  A0 - +L1,
#  L1 - +A1,
#  L0 ++ Y,
#  L1 ++ Y,
#  simplify = FALSE)
#
#fig1  <- igraph::set.edge.attribute(graph = fig1, name  = "description",index = c(4,7), value = "U")
#
#NODES <- names(V(fig1))
#
#ce1   <- causal.effect(y="Y", x=c("A0", "A1"), G = fig1, primes = T, steps = T)
#
#ce1$P %>% katexR::katex()
#
#alles <- ce1$steps

##########################################################################################################
library(rlang)

y <- "hund"

a <- function(x) {
  quo_name(x)
}

b <- function(x) {
  a(x)
}

d <- function(x) {
  w <- enquo(x)

  b(w)
}

qq <- y

d(qq)

