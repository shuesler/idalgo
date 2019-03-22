
liste %>% names

liii <- list()

ppp <- function(l) {

  liii <<- append(liii, names(l))

  pruef <- map(l, is.list) %>% purrr::as_vector() %>% unname()


  if(any(pruef) == FALSE) {
    return(print("FERTIG"))
  }


  tmp <- list()
  neu <- l[pruef]
  for(i in seq_along(neu)) {

    tmp <- append(tmp, neu[[i]])


  }


  ppp(tmp)


}


ppp(liste)


library(tidyverse)
library(rlang)


liste <- list(
  a = c(a1 = list("a1"), a2 = list("a2")),
  nichts1 = 1,
  b = c(b1 = list("b1"), b2 = list("b2")),
  nichts2 = 2,
  c = c(c1 = list("c1"), c2 = list("c2")),
  nichts3 = 3,
  d = c(d1 = list("d1"), d2 = list("d2")),
  nichts4 = NULL,
  e = c(e1 = list("e1"), e2 = list("e2")),
  list(9, list(10, list(99), list(88)))
)




map(aaa, is.list) %>% purrr::as_vector() %>% unname()


foo <- function(l){

  listviewer::number_unnamed(l)

  lapply(l,
         function(x)
           if(is.list(x) && length(x)==0) "nichts"
         else
           if(is.list(x)) foo(x)
         else x)
}



foo(liste)
listviewer::number_unnamed(liste)


where <- function(name, env = caller_env()) {
  if (identical(env, empty_env())) {
    # Base case
    stop("Can’t find", name, call. = FALSE)
  } else if (env_has(env, name)) {
    # Success case
    env
  } else {
    # Recursive case
    where(name, env_parent(env))
  }
}





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

