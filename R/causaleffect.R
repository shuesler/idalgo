
#' @export
doll <- `$`

#' @export
`%ts%`    <- function(nodes, topo.order) {nodes[order(match(nodes, topo.order))]}

#' @export
ancestors <- function(node, G, topo) {
  an.ind  <- unique(unlist(neighborhood(G, order = igraph::vcount(G), nodes = node, mode = "in")))
  an      <- V(G)[an.ind]$name
  an      <- an %ts% topo
  return (an)
}

#' @export
observed.graph <- function(G) {
  obs.edges <- setdiff(E(G), E(G)[which(edge.attributes(G)$description == "U")])
  G.obs     <- subgraph.edges(G, E(G)[obs.edges], delete.vertices = FALSE)
  return(G.obs)
}

#' @export
latent.projection <- function(G, l) {
  to <- NULL
  from <- NULL
  description <- NULL
  for (i in 1:length(l)) {
    e <- E(G)
    v <- get.vertex.attribute(G, "name")
    inc.edges <- e[to(l[i]) & (is.na(description) | description != "U")]
    out.edges <- e[from(l[i]) & (is.na(description) | description != "U")]
    unobs.edges <- e[to(l[i]) & description == "U" & !is.na(description)]
    inc.ind <- get.edges(G, inc.edges)[ ,1]
    out.ind <- get.edges(G, out.edges)[ ,2]
    unobs.ind <- setdiff(get.edges(G, unobs.edges)[ ,1], out.ind)
    inc.len <- length(inc.ind)
    out.len <- length(out.ind)
    unobs.len <- length(unobs.ind)
    if (inc.len > 0 & out.len > 0) {
      obs.new <- t(as.matrix(expand.grid(inc.ind, out.ind)))
      G <- G + edges(v[c(obs.new)]) # replace path v_1 -> L -> v_2 with v_1 -> v_2
    }
    if (out.len > 1) {
      unobs.new <- combn(out.ind, 2)
      G <- G + edges(v[c(unobs.new, unobs.new[2:1, ])], description = rep("U", 2 * ncol(unobs.new))) # replace path v_1 <- L -> v_2 with v_1 <-> v_2
    }
    if (unobs.len > 0 & out.len > 0) {
      unobs.old <- t(as.matrix(expand.grid(unobs.ind, out.ind)))
      G <- G + edges(v[c(unobs.old, unobs.old[2:1, ])], description = rep("U", 2 * ncol(unobs.old))) # replace path v_1 <-> L -> v_2 with v_1 <-> v_2
    }
    G <- induced.subgraph(G, setdiff(v, l[i]))
    e.dat <- as.data.frame(get.edges(G, E(G)))
    e.dat[ ,3] <- edge.attributes(G)
    G <- subgraph.edges(G, which(!duplicated(e.dat)))
  }
  return(G)
}


##################################################################################################################################

#' @export
causal.effect <- function(y, x, z = NULL, G, expr = TRUE, simp = FALSE, steps = FALSE, primes = FALSE, prune = FALSE, stop_on_nonid = TRUE) {
  if (length(edge.attributes(G)) == 0) {
    G                           <- set.edge.attribute(G, "description", 1:length(E(G)), NA)
  }
  # ------------------------------------------------------------------------------
  G.obs                         <- observed.graph(G)
  # ------------------------------------------------------------------------------
  if (!is.dag(G.obs)) stop("Graph 'G' is not a DAG")
  # ------------------------------------------------------------------------------
  topo                          <- topological.sort(G.obs)
  topo                          <- get.vertex.attribute(G, "name")[topo]
  # ------------------------------------------------------------------------------
  if (length(setdiff(y, topo)) > 0) stop("Set 'y' contains variables not present in the graph.")
  if (length(setdiff(x, topo)) > 0) stop("Set 'x' contains variables not present in the graph.")
  if (length(z) > 0 && !identical(z, "")) {
    if (length(setdiff(z, topo)) > 0) stop("Set 'z' contains variables not present in the graph.")
  }
  if (length(intersect(x, y)) > 0) stop("Sets 'x' and 'y' are not disjoint.")
  if (length(intersect(y, z)) > 0) stop("Sets 'y' and 'z' are not disjoint.")
  if (length(intersect(x, z)) > 0) stop("Sets 'x' and 'z' are not disjoint.")
  # ------------------------------------------------------------------------------
  res                           <- list()
  algo                          <- ""
  res.prob                      <- probability()
  # ------------------------------------------------------------------------------
  if (is.null(z) || identical(z, "") || identical(z, character(0))) {
    if (prune) {
      res                       <- pid(y, x, probability(), G, G.obs, topo, topo, list())
      algo                      <- "pid"
    } else {
      #
      res                       <- id(y, x, probability(), G, G.obs, topo, topo, list()) # +++++++++++++++++++++++++++++++++++++++++++++++++++
      algo                      <- "id"
    }
    res.prob                    <- res$P
    # ------------------------------------------------------------------------------
  } else {
    res                         <- idc(y, x, z, probability(), G, G.obs, topo, topo, list(), prune)
    res.num                     <- res$P
    res.den                     <- res.num
    res.den$sumset              <- union(res.den$sumset, y)
    res.prob$fraction           <- TRUE
    res.prob$num                <- res.num
    res.prob$den                <- res.den
    algo                        <- "idc"
  }
  # ------------------------------------------------------------------------------
  res.tree                      <- res$tree
  # ------------------------------------------------------------------------------
  if (res$tree$call$id) {
    if (simp) {
      G.unobs                   <- unobserved.graph(G)
      G.adj                     <- as.matrix(get.adjacency(G.unobs))
      topo.u                    <- topological.sort(G.unobs)
      topo.u                    <- get.vertex.attribute(G.unobs, "name")[topo.u]
      res.prob                  <- deconstruct(res.prob, probability(), topo)
      res.prob                  <- parse.expression(res.prob, topo, G.adj, G, G.obs)
    }
    attr(res.prob, "algorithm") <- algo
    attr(res.prob, "query")     <- list(y = y, x = x, z = z)
    if (expr) res.prob          <- get.expression(res.prob, primes)
    if (steps) return(list(P = res.prob, steps = res.tree, id = TRUE))
    return(res.prob)
  } else {
    if (stop_on_nonid) stop("Not identifiable.", call. = FALSE)
    res.prob                    <- probability()
    attr(res.prob, "algorithm") <- algo
    attr(res.prob, "query")     <- list(y = y, x = x, z = z)
    if (steps) return(list(P = res.prob, steps = res$tree, id = FALSE))
    if (expr) return("")
    return(NULL)
  }
  # ------------------------------------------------------------------------------
}

##################################################################################################################################

#' @export
idc <- function(y, x, z, P, G, G.obs, v, topo, tree, prune) {
  to                                <- NULL
  if (length(P$var) == 0) tree$call <- list(y = y, x = x, z = z, P = probability(var = v), G = G, line = "", v = v, id = FALSE)
  else tree$call                    <- list(y = y, x = x, z = z, P = P, G = G, line = "", v = v, id = FALSE)
  tree$branch                       <- list()
  offset                            <- (prune) * 2
  from                              <- NULL
  G.xz                              <- unobserved.graph(G)
  edges.to.x                        <- E(G.xz)[to(x)]
  edges.from.z                      <- E(G.xz)[from(z)]
  G.xz                              <- subgraph.edges(G.xz, E(G.xz)[setdiff(E(G.xz), union(edges.to.x, edges.from.z))], delete.vertices = FALSE)
  A                                 <- as.matrix(get.adjacency(G.xz))
  nxt                               <- list()
  for (node in z) {
    cond                            <- setdiff(z, node)
    if (dSep(A, y, node, union(x, cond))) {
      tree$call$line                <- 9 + offset
      tree$call$z.prime             <- node
      nxt                           <- idc(y, union(x, node) %ts% topo, cond, P, G, G.obs, v, topo, list(), prune)
      tree$branch[[1]]              <- nxt$tree
      tree$call$id                  <- nxt$tree$call$id
      return(list(P = nxt$P, tree = tree))
    }
  }
  if (prune) nxt                    <- pid(union(y, z) %ts% topo, x, P, G, G.obs, v, topo, list())
  else nxt                          <- id(union(y, z) %ts% topo, x, P, G, G.obs, v, topo, list()) # +++++++++++++++++++++++++++++++++++++++++++++++++++
  tree$call$line                    <- 10 + offset
  tree$call$id                      <- nxt$tree$call$id
  tree$branch[[1]]                  <- nxt$tree
  return(list(P = nxt$P, tree = tree))
}

##################################################################################################################################

#' @export
id <- function(y, x, P, G, G.obs, v, topo, tree) {
  to                                                            <- NULL
  from                                                          <- NULL
  description                                                   <- NULL
  if (length(P$var) == 0 & !(P$product | P$fraction)) tree$call <- list(y = y, x = x, P = probability(var = v), G = G, line = "", v = v, id = FALSE)
  else tree$call                                                <- list(y = y, x = x, P = P, G = G, line = "", v = v, id = FALSE)

  # line 1
  if (length(x) == 0) {
    if (P$product | P$fraction) {
      P$sumset                                                  <- union(setdiff(v, y), P$sumset) %ts% topo
      # P                                                       <- simplify.expression(P, NULL)
    } else {
      P$var                                                     <- y
    }
     #1
    tree$call$line                                              <- 1
    tree$call$id                                                <- TRUE
    tree$root                                                   <- P

    return(list(P = P, tree = tree))
  }

  an                                                            <- ancestors(y, G.obs, topo)

  # line 2
  if (length(setdiff(v, an)) != 0) {
    G.an                                                        <- induced.subgraph(G, an)
    G.an.obs                                                    <- observed.graph(G.an)
    if (P$product | P$fraction) {
      P$sumset                                                  <- union(setdiff(v, an), P$sumset) %ts% topo
      # P                                                       <- simplify.expression(P, NULL)
    } else {
      P$var                                                     <- an
    }

    nxt                                                         <- id(y, intersect(x, an), P, G.an, G.an.obs, an, topo, list()) #2 #2 # +++++++++++++++++++++++++++++++++++++++++++++++++++
    tree$branch[[1]]                                            <- nxt$tree
     #2
    tree$call$line                                              <- 2
    tree$call$id                                                <- nxt$tree$call$id
    tree$call$an                                                <- an
    return(list(P = nxt$P, tree = tree))
  }

  # line 3
  G.xbar                                                        <- subgraph.edges(G, E(G)[!(to(x) | (from(x) & (description == "U" & !is.na(description))))], delete.vertices = FALSE)
  an.xbar                                                       <- ancestors(y, observed.graph(G.xbar), topo)
  w                                                             <- setdiff(setdiff(v, x), an.xbar)
  w.len                                                         <- length(w)
  if (w.len != 0) {

    nxt                                                         <- id(y, union(x, w) %ts% topo, P, G, G.obs, v, topo, list()) #3 #3   # +++++++++++++++++++++++++++++++++++++++++++++++++++
    tree$branch[[1]]                                            <- nxt$tree
     #3
    tree$call$line                                              <- 3
    tree$call$id                                                <- nxt$tree$call$id
    tree$call$w                                                 <- w
    tree$call$an.xbar                                           <- an.xbar
    return(list(P = nxt$P, tree = tree))
  }

  # line 4
  G.remove.x                                                    <- induced.subgraph(G, v[!(v %in% x)])
  s                                                             <- c.components(G.remove.x, topo)

  if (length(s) > 1) {

     #4
    tree$call$line                                              <- 4
    nxt                                                         <- lapply(s, function(t) {
      return(id(t, setdiff(v, t), P, G, G.obs, v, topo, list()))
    })
    product.list                                                <- lapply(nxt, "[[", "P")
    tree$branch                                                 <- lapply(nxt, "[[", "tree")
    tree$call$id                                                <- all(sapply(nxt, function(x) x$tree$call$id))
    return(list(
      P = probability(sumset = setdiff(v, union(y, x)), product = TRUE, children = product.list),
      tree = tree
    ))
  } else {
    s                                                           <- s[[1]]

    # line 5
    cc                                                          <- c.components(G, topo)
    if (identical(cc[[1]], v)) {
      tree$call$s                                               <- cc[[1]]
       #5
      tree$call$line                                            <- 5
      tree$call$id                                              <- FALSE
      tree$root                                                 <- P
      return(list(P = P, tree = tree))
    }

    # line 6
    pos                                                         <- Position(function(x) identical(s, x), cc, nomatch = 0)
    if (pos > 0) {
       #6
      tree$call$line                                            <- 6
      tree$call$s                                               <- s
      ind                                                       <- which(v %in% s)
      s.len                                                     <- length(s)
      product.list                                              <- vector(mode = "list", length = s.len)
      P.prod                                                    <- probability()
      for (i in s.len:1) {
        # cond.set                                              <- causal.parents(s[i], v[1:ind[i]], G, G.obs, topo)
        cond.set                                                <- v[0:(ind[i]-1)]
        if (P$product) {
          P.prod                                                <- parse.joint(P, s[i], cond.set, v, topo)
          # P.prod                                              <- simplify.expression(P.prod, NULL)
        } else {
          P.prod                                                <- P
          P.prod$var                                            <- s[i]
          P.prod$cond                                           <- cond.set
        }
        product.list[[s.len - i + 1]]                           <- P.prod
      }
      if (s.len > 1) {
        P.new                                                   <- probability(sumset = setdiff(s, y), product = TRUE, children = product.list)
        # P.new                                                 <- simplify.expression(P.new, NULL)
        tree$root                                               <- P.new
        tree$call$id                                            <- TRUE

        return(list(P = P.new, tree = tree))
      }
      if (P.prod$product | P.prod$fraction) {
        P.prod$sumset                                           <- union(P.prod$sumset, setdiff(s, y)) %ts% topo
        # P.prod                                                <- simplify.expression(P.prod, NULL)
      } else {
        P.prod$var                                              <- setdiff(P.prod$var, union(P.prod$sumset, setdiff(s, y)))
      }
      tree$root                                                 <- P.prod
      tree$call$id <- TRUE

      return(list(P = P.prod, tree = tree))
    }

    # line 7
    #
    tree$call$s                                                 <- s
    s                                                           <- Find(function(x) all(s %in% x), cc)
     #7
    tree$call$line                                              <- 7
    tree$call$s.prime                                           <- s
    s.len                                                       <- length(s)
    ind                                                         <- which(v %in% s)
    G.s                                                         <- induced.subgraph(G, s)
    G.s.obs                                                     <- observed.graph(G.s)
    product.list                                                <- vector(mode = "list", length = s.len)
    P.prod                                                      <- probability()
    for (i in s.len:1) {
      # cond.set                                                <- causal.parents(s[i], v[1:ind[i]], G, G.obs, topo)
      cond.set                                                  <- v[0:(ind[i]-1)]
      if (P$product) {
        P.prod                                                  <- parse.joint(P, s[i], cond.set, v, topo)
      } else {
        P.prod                                                  <- P
        P.prod$var                                              <- s[i]
        P.prod$cond                                             <- cond.set
      }
      product.list[[s.len - i + 1]]                             <- P.prod
    }
    x.new                                                       <- intersect(x, s)
    nxt                                                         <- NULL
    if (s.len > 1) {

      nxt                                                       <- id(y, x.new, probability(product = TRUE, children = product.list), G.s, G.s.obs, s, topo, list()) # 7 # +++++++++++++++++++++++

      } else {

        nxt                                                    <- id(y, x.new, product.list[[1]], G.s, G.s.obs, s, topo, list()) # 7  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      }

    tree$branch[[1]]                                            <- nxt$tree
    tree$call$id                                                <- nxt$tree$call$id
    return(list(P = nxt$P, tree = tree))
  }

}
##################################################################################################################################

#' @export
c.components <- function(G, topo) {

  # Beispiel mit fig 1 aus Vignette:

  # -------------
  A <- as.matrix(get.adjacency(G))
  #   w x z y
  # w 0 1 1 0
  # x 0 0 1 1
  # z 0 0 0 1
  # y 0 1 0 0
  # -------------

  # -------------
  # t(A)
  #  w x z y
  #w 0 0 0 0
  #x 1 0 0 1
  #z 1 1 0 0
  #y 0 1 1 0
  # -------------

  v <- get.vertex.attribute(G, "name")
  #[1] "w" "x" "z" "y"

  indices <- which(A >= 1 & t(A) >= 1, arr.ind = TRUE)
  #  row col
  #y   4   2
  #x   2   4

  bidirected <- NULL

  # -------------
  e <- E(G)
  #+ 6/6 edges from 7053173 (vertex names):
  #  [1] w->x w->z x->z z->y x->y y->x
  # -------------

  # -------------
  if (nrow(indices) > 0) {
    bidirected <- unlist(apply(indices, 1, function(x) {
      e[v[x[1]] %->% v[x[2]]]
    }))
  }
  # y x
  # 6 5
  # -------------

  # -------------
  G.bidirected <- subgraph.edges(G, bidirected, delete.vertices = FALSE)
  #IGRAPH e57c8f9 DN-- 4 2 --
  #+ attr: name (v/c), description (e/c)
  #+ edges from e57c8f9 (vertex names):
  #  [1] x->y y->x
  # -------------

  # -------------
  subgraphs    <- decompose.graph(G.bidirected)
  #[[1]]
  #IGRAPH e58171c DN-- 1 0 --
  #+ attr: name (v/c), description (e/c)
  #+ edges from e58171c (vertex names):
  #
  #[[2]]
  #IGRAPH e58171c DN-- 2 2 --
  #+ attr: name (v/c), description (e/c)
  #+ edges from e58171c (vertex names):
  #  [1] x->y y->x
  #
  #[[3]]
  #IGRAPH e58171c DN-- 1 0 --
  #+ attr: name (v/c), description (e/c)
  #+ edges from e58171c (vertex names):
  # -------------


  # -------------
  cc <- lapply(subgraphs, function(x) {
    v.sub <- get.vertex.attribute(x, "name")
    return(v.sub %ts% topo)
  })

  #`%ts%` <- function(nodes, topo.order) {
  #  nodes[order(match(nodes, topo.order))]
  #}


  # -------------

  # -------------
  cc.rank <- order(sapply(cc, function(x) {
    sum(which(topo %in% x))
  }), decreasing = TRUE)
  # -------------

  return(cc[cc.rank])


}

##################################################################################################################################

#' @export
parse.expression                                          <- function(P, topo, G.adj, G, G.obs) {
  if (P$fraction) {
    P                                                     <- cancel.out(P)
    P$den                                                 <- parse.expression(P$den, topo, G.adj, G, G.obs)
    if (length(P$den) == 0) {
      sum_p                                               <- P$sumset
      P                                                   <- P$num
      P$sumset                                            <- union(sum_p, P$sumset) %ts% topo
      if (P$product) {
        if (length(P$children) == 1) {
          sum_p                                           <- P$sumset
          P                                               <- P$children[[1]]
          P$sumset                                        <- union(sum_p, P$sumset) %ts% topo
        }
      }
      return(P)
    }
    if (length(P$sumset) > 0 && length(P$den) > 0) {
      nodep                                               <- setdiff(P$sumset, dependencies(P$den))
      if (length(nodep) > 0) {
        P$num$sumset                                      <- union(P$num$sumset, nodep) %ts% topo
        P$sumset                                          <- setdiff(P$sumset, nodep) %ts% topo
      }
    }
    P$num                                                 <- parse.expression(P$num, topo, G.adj, G, G.obs)
    P                                                     <- cancel.out(P)
    return(P)
  }
  simplify_terms                                          <- TRUE
  if (P$product) {
    non_atomic                                            <- sapply(P$children, FUN = function(x) (x$product || length(x$sumset) > 0 || x$fraction || x$sum))
    if (sum(non_atomic) > 0) {
      parse_children                                      <- P$children[non_atomic]
      P$children                                          <- P$children[!non_atomic]
      for (i in 1:length(parse_children)) {
        P.parse                                           <- parse.expression(parse_children[[i]], topo, G.adj, G, G.obs)
        if (!is.null(P.parse$collapse)) {
          P$children                                      <- c(P$children, P.parse$children)
        } else {
          P$children[[length(P$children) + 1]]            <- P.parse
        }
      }
    }
    if (length(P$children) > 0) {
      non_atomic                                          <- sapply(P$children, FUN = function(x) (x$product || length(x$sumset) > 0 || x$fraction || x$sum))
      if (sum(non_atomic) > 0) simplify_terms             <- FALSE
    } else {
      return(NULL)
    }
  }
  if (length(P$sumset) == 0) return(P)
  if (!P$product) {
    if (identical(P$sumset, P$var)) {
      return(NULL)
    } else {
      return(P)
    }
  }
  if (simplify_terms) {
    ord.children                                          <- order(unlist(lapply(P$children, FUN = function(x) which(topo == x$var))), decreasing = TRUE)
    ord.sum                                               <- order(sapply(P$sumset, FUN = function(x) which(topo == x)), decreasing = TRUE)
    P$children                                            <- P$children[ord.children]
    P$sumset                                              <- P$sumset[ord.sum]
    P                                                     <- simplify(P, topo, G.adj, G, G.obs)
    if (length(P$children) == 0) return(NULL)
  }
  P.parse                                                 <- probability(product = TRUE, children = list())
  remove                                                  <- c()
  j                                                       <- 0
  if (length(P$sumset) > 0) {
    for (i in 1:length(P$children)) {
      dep                                                 <- dependencies(P$children[[i]])
      if (length(intersect(dep, P$sumset)) == 0) {
        remove                                            <- c(remove, i)
        j                                                 <- j + 1
      }
    }
  } else {
    return(P)
  }
  if (j > 0) {
    P.parse$children                                      <- P$children[remove]
    P.parse$collapse                                      <- TRUE
    P$children                                            <- P$children[-remove]
    if (length(P$sumset) > 0) {
      if (length(P$children) == 1) {
        sum_p                                             <- P$sumset
        P                                                 <- P$children[[1]]
        P$sumset                                          <- union(sum_p, P$sumset) %ts% topo
        P                                                 <- parse.expression(P, topo, G.adj, G, G.obs)
      }
    }
    if (length(P$children) > 0) P.parse$children[[j + 1]] <- P
    return(P.parse)
  }
  return(P)
}

cancel.out                       <- function(P) {
  if (P$product) {
    for (i in 1:length(P$children)) {
      P$children[[i]]            <- cancel.out(P$children[[i]])
    }
  }
  if (P$fraction) {
    P$num                        <- cancel.out(P$num)
    P$den                        <- cancel.out(P$den)
    if (identical(P$num, P$den, attrib.as.set = TRUE)) return(NULL)
    i                            <- 1
    k                            <- 0
    if (length(P$num$sumset) == 0 && length(P$den$sumset) == 0 && P$num$product && P$den$product) {
      while (i <= length(P$num$children) && length(P$num$children) > 0 && length(P$den$children) > 0) {
        is.element               <- FALSE
        for (j in 1:length(P$den$children)) {
          if (identical(P$num$children[[i]], P$den$children[[j]], attrib.as.set = TRUE)) {
            is.element           <- TRUE
            k                    <- j
            break
          }
        }
        if (is.element) {
          P$num$children         <- P$num$children[-i]
          P$den$children         <- P$den$children[-k]
          i                      <- 0
        }
        i                        <- i + 1
      }
    }
    if (length(P$den$children) == 0 && !P$den$fraction) {
      P$num$sumset               <- P$sumset
      return(P$num)
    }
    if (length(P$den$children) == 1) {
      P$den$children[[1]]$sumset <- union(P$den$sumset, P$den$children[[1]]$sumset)
      P$den                      <- P$den$children[[1]]
    }
    if (P$num$product && length(P$num$sumset) == 0) {
      j                          <- 1
      while (j <= length(P$num$children)) {
        if (identical(P$num$children[[j]], P$den, attrib.as.set = TRUE)) {
          P$num$children         <- P$num$children[-j]
          if (length(P$num$children) == 1) {
            P$num                <- P$num$children[[1]]
          }
          P$num$sumset           <- P$sumset
          return(P$num)
        }
        j                        <- j + 1
      }
    }
    if (length(P$num$children) == 1) {
      P$num$children[[1]]$sumset <- union(P$num$sumset, P$num$children[[1]]$sumset)
      P$num                      <- P$num$children[[1]]
    }
    if (identical(P$num, P$den, attrib.as.set = TRUE)) return(NULL)
  }
  return(P)
}
##################################################################################################################################

#' @export
deconstruct                                                  <- function(P, P.context, topo) {

  # Fraction in a context
  if (P$fraction) {
    if (length(P$sumset) == 0) {
      if (P.context$fraction) {
        if (length(P.context$num$sumset) == 0) {
          P.context$num                                      <- deconstruct(P$num, P.context$num, topo)
        } else {
          P.temp                                             <- probability(product = TRUE)
          P.temp$children[[1]]                               <- P.context$num
          P.temp                                             <- deconstruct(P$num, P.temp, topo)
          P.context$num                                      <- P.temp
        }
        if (length(P.context$den$sumset) == 0) {
          P.context$den                                      <- deconstruct(P$den, P.context$den, topo)
        } else {
          P.temp                                             <- probability(product = TRUE)
          P.temp$children[[1]]                               <- P.context$den
          P.temp                                             <- deconstruct(P$den, P.temp, topo)
          P.context$den                                      <- P.temp
        }
        return(P.context)
      }
      if (P.context$product) {
        P.temp                                               <- probability(fraction = TRUE)
        P.temp$sumset                                        <- P.context$sumset
        P.context$sumset                                     <- character(0)
        P.temp$num                                           <- deconstruct(P$num, P.context, topo)
        P.temp$den                                           <- deconstruct(P$den, probability(), topo)
        return(P.temp)
      }
      if (P.context$sum) {
        P.context$children[[length(P.context$children) + 1]] <- deconstruct(P, probability(), topo)
        return(P.context)
      }
    } else {
      if (P.context$fraction) {
        P.context$num                                        <- deconstruct(P, P.context$num, topo)
        return(P.context)
      }
      if (P.context$product || P.context$sum) {
        P.context$children[[length(P.context$children) + 1]] <- deconstruct(P, probability(), topo)
        return(P.context)
      }
    }
    P.context$fraction                                       <- TRUE
    P.context$sumset                                         <- P$sumset
    P.context$num                                            <- deconstruct(P$num, probability(), topo)
    P.context$den                                            <- deconstruct(P$den, probability(), topo)
    return(P.context)
  }

  # Product in a context
  if (P$product) {
    if (length(P$sumset) == 0) {
      if (P.context$fraction) {
        P.context$num                                        <- deconstruct(P, P.context$num, topo)
        return(P.context)
      }
      if (P.context$product) {
        for (i in 1:length(P$children)) {
          P.context                                          <- deconstruct(P$children[[i]], P.context, topo)
        }
        return(P.context)
      }
      if (P.context$sum) {
        P.context$children[[length(P.context$children) + 1]] <- deconstruct(P, probability(), topo)
        return(P.context)
      }
    } else {
      if (P.context$fraction) {
        P.context$num                                        <- deconstruct(P, P.context$num, topo)
        return(P.context)
      }
      if (P.context$product || P.context$sum) {
        P.context$children[[length(P.context$children) + 1]] <- deconstruct(P, probability(), topo)
        return(P.context)
      }
    }
    P.context$product                                        <- TRUE
    P.context$sumset                                         <- P$sumset
    for (i in 1:length(P$children)) {
      P.context                                              <- deconstruct(P$children[[i]], P.context, topo)
    }
    return(P.context)
  }

  # Sum in a context
  if (P$sum) {
    if (P.context$fraction) {
      P.context$num                                          <- deconstruct(P, P.context$num, topo)
      return(P.context)
    }
    if (P.context$product || P.context$sum) {
      P.context$children[[length(P.context$children) + 1]]   <- deconstruct(P, probability(), topo)
      return(P.context)
    }
    P.context$sum                                            <- TRUE
    P.context$sumset                                         <- P$sumset
    for (i in 1:length(P$children)) {
      P.context                                              <- deconstruct(P$children[[i]], P.context, topo)
    }
    return(P.context)
  }

  # Atomic expression in a context
  if (P.context$fraction) {
    P.context$num                                            <- deconstruct(P, P.context$num, topo)
    return(P.context)
  }

  if (P.context$product) {
    init                                                     <- length(P.context$children)
    if (length(P$sumset) == 0) {
      n                                                      <- length(P$var)
      for (i in n:1) {
        P.context$children[[init + n - i + 1]]               <- probability(
          var = P$var[i], cond = union(P$cond, P$var[-(i:n)]) %ts% topo,
          domain = P$domain, do = P$do
        )
      }
    } else {
      n                                                      <- length(P$var)
      if (n > 1) {
        P.temp                                               <- probability(product = TRUE, children = list(), sumset = P$sumset)
        for (i in n:1) {
          P.temp$children[[n - i + 1]]                       <- probability(
            var = P$var[i], cond = union(P$cond, P$var[-(i:n)]) %ts% topo,
            domain = P$domain, do = P$do
          )
        }
        P.context$children[[init + 1]]                       <- P.temp
      }
    }
    return(P.context)
  }

  if (P.context$sum) {
    init                                                     <- length(P.context$children)
    P.temp                                                   <- probability(product = TRUE, children = list(), sumset = P$sumset)
    n                                                        <- length(P$var)
    for (i in n:1) {
      P.temp$children[[n - i + 1]]                           <- probability(
        var = P$var[i], cond = union(P$cond, P$var[-(i:n)]) %ts% topo,
        domain = P$domain, do = P$do
      )
    }
    P.context$children[[init + 1]]                           <- P.temp
    return(P.context)
  }

  # Atomic expression with multiple variables
  n                                                          <- length(P$var)
  if (n > 1) {
    P.context$product                                        <- TRUE
    P.context$sumset                                         <- P$sumset
    P.context$children                                       <- list()
    for (i in n:1) {
      P.context$children[[n - i + 1]]                        <- probability(
        var = P$var[i], cond = union(P$cond, P$var[-(i:n)]) %ts% topo,
        domain = P$domain, do = P$do
      )
    }
    return(P.context)
  }

  return(P)
}

##################################################################################################################################

#' @export
get.expression <- function(x, primes = FALSE) {

  query         <- unique(unlist(attr(x, "query")))
  prime.counter <- setNames(rep(1, length(query)), query)
  target.sym    <- "^*("
  single.source <- FALSE

  if (!is.null(attr(x, "algorithm"))) {
    if (attr(x, "algorithm") == "zid") target.sym <- "("
  }
  if (!is.null(attr(x, "sources"))) {
    if (attr(x, "sources") == 1) single.source <- TRUE
  }
  return(get.expression.internal(x, primes, prime.counter, FALSE, target.sym, single.source))
}

#' @export
get.expression.internal                                           <- function(x, primes, prime.counter, start.sum, target.sym, single.source) {
  P                                                               <- ""
  s.print                                                         <- length(x$sumset) > 0
  super                                                           <- character(0)
  sum.string                                                      <- character(0)
  var.string                                                      <- character(0)
  cond.string                                                     <- character(0)
  if (s.print) {
    if (primes) {
      update                                                      <- set.primes(x$sumset, TRUE, prime.counter)
      super                                                       <- update$super
      prime.counter                                               <- update$counter
      sum.string                                                  <- paste0(x$sumset, super[x$sumset], collapse = ",")
    } else {
      sum.string                                                  <- paste0(x$sumset, collapse = ",")
    }
    if (start.sum) {
      P                                                           <- paste0(P, "\\left(\\sum_{", sum.string, "}", collapse = "")
    } else {
      P                                                           <- paste0(P, "\\sum_{", sum.string, "}", collapse = "")
    }
  }
  if (x$fraction) {
    P                                                             <- paste0(P,
                                                                            "\\frac{",
                                                                            get.expression.internal(x$num, primes, prime.counter, FALSE, target.sym, single.source),
                                                                            "}{",
                                                                            get.expression.internal(x$den, primes, prime.counter, FALSE, target.sym, single.source),
                                                                            "}",
                                                                            collapse = "")
  }
  if (x$sum) {
    P                                                             <- paste(P, "\\left(", sep = "", collapse = "")
    add.strings                                                   <- c()
    for (i in 1:length(x$children)) {
      new.sum                                                     <- FALSE
      if (x$children[[i]]$product || x$children[[i]]$sum) new.sum <- TRUE
      add.strings[i]                                              <- paste0(c(
        "w_{", i, "}^{(", x$weight, ")}",
        get.expression.internal(x$children[[i]], primes, prime.counter, new.sum, target.sym, single.source)
      ), collapse = "")
    }
    add.strings                                                   <- paste(add.strings, sep = "", collapse = " + ")
    P                                                             <- paste0(P, add.strings, "\\right)", collapse = "")
  }
  if (x$product) {
    for (i in 1:length(x$children)) {
      new.sum                                                     <- FALSE
      if (x$children[[i]]$product || x$children[[i]]$sum) new.sum <- TRUE
      P                                                           <- paste0(P,
                                                                            get.expression.internal(x$children[[i]], primes, prime.counter, new.sum, target.sym, single.source),
                                                                            collapse = ""
      )
    }
  }
  if (!(x$sum || x$product || x$fraction)) {
    P                                                             <- paste0(P, "P", collapse = "")
    if (length(x$do) > 0) {
      do.string                                                   <- paste0(x$do, collapse = ",")
      P                                                           <- paste0(P, "_{", do.string, "}", collapse = "")
    }
    if (primes) {
      update                                                      <- set.primes(x$var, FALSE, prime.counter)
      super                                                       <- update$super
      prime.counter                                               <- update$counter
      var.string                                                  <- paste0(x$var, super[x$var], collapse = ",")
    } else {
      var.string                                                  <- paste0(x$var, collapse = ",")
    }
    if (x$domain > 0) {
      if (x$domain == 1) {
        P                                                         <- paste0(P, target.sym, var.string, collapse = "")
      } else {
        if (single.source) {
          P                                                       <- paste0(P, "(", var.string, collapse = "")
        } else {
          P                                                       <- paste0(P, "^{(", x$domain - 1, ")}(", var.string, collapse = "")
        }
      }
    } else {
      P                                                           <- paste0(P, "(", var.string, collapse = "")
    }
    if (length(x$cond) > 0) {
      if (primes) {
        update                                                    <- set.primes(x$cond, FALSE, prime.counter)
        super                                                     <- update$super
        prime.counter                                             <- update$counter
        cond.string                                               <- paste0(x$cond, super[x$cond], collapse = ",")
      } else {
        cond.string                                               <- paste0(x$cond, collapse = ",")
      }
      cond.string                                                 <- paste0("\u007C", cond.string, ")", collapse = "")
    }
    else {
      cond.string                                                 <- ")"
    }
    P                                                             <- paste0(P, cond.string)
  }
  if (s.print & start.sum) P                                      <- paste0(P, "\\right)", collapse = ",")
  return(P)
}

set.primes <- function(vars, new, counter) {
  primed <- intersect(vars, names(counter))
  initial <- setdiff(vars, names(counter))
  if (new) {
    counter[primed] <- counter[primed] + 1
    counter[initial] <- 1
  } else {
    counter[initial] <- 0
  }
  primes <- sapply(counter, function(x) {
    if (x > 1) return (paste0("^{", paste0(rep("\\prime", x - 1), collapse = ""), "}", collapse = ""))
    return ("")
  })
  return (list(counter = counter, super = setNames(primes, names(counter))))
}

##################################################################################################################################

#' @export
probability <- function(
  var      = character(),
  cond     = character(),
  sumset   = character(),
  do       = character(),
  product  = FALSE,
  children = list(),
  fraction = FALSE,
  den      = list(),
  num      = list(),
  domain   = 0,
  sum      = FALSE,
  weight   = numeric(1)) {
  p <- list(
    var      = var,
    cond     = cond,
    sumset   = sumset,
    do       = do,
    product  = product,
    fraction = fraction,
    sum      = sum,
    children = children,
    den      = den,
    num      = num,
    domain   = domain,
    weight   = weight
  )
  class(p) <- "probability"
  return(p)
}

##################################################################################################################################
##################################################################################################################################

# Smoking-Example:

#G <- graph.formula(x -+ z,
#                      z -+ y,
#                      x -+ y,
#                      y -+ x,
#                      simplify = FALSE)
#
#
#
#G <- set.edge.attribute(
#  graph = G,
#  name  = "description",
#  index = c(3,4),
#  value = "U")
#
#
#G.obs <- observed.graph(G)
#
#topo  <- topological.sort(G.obs)
#topo  <- get.vertex.attribute(G, "name")[topo]
#
#causal.effect(y = "y", x="x", G = G, prime=T) %>% katexR::katex()



## Vignetten-Example:
#
#G              <- graph.formula(W - +X, W - +Z, X - +Z, Z - +Y, X - +Y, Y - +X, simplify = FALSE)
#G              <- set.edge.attribute(graph = G, name = "description", index = c(5, 6), value = "U")
#
#G.obs          <- observed.graph(G)
#
#topo           <- topological.sort(G.obs)
#topo           <- get.vertex.attribute(G, "name")[topo]
#
#
#
#causal.effect(y = "Y", x="X", G = G, prime=T) %>% katexR::katex()
#
#causal.effect(y = "Y", x="X", G = G, prime=F) %>% katexR::katex()
#
#library(tidyverse)
#load("C:/Users/shs/Desktop/causaleffect/meineSitzung.RData")
#alles <- id("Y", "X", probability(), G, G.obs, topo, topo, list())



