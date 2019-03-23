
#matrix_kontroller <- function(x) {
#  if (is.null(x)) {
#    NULL
#  } else {
#    if (!is.matrix(x)) {
#      matrix(as.numeric(x), ncol = 2)
#    } else {
#      x
#    }
#  }
#}
#
##matrix_kontroller(lay[names(V(igraphs[[i]])),]))
#
##lay[names(V(igraphs[[i]])),])
#
#
#mat_from_gml <- function(gml) {
#
#  mat_tmp <- as.matrix(data.frame(x=gml$coord[[1]], y=gml$coord[[2]]))
#  row.names(mat_tmp) <- as.character(gml$coord[["L1"]])
#  mat_tmp
#
#
#}
#
#
#mat_update <- function(x) {
#
#  tmp_lay <- igraph::get.graph.attribute(x, "layout")
#  tmp_nam <- igraph::get.vertex.attribute(x, "name")
#  x       <- igraph::delete_graph_attr(x, "layout")
#  x       <- igraph::set_graph_attr(x, "layout", tmp_lay[tmp_nam,])
#  x
#
#}




library(idalgo)
#library(igraph)

gml   <- read.gml("C:\\Users\\shs\\Desktop\\graph_ed\\2.gml", coords = T)
#
G     <- parse.graphml("C:\\Users\\shs\\Desktop\\graph_ed\\2.graphml")
#
G     <- set_graph_attr(G, "layout", mat_from_gml(gml))

plot(G)

sils  <- idalgo::latent.projection(G, "gene")

igraph::get.graph.attribute(G, "layout")
igraph::get.graph.attribute(sils, "layout")


#is_edge_U <- function(x) {
#
#  tmp1 <- get.edge.attribute(x, "description")
#
#  dplyr::case_when(tmp1 == "U" ~ TRUE,
#                   tmp1 == ""  ~ FALSE,
#                   TRUE        ~ FALSE)
#
#}


E(sils)[is_edge_U(sils)]

sils <- set_edge_attr(sils, "curved", E(sils), 0)
sils <- set_edge_attr(sils, "curved", E(sils)[is_edge_U(sils)], .5)


sils <- set_edge_attr(sils, "color", E(sils), "black")
sils <- set_edge_attr(sils, "color", E(sils)[is_edge_U(sils)], "red")


plot(sils)














V(sils)

plot(sils)










library(idalgo)
#
## Vignetten - Beispiel
#
fig1    <- igraph::graph.formula(X -+ Z, Z -+ Y, g -+ X, g -+ Y, simplify = FALSE)
#fig1    <- igraph::set.edge.attribute(graph = fig1, name  = "description", index = c(3,4), value = "U")


mat <- matrix(nrow = 4,ncol = 2,byrow = T, data = c(
  0, 0,
  1, 0,
  2, 0,
  1, 1))

fig1     <- set_graph_attr(fig1, "layout", mat)

plot.igraph(fig1)


sils  <- idalgo::latent.projection(fig1, "g")

plot.igraph(sils)

unclass(sils)


plot(sils)









unclass(sils)


plot(sils)




V(fig1)

#X    Z    Y    gene

library(idalgo)
#
## Vignetten - Beispiel
#
fig1   <- graph.formula(W - +X, W - +Z, X - +Z, Z - +Y, X - +Y, Y - +X, simplify = FALSE)
fig1   <- set.edge.attribute(graph = fig1, name = "description", index = c(5, 6), value = "U")


plot(G)



plot(sils)
sils



sils  <- idalgo::mat_update(sils)

plot(sils)






unclass(G)
#[[9]][[3]]
#[[9]][[3]]$description
#[1] "" "" "" ""
#
#[[9]][[3]]$id
#[1] "n0" "n1" "n2" "n3"
#
#[[9]][[3]]$name
#[1] "X"    "Z"    "Y"    "gene"
#
#
#[[9]][[4]]
#[[9]][[4]]$description
#[1] "" "" "" ""
#
#[[9]][[4]]$id
#[1] "e0" "e1" "e2" "e3"

unclass(sils)
#[[9]][[2]]
#[[9]][[2]]$Beschreibung
#[1] ""
#
#
#[[9]][[3]]
#[[9]][[3]]$description
#[1] "" "" ""
#
#[[9]][[3]]$id
#[1] "n0" "n1" "n2"
#
#[[9]][[3]]$name
#[1] "X" "Z" "Y"
#
#
#[[9]][[4]]
#[[9]][[4]]$description
#[1] ""  ""  "U" "U"
#
#[[9]][[4]]$id
#[1] "e2" "e3" NA   NA














get.vertex.attribute(G, "name")

get.vertex.attribute(sils, "name")



get.graph.attribute(G, "layout")
get.graph.attribute(sils, "layout")



if(!is.null(get.graph.attribute(G, "layout"))) {

  G <- mat_update(G)


}




#(as.matrix(data.frame(gml$coord[[1]], gml$coord[[2]]))










as.character(gml$coord[["L1"]])

row.names(tmp99)




G <- set_graph_attr(G, "layout", tmp99)











sss(G)

plot(sils, layout=sss(sils))



add_layout_(G, sss)




G <- `row.names<-`(as.matrix(data.frame(gml$coord[[1]], gml$coord[[2]]), value = as.character(gml$coord[["L1"]])))









V(G)
V(sils)
get.vertex.attribute(G, "name")
get.vertex.attribute(sils, "name")

get.graph.attribute(G, "layout")
get.graph.attribute(sils)



V(G)




G <- unclass(G)

tmp <- rlang::get_env(G[[10]])

tmp$me


G

G[[9]][[2]][["layout"]] <- NULL
class(G) <- "igraph"
tmp$tester



makeActiveBinding("tester", fun = ,env = tmp)


plot(G)

`.rowNamesDF<-`(iris, value=letters[1:5])

`row.names<-`()

get("row.names<-")



get.graph.attribute(G, "layout")
get.graph.attribute(sils, "layout")






plot(sils)


#lay[names(V(igraphs[[i]])),])



