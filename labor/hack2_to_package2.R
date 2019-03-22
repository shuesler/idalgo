

#library(causaleffect)
#library(igraph)
#library(googledrive)
#library(rlang)
#library(tidyverse)
#library(data.tree)
##library(collapsibleTree)
##library(lobstr)

#doll  <- magrittr::use_series
#options(tibble.print_min = 100)
##########################################################################################################
library(idalgo)
#devtools::load_all(".")

# Vignetten - Beispiel

fig1  <- graph.formula(W - +X, W - +Z, X - +Z, Z - +Y, X - +Y, Y - +X, simplify = FALSE)
fig1  <- set.edge.attribute(graph = fig1, name = "description", index = c(5, 6), value = "U")

mat   <- matrix(nrow = 4,ncol = 2,byrow = T,
                data = c(
                  1, 1, # w
                  0, 0, # x
                  2, 0, # z
                  2, 3  # y
                ))

row.names(mat) <- c("W", "X", "Z", "Y")

NODES <- names(V(fig1))

ce1   <- causal.effect(y = "Y", x = "X", z = NULL, G = fig1, expr = TRUE, steps = TRUE)

alles <- ce1$steps

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

igraph_addr               <- list_parser(alles, "IGRAPH")

platzhalter_namen_igraphs <- paste0("graph", 1:8)

expr_erw                  <- purrr::map2(igraph_addr, platzhalter_namen_igraphs, ~ rlang::expr(`<-`(!!.x, !!.y)))

eval_bare                 <- rlang::eval_bare
map                       <- purrr::map

map(expr_erw, ~eval_bare(.x, env = rlang::global_env()))

alles


##########################################################################################################
##########################################################################################################
#https://stackoverflow.com/questions/29818918/looping-nested-lists-in-r/29819032


nms <- names(methods:::.BasicFunsList)        ## ?.BasicFunsList for more info

length(nms)
c(head(nms, 8), tail(nms, 8))



library(lobstr)

#https://stackoverflow.com/questions/29818918/looping-nested-lists-in-r/29819032
#foo <- function(l){
#  lapply(l, function(x) if(is.list(x) && length(x)==0) "" else if(is.list(x)) foo(x) else x)
#}






alles[["branch"]][[3]][["branch"]][[1]][["call"]][["G"]] <- "test"


library(rlang)
library(magrittr)


rosi <- iris

hello <- rep("hello", 150)

head(rosi)

expr(rosi[["sils"]] <- hello) %>% eval_tidy(env=global_env())

head(rosi)

expr(rosi[["sils"]] <- hello) %>% eval_bare(env=global_env())

head(rosi)





`<-`(rosi[["sils"]], hello)






sils <- iris

inset2(globalenv()$sils, "hh", value = rep("hello", 150))





hh <- 'branch[[3]][["branch"]][[1]][["call"]][["G"]]'

hh <- parse_quo(hh, alles)


eval_tidy(expr(G %<>% "haus"), data = alles[["branch"]][[3]][["branch"]][[1]][["call"]])



dm$.top_env


env_poke(env = dm, nm = "G", "hello")


dm <- as_data_mask(alles[["branch"]][[3]][["branch"]][[1]][["call"]])


expr(`$`(alles, !!hh))


sils


`%su%` <- `[[`

hello <-  `[[<-`







hello(iris, rep("hello", 150), "Species")

library(magrittr)

alles %su% "branch" %su% 3  %su% "branch" %su% 1 %su% "call" %su% "G" `<-` c("test2", "s")


#la <- lazyeval::as.lazy_dots(igraph_addr)
#la <- lazyeval::as.lazy_dots(igraph_addr, globalenv())
#lazyeval::lazy_eval(la)




igraph_addr               <- purrr::map(igraph_addr, rlang::expr_name)

igraph_addr               <- purrr::map(igraph_addr, ~ stringr::str_replace(.x, "\\$G", ""))

igraph_addr               <- purrr::map(igraph_addr, rlang::parse_expr)


igraph_addr2              <- purrr::map(igraph_addr, ~ rlang::expr(`[[`(!!.x, 4)))

platzhalter_namen_igraphs <- paste0("graph", 1:8)

igraph_addr2              <- purrr::map2(igraph_addr2, platzhalter_namen_igraphs, ~ rlang::expr(`<-`(!!.x, !!.y)))



la <- lazyeval::as.lazy_dots(igraph_addr2, globalenv())
lazyeval::lazy_eval(la)




igraph_addr2               <- purrr::map(igraph_addr2, ~new_quosure(expr = .x, global_env()))



igraph_addr2 %>% purrr::map(eval_tidy)


function(x) {
  printnames()
}

recurse(l=alles, func=names)

igraph_addr2 %>% purrr::map(eval_tidy)


alles




sils                      <-


sils %>% purrr::map(~ .x[[4]])





igraph_addr2              <- purrr::map(igraph_addr, ~ rlang::expr(`[[`(!!.x, 4)))

igraph_addr2              <- purrr::map2(igraph_addr2, platzhalter_namen_igraphs, ~ rlang::expr(`<-`(!!.x, !!.y)))


igraph_addr2 %>% purrr::map(~eval_tidy(.x, env=global_env()))

igraph_addr %>% purrr::map(~eval_tidy(.x, env=global_env()))


sils


sils[[1]] %>% names

global_env()$alles


purrr::map(igraph_addr, as.character)







a <- function(x) {

rlang::get_env(unclass(x)[[10]])

}



b <- function(x) {

  rlang::env_unbind(x, nms = "me")
  rlang::env_unbind(x, nms = "myid")
  rlang::env_unbind(x, nms = ".__igraph_version__.")

}



d <- function(x) {

  rlang::env_poke(x, nm = "me",   value = "hello1")
  rlang::env_poke(x, nm = "myid", value = "hello2")
  rlang::env_poke(x, nm = ".__igraph_version__.", value = "hello3")

}



sils <- purrr::map(sils, a)
sils <- purrr::map(sils, d)



alles

test <- env()


env_bind(test, a = 1 )


env_print(test)

env_unbind(test, "a")

env_print(test)








sils <- purrr::map(sils, a)



sils %>% purrr::map(class)
sils %>% purrr::map(names)

sils %>% purrr::map(env_print)

sils %>% purrr::map(lobstr::ref)



#alles$branch[[1]]$branch[[1]]$branch[[1]]$call$G
#
#
#
#`<-`(`$`(`$`(`[[`(`$`(`[[`(`$`(`[[`(`$`(alles, branch), 1), branch), 1), branch),1), call), G), "graph100000")
#
#
#`$`(`$`(`[[`(`$`(`[[`(`$`(`[[`(`$`(alles, branch), 1), branch), 1), branch),1), call), G)
#
#
#
#expr(`$`(`$`(`[[`(`$`(`[[`(`$`(`[[`(`$`(.xyz, branch), 1), branch), 1), branch),1), call), G)) %>% rlang::eval_bare()







class(get_env(unclass(alles$branch[[1]]$branch[[1]]$branch[[1]]$call$G)[[10]]))


address(alles$branch[[1]]$branch[[1]]$branch[[1]]$call$G)


rlang::get






lobstr::ref(alles$branch[[1]]$branch[[1]]$branch[[1]]$call$G)


find.by.address(address(alles$branch[[1]]$branch[[1]]$branch[[1]]$call$G))


lobstr::ref(ss)


www <- rlang::get_env(www)

liste <- as.list(letters)

lobstr::ref(liste, character = T)


ss <- list(env())

www <- get_env(ss[[1]])


env_print(ss[[1]])


rlang::env_bind(rlang::get_env(ss[[1]]), a = 1)






sils <- new_function(
  exprs(x =),
  expr({x$branch[[1]]$branch[[1]]$branch[[1]]$call$G})
)



aaa <- as_data_mask(alles$branch[[1]]$branch[[1]]$branch[[1]]$call)


lobstr::ref(alles$branch[[1]]$branch[[1]]$branch[[1]]$call)

lobstr::ref(aaa$.top_env)


eval_tidy(expr(G), aaa)




eval_tidy(expr(G), data = alles$branch[[1]]$branch[[1]]$branch[[1]]$call)

lobstr::obj_addr(alles$branch[[1]]$branch[[1]]$branch[[1]]$call$G)







eval_tidy(expr(alles$branch[[1]]$branch[[1]]$branch[[1]]$call$G <- "hello"), data = alles$branch[[1]]$branch[[1]]$branch[[1]]$call)


sils(alles)


iris$Species <- Species






rlang::eval_tidy(rlang::expr(assign("G", "huhuhuhuu", envir = rlang::caller_env())), data = alles$branch[[1]]$branch[[1]]$branch[[1]]$call)



rlang::eval_tidy(expr(env_unbind(unclass(G)[[10]],"me", inherit = T)), data = alles$branch[[1]]$branch[[1]]$branch[[1]]$call)




rlang::eval_tidy(rlang::expr(silsi <<- G), data = alles$branch[[1]]$branch[[1]]$branch[[1]]$call)


silsi


identical(silsi, alles$branch[[1]]$branch[[1]]$branch[[1]]$call$G)


lobstr::ref(silsi[1])








rlang::eval_tidy(rlang::expr(lobstr::ref(.data$G)), data = alles$branch[[1]]$branch[[1]]$branch[[1]]$call)

rlang::eval_tidy(rlang::expr(`<-`(G, "hello")), data = alles$branch[[1]]$branch[[1]]$branch[[1]]$call)


rlang::eval_tidy(rlang::expr(lobstr::ref(G)), data = alles$branch[[1]]$branch[[1]]$branch[[1]]$call)

rlang::eval_tidy(expr(silsi <<- current_env()), data = alles$branch[[1]]$branch[[1]]$branch[[1]]$call)


env_print(silsi)

lobstr::ref(silsi$.env)

expr(unclass(G)[[10]])




rlang::env_unbind(www, "myid")
rlang::env_unbind(www, "myid")

rlang::env_bind(www, me="graph")

lobstr::ref(www)



alles$branch[[1]]$branch[[1]]$branch[[1]]$call$G


`<-`(`$`(`$`(`[[`(`$`(`[[`(`$`(`[[`(`$`(alles, branch), 1), branch), 1), branch),1), call), G), "graph10")


alles$branch[[1]]$branch[[1]]$branch[[1]]$call$G





rlang::expr(`<-`(`$`(`$`(`[[`(`$`(`[[`(`$`(`[[`(`$`(!!.xyz, branch), 1), branch), 1), branch),1), call), G), "graph100000")) %>% rlang::eval_bare()









igraph_addr_eval <- igraph_addr %>% purrr::map(rlang::eval_tidy)
##########################################################################################################

plotl         <- igraph_plots_to_tmp(igraph_addr_eval, lay=mat)


tib           <- make_arg_tibble(plotl)

drive_plots   <-
  purrr::pmap(tib, uploader) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(url           = paste0("https://docs.google.com/uc?id=", id),
                namen_igraphs = platzhalter_namen_igraphs)

##########################################################################################################

expr_erw  <- purrr::map2(igraph_addr, platzhalter_namen_igraphs, ~ rlang::expr(`<-`(!!.x, !!.y)))


expr_erw %>% shseval()


##########################################################################################################

alles  <- replace_null(alles, "nichts")
alles  <- number_unnamed(alles)

##########################################################################################################


werte      <- unlist(alles) %>% unname()

hierarchie <- unlist(alles) %>% names() %>% stringr::str_replace_all("\\.", "/")


hierarchie <- hierarchie %>%
  stringr::str_replace_all("children", "kinder") %>%
  stringr::str_replace_all("root", "ende") %>%
  stringr::str_replace_all("tree", "baum") %>%
  stringr::str_replace_all("branch", "ast")

hierarchie <- paste0("ausgang/", hierarchie)

tib        <- tibble::tibble(pathString = hierarchie, werte = werte)

daten <- data.tree::as.Node(tib)

daten$Set(nummer = 1:daten$totalCount)

tmp <- data.tree::ToDataFrameTree(daten, "name", "pathString", "werte", "nummer") %>% dplyr::tbl_df()


#################################

lines <- tmp %>% dplyr::filter(name=="line") %>% dplyr::mutate(pathString=paste0(pathString,werte)) %>% dplyr::select(pathString)

#################################

tmp2 <- daten$Get("parent") %>% tibble::enframe() %>% dplyr::rename(parent = value)

tmp2 <- tmp2 %>% dplyr::mutate(gparent = purrr::map(parent, ~try(doll(.x, "parent"), silent = T)))


tmp2 <- tmp2 %>% dplyr::mutate(name_parent  = purrr::map(parent,           ~try(doll(.x, "name"),           silent = T)),
                               name_gparent = purrr::map(gparent,          ~try(doll(.x, "name"),           silent = T)),
                               name_parent  = as.character(name_parent),
                               name_gparent = as.character(name_gparent),
                               path_parent  = purrr::map(parent,           ~try(doll(.x, "path"),           silent = T)),
                               path_gparent = purrr::map(gparent,          ~try(doll(.x, "path"),           silent = T)),
                               path_parent  = purrr::map_chr(path_parent,  ~try(paste0(.x, collapse = "/"), silent = T)),
                               path_gparent = purrr::map_chr(path_gparent, ~try(paste0(.x, collapse = "/"), silent = T)))


#################################

tmp2 <- tmp2 %>% dplyr::select(name_parent:path_gparent)

tmp  <- dplyr::bind_cols(tmp, tmp2)

oo <- purrr::map_chr(c(platzhalter_namen_igraphs, NODES), ~or_filter("werte", .x)) %>%
  paste0(collapse = " | ") %>%
  rlang::parse_expr()


tmp <- tmp %>%
  dplyr::filter(!!oo) %>%
  dplyr::mutate(ends_digit=stringr::str_ends(pathString, "[[:digit:]]")) %>%
  dplyr::group_split(ends_digit) %>%
  purrr::set_names(c("ends_digit_FALSE", "ends_digit_TRUE"))


#################################

tmp$ends_digit_TRUE <- tmp$ends_digit_TRUE %>%
  dplyr::group_by(path_parent) %>%
  tidyr::nest() %>%
  dplyr::mutate(werte=purrr::map(data, ~ .x %>%
                                   dplyr::pull(werte))) %>%
  dplyr::mutate(werteB=purrr::map_chr(werte, ~paste0(.x, collapse = " "))) %>%
  dplyr::filter(!stringr::str_ends(path_parent, "/v"))  %>%
  dplyr::mutate(pathString = paste0(path_parent, "/", werteB))


tmp$ends_digit_FALSE <- tmp$ends_digit_FALSE %>% dplyr::filter(!stringr::str_ends(pathString, "/v")) %>%
  dplyr::mutate(pathString = paste0(pathString, "/", werte))

#############################################

tri <- drive_plots %>% dplyr::select(namen_igraphs, url)

tri <-tri %>% dplyr::mutate(bild=br_img(url)) %>% dplyr::select(werte=namen_igraphs, bild)

tmp$ends_digit_FALSE <- dplyr::left_join(tmp$ends_digit_FALSE,  tri, by="werte")


#############################################

tmp <- dplyr::bind_rows(
  tmp$ends_digit_FALSE  %>% dplyr::select(pathString, bild),
  tmp$ends_digit_TRUE   %>% dplyr::select(pathString),
  lines)

tmp <- tmp %>% dplyr::mutate(bild=dplyr::coalesce(bild, "<br><img src=''>"))

#############################################


tmp2 <- data.tree::as.Node(tmp)

tmp <- data.tree::ToDataFrameTree(tmp2, "name", "pathString", "bild") %>%
  dplyr::tbl_df() %>%
  dplyr::mutate(bild=dplyr::coalesce(bild, "<br><img src=''>"))

#############################################



fa <- colorspace::rainbow_hcl(9)

tmp <- tmp %>%
  dplyr::mutate(Farbe = dplyr::case_when(
    !!!purrr::map(platzhalter_namen_igraphs, ~ patterns_lazy("name", .x, "cyan")),
    expr(stringr::str_detect(.data$name, "line") ~ "red"),

    name == "cond"   ~  fa[1],
    name == "var"    ~  fa[2],
    name == "y"      ~  fa[3],
    name == "x"      ~  fa[4],
    name == "P"      ~  fa[5],
    name == "sumset" ~  fa[6],
    name == "s"      ~  fa[7],
    name == "an"     ~  fa[8],
    name == "G"      ~  fa[9],

    TRUE ~ "weiss"))


tmp2$Set(Farbe=tmp$Farbe)
tmp2$Set(Bild =tmp$bild)

collapsibleTree::collapsibleTree(tmp2,
                                 fill        = "Farbe",
                                 tooltip     = TRUE,
                                 tooltipHtml = "Bild",
                                 collapsed   = F,
                                 height      = 2000,
                                 width       = 1500)









