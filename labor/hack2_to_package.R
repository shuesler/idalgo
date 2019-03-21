
#library(causaleffect)
#library(igraph)
#library(googledrive)
#library(rlang)
#library(tidyverse)
#library(data.tree)
##library(collapsibleTree)
##library(lobstr)

#doll  <- magrittr::use_series
options(tibble.print_min = 100)
##########################################################################################################

devtools::load_all(".")

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

#list_parser <- function(x, y) {
#
#  x      <- rlang::enquo(x)
#  x_char <- rlang::quo_name(x)
#
#  tmp    <- tempfile(fileext = ".txt")
#
#  sink(tmp)
#  print(rlang::eval_tidy(x))
#  sink()
#
#  txt    <- readr::read_lines(tmp)
#  tmp2   <- stringr::str_detect(txt, y)
#  tmp3   <- txt[seq_along(tmp2)[tmp2]-1]
#
#
#  tmp4   <- paste0(x_char, tmp3)
#
#  tmp5   <- rlang::parse_exprs(tmp4)
#
#  fs::file_delete(tmp)
#
#  tmp5
#
#}

# ------------------------------------------------------------------------------
#igraph_plots_to_tmp <- function(igraphs, lay = NULL, open = FALSE) {
#  platzhalter_namen_igraphs <<- paste0("graph", seq_along(igraphs))
#  plotliste <- list()
#
#
#    for (i in seq_along(igraphs)) {
#      tmp <- tempfile(fileext = ".jpeg")
#      jpeg(tmp)
#      set.seed(1)
#      plot(igraphs[[i]], layout = lay)
#      dev.off()
#
#      plotliste[[i]] <- tmp
#    }
#
#
#
#  if (open) {
#    fold <- plotliste %>% str_split("file") %>% `[[`(1) %>% `[[`(1)
#    shell.exec(fold)
#  }
#
#
#  plotliste %>% as_vector()
#}
# ------------------------------------------------------------------------------

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
## ------------------------------------------------------------------------------
#igraph_plots_to_tmp <- function(igraphs, lay = NULL, open = FALSE) {
#  platzhalter_namen_igraphs <<- paste0("graph", seq_along(igraphs))
#  plotliste <- list()
#
#
#  for (i in seq_along(igraphs)) {
#    tmp <- tempfile(fileext = ".jpeg")
#    jpeg(tmp)
#    set.seed(1)
#    plot(igraphs[[i]], layout = matrix_kontroller(lay[names(V(igraphs[[i]])),]))
#    dev.off()
#
#    plotliste[[i]] <- tmp
#  }
#
#
#
#  if (open) {
#    fold <- plotliste %>% str_split("file") %>% `[[`(1) %>% `[[`(1)
#    shell.exec(fold)
#  }
#
#
#  plotliste %>% purrr::as_vector()
#}
# ------------------------------------------------------------------------------

#make_arg_tibble <- function(plotvec, path = "~/graphen/") {
#
#  tibble(
#    media = plotvec,
#    path  = rep(path, length(plotvec)),
#    name  = plotl %>%
#      map(~ str_split(.x, "\\\\") %>%
#            as_vector()) %>% map_chr(~ tail(.x, 1))
#  )
#
#}
#
#
#uploader <- function(media, path, name) {
#
#  googledrive::drive_upload(
#    media,
#    path,
#    name,
#    type = "image/jpeg"
#  ) %>% googledrive::drive_share(role = "reader", type = "anyone")
#
#}
#
#
#
#shseval <- function(x, code = FALSE) {
#
#  tmp  <- tempfile(fileext = ".R")
#
#  sink(tmp)
#  for (i in seq_along(x)) {
#    print(x[[i]])
#  }
#  sink()
#
#  source(tmp, local = globalenv())
#
#  if(code) {
#
#    shell.exec(tmp)
#  }
#
#
#}



##########################################################################################################
##########################################################################################################

igraph_addr      <- list_parser(alles, "IGRAPH")
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

alles  <- roomba::replace_null(alles, "nichts")
alles  <- listviewer::number_unnamed(alles)

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
br  <- function(x) paste0("<br><img src='", x,"'>")
tri <-tri %>% dplyr::mutate(bild=br(url)) %>% dplyr::select(werte=namen_igraphs, bild)

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






