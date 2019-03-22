#' @export
aio <- function(schritte, mat = NULL) {
  schritte <- enquo(schritte)

  tmptmp   <- geparste_liste(schritte)

  drive_pl <- upload_plots(
    igra_addr_eval = tmptmp$igraph_addr_eval,
    platzh_namen_igraphs = tmptmp$platzhalter_namen_igraphs,
    mat = mat
  )

  map(tmptmp$expr_erw, ~ eval_bare(.x, env = rlang::global_env()))

  parat <- rlang::eval_tidy(schritte)

  parat    <- replace_null(parat, "nichts")
  parat    <- listviewer::number_unnamed(parat)

  # ----------------------------

  werte      <- unlist(parat) %>% unname()

  hierarchie <- unlist(parat) %>% names() %>% stringr::str_replace_all("\\.", "/")


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

  oo <- purrr::map_chr(c(tmptmp$platzhalter_namen_igraphs, NODES), ~or_filter("werte", .x)) %>%
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

  tri <- drive_pl %>% dplyr::select(namen_igraphs, url)

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

      !!!purrr::map(tmptmp$platzhalter_namen_igraphs, ~ patterns_lazy("name", .x, "cyan")),

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

      TRUE ~ "white"))


  tmp2$Set(Farbe=tmp$Farbe)
  tmp2$Set(Bild =tmp$bild)


  tmp2


}


