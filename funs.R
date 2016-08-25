get_urls <- function(x) {
  res <- httr::GET(x)
  tmp <- httr::content(res, "text", encoding = "UTF-8")
  html <- xml2::read_html(tmp)
  xml2::xml_attr(xml2::xml_find_all(html, "//a[contains(@href, 'gimletmedia.com/episode')]"), "href")
}

transcript_fetch <- function(x, sleep = 0) {
  Sys.sleep(sleep)
  tt <- xml2::read_html(x)
  xml2::xml_text(xml2::xml_find_first(tt, '//div[@class=\"episode__transcript\"]'))
}

transcript_parse <- function(z) {
  z <- strtrim(strsplit(z, '\n')[[1]])
  xx <- vapply(strextract(z, "^[A-Z].+:"), function(bb) strsplit(bb, ":")[[1]][1], "", USE.NAMES = FALSE)
  uniqnms <- unique(xx)
  tmp <- setNames(lapply(uniqnms, function(w) {
    z[stringi::stri_detect(z, regex = paste0(w, ":"))]
  }), uniqnms)
  # synonymize
  ## pj
  pj <- unname(do.call("c", tmp[names(tmp) %in% c('PJ', 'PJ VOGT')]))
  tmp[names(tmp) %in% c('PJ', 'PJ VOGT')] <- NULL
  tmp$PJ <- pj
  ## alex
  alex <- unname(do.call("c", tmp[names(tmp) %in% c('ALEX', 'ALEX GOLDMAN')]))
  tmp[names(tmp) %in% c('ALEX', 'ALEX GOLDMAN')] <- NULL
  tmp$`ALEX GOLDMAN` <- alex
  tmp
}

try_tokenize <- function(x) {
  if (!is.null(x)) {
    unlist(
      tokenize_words(x, stopwords = stopwords("en"))
    )
  } else {
    NULL
  }
}

sent_cont_plot <- function(z, x) {
  z %>%
    filter(word != "like") %>%
    filter(name == x) %>%
    inner_join(bing) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup() %>%
    filter(n > 17) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    theme_grey(base_size = 16) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("Contribution to sentiment")
}

count_word <- function(x, word) length(grep(word, x, ignore.case = TRUE))

strextract <- function(str, pattern) regmatches(str, regexpr(pattern, str))

strtrim <- function(str) gsub("^\\s+|\\s+$", "", str)
