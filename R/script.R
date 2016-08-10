library(httr)
library(xml2)
library(stringi)
library(dplyr)
library("ggplot2")

res <- httr::GET(ra_base())
x <- httr::content(res, "text", encoding = "UTF-8")
html <- xml2::read_html(x)
urls <- xml2::xml_attr(xml2::xml_find_all(html, "//a[contains(@href, 'gimletmedia.com/episode')]"), "href")
epnames <- sub("/", "", strextract(urls, "[0-9]+-.+"))

transcript_fetch <- function(x) {
  tt <- xml2::read_html(x)
  xml2::xml_text(xml2::xml_find_first(tt, '//div[@class=\"episode__transcript\"]'))
}

txts <- lapply(urls, transcript_fetch)

transcript_parse <- function(z) {
  z <- strtrim(strsplit(z, '\n')[[1]])
  xx <- vapply(strextract(z, "^[A-Z].+:"), function(bb) strsplit(bb, ":")[[1]][1], "", USE.NAMES = FALSE)
  uniqnms <- unique(xx)
  setNames(lapply(uniqnms, function(w) {
    z[stringi::stri_detect(z, regex = paste0(w, ":"))]
  }), uniqnms)
}

txtsp <- lapply(txts, transcript_parse)

dat <- setNames(lapply(txtsp, function(m) {
  bind_rows(lapply(m, function(v) {
    tmp <- unname(vapply(v, nchar, 1))
    data_frame(n = length(tmp), mean = mean(tmp), var(tmp))
  }), .id = "name")
}), epnames)

bind_rows(dat, .id = "episode") %>%
  filter(nchar(name) < 30) %>%
  filter(grepl("^PJ$|^ALEX BLUMBERG$|^ALEX GOLDMAN$|^ALEX$", name)) %>%
  ggplot(., aes(name, mean, colour = name)) +
    geom_point() +
    coord_flip() +
    facet_wrap(~ episode)
