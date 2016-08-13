library(httr)
library(xml2)
library(stringi)
library(dplyr)
library("ggplot2")

ra_base <- "https://gimletmedia.com/show/reply-all/episodes"
urls <- c(ra_base, file.path(ra_base, "page", 2:8))

get_urls <- function(x) {
  res <- httr::GET(x)
  tmp <- httr::content(res, "text", encoding = "UTF-8")
  html <- xml2::read_html(tmp)
  xml2::xml_attr(xml2::xml_find_all(html, "//a[contains(@href, 'gimletmedia.com/episode')]"), "href")
}

res <- lapply(urls, get_urls)

epnames <- sub("/", "", strextract(unlist(res), "[0-9]+-.+"))

transcript_fetch <- function(x, sleep = 0) {
  Sys.sleep(sleep)
  tt <- xml2::read_html(x)
  xml2::xml_text(xml2::xml_find_first(tt, '//div[@class=\"episode__transcript\"]'))
}

txts <- lapply(unlist(res), transcript_fetch, sleep = 1)

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

count_word <- function(x, word) {
  length(grep(word, x, ignore.case = TRUE))
}

txtsp <- lapply(txts, transcript_parse)

dat <- setNames(lapply(txtsp, function(m) {
  bind_rows(lapply(m, function(v) {
    tmp <- unname(vapply(v, nchar, 1))
    data_frame(
      n = length(tmp),
      mean = mean(tmp),
      n_laugh = count_word(v, "laugh"),
      n_groan = count_word(v, "groan")
    )
  }), .id = "name")
}), epnames)

# library(tm)
# corp <- tm::Corpus(tm::VectorSource(v))
# corp <- tm::tm_map(corp, removeWords, stopwords('english'))
# dtm <- tm::DocumentTermMatrix(corp)
# tm::findFreqTerms(dtm, 4, 6)

data <- bind_rows(dat, .id = "episode") %>%
  filter(!is.na(episode)) %>%
  filter(grepl("^PJ$|^ALEX GOLDMAN$", name)) %>%
  mutate(ep_no = as.numeric(strextract(episode, "^[0-9]+"))) %>%
  group_by(ep_no) %>%
  mutate(nrow = NROW(ep_no)) %>%
  ungroup() %>%
  filter(nrow == 2)

ggplot(data, aes(ep_no, n, colour = name)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_line(aes(group = ep_no), colour = "black")
# coord_flip() +
# facet_wrap(~ ep_no)

ggplot(data, aes(ep_no, mean, colour = name)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_line(aes(group = ep_no), colour = "black")

