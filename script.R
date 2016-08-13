library("httr")
library("xml2")
library("stringi")
library("dplyr")
library("ggplot2")

# source functions
source("funs.R")

# base url
ra_base <- "https://gimletmedia.com/show/reply-all/episodes"

# make all urls for each page of episodes
urls <- c(ra_base, file.path(ra_base, "page", 2:8))

# get urls for each episode
res <- lapply(urls, get_urls)

# get episode names
epnames <- sub("/", "", strextract(unlist(res), "[0-9]+-.+"))

# fetch transcripts
txts <- lapply(unlist(res), transcript_fetch, sleep = 1)

# parse transcripts
txtsp <- lapply(txts, transcript_parse)

# summarise data for each transcript
dat <- stats::setNames(lapply(txtsp, function(m) {
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

library("tm")
corp <- tm::Corpus(tm::VectorSource(v))
corp <- tm::tm_map(corp, removeWords, stopwords('english'))
dtm <- tm::DocumentTermMatrix(corp)
tm::findFreqTerms(dtm, 4, 6)

# bind data together to single dataframe, and filter, summarise
data <- bind_rows(dat, .id = "episode") %>%
  filter(!is.na(episode)) %>%
  filter(grepl("^PJ$|^ALEX GOLDMAN$", name)) %>%
  mutate(ep_no = as.numeric(strextract(episode, "^[0-9]+"))) %>%
  group_by(ep_no) %>%
  mutate(nrow = NROW(ep_no)) %>%
  ungroup() %>%
  filter(nrow == 2)

# N
ggplot(data, aes(ep_no, n, colour = name)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_line(aes(group = ep_no), colour = "black")

# mean
ggplot(data, aes(ep_no, mean, colour = name)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_line(aes(group = ep_no), colour = "black")

