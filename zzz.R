strextract <- function(str, pattern) regmatches(str, regexpr(pattern, str))

strtrim <- function(str) gsub("^\\s+|\\s+$", "", str)

ra_base <- function() "https://gimletmedia.com/show/reply-all/episodes/"

comp <- function(l) Filter(Negate(is.null), l)
