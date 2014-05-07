
# load packages ---------------------------------------------------------------------------------------------------

library(package = "RCurl")
library(package = "stringr")
library(package = "XML")

# get list of petitions -------------------------------------------------------------------------------------------

# main page url is https://petitions.whitehouse.gov/petitions
next_link <- "https://petitions.whitehouse.gov/petitions"

# if the link is non-null, follow it and grab petitions
petitions <- data.frame(title = character(length = 0), link = character(length = 0), stringsAsFactors = FALSE)
while (!is.null(x = next_link)) {
  # get/parse html
  tree <- htmlParse(file = getURL(url = next_link), asText = TRUE)
  
  # on current page, find all <div> tags with class "entry"
  # this isn't a perfect solution: a <div> with class "sentry" would also be returned
  # for a given entry, pull out the title and link to more information
  entry_info <- xpathApply(doc = tree, path = "//div[contains(@class,'entry')]/div[contains(@class,'title')]/a[@href]",
                           fun = function(entry) {
                             title <- xmlValue(x = entry)
                             link <- xmlGetAttr(node = entry, name = "href")
                             return(data.frame(title = title, link = link, stringsAsFactors = FALSE))
                           })
  
  # append petitions to data set
  petitions <- rbind(petitions, do.call(what = "rbind", args = entry_info))
  
  # get next link to more petitions
  next_link <- xpathApply(doc = tree, path = "//div[contains(@id,'petition-bars')]/a[@href]/@href",
                          fun = function(href) {
                            href <- paste0("https://petitions.whitehouse.gov", href)
                            href <- substr(x = href, start = 1, stop = nchar(x = href) - 1)
                            return(href)})
}

# get petition information ----------------------------------------------------------------------------------------

# this section should be pretty similar to the one above
# the major difference should be the information to extract

# select a petition to investigate further
petition <- petitions[1,]

# the petition link should have https://petitions.whitehouse.gov prepended
petition_link <- paste0("https://petitions.whitehouse.gov", petition[,"link"])

# if the link is non-null, follow it and grab petitions
petition_info <- list(created_date = NA, end_date = NA, n_signatures = NA)
signatures <- data.frame(initials = character(length = 0), city = character(length = 0), state = character(length = 0),
                         signature_position = integer(length = 0), sign_date = character(length = 0),
                         stringsAsFactors = FALSE)
first_page <- TRUE
while (!is.null(x = petition_link)) {
  # get/parse html
  tree <- htmlParse(file = getURL(url = petition_link), asText = TRUE)
  
  # on the first page, get the petition information
  if (first_page) {
    petition_info["created_date"] <- gsub(pattern = "Created: ", replacement = "",
                                          x = xpathApply(doc = tree, path = "//div[contains(@class,'date')]",
                                                         fun = xmlValue)[[1]])
    petition_info["end_date"] <- gsub(pattern = "Signatures needed by | to reach goal of 100,000", replacement = "",
                                      x = xpathApply(doc = tree, path = "//div[contains(@id,'sig-needed')]/h4",
                                                     fun = xmlValue)[[1]])
    petition_info["n_signatures"] <- as.integer(x = xpathApply(doc = tree, path = "//div[contains(@id,'total-on')]/div",
                                                               fun = xmlValue)[[1]])
    first_page <- FALSE
  }
  
  # on current page, find all <div> tags with class "entry-creator" or "entry-reg"
  # for a given signature, pull out the initials, city, state, signature position, and sign date
  signature_info <- xpathApply(doc = tree,
                               path = "//div[contains(@class,'entry-creator') or contains(@class,'entry-reg')]",
                               fun = function(signature) {
                                 value <- str_trim(string = scan(text = xmlValue(x = signature), what = character(),
                                                                 sep = "\n", quiet = TRUE), side = "both")
                                 initials <- ifelse(test = value[5] == "", yes = NA, no = value[5])
                                 city_state <- ifelse(test = value[7] == "", yes = "",
                                                      no = strsplit(x = value[7], split = ", "))
                                 if (city_state != "") {
                                   city <- city_state[[1]][1]
                                   state <- city_state[[1]][2]
                                 } else {
                                   city <- state <- NA
                                 }
                                 signature_position <- ifelse(test = value[9] == "", yes = as.integer(x = NA),
                                                              no = gsub(pattern = "Signature # ", replacement = "",
                                                                        x = value[9]))
                                 sign_date <- ifelse(test = value[8] == "", yes = NA, no = value[8])
                                 return(data.frame(initials = initials, city = city, state = state,
                                                   signature_position = signature_position, sign_date = sign_date,
                                                   stringsAsFators = FALSE))
                               })
  
  # append petitions to data set
  signatures <- rbind(signatures, do.call(what = "rbind", args = signature_info))
  
  # get next link to more petitions
  petition_link <- xpathApply(doc = tree, path = "//a[contains(@class,'load-next')]/@href",
                          fun = function(href) {
                            href <- paste0("https://petitions.whitehouse.gov", href)
#                             href <- substr(x = href, start = 1, stop = nchar(x = href) - 1)
                            return(href)})[[1]]
}