
# load packages ---------------------------------------------------------------------------------------------------

library(package = "RCurl")
library(package = "XML")

# get list of petitions -------------------------------------------------------------------------------------------

# main page url is https://petitions.whitehouse.gov/petitions
next_link <- "https://petitions.whitehouse.gov/petitions"

# if the link is non-null, follow it and grab petitions
petitions <- data.frame(title = character(length = 0), link = character(length = 0), stringsAsFactors = FALSE)
while (!is.null(next_link)) {
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