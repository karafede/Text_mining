
library(RCurl)

# now lets extract the HTML code from my blog using getURL()
# from the RCurl package

getURL("http://www.markheckmann.de")

# this looks pretty unstructured. But we may have an organized
# view using htmlTreeParse() from the XML package
# This is just to see what we are dealing with

library(XML)
htmlTreeParse(getURL("http://www.markheckmann.de")
              
              # Now let's do a google request using the browsers command
              # line. This can be achieved via the RCurl getForm() function,
              # which constructs and sends such a line. Here we can choose
              # hl=language, q= search terms and several other parameters.
              # Let's search for the term "r-project".
              
              site <- getForm("http://www.google.com/search", hl="en",
                              lr="", q="r-project", btnG="Search")
              htmlTreeParse(site)
              
              # Now we have the Google result HTML code and have to
              # extract the relevant information from it.
              
              typeof(site)
              
              # As we see, site contains plain character HTML code, so
              # I can use use simple text manipulation functions here.
              
              # What part of the code do I have to extract now? Somewhere
              # in the HTML code there is a line like this:
              #                  <b> some numerics </b>
              # So the number is in bewteen the <b> </b> argument. How
              # can we get this?
              
              text <- "We are looking for something like <b>12.345</b>
              or similar"
              gregexpr("<b>12.345</b>", text, fixed = TRUE)
              
              # gregexpr will return the position of the text we are searching
              # for. Now we need to generalize this to all numbers. I am
              # still not too familiar with regular expressions. Chapter
              # seven in Spector, P. (2008). Data Manipulation with R (UseR)
              # contains a good explanation of these.
              
              gregexpr('<b>[0-9.,]{1,20}</b>', text)
              
              # This does the job! The problem now is that there are a
              # number of brackets like the one above containing numbers.
              # So we need a to find the exact parts which to extract. 
              # In an English google search there is the words "of about"
              # followed by the search count. In German it is preceeded by
              # the word "ungefähr". I will use these as indicator words to
              # spot the position from where to extract.
              
              indicatorWord <- "of about"
              
              # start extraction after indicator word
              posExtractStart <- gregexpr(indicatorWord, siteHTML,
                                          fixed = TRUE)[[1]]
              
              # extract string of 30 chracters length which should be enough
              # to get the numbers
              stringExtract <- substring(siteHTML, first=posExtractStart,
                                         last = posExtractStart + 30)
              
              # search for <b>number</b> (see above)
              posResults <- gregexpr('<b>[0-9.,]{1,20}</b>', stringExtract)
              posFirst <- posResults[[1]][1]
              textLength  <- attributes(posResults[[1]])$match.length
              stringExtract <- substring(stringExtract, first=posFirst,
                                         last = posFirst + textLength)
              
              # actually the last four lines are usually not necessary. Just
              # in case the search term itself is numeric we would run the
              # risk of unwillingly extracting some abundant numerics
              # distorting the count results.
              
              # erase everything but the numbers
              stringExtract <- gsub("[^0-9]", "", stringExtract)
              
              print(stringExtract)
              
              # now we can use this for the calculation of the normalized
              # google distance
              
              