
library(rvest)

# install selectorGadget
# http://selectorgadget.com/
lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")

# parsing the file with html():
# use html_node() to find the first node that matches that selector, 
# extract its contents with html_text(), and convert it to numeric with as.numeric()

lego_movie %>% 
#  html_node("strong span") %>%
  html_node(".ratingValue span") %>%
  html_text() %>%
  as.numeric()

lego_movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()


lego_movie %>%
  html_nodes("table") %>%

  html_table()