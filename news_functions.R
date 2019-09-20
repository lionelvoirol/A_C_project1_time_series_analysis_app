# news collection file

collect_news <- function(data, stock_name){
  options(warn=-1)
  # The search bar requires a very special structure. We respect this with this search
  part1 <- "https://news.google.com/rss/search?q="
  part2 <- "&hl=en-US&gl=US&ceid=US:en"
  name_without_spaces <- str_replace_all( data[which(data$symbol == stock_name),2] , " " , "%")
  news_source_url <- paste(part1, name_without_spaces, "%stock", part2, sep = "")
  news <- tidyfeed(news_source_url)

  title1 <- news$item_title[1];title2 <- news$item_title[2];title3 <- news$item_title[3]
  title4 <- news$item_title[4];title5 <- news$item_title[5];title6 <- news$item_title[6]
  title7 <- news$item_title[7];title8 <- news$item_title[8];title9 <- news$item_title[9]
  title10 <- news$item_title[10]
  
  link1 <- news$item_link[1]
  link2 <- news$item_link[2]
  link3 <- news$item_link[3]
  link4 <- news$item_link[4]
  link5 <- news$item_link[5]
  link6 <- news$item_link[6]
  link7 <- news$item_link[7]
  link8 <- news$item_link[8]
  link9 <- news$item_link[9]
  link10 <- news$item_link[10]
  
  titles_and_links <- (list(link1,title1,link2,title2,link3,title3,
                            link4,title4,link5,title5,link6,title6,
                            link7,title7,link8,title8,link9,title9,
                            link10,title10))
  
  
  
  options(warn=0)
  return(titles_and_links)
}