murl <- read_html(as.character(paste(aurl,2,sep="=")))
rev <- murl %>%
  html_nodes(".review-text") %>%
  html_text()
amazon_reviews <- c(amazon_reviews,rev)

html_text(html_nodes(murl,".review-text"))

murl %>%
  html_nodes(".review-text") %>%
  html_text()



p <- read_html("https://www.amazon.in/Apple-MacBook-Air-13-3-inch-Integrated/product-reviews/B073Q5R6VR/ref=cm_cr_arp_d_paging_btm_60?showViewpoints=1&pageNumber=61")
length(html_text(html_nodes(p,".review-text")))

p=1
aurl <- "https://www.amazon.in/Predator-Processor-Graphics-Keyboard-G3-571-77QK/product-reviews/B06Y4GZS9C/ref=cm_cr_arp_d_paging_btm_2?showViewpoints=1&pageNumber"
predator <- NULL
while(p>0){
  i=1
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  predator <- c(predator,rev)
  i <- i+1
  p=length(rev)
}
