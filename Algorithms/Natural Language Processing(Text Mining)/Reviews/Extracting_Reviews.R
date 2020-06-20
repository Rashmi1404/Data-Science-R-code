setwd("E:\\Bokey\\Excelr Data\\R Codes\\Text Mining_R")
library(rvest)
library(XML)
library(magrittr)

# Amazon Reviews #############################
aurl <- "https://www.amazon.in/Apple-MacBook-Air-13-3-inch-Integrated/product-reviews/B073Q5R6VR/ref=cm_cr_arp_d_paging_btm_3?showViewpoints=1&pageNumber"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"apple.txt",row.names = F)

## Using While loop to get all reviews without using page number ############## 
#### Simple Example ##################################################
samp_url <- "https://www.amazon.in/Predator-Processor-Graphics-Keyboard-G3-571-77QK/product-reviews/B06Y4GZS9C/ref=cm_cr_getr_d_paging_btm_1?showViewpoints=1&pageNumber"
i=1
p=1
predator <- NULL
while(p>0){
  t_url <- read_html(as.character(paste(samp_url,i,sep="=")))
  rev <- t_url %>%
    html_nodes(".review-text") %>%
    html_text()
  predator <- c(predator,rev)
  i <- i+1
  p=length(rev)
}

length(predator)

#######################################################################################


# Snapdeal reviews #############################
surl_1 <- "https://www.snapdeal.com/product/samsung-galaxy-J3-8gb-4g/676860597612/ratedreviews?page="
surl_2 <- "&sortBy=HELPFUL&ratings=4,5#defRevPDP"
snapdeal_reviews <- NULL
for (i in 1:30){
  surl <- read_html(as.character(paste(surl_1,surl_2,sep=as.character(i))))
  srev <- surl %>%
    html_nodes("#defaultReviewsCard p") %>%
    html_text()
  snapdeal_reviews <- c(snapdeal_reviews,srev)
}

write.table(snapdeal_reviews,"samsung.txt",row.names = FALSE)
getwd()
# Sample urls 
# url  = http://www.amazon.in/product-reviews/B01LXMHNMQ/ref=cm_cr_getr_d_paging_btm_4?ie=UTF8&reviewerType=all_reviews&showViewpoints=1&sortBy=recent&pageNumber=1
# url = http://www.amazon.in/Moto-G5-GB-Fine-Gold/product-reviews/B01N7JUH7P/ref=cm_cr_getr_d_paging_btm_3?showViewpoints=1&pageNumber=1
# url = http://www.amazon.in/Honor-6X-Grey-32GB/product-reviews/B01FM7JGT6/ref=cm_cr_arp_d_paging_btm_3?showViewpoints=1&pageNumber=1

########## Extracting reviews from a travel website ###################
a<-10
rev<-NULL
url1<-"https://www.tripadvisor.in/Hotel_Review-g147399-d2354539-Reviews-or"
url2<-"-The_Venetian_on_Grace_Bay-Providenciales_Turks_and_Caicos.html#REVIEWS"
for(i in 0:8){
  url<-read_html(as.character(paste(url1,i*a,url2,sep="")))
  ping<-url %>%
    html_nodes(".partial_entry") %>%
    html_text() 
  rev<-c(rev,ping)
}
write.table(rev,"travel.txt")

# ############# IMDB reviews Extraction ################
# a<-10
# wonder_woman<-NULL
# url1<-"http://www.imdb.com/title/tt0451279/reviews?start="
# for(i in 0:22){
#   url<-read_html(as.character(paste(url1,i*a,sep="")))
#   wonder<-url %>%
#     html_nodes("#tn15content div+ p") %>%
#     html_text() 
#   wonder_woman<-c(wonder_woman,wonder)
# }
# write.table(wonder_woman,file="wonder_woman.txt")
