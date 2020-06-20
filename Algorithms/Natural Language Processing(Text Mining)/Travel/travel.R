
library(rvest)
library(magrittr)
library(XML)
a<-10
rev<-NULL
# Below we are splitting our entire url into 2 parts as we can see that page number lies in between them
url1<-"https://www.tripadvisor.in/Hotel_Review-g147399-d2354539-Reviews-or"
url2<-"-The_Venetian_on_Grace_Bay-Providenciales_Turks_and_Caicos.html"

# Running over a for loop to get content from each page 
for(i in 0:8){
  url<-read_html(as.character(paste(url1,i*a,url2,sep="")))
  
  # Using pipe (%>%) makes use easier to understand what exactly each function is actually doing
  ping<-url %>%
    html_nodes(".partial_entry") %>%  ## this the node where our reviews were residing 
    html_text() # this function is to get the text part present between the tags 
  rev<-c(rev,ping)
}

# writing all the extracted reviews into txt file.
write.table(rev,"travel.txt")
