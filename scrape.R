library (tidyverse)
library (rvest)
library (httr)
library (stringr)

isbnList <- c() # declare an empty list to store isbns
starC <- c ("one", "two", "three", "four", "five") # this is for getting all each individual star type review
filterC <- c ("&sortBy=recent", "&sortBy=helpful") # this is for two categories
user_a <- user_agent ("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.3.1 Safari/605.1.1")
#set_config (use_proxy(url="201.71.2.177", port = 999)) # YOU NEED A FREE PROXY OR YOUR MAIN IP GETS BANNED

for (i in 1:75) # ive seen certain searches be able to produce 75 pages but this link only allows 50
{
  Sys.sleep (3)
  urlSearchBooks <- paste0  ("https://www.amazon.com/s?k=book&i=stripbooks&s=review-count-rank&page=", i, "&crid=31ZFA7WH3ZCE6&qid=1710631307&sprefix=bo%2Cstripbooks%2C126&ref=sr_pg_", i)
  doc <- session (urlSearchBooks, user_a)
  doc %>% html_nodes ("[class='a-link-normal s-no-outline']") -> listOfBooks

  for (j in listOfBooks) # look for /dp/ then get that ASIN number, toss out the rest
  {
    bookUrl <- toString (j)
    bookUrl <- sub (".*/dp/*", "", bookUrl)
    bookUrl <- sub ("/ref.*", "", bookUrl)
    isbnList <- append (isbnList, bookUrl)
  }
}

for (i in 1:100)
{
  Sys.sleep (3)
  urlSearchBooks <- paste0 ("https://www.amazon.com/s?k=computer&i=stripbooks&rh=n%3A3508&s=review-count-rank&page=", i, "&qid=1710724565&ref=sr_pg_", i)
  doc <- session (urlSearchBooks), user_a)
  doc %>% html_nodes ("[class='a-link-normal s-no-outline']") -> listOfBooks

  for (j in listOfBooks) 
  {
    bookUrl <- toString (j)
    bookUrl <- sub (".*/dp/*", "", bookUrl)
    bookUrl <- sub ("/ref.*", "", bookUrl)
    isbnList <- append (isbnList, bookUrl)
  }
}

isbnList <- unique (isbnList) # make sure that duplicate books are not in the list

scrape <- function (ASIN, pageNum, starIter, tagIter)
{
  url_reviews <- paste0("https://www.amazon.com/product-reviews/", ASIN ,"/ref=cm_cr_arp_d_viewopt_sr?pageNumber=", pageNum, "&filterByStar=", starIter, "_star", tagIter)
  doc <- read_html (url_reviews) # catch the 404 right here
  doc %>% html_nodes ("[data-hook='review-body']") %>% html_text() -> review_text
  doc %>% html_nodes ("[data-hook='review-star-rating']") %>%html_text() -> review_star
 
  doc %>%  html_element ("title") %>% html_text() -> output # DEBUG ONLY: this has no real impact to the scraping it can be deleted
  print (output) #DEBUG ONLY: notice the amazon sign in page which means that amazon blocked iterative scraping
  
  tibble (review_text, review_star) %>% return()
}

Reviews <- c (NA)
Stars <- c (NA)
df = data.frame (Reviews, Stars)

for (book in isbnList)
{
  for (tag in filterC)
  {
    for (iter in starC)
    {
      for (i in 1:10) # it will not work past page 1 due to amazon blocks
      {
        print (paste0 ("ON BOOK: ", book, " |ON TAG: ", tag, " |ON STAR: ", iter, " |ON PAGE: ", i))
        Sys.sleep(1)
        redFlag <- FALSE
        temp <- tryCatch (scrape (ASIN = toString (book), pageNum = i, starIter = iter, tagIter = tag), error = function (e) {redFlag <<- TRUE}) 
        print(redFlag)
        
        if (redFlag == TRUE) # if the page 404s, move on to next ISBN
        {
          next
        }

        if (dim(temp)[1] == 0) # if the x dimension (so any row) is 0 then stop scraping that url
        {
          break # this has to be here because amazon will redirect on page 2-10 for no real reason
        }
        
        df2 <- data.frame (temp$review_text, temp$review_star)
        names (df2) <- names (df)
        df <- rbind (df, df2) # really odd that R doesnt let me directly use append, must use rbind
      }
    }
  }
}

write.csv (df, "output6.csv")
print ("FINISHED SUCCESSFULLY")