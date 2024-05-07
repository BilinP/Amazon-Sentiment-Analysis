library (tidyverse)
library (rvest)
library (httr)
library (stringr)

############################
#
# INITIALIZE IMPORTANT VARIABLES ELSE NONE OF THIS WORKS
#
############################

isbn_list <- c() # declare an empty list to store isbns
starC <- c ("one", "two", "three", "four", "five") # this is for getting all each individual star type review
filterC <- c ("&sortBy=recent", "&sortBy=helpful") # this is for two categories
Reviews <- c (NA)
Stars <- c (NA)
df = data.frame (Reviews, Stars)

# network related functions to try to avoid amazon blocks
userA <- "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.3"
set_config (use_proxy (url = "35.185.196.38", port = 3128)) 
# YOU NEED A FREE PROXY OR YOUR MAIN IP GETS BANNED

############################
# 
# GENERATE LIST OF BOOKS IN ORDER TO SCRAPE THEIR REVIEWS
#
############################

for (i in 1:75) # for 75 pages get all books per page
{
  Sys.sleep (3)
  url_search_books <- paste0 ("https://www.amazon.com/s?k=book&i=stripbooks&s=review-count-rank&page=", i, "&crid=31ZFA7WH3ZCE6&qid=1710631307&sprefix=bo%2Cstripbooks%2C126&ref=sr_pg_", i)
  doc <- session (url_search_books, user_agent(userA))
  doc %>% 
	html_nodes ("[class='a-link-normal s-no-outline']") -> list_of_books
  
  for (j in list_of_books) # look for /dp/ then get that ASIN number, toss out the rest
  {
    book_url <- toString (j)
    book_url <- sub (".*/dp/*", "", book_url)
    book_url <- sub ("/ref.*", "", book_url)
	isbn_list <- append (isbn_list, book_url)
  }
}

for (i in 1:100) # for 100 pages get all books per page
{
  Sys.sleep (3)
  url_search_books <- paste0 ("https://www.amazon.com/s?k=computer&i=stripbooks&rh=n%3A3508&s=review-count-rank&page=", i, "&qid=1710724565&ref=sr_pg_", i)
  doc <- session (url_search_books, user_agent(userA))
  doc %>% 
	html_nodes ("[class='a-link-normal s-no-outline']") -> list_of_books
  
  for (j in list_of_books) 
  {
    book_url <- toString (j)
    book_url <- sub (".*/dp/*", "", book_url)
    book_url <- sub ("/ref.*", "", book_url)
    isbn_list <- append (isbn_list, book_url)
  }
}

isbn_list <- unique (isbn_list) # make sure that duplicate books are not in the list

############################
#
# SCRAPE FUNCTION THAT RETURNS ALL 10 REVIEWS PER REVIEW PAGE
#
############################

scrape <- function (ASIN, page_num, star_iter, tag_iter) # iter = iteration
{
  url_reviews <- paste0("https://www.amazon.com/product-reviews/", ASIN ,"/ref=cm_cr_arp_d_viewopt_sr?page_number=", page_num, "&filterByStar=", star_iter, "_star", tag_iter)
  doc <- read_html (url_reviews) # catch the 404 right here
  print (doc)
  doc %>% 
	html_nodes ("[data-hook='review-body']") %>% 
	html_text() -> review_text
  doc %>%
	html_nodes ("[data-hook='review-star-rating']") %>% 
	html_text() -> review_star
  
  doc %>%  html_element ("title") %>% html_text() -> output # DEBUG ONLY: this has no real impact to the scraping it can be deleted
  print (output) #DEBUG ONLY: notice the amazon sign in page which means that amazon blocked iterative scraping
  
  tibble (review_text, review_star) %>% return()
}

############################
#
# FOR EVERY BOOK
#   FOR EVERY CATEGORY FILTER
#     FOR EVERY STAR RATING FROM 1-5
#       FOR EVERY PAGE 1-10
#         SCRAPE 10 REVIEWS AND PUT IT INTO TEMP SO IT CAN BE WRITTEN TO A FILE
#
############################

progress <- 0

for (book in isbn_list)
{
  for (tag in filterC)
  {
    for (iter in starC)
    {
      for (i in 1:10) # it will not work past page 1 due to amazon blocks
      {
        print (paste0 ("ON BOOK: ", book, " |ON TAG: ", tag, " |ON STAR: ", iter, " |ON PAGE: ", i))
        #Sys.sleep(1)
        redFlag <- FALSE
        temp <- tryCatch (scrape (ASIN = toString (book), page_num = i, star_iter = iter, tag_iter = tag), error = function (e) {redFlag <<- TRUE}) 
        print(redFlag)
        
        if (redFlag == TRUE) # if the page 404s, move on to next ISBN
        {
          next
        }
        
        if (dim (temp)[1] == 0) # if the x dimension (so any row) is 0 then stop scraping that url
        {
          break # this has to be here because amazon will redirect on page 2-10 for no real reason
          # i want to use next but it will just waste time
        }
        
        df2 <- data.frame (temp$review_text, temp$review_star)
        names (df2) <- names (df)
        df <- rbind (df, df2) # really odd that R doesnt let me directly use append, must use rbind
      }
    }
  }
  
  progress <- progress + 1
}

write.csv (df, "output9.csv")
print ("FINISHED SUCCESSFULLY")