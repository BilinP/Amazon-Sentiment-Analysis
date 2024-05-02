library (tidyverse)
library (rvest)
library (stringr)
library(RSelenium)
library(crayon)


#all ASIN numbers for books
ASINs <- list("B073HBQXMT","B0759XMXLV","B0078XQQIC","B0997FBQFJ","593311299","1984896393","B0BQ1FX4CC","B0BMF2M8Z6","1728234220","B06XHMLMT4","B0CPTDJLYK","B00B4DR9GK","679805273","B09CDX284F","B09J97T32C","B0B2TSN4CZ","B009YQ719O","1492663999","B003TO5GXU","B0BSK3GDJ7","B084BS795T","B09WBTRGNL","B004XJRQUQ","B08D9WJ9G8","B00M284V44","B088798Q2H","B0018OQ2F4","1419748688","B005C7QVUE","B01N6DZ5W9","B00CRKNR88","B07Y22KM21","1416968296","B019PHYQG2","B0C2QRBG5Z","B009YQ736U","615835600","1728274893","159448385X","B07D6XQQRY","B0B9RNFT4S","1501128035","B01NBDMFWA","B003NYOBPA","1250857430","B00LFVU0AS","B07RLSB7QV","B07DTB52HJ","399501487","B01MYZ8X5C","B0BP8JRWLP","1250803845","1728274885","553213105","1984818430","1338535625","B08WCFQVR8","1542028752","B005MM7F7W","B09FS9ZGJD","593438531","60555661","762447699","1941325823","1250076226","B00J8VN0YU","B0BYHPHG1L","B087JJ2K54","1737507730","1455570249","307455920","B00U6SFUVA","1476791457","B0BQLKNTYR","1721284443","1957635037","B09RQ4L751","1476755744","1728297087","B085VXLKRJ","593359534","B0944BZ5XX","B07MQRYBKP","B010MHAEGA","B0BKR8VBYV","60850523","B08L9K245G","62430254","1591451884","1954839499","1674844468","B089WHV9Y7","B008U2P6HC","B00UG8RP22","B07PYJB9Z9","B0BTRSXLMZ","B085FYFZCC","B088C3PM7N","031631031X","B012BNM1LO","B00FRVH3V6","B000LC3JX2","B09FQZ7R24","B08C7DQWRC","812993543","B01602244G","385737955","B08C36KC3J","B005Z9GAJG","1524714755","593128176","B07BHTMGJ4","B09RWNTLMR","B00AFPQQE4","B08DKZX3FX","B01COR1GM2","62073486","B006ST49K4","B00GU2RLMC","B08LDNC526","B00P00QPPY","B0721NKNHR","B0B8JY1HCH","1451645856","B0B3NFRZHK","B08L3NHJ88","1250169445","B00DEKLQOC","B07HZ2Q1MK","B0001MC01Y","762462876","62439596","B082FQRWWR","593101545","B0947FLX32","142314189X","1616499605","B07X8QV4PV","B077GZBL1Y","1538727358","B078FG5BS8","074324754X","1728282136","964729237","B0C4QD5VPB","B06ZXTHNSJ","B00006JMD2","1594632383","1668002523","B091RHWB84","684801221","B0BRBPSXCW","B001BACYFM","B00U6DNZOY","544336267","140177396","316769177","B07JYC69YM","1623159911","143121162","B00I765ZEU","B00SVCEIAM","B0009JKV9W","B0043M4ZH0","B0BRPZLG9G","62457802","B07BMCMS5L","B08GV5KZN8","1451696191","B07D2BJ2JQ","B082WJBS2V","1572245379","1542020166","125026569X","B0992Y537S","B074TT2GVY","B09PG2KSJH","B08S5H44KQ","B019NMZ5MI","1538752840","B00026WUO6","B09ZKRWCG4","B08S5K9LXX","1400031702","B072C4XBZ9","B071ZQJW42","B08KSCBP6H","081298496X","62059947","B001QKBHG4","B0B3HDFWY9","B01ER6Z7RI","B01496P8T8","1250088569","7554850","B0C5YJ1FKQ","385341008","B01N1YXMP1","1442450703","316310379","B097S6JH9H","031631613X","B000X1MX7Y","1101971061","014240733X","B0BWNTYGB7","B007EJSMC8","1250819350","006325235X","399562974","039335668X")
stars <- c ("one", "two", "three", "four", "five") # all  individual star type reviews
sorts <- c ("&sortBy=recent", "&sortBy=helpful")# different way to sort reviews either by recent or top reviews

amazon_account_email<-"" # amazon account email
amazon_account_password<-"" #amazon account password
outputCSV<-"output16.csv" #output csv file to store data
next_button_found <<- TRUE


#functions loops through 10 pages of reviews which has 10 reviews per page thus collecting around 100 reviews
scraper <- function(asinChr, starChr, sortChr)
{
  url <- paste0("https://www.amazon.com/product-reviews/", asinChr ,"/ref=cm_cr_arp_d_viewopt_srt?filterByStar", starChr, "&pageNumber=1",sortChr,"&filterByStar=",starChr,"_star")
  remDr$navigate(url) 
  if(sorts[1]==sortChr){
    sortBy<-"Recent"
  }else{ sortBy<-"Top Review"}
  
  for (i in 1:10){
    tryCatch({
      
      Sys.sleep(1)
      html<- read_html(remDr$getPageSource()[[1]])
      
      html %>%
        html_nodes("[data-hook='review-star-rating']") %>%
        html_text()->review_star 
      html %>%
        html_nodes("[data-hook='review-body']") %>%
        html_text(trim = TRUE)->review_text 
      html %>%
        html_nodes(".a-text-ellipsis .a-link-normal") %>%
        html_text()->title
      html %>%
        html_nodes("[data-hook='review-title']") %>%
        html_text(trim = TRUE)->review_title
      review_title <- review_title[-1]
      review_title <- review_title[-1]
      review_title <- sub(".*\n(.*?)$", "\\1",  review_title)
      temp <- data.frame (title,review_title,review_text, review_star,i,sortBy)
      names(temp) <- names (df)
      df <<- rbind (df, temp)
      print (paste0 ("ON BOOK: ", asinChr, " |Sort by: ", sortBy, " |ON STAR: ", starChr, " |ON PAGE: ", i))
    }, error = function(e) {
      cat(red("ERROR: ON BOOK: ", asinChr, " |Sort by: ", sortBy, " |ON STAR: ", starChr, " |ON PAGE: ", i, "\n"))
      print(e)
    })
    # Check if "Next" button is found
    tryCatch({
      if (i != 10) { 
        next_button <- remDr$findElement(using = "css", ".a-last a")
        next_button$clickElement()
      }
    }, error = function(e) {
      cat("Next button not found. Exiting function.\n")
      next_button_found <<- FALSE
    })
    
    # If "Next" button is not found, exit the function
    if (!next_button_found) {
      return()
    }
    
  }
}


#start RSelenium browser
remote<-rsDriver(browser = "firefox",verbose = FALSE,port=4445L,geckover="latest")
remDr <- remote[["client"]]
# opens login link from the browser 
remDr$navigate("https://www.amazon.com/ap/signin?openid.pape.max_auth_age=0&openid.return_to=https%3A%2F%2Fwww.amazon.com%2Fproduct-reviews%2FB07FSXPMHY%2Fref%3Dnav_custrec_signin%3FfilterByStarfive%26pageNumber%3D1%26sortBy%3Drecent%26filterByStar%3Dfive_star&openid.identity=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0%2Fidentifier_select&openid.assoc_handle=usflex&openid.mode=checkid_setup&openid.claimed_id=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0%2Fidentifier_select&openid.ns=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0")

#enter email and password 
Sys.sleep(1)
email_input <- remDr$findElement(using = "css", "#ap_email")
email_input$sendKeysToElement(list(amazon_account_email,key = "enter"))
Sys.sleep(1)
pass_input <- remDr$findElement(using = "css", "#ap_password")
pass_input$sendKeysToElement(list(amazon_account_password,key = "enter"))

#Waits for manual input if the  CAPTCHA is solved  which require someone to type 'yes' if its solved or doesn't appear. 
captcha_solved <- FALSE
while (!captcha_solved) {
  print("Is Captcha solved (Enter 'yes' if so): ")
  captcha_status <- readline()
  if (tolower(captcha_status) == "yes") {
    captcha_solved <- T
  } else {
    Sys.sleep(3)  
  }
}
print("CAPTCHA has been solved. Continuing with the rest of the code...")

#checks if output file exist. If not it creates
if (!file.exists(outputCSV)) {
  file.create(outputCSV)
}
#initialize data frame with column headers. 
df <- data.frame(Book = character(),
                 'Review Title' = character(),
                 Reviews = character(),
                 Stars = character(),
                 Page = integer(),
                 Sort=character(),
                 stringsAsFactors = FALSE)

write.csv(df, outputCSV,row.names = FALSE)

#loops thorough each book, each sorting method, and each star
for (ASIN in ASINs)
{
  for (sort in sorts)
  {
    for (star in stars)
    {
      Sys.sleep(1)
      scraper(asinChr=toString(ASIN),starChr = star,sortChr = sort)
      if (!next_button_found) {
        break
      }
      existing<- read.csv(outputCSV) #gets existing data
      combined <- rbind(existing, df) #combine existing and new 
      write.csv(combined, outputCSV,row.names = FALSE) #writes back into csv file
      df <- df[0,] #empties data frame
    }
    if (!next_button_found) {
      next_button_found<<-TRUE
      break
    }
  }
}
print ("FINISHED SUCCESSFULLY")
