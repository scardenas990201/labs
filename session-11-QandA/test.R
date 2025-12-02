pacman::p_load(rvest,dplyr,stringr,tidyr,purrr,tidyr)


url <- "http://books.toscrape.com/"
webpage <- read_html(url)

View(webpage)


#Titles
titles_elements <- webpage %>%
  rvest::html_elements(xpath = "//article[@class='product_pod']/h3/a")
  
titles_attributes <- titles_elements %>% 
  html_attr("title")



#Price
price_elements <- webpage %>%
  rvest::html_elements(xpath = "///article[@class='product_pod']/div[@class='product_price']/p[1]")


price_text<- price_elements %>% 
  html_text2()


#Rating

rating_elements <- webpage %>% 
  html_elements(xpath = "//article[@class='product_pod']/p[contains(@class, 'star-rating')]") 
  


rating_text<- rating_elements %>% 
html_attr("class") %>% 
  str_extract("(?<=star-rating )\\w+") %>% 
  recode("One" = 1,
              "Two" = 2,
              "Three" = 3,
              "Four" = 4,
              "Five" = 5)



###STOCK

#Links for stock
link_elements <- webpage %>%
  rvest::html_elements(xpath = "//article[@class='product_pod']/h3/a")

links <- link_elements %>% 
  html_attr("href")


#Function to scrap each page

base_url <- "http://books.toscrape.com/"
full_links <- paste0(base_url, links)


link_function <- function(link) {
  book_page<- read_html(link)
  text <- book_page %>%
    rvest::html_element(xpath = "//table[@class='table table-striped']/tr[th[text()='Availability']]/td") %>%
    rvest::html_text2()
  return(text)
}

descriptions <- map_chr(full_links, link_function)

#Stock_number

stock_numbers <- str_extract(descriptions, "\\d+")





#Cbind
final_data_set<- cbind(titles_attributes,price_text,rating_text,stock_numbers)

View(final_data_set)


  
