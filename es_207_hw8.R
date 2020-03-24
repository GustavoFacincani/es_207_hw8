#Extracting middle strings

L <- str_length(x)
m <- ceiling(L / 2)

#Formula for odd strings
str_sub(x, m, m)

#Formula for even string
str_sub(y, m, m+1)




#Function to turn a vector into a string

str_commasep <- function(x, delim = ",") {
  n <- length(x)
  #when length = 0
  if (n == 0) {
    ""
    #when length = 1
  } else if (n == 1) {
    x
    #when lenght = 2
  } else if (n == 2) {
    #Use str_c() to combine strings
    str_c(x[[1]], "and", x[[2]], sep = " ")
  } else {
    # commas after all elements but the last, using seq_len, which creates a sequence that starts at 1 and with steps of 1 finishes at the number value
    not_last <- str_c(x[seq_len(n - 1)], delim)
    #Now we set the last element
    last <- str_c("and", x[[n]], sep = " ")
    #Combine parts with spaces
    str_c(c(not_last, last), collapse = " ")
  }
}
#Vector of lenght 0
str_commasep("")
#Vector of lenght 1
str_commasep("a")
#Vector of lenght 2
str_commasep(c("a", "b"))
#Vector of lenght 3
str_commasep(c("a", "b", "c"))
#Vector of lenght 4
str_commasep(c("a", "b", "c", "d"))




#find words that start with a vowel
str_subset(stringr::words, "^[aeiou]")


#Find only words that contain consonants
str_subset(stringr::words, "^[^aeiou]+$")


#Find words that end with ing or ise
str_subset(stringr::words, "i(ng|se)$")


#Find most common words
tibble(word = unlist(str_extract_all(sentences, boundary("word")))) %>%
  mutate(word = str_to_lower(word)) %>% #to count lower and upper case
  count(word, sort = TRUE) %>% #sort to put it in a rank, if FALSE it will be in alphabetic order
  head(5)

#summarize data by site and by month and year
require(lubridate)
permonth <- o3.filelist %>%
  rbindlist() %>%
  mutate(month = format(date, "%mm"), year = format(date, "%YYYY")) %>%
  group_by(site = as.factor(site), month, date) %>%
  summarize(o3 = mean(obs, na.rm = TRUE))
permonth

#Find difference between diurnal pattern vs. nocturnal
o3diurnal <- function(x){
  x$yr<- str_extract(x$date,"\\d{4}")
  x$mon <- str_sub(x$date, 6,-4)
  x$time <- ifelse(x$start_hour >= 7 & x$start_hour<= 15, "day", "night")}
#Grouping
out <- group_by(x, site = as.factor(site), Year = as.factor(yr),
                Month = as.factor(mon),Time = time) %>%
  summarize(o3 = mean(obs, na.rm = TRUE))

diurnalonly <- o3.filelist %>% 
  map(o3diurnal)
diurnalonly


#Separate out information from tibbles
realloc <- loc %>%
  filter(!str_detect(Address, 'Location Approximate|Address Not Known')) %>% #filtering places withouth address
  filter(!is.na(`Zip Code`)) %>% #filtering blanks
  filter(str_detect(`Zip Code`, "[0-9]{5}")) %>% #filtering wrong zip codes with less than 5 numbers
  mutate(a_count = str_count(Address,"\\d+\\s\\w")) %>%
  filter(!(is.na(a_count)|(a_count == 0)))
nrow(loc)-nrow(realloc)
```