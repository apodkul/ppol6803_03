library(dplyr)

### Working with API 
# Example: FDA Food Enforcement API 
# Documentation link: https://open.fda.gov/apis/food/enforcement/how-to-use-the-endpoint/
url <- 'https://api.fda.gov/'
end_point <- 'food/enforcement.json'

library(httr); library(stringr)
fda_call <- GET(url = str_c(url, end_point))
http_status(fda_call)

content(fda_call)

fda_call <- GET(url = stringr::str_c(url, end_point), 
                query = list(search = "distribution_pattern = 'nationwide'", 
                             limit = 10)
                )

content(fda_call, as = 'text')
content(fda_call, as = 'parsed')

#What if we want to use the data in a dataframe? This might get messy but... 
#install.packages("jsonlite")
library(jsonlite)

content(fda_call, as = 'text') %>%
  fromJSON() %>%
  data.frame()


### Working with an API wrapper 
#Notes: https://ropengov.github.io/eurostat/articles/website/eurostat_tutorial.html
#install.packages("eurostat")
library(eurostat)

get_eurostat_toc() %>% 
  View()

output <- get_eurostat(id = 'demo_r_births',
                             filters = list(
                               geo = c("IT", "FI", 
                                       "FR", "DE"),
                               lastTimePeriod = 1
                             ),
                             type = "label", time_format = "num"
)


### Scraping an html table 
# Example: OPM 2024 General Salary Tables
library(rvest)
url <- 'https://www.opm.gov/policy-data-oversight/pay-leave/salaries-wages/salary-tables/24Tables/html/GS.aspx'

outputs <- url %>% 
  read_html()

outputs %>% 
  html_table()



### Scraping Section Headers
# Example: https://www.r-bloggers.com
url <- 'https://www.r-bloggers.com'
item <- '.loop-title'

output <- url %>% 
  read_html()

output %>%
  html_elements(item)

output %>%
  html_elements(item) %>%
  html_text2()

titles <- output %>%
  html_elements(item) %>%
  html_text2()

output %>%
  html_elements(item) %>% 
  html_elements('a') %>% 
  html_attr("href") 

hyperlinks <- output %>%
  html_elements(item) %>% 
  html_elements('a') %>% 
  html_attr("href") 

file <- data.frame(titles, hyperlinks)
View(file)