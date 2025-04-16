## Lab 12
## In-Class Coding Notes 

##########################################
#### Plotly
##########################################
life_expect <- read.csv("https://raw.githubusercontent.com/apodkul/ppol6803_03/refs/heads/main/Data/life_expect.csv")

p <- ggplot(life_expect, 
            aes(x = log(GDP_per_capita), 
                y = life_expectancy, 
                color = Continent, 
                text = paste(Continent, ": ", Entity, 
                             '\n', life_expectancy))) + 
  geom_point() + 
  theme_minimal()

ggplotly(p, tooltip = 'text') %>%
  config(displayModeBar = TRUE,
         modeBarButtonsToRemove = 
           c("zoomIn2d", "zoomOut2d"),
         displaylogo = FALSE)

##########################################
#### RSelenium (follow along)
##########################################
library(RSelenium)
library(rvest)

url <- "https://www.arlingtonva.us/Government/Departments/Police-Department/Crime-Statistics"

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox"
)
remDr$open()

remDr$navigate(url)
remDr$screenshot(display =  T) 
  
#find search box
search_box <- remDr$findElement(using = "css selector", "#ctl07_txtSearch")
search_box$sendKeysToElement(list("crime data", key = "enter"))
remDr$screenshot(display =  T) 

#scrape page
page <- read_html(remDr$getPageSource()[[1]])

# get links
results <- page %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  unique()

##########################################
#### Assorted R Packages
##########################################

##########################################
### googlesheets4
#install.packages("googlesheets4")
library(googlesheets4)

#Easiest way to authenticate
gs4_auth()

#Read
example_ss_id <- '1XTMcmuu0agQVC7IDN8UQvw1KfDPI_MMuY9vmFCyoOTg'
example <- read_sheet(ss = example_ss_id, 
                      sheet = 1)

#Write
cc_tweets <- read.csv('https://github.com/apodkul/ppol670_01/raw/main/Data/Climate_tweets.csv')

write_sheet(ss = example_ss_id, 
            data = cc_tweets,
            sheet = 'ClimateTweets')


##########################################
### inspectdf
#install.packages("inspectdf")
library(inspectdf)
admissions <- read.csv("https://raw.githubusercontent.com/apodkul/ppol6803_03/refs/heads/main/Data/Admissions_missing.csv")

inspect_na(admissions)

inspect_types(admissions)

inspect_num(admissions)

inspect_cor(admissions)


##########################################
### janitor
#install.packages("janitor")
library(janitor)
admissions <- read.csv("https://raw.githubusercontent.com/apodkul/ppol6803_03/refs/heads/main/Data/Admissions_missing.csv")

clean_names(admissions)

tabyl(admissions$Research)

admissions$x <- 1
remove_constant(admissions)

admissions$x <- NA
admissions$y <- NA
admissions$z <- NA
remove_empty(admissions, which = 'cols')

##########################################
### esquisse
#install.packages("esquisse")
library(esquisse)
life_expect <- read.csv("https://raw.githubusercontent.com/apodkul/ppol6803_03/refs/heads/main/Data/life_expect.csv")

esquisser(data = life_expect)


##########################################
### cronR
#install.packages("cronR")
library(cronR)

#go to add-ins

