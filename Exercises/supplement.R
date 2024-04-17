# Lab 13

################################
### Additional Data Viz.
################################
library(dplyr)


heart_data <- read.csv("https://raw.githubusercontent.com/apodkul/ppol6803_03/main/Data/processed_cleveland.csv")

### ggvis
library(ggvis)
slider <- input_slider(10, 1000, label = "size")
heart_data %>% 
  ggvis(~age, ~chol, fill = ~heart) %>%
  layer_points(size := input_slider(1, 50, 1, label = "Size of Point")) %>%
  layer_smooths() 

  
### Highcharter
library(highcharter)
hchart(
  heart_data,
  "scatter",
  hcaes(x = age, y = chol, group = heart)
)

hchart(heart_data$chol, 
       name = "Cholesterol", color = "#800000") 

### Esquisse
library(esquisse)
esquisser(data = heart_data)

################################
### Another RSelenium Example (Follow along)
################################
#if using docker, spin up docker container
library(RSelenium)
url <- 'https://abcnews.go.com/elections'

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,  
  browserName = "firefox"
)

# Start the remote driver
remDr$open()

remDr$navigate(url)

remDr$screenshot(display = TRUE) #see what we're doing

search_box <- remDr$findElement(using = "css", value = ".search__input")
search_box$sendKeysToElement(list('Nikki Haley'))
remDr$screenshot(display = TRUE) #see what we're doing
search_box$sendKeysToElement(list(key = 'enter'))
remDr$screenshot(display = TRUE) #see what we're doing

page_source <- remDr$getPageSource()
page_source[[1]] %>%
  rvest::read_html() %>%
  rvest::html_elements(css = '.AnchorLink') 
