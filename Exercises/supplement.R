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
