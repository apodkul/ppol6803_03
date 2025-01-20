### PPOL6803-03
### Week 02: Lab Session

# This is a comment (note the hashtag)

## Manually Write Data (in multiple steps)
x <- c(1, 10, 2, 4,
       5, 2, 8, 9, 
       3, 4)
y <- c("Red", "Green", "Blue", "Blue", 
       "Yellow", "Orange", "Red", "Blue", 
       "Green", "Blue")
z <- 1:10
test_df <- data.frame(x, y, z)

test_df

test_df[]
test_df[1,]
test_df[,1]
test_df[,'y']
test_df[,4] #what happened?

test_df$x
test_df$x[1] 
test_df$a

test_fd 
test_DF
ls() 
rm(x)
ls()

## Load a .csv file
data <- read.csv('https://raw.githubusercontent.com/apodkul/ppol6803_03/main/Data/census_demo.csv')


## Explore the data file 
dim(data)
nrow(data)
ncol(data)

head(data)
str(data)
summary(data)

View(data) #note capitalization of the command

# quick question: what happened to test_df?

## Access variables
names(data)
colnames(data)
data$COUNTY

## Create a new variable 
data$Male_Percent <- data$TOT_MALE/data$TOT_POP
head(data$Male_Percent)
summary(data$Male_Percent)

data$Male_Percent <- data$Male_Percent*100
head(data$Male_Percent)
summary(data$Male_Percent)


## Explore a categorical variable
table(data$STNAME)
prop.table(table(data$STNAME))

## Explore a numeric variable
summary(data$TOT_POP)
mean(data$TOT_POP)
sd(data$TOT_POP)


## Working with Packages 
install.packages('haven') # How often do we have to install it?
library(haven)

## Export data 
write.csv(x = data, 'file_name.csv', 
          row.names = F)

write_dta(data, 'file_name.dta')

## Where did the files go? 
getwd() 
setwd('')

### R Projects

