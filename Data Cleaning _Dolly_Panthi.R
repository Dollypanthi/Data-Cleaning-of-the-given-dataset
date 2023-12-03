#####  Task 1 ########
##### Data Cleaning #####

#Getting the current working directory
getwd()
#Setting new working directory
setwd('C:/KMPlayer/archive (4)')
# Loading the readr package
library(readr)

#Import the dataset and specifying the column types for "Price" as numeric and "Rate" as integer, treating "NA" as missing values.
flipkart <- read_csv("flipkart_product.csv", col_types = cols(Price = col_number(), Rate = col_integer()), na = "NA")
View(flipkart)

# Removing Rows with missing values
sum(is.na(flipkart))
flipkart <- na.omit(flipkart)

# Checking the structure of the Summary Column
str(flipkart$Summary)

# Now we have to remove punctuation from summary column
library(dplyr)
library(stringi) # for advanced string manipulation 

# The stri_trans_general function from the stringi package is used here to convert non-Latin characters to their closest Latin equivalents.
flipkart <- flipkart %>%
  mutate(
    Summary = stri_trans_general(Summary, "Latin-ASCII"),
    ProductName = stri_trans_general(ProductName, "Latin-ASCII"))

# Removing the punctuation
flipkart <- flipkart %>%
  mutate(Summary = gsub("[[:punct:]]", "", Summary))
View(flipkart)

######## Removing characters with undefined names
str(flipkart)
library(dplyr)
library(stringr)

#This gsub function removes all characters that are not letters, digits, spaces, or forward slashes.
flipkart <- flipkart %>%
  mutate(
    ProductName = gsub("[^a-zA-Z0-9/ ]", "", ProductName))
View(flipkart)

# The gsub function replaces characters with undefined names in the specified columns with a space.
flipkart <- flipkart %>%
  mutate(
    Review = gsub('[^a-zA-Z0-9(/)]', ' ', Review),
    Summary = gsub('[^a-zA-Z0-9(/)]', ' ', Summary))

# The 'str_squish' function collapses multiple adjacent white spaces into a single space.
flipkart <- flipkart %>%
  mutate(
    Summary = str_squish(Summary),
    Review = str_squish(Review))

# Converting the summary and review columns into lower case.
flipkart <- flipkart %>%
  mutate(
    Summary = tolower(Summary),
    Review = tolower(Review))

View(flipkart)

# Saving the data frame flipkart to a CSV file
write.csv(flipkart, "clean_flipkart_Products.csv", row.names = FALSE)
View(flipkart)



# Assuming you have a dataset named 'flipkart_data.csv' with a column 'SentimentCategory'
# Install and load necessary packages if not already installed
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

# Load the required libraries
library(tidyverse)
library(psych)

summary(flipkart)

   # Calculate the counts of each unique value in 'Rate'
        star_counts <- table(flipart$Rate)
        
        # Create a bar plot
        barplot(star_counts,
                main = 'Count of Reviews by Stars',
                xlab = 'Review Stars',
                ylab = 'Count',
                col = 'skyblue',  # You can customize the color
                ylim = c(0, max(star_counts) + 5),  # Adjust ylim for better visualization
                beside = TRUE  # Display bars beside each other
        )
        
        # Set x-axis label
        axis(1, at = seq_along(star_counts), labels = names(star_counts))
        
        # Display the plot
        dev.off()
        
      
install.packages("tm")
install.packages("Snowballc")        
installed.packages("wordcloud2")
install.packages("RColorBrewer")
install.packages("syuzhet")
install.packages("sentimentr")
install.packages("ggplot2")
###Load

library("tm")
library("SnowballC")
library('wordcloud2')
library("RColorBrewer")
library("syuzhet")
library("sentimentr")
library("ggplot2")
# Assuming 'df' is your data frame with columns 'ReviewSentiment' and 'SummarySentiment'
# Make sure the column names match the actual column names in your data frame

# Check if columns exist
if ('ReviewSentiment' %in% colnames(flipkart) && 'SummarySentiment' %in% colnames(flipkart)) {
  
  # Check for missing values
  if (any(is.na(flipkart$ReviewSentiment)) || any(is.na(flipkart$SummarySentiment))) {
    # Handle missing values if necessary
    print("Warning: Missing values detected.")
  } else {
    # Combine sentiments
    flipkart$CombinedSentiment <- (flipkart$ReviewSentiment + flipkart$SummarySentiment) / 2
  }
  
} else {
  print("Error: Columns 'ReviewSentiment' and/or 'SummarySentiment' not found.")
}

# Assuming 'df' is your data frame with columns 'ReviewSentiment' and 'SummarySentiment'
flipkart$CombinedSentiment = (flipkart$ReviewSentiment + flipkart$SummarySentiment) / 2
# Assuming 'df' is your data frame
print(head(flipkart))
# Assuming 'df' is your data frame
library(dplyr)

# Copy 'CombinedSentiment' to 'SentimentScore'
flipkart$SentimentScore <- flipkart$CombinedSentiment

# Print the first few rows
print(head(flipkart))

# Drop columns 'ReviewSentiment', 'SummarySentiment', 'CombinedSentiment'
flipkart <- select(flipkart, -c(ReviewSentiment, SummarySentiment, CombinedSentiment))

# Print the first few rows again
print(head(flipkart))
head(flipkart)


# Assuming 'df' is your data frame
# Assuming 'SentimentCategory' is a factor (categorical) column in your DataFrame
# You can convert it to a factor if it's not already
flipkart$SentimentCategory <- as.factor(flipkart$SentimentCategory)

# Get the counts of each sentiment category
sentiment_counts <- table(flipkart$SentimentCategory)

# Plotting the bar chart
barplot(sentiment_counts, col=c('red', 'grey', 'green'),
        main='Distribution of Sentiment Categories',
        xlab='Sentiment Category',
        ylab='Number of Sentiments')
# Assuming 'df' is your data frame
# Assuming 'X' and 'Y' are columns in your DataFrame that you want to plot
# Replace 'X' and 'Y' with the actual column names

plot(flipkart$X, flipkart$Y, col='blue', pch=16, main='Scatter Plot', xlab='X-axis Label', ylab='Y-axis Label')
plot(flipkart)

# Assuming 'df' is your data frame
# Assuming 'Price' and 'Rate' are columns in your DataFrame that you want to plot
# Replace 'Price' and 'Rate' with the actual column names

# Create a scatter plot
plot(flipkart$Price, flipkart$Rate, col = 'blue', pch = 16, main = 'Ratings vs. Prices', xlab = 'Price', ylab = 'Rating')

# Adjust plot parameters as needed
# Assuming 'df' is your data frame
# Assuming 'Price' and 'Rate' are columns in your DataFrame that you want to plot
# Replace 'Price' and 'Rate' with the actual column names

# Install and load the required library
install.packages("ggplot2")
library(ggplot2)

# Create a scatter plot using ggplot2
ggplot(flipkart, aes(x = Price, y = Rate)) +
  geom_point(color = 'blue', size = 3, alpha = 0.5) +
  labs(title = 'Ratings vs. Prices', x = 'Price', y = 'Rating')
besides = TRUE
plot


# Assuming 'df' is your data frame
# Assuming 'Rate' is the column you want to convert to numeric
# Replace 'Rate' with the actual column name

# Convert non-numeric values to NA
flipkart$Rate <- as.numeric(flipkart$Rate)

# Categorize values based on their rating
flipkart$RatingCategory <- cut(flipkart$Rate,
                         breaks = c(-Inf, 1, 2, 3, 4, 5, Inf),
                         labels = c('Invalid Rating', 'Very Poor', 'Poor', 'Average', 'Good', 'Excellent'))

# Assuming 'df' is your data frame
# Assuming 'RatingCategory' is a factor (categorical) column in your DataFrame
# You can convert it to a factor if it's not already
flipkart$RatingCategory <- as.factor(flipkart$RatingCategory)

# Get the counts of each rating category
rating_counts <- table(flipkart$RatingCategory)

# Create a pie chart
pie(rating_counts,
    labels = levels(flipkart$RatingCategory),
    main = 'Distribution of Product Ratings',
    col = c('red', 'orange', 'yellow', 'green', 'blue', 'purple'),
    clockwise = TRUE,
    init.angle = 140,
    border = 'white')

# Add a legend
legend('topright', levels(df$RatingCategory), fill = c('red', 'orange', 'yellow', 'green', 'blue', 'purple'))

# Ensure an equal aspect ratio
asp <- diff(par('usr')[1:2])/diff(par('usr')[3:4])
asp <- asp * (diff(par('pin')[1:2])/diff(par('pin')[3:4]))
asp <- asp * par('cra')[2]/par('cra')[1]
par(asp = asp)

# Display the pie chart
pie()
# Assuming 'df' is your data frame
# Assuming 'Price' and 'Rate' are columns in your DataFrame that you want to plot
# Replace 'Price' and 'Rate' with the actual column names

# Create a scatter plot
plot(flipkart$Price, flipkart$Rate, col='blue', pch=16, main='Ratings vs. Prices', xlab='Price', ylab='Rating')

# Adjust plot parameters as needed
