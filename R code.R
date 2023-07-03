setwd("F:/github local/ANA515P-Exp-Practicum")
#import dataset
if (!require(readxl)) install.packages("readxl")
library(readxl)

data1 <- read_excel("file1.xlsx")
data2 <- read_excel("file2.xlsx")

# First, install and load the package
if (!require(lubridate)) install.packages("lubridate")
library(lubridate)

# Assuming Timestamp is character, convert it to datetime
data1$Timestamp <- ymd_hms(data1$Timestamp)

# Remove the time portion
data1$Timestamp <- floor_date(data1$Timestamp, "day")
data1$Timestamp <- as.Date(data1$Timestamp)
data2$Timestamp <- as.Date(data2$Timestamp)
head(data2)

# adjust the unit in Entering Grade Level
# First, load the dplyr package
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
# Now, mutate the column to replace the values
data1 <- data1 %>%
  mutate(`Entering Grade Level` = case_when(
    `Entering Grade Level` %in% c("K", "kinder", "kindergarten") ~ "0",
    `Entering Grade Level` == "first" ~ "1",
    `Entering Grade Level` == "second" ~ "2",
    TRUE ~ as.character(`Entering Grade Level`)
  ))

data2 <- data2 %>%
  mutate(`Entering Grade Level` = case_when(
    `Entering Grade Level` %in% c("K", "kinder", "kindergarten") ~ "0",
    `Entering Grade Level` == "first" ~ "1",
    `Entering Grade Level` == "second" ~ "2",
    TRUE ~ as.character(`Entering Grade Level`)
  ))

# Adjust the value Anderson in District
# Load the dplyr package
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
# Replace "Anderson" with "3"
data1$District[data1$District == "Anderson"] <- "3"
# Replace NA values with 0
data1$District[is.na(data1$District)] <- 0
data1$`OLSAT Verbal Percentile`[is.na(data1$`OLSAT Verbal Percentile`)] <- 0
data1$`NNAT Non Verbal Percentile`[is.na(data1$`NNAT Non Verbal Percentile`)] <- 0
# Convert the 'District' column to numeric
data1$District <- as.numeric(data1$District)
data1$`OLSAT Verbal Percentile` <- as.numeric(data1$`OLSAT Verbal Percentile`)
data1$`NNAT Non Verbal Percentile` <- as.numeric(data1$`NNAT Non Verbal Percentile`)

# Remove 13th and 14th columns
data2 <- data2[,-c(13, 14)]
# Combine the data
combined_data <- rbind(data1, data2)
# Write the combined data to a new file
write.csv(combined_data, "combined_file.csv")

# Filter the district is NA or overall Score is 0
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
filtered_data <- combined_data %>%
  filter(`Overall Score` == 0 | District == 0)
#use visuals (histogram, boxplot, graphs, …) to show the overall score under different districts
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
ggplot(filtered_data, aes(x = District, y = `Overall Score`)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of Overall Scores by District", x = "District", y = "Overall Score")

