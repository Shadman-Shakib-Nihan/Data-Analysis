
# Install naniar package
install.packages("naniar")

#Load data

data <- read.csv("SuperMarketD.csv")
View(data)

colSums(is.na(data))

# Then load it


#view missing data task 1
data[data == ""] <- NA
data[data == " "] <- NA
sum(is.na(data))
colSums(is.na(data))

library(naniar)

# Simple missing data visualization
gg_miss_var(data)          # Bar plot per variable
gg_miss_upset(data)        # UpSet plot for missing combinations
vis_miss(data)             # Heatmap-style visualization


 #Replace by Most Frequent / Average Value ---
  # For numeric columns, missing values are replaced by the Mean (Average).
  # For categorical columns, missing values are replaced by the Mode (Most Frequent).
  dataset_replaced <- data

# Helper function to calculate the mode (most frequent value)
get_mode <- function(v) {
  v <- v[!is.na(v)]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (col in names(dataset_replaced)) {
  if (any(is.na(dataset_replaced[[col]]))) {
    if (is.numeric(dataset_replaced[[col]])) {
      # Replace missing numeric values with the Rounded Average
      dataset_replaced[[col]][is.na(dataset_replaced[[col]])] <- round(mean(dataset_replaced[[col]], na.rm = TRUE))
    } else {
      # Replace missing categorical/factor values with the Most Frequent (Mode)
      dataset_replaced[[col]][is.na(dataset_replaced[[col]])] <- get_mode(dataset_replaced[[col]])
    }
  }
}

cat("\nMethod 2: Replace by Most Frequent / Average Value\n")
cat("Remaining missing values after replacement:\n")
print(colSums(is.na(dataset_replaced)))



#Task 2: Detect and Handle Outliers


max(data$Unit.price)
min(data$Unit.price)
sum(data$Unit.price >= 99.97, na.rm = TRUE)


# In this dataset, 'Age' is the primary numeric column where outliers may exist.
# We will use the Interquartile Range (IQR) method to detect and handle these outliers.

dataset_outlier_handled <- dataset_replaced

# Function to handle outliers using IQR capping
handle_outliers_iqr <- function(data_vec) {
  Q1 <- quantile(data_vec, 0.25, na.rm = TRUE)
  Q3 <- quantile(data_vec, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  
  # Identify outliers
  outliers <- data_vec < lower_bound | data_vec > upper_bound
  cat("Outliers detected:", sum(outliers), "\n")
  cat("Lower Bound:", lower_bound, " | Upper Bound:", upper_bound, "\n")
  
  # Cap (Winsorize) the outliers to the bounds
  data_vec[data_vec < lower_bound] <- lower_bound
  data_vec[data_vec > upper_bound] <- upper_bound
  
  return(data_vec)
}

cat("\n--- Task 2: Outlier Detection and Handling for 'Age' ---\n")
# Detect and handle outliers in Age
dataset_outlier_handled$Unit.price <- handle_outliers_iqr(dataset_outlier_handled$Unit.price)

cat("Outliers handled successfully in the 'Unit.price' column.\n")

# Summary comparison
cat("\nSummary of Age BEFORE outlier handling:\n")
print(summary(dataset_replaced$Unit.price))
cat("\nSummary of Age AFTER outlier handling:\n")
print(summary(dataset_outlier_handled$Unit.price))     