
# Install naniar package
install.packages("naniar")

#Load data

data <- read.csv("SuperMarketD.csv")
View(data)

colSums(is.na(data))

# Then load it


#view missing data task 1-----------------------------------------------------------------------------
data[data == ""] <- NA
data[data == " "] <- NA
sum(is.na(data))
colSums(is.na(data))

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


#task 2 graph-----------------------------------------------------------------------------

library(naniar)

# Simple missing data visualization
gg_miss_var(data)          # Bar plot per variable
gg_miss_upset(data)        # UpSet plot for missing combinations
vis_miss(data)             # Heatmap-style visualization


 



#Task 3: Detect and Handle Outliers-----------------------------------------------------------------------------





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




max(data$Unit.price)
min(data$Unit.price)
max(data$gross.income)
min(data$gross.income)

max(data$Un)
min(data$Unit.price)
## --- Task 4: Numeric to Categorical (Binning for Sales Data) ---

# Create a copy of the dataset
dataset_converted <- dataset_outlier_handled  

cat("\n--- Task 4: Binning for Unit.price, Quantity, gross.income ---\n")

# --- Step 1: Safe numeric conversion ---
numeric_cols <- c("Unit.price", "Quantity", "gross.income")

for (col in numeric_cols) {
  if (col %in% colnames(dataset_converted)) {
    dataset_converted[[col]] <- as.numeric(as.character(dataset_converted[[col]]))
    cat("Converted column", col, "to numeric.\n")
  } else {
    cat("Warning: Column", col, "not found in dataset!\n")
  }
}

# --- Step 2: Binning ---

# Unit.price (10-146)
if ("Unit.price" %in% colnames(dataset_converted)) {
  dataset_converted$Unit_Price_Group <- cut(dataset_converted$Unit.price,
                                            breaks = c(0, 50, 100, 146),
                                            labels = c("Low", "Medium", "High"),
                                            include.lowest = TRUE)
}

# Quantity (1-10)
if ("Quantity" %in% colnames(dataset_converted)) {
  dataset_converted$Quantity_Group <- cut(dataset_converted$Quantity,
                                          breaks = c(0, 3, 7, 10),
                                          labels = c("Small", "Medium", "Large"),
                                          include.lowest = TRUE)
}

# gross.income (0.5-50)
if ("gross.income" %in% colnames(dataset_converted)) {
  dataset_converted$Gross_Income_Group <- cut(dataset_converted$gross.income,
                                              breaks = c(0, 15, 35, 50),
                                              labels = c("Low", "Medium", "High"),
                                              include.lowest = TRUE)
}

# --- Step 3: Verification ---
cols_to_print <- c()
if ("Unit.price" %in% colnames(dataset_converted)) cols_to_print <- c(cols_to_print, "Unit.price", "Unit_Price_Group")
if ("Quantity" %in% colnames(dataset_converted)) cols_to_print <- c(cols_to_print, "Quantity", "Quantity_Group")
if ("gross.income" %in% colnames(dataset_converted)) cols_to_print <- c(cols_to_print, "gross.income", "Gross_Income_Group")

cat("\nSample of converted bins:\n")
print(head(dataset_converted[, cols_to_print]))

cat("\nValue counts:\n")
if ("Unit_Price_Group" %in% colnames(dataset_converted)) {
  cat("\nUnit_Price_Group:\n")
  print(table(dataset_converted$Unit_Price_Group))
}

if ("Quantity_Group" %in% colnames(dataset_converted)) {
  cat("\nQuantity_Group:\n")
  print(table(dataset_converted$Quantity_Group))
}

if ("Gross_Income_Group" %in% colnames(dataset_converted)) {
  cat("\nGross_Income_Group:\n")
  print(table(dataset_converted$Gross_Income_Group))
}