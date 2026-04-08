
# Install naniar package
install.packages("naniar")

#Load data

data <- read.csv("SuperMarketD.csv")
View(data)


# Then load it




#view missing data task 1-----------------------------------------------------------------------------

#view missing data task 1


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
#dd

if ("Gross_Income_Group" %in% colnames(dataset_converted)) {
  cat("\nGross_Income_Group:\n")
  print(table(dataset_converted$Gross_Income_Group))
}




## Task 5: Normalization of Continuous Attributes

# Apply Min-Max Normalization to the numeric columns in the cleaned dataset.
# This scales each numeric feature to the range [0, 1].

dataset_normalized <- dataset_converted

cat("\n--- Task 5: Normalization ---\n")

# --- Min-Max Normalization ---
min_max_norm <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

cat("Applying Min-Max Normalization to 'Unit.price'...\n")
dataset_normalized$Unit.price_Normalized <- min_max_norm(dataset_normalized$Unit.price)

# Verification
cat("\nSummary of Normalized Unit.price (Min-Max):\n")
print(summary(dataset_normalized$Unit.price_Normalized))

# Sample comparison
cat("\nSample of normalized results:\n")
print(head(dataset_normalized[, c("Unit.price", "Unit.price_Normalized")]))



## Task 6: Handle Duplicate Rows

# Finding and removing duplicate observations from the dataset.

cat("\n--- Task 6: Handle Duplicate Rows ---\n")

# Count number of duplicate rows
duplicate_count <- sum(duplicated(dataset_normalized))
cat("Number of duplicate rows found:", duplicate_count, "\n")

# Show the duplicate rows (if any)
if (duplicate_count > 0) {
  cat("Duplicate rows detected. Removing duplicates...\n")
  # Actually remove the duplicates
  dataset_final <- dataset_normalized[!duplicated(dataset_normalized), ]
} else {
  cat("No duplicate rows found. Keeping dataset as is.\n")
  dataset_final <- dataset_normalized
}

# Final Verification
cat("Original Row Count:", nrow(dataset_normalized), "\n")
cat("Final Row Count (After Removing Duplicates):", nrow(dataset_final), "\n")


## Task 7: Data Filtering Methods-----------------------------------------------------


library(dplyr)

cat("\n--- Customer Purchase Behavior Analysis (Extended) ---\n")

# --- Step 1: Convert to factor (optional but clean) ---
dataset_final$Gender <- as.factor(dataset_final$Gender)
dataset_final$Product.line <- as.factor(dataset_final$Product.line)
dataset_final$Quantity_Group <- as.factor(dataset_final$Quantity_Group)
dataset_final$Payment <- as.factor(dataset_final$Payment)
dataset_final$Unit_Price_Group <- as.factor(dataset_final$Unit_Price_Group)
dataset_final$Gross_Income_Group <- as.factor(dataset_final$Gross_Income_Group)


gender_product <- dataset_final %>%
  group_by(Gender, Product.line) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(Gender, desc(count))

cat("\nMost bought Product line per Gender:\n")
print(gender_product)


cat("\n--- Total Gross Income Analysis by Product Line ---\n")

# --- Step 1: Ensure gross.income is numeric ---
dataset_final$gross.income <- as.numeric(as.character(dataset_final$gross.income))

# --- Step 2: Calculate total gross income per Product.line ---
income_by_product <- dataset_final %>%
  group_by(Product.line) %>%
  summarise(Total_Gross_Income = sum(gross.income, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Total_Gross_Income))

# --- Step 3: Print sorted result ---
cat("\nProduct lines ranked by Total Gross Income:\n")
print(income_by_product)








cat("\n--- Task 8: Dataset Balancing (Gross_Income_Group) ---\n")

# Target attribute for balancing
target_attr <- "Gross_Income_Group"

# Original distribution
cat("Original distribution of 'Gross_Income_Group':\n")
class_counts <- table(dataset_final[[target_attr]])
print(class_counts)

# -------------------------------
# Method 1: Under-sampling the Majority Classes
# -------------------------------
cat("\nMethod 1: Under-sampling\n")

# Smallest class size
min_class_size <- min(class_counts[class_counts > 0])

# Sample each class to min_class_size
dataset_undersampled <- do.call(rbind, lapply(
  split(dataset_final, dataset_final[[target_attr]]),
  function(df) df[sample(nrow(df), min_class_size), ]
))

cat("Distribution after under-sampling:\n")
print(table(dataset_undersampled[[target_attr]]))

# -------------------------------
# Method 2: Over-sampling the Minority Classes
# -------------------------------
cat("\nMethod 2: Over-sampling\n")

# Largest class size
max_class_size <- max(class_counts)

# Sample each class to max_class_size (with replacement)
dataset_oversampled <- do.call(rbind, lapply(
  split(dataset_final, dataset_final[[target_attr]]),
  function(df) df[sample(nrow(df), max_class_size, replace = TRUE), ]
))

cat("Distribution after over-sampling:\n")
print(table(dataset_oversampled[[target_attr]]))



## Task 9: Split the Dataset (Training and Testing)

# Splitting data is essential for validating models.
# We use one part to train the model and the other to test its performance.

cat("\n--- Task 9: Training and Testing Split ---\n")

# Set seed for reproducibility (ensures we get the same split every time)
set.seed(123)

# Define the split percentage (e.g., 70% training, 30% testing)
train_percentage <- 0.7
sample_size <- floor(train_percentage * nrow(dataset_final))

# Randomly select indices for the training set
train_indices <- sample(seq_len(nrow(dataset_final)), size = sample_size)

# Create the Training and Testing datasets
train_set <- dataset_final[train_indices, ]
test_set <- dataset_final[-train_indices, ]

# Verification
cat("Total observations in original dataset:", nrow(dataset_final), "\n")
cat("Observations in Training Set (70%):", nrow(train_set), "\n")
cat("Observations in Testing Set (30%):", nrow(test_set), "\n")

# Verification of split integrity (no overlap)
cat("Overlap between sets:", length(intersect(rownames(train_set), rownames(test_set))), "\n")




## --- Task 10: Descriptive Statistics and Interpretation ---

cat("\n--- Task 10: Descriptive Statistics ---\n")

# --- 1. Overall Summary of Unit.price ---
cat("Overall Summary of Unit.price (Full Dataset):\n")
print(summary(dataset_final$Unit.price))

cat("Standard Deviation of Unit.price:",
    sd(dataset_final$Unit.price, na.rm = TRUE), "\n")

cat("Variance of Unit.price:",
    var(dataset_final$Unit.price, na.rm = TRUE), "\n")


# --- 2. Descriptive Statistics Grouped by Gross_Income_Group ---

cat("\nMean Unit.price by Gross_Income_Group:\n")
mean_by_group <- aggregate(Unit.price ~ Gross_Income_Group,
                           data = dataset_final,
                           FUN = mean)
print(mean_by_group)

cat("\nMedian Unit.price by Gross_Income_Group:\n")
median_by_group <- aggregate(Unit.price ~ Gross_Income_Group,
                             data = dataset_final,
                             FUN = median)
print(median_by_group)

cat("\nStandard Deviation by Gross_Income_Group:\n")
sd_by_group <- aggregate(Unit.price ~ Gross_Income_Group,
                         data = dataset_final,
                         FUN = sd)
print(sd_by_group)


# --- 3. Interpretation of Results ---

cat("\n--- Result Interpretation ---\n")

# Clean labels (safe matching)
mean_by_group$CleanLabel <- tolower(trimws(mean_by_group$Gross_Income_Group))

val_low    <- mean_by_group$Unit.price[mean_by_group$CleanLabel == "low"]
val_medium <- mean_by_group$Unit.price[mean_by_group$CleanLabel == "medium"]
val_high   <- mean_by_group$Unit.price[mean_by_group$CleanLabel == "high"]

cat(
  "Interpretation: The average Unit.price for 'High' income group is ",
  round(as.numeric(val_high[1]), 2),
  ", while for 'Medium' it is ",
  round(as.numeric(val_medium[1]), 2),
  ", and for 'Low' it is ",
  round(as.numeric(val_low[1]), 2),
  ".\n"
)





#-----
# -------------------------------
# Task 11: Compare Average Values Across Categories
# -------------------------------

# This task compares a numerical variable across categories of categorical variables

cat("\n--- Task 11: Comparison Across Categories ---\n")

# --------------------------------
# Relation 1: Sales by Gender
# --------------------------------
compare_sales <- aggregate(Sales ~ Gender, 
                           data = dataset_final, 
                           FUN = mean)

# Extract Male & Female values
mean_male <- compare_sales$Sales[compare_sales$Gender == "Male"]
mean_female <- compare_sales$Sales[compare_sales$Gender == "Female"]

cat("\nRelation 1: Average Sales by Gender\n")
cat("- Male Customers:", round(mean_male, 2), "\n")
cat("- Female Customers:", round(mean_female, 2), "\n")

# --------------------------------
# Relation 2: Quantity by Product Line
# --------------------------------
cat("\nRelation 2: Quantity by Product Line\n")

quantity_by_product <- aggregate(Quantity ~ Product.line, 
                                 data = dataset_final, 
                                 FUN = mean)

print(quantity_by_product)

# --------------------------------
# Relation 3: Gross Income by Payment Method
# --------------------------------
cat("\nRelation 3: Gross Income by Payment Method\n")

income_by_payment <- aggregate(gross.income ~ Payment, 
                               data = dataset_final, 
                               FUN = mean)

print(income_by_payment)

# --------------------------------
# Combined Interpretation
# --------------------------------
cat("\n--- Result Interpretation ---\n")

# Gender comparison
diff_val <- abs(mean_male - mean_female)

cat("1. Gender Comparison:\n")
cat("   The average sales difference between Male and Female customers is", 
    round(diff_val, 2), ".\n")

if(mean_male > mean_female){
  cat("   Male customers spend more on average.\n")
} else if(mean_female > mean_male){
  cat("   Female customers spend more on average.\n")
} else {
  cat("   Both groups spend equally on average.\n")
}

# Product line interpretation
cat("\n2. Product Line & Quantity:\n")
cat("   The average quantity sold varies across product lines, indicating differences in product demand.\n")

# Payment method interpretation
val_cash_income <- income_by_payment$gross.income[income_by_payment$Payment == "Cash"]
val_card_income <- income_by_payment$gross.income[income_by_payment$Payment == "Credit card"]

cat("\n3. Payment Method & Income:\n")

if(length(val_cash_income) > 0 & length(val_card_income) > 0){
  cat("   Cash payments generate an average income of", round(val_cash_income[1], 2),
      "while Credit card payments generate", round(val_card_income[1], 2), ".\n")
} else {
  cat("   Income varies across different payment methods.\n")
}



#---------------


# -------------------------------
# Task 12: Compare Variability Across Categories
# -------------------------------

# This task compares variability (IQR, SD, Variance, Range) 
# of a numerical variable across categories

cat("\n--- Task 12: Variability Across Categories ---\n")

# --------------------------------
# Variability of Sales by Product Line
# --------------------------------
cat("Dispersion Metrics for Sales by Product Line:\n")

# 1. Standard Deviation (SD)
sd_comp <- aggregate(Sales ~ Product.line, data = dataset_final, FUN = sd)
names(sd_comp)[2] <- "SD"

# 2. Variance
var_comp <- aggregate(Sales ~ Product.line, data = dataset_final, FUN = var)
names(var_comp)[2] <- "Variance"

# 3. Inter-quartile Range (IQR)
iqr_comp <- aggregate(Sales ~ Product.line, data = dataset_final, FUN = IQR)
names(iqr_comp)[2] <- "IQR"

# 4. Range (Max - Min)
range_comp <- aggregate(Sales ~ Product.line, data = dataset_final, 
                        FUN = function(x) diff(range(x)))
names(range_comp)[2] <- "Range"

# Merge all metrics into one summary table
dispersion_summary <- merge(merge(merge(sd_comp, var_comp), iqr_comp), range_comp)
print(dispersion_summary)

# --------------------------------
# Relation 2: Sales Variability by Gender
# --------------------------------
cat("\nRelation 2: Sales Variability by Gender:\n")

# Compute SD and IQR and rename columns first
sd_gender <- aggregate(Sales ~ Gender, data = dataset_final, FUN = sd)
names(sd_gender)[2] <- "SD"

iqr_gender <- aggregate(Sales ~ Gender, data = dataset_final, FUN = IQR)
names(iqr_gender)[2] <- "IQR"

# Merge properly by Gender
gender_variability <- merge(sd_gender, iqr_gender, by = "Gender")

print(gender_variability)

# --------------------------------
# Result Interpretation
# --------------------------------
cat("\n--- Result Interpretation ---\n")

# Most stable product line (lowest SD)
cat("1. Product Line Stability: The most stable sales are observed in the '", 
    as.character(dispersion_summary[which.min(dispersion_summary$SD), 1]), "' product line.\n")

# Gender stability
cat("2. Gender Stability: ", 
    as.character(gender_variability[which.min(gender_variability$SD), 1]), 
    " customers show more consistent purchasing behavior (Lower SD: ", 
    round(min(gender_variability$SD), 2), ") compared to the other group.\n")





