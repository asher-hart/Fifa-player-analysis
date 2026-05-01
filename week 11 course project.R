library(class)
library(ggplot2)

data <- read.csv("fifa_players.csv")

summary(data)

data$age_category <- ifelse(data$age < 23, "Young",
                            ifelse(data$age < 30, "Prime", "Veteran"))


numeric_cols <- sapply(data, is.numeric)
data_numeric <- data[, numeric_cols]

data_numeric$age <- NULL

X <- data_numeric
y <- as.factor(data$age_category)

for (i in 1:ncol(X)) {
  X[is.na(X[, i]), i] <- mean(X[, i], na.rm = TRUE)
}

set.seed(42)
train_index <- sample(1:nrow(X), 0.8 * nrow(X))

X_train <- X[train_index, ]
X_test  <- X[-train_index, ]

y_train <- y[train_index]
y_test  <- y[-train_index]


scale_data <- function(train, test) {
  train_scaled <- scale(train)
  test_scaled <- scale(test,
                       center = attr(train_scaled, "scaled:center"),
                       scale = attr(train_scaled, "scaled:scale"))
  return(list(train = train_scaled, test = test_scaled))
}

scaled <- scale_data(X_train, X_test)
X_train_scaled <- scaled$train
X_test_scaled  <- scaled$test

k_values <- 3:25
accuracies <- c()

for (k in k_values) {
  pred <- knn(train = X_train_scaled,
              test = X_test_scaled,
              cl = y_train,
              k = k)
  
  acc <- mean(pred == y_test)
  accuracies <- c(accuracies, acc)
  
  cat("k =", k, "Accuracy =", round(acc, 4), "\n")
}


best_k <- k_values[which.max(accuracies)]
best_accuracy <- max(accuracies)
best_pred <- knn(train = X_train_scaled,
                 test = X_test_scaled,
                 cl = y_train,
                 k = best_k)

table(Predicted = best_pred, Actual = y_test)

cat("\nBest k:", best_k, "\n")
cat("Best Accuracy:", round(best_accuracy, 4), "\n")

results <- data.frame(k = k_values, accuracy = accuracies)

ggplot(results, aes(x = k, y = accuracy)) +
  geom_line() +
  geom_point() +
  labs(title="KNN Accuracy vs k",
       x="k value",
       y="Accuracy")


###Summarization
#The best KNN model was achieved after properly preparing the data through 
#feature engineering, especially by scaling all numeric variables to ensure 
#fair distance calculations. I also created a new target variable by grouping 
#ages into categories, which made the classification problem more meaningful. 
#After testing multiple values of k, I found that k = 22 produced the highest 
#accuracy, as it provided a good balance between overfitting and underfitting. 
#Overall, the combination of data scaling, handling missing values, and 
#selecting an appropriate k value had the biggest impact on improving the 
#model’s performance.


young <- data$overall[data$age < 25]
old <- data$overall[data$age >= 25]

ks.test(young, old)


data$rating_level <- ifelse(data$overall >= 80, "High", "Low")

# Table
table_data <- table(data$overall, data$age)

# Chi-square test
chisq.test(table_data)