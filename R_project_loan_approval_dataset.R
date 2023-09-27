df<- read.csv("C:/Users/Rosemary/Downloads/archive (1)/loan_approval_dataset.csv")
View(df)
dimensions <- dim(df)
num_rows <- dimensions[1]
num_cols <- dimensions[2]
cat(num_rows)
cat(num_cols)

#checking missing values
missing_values<-is.na(df)
colSums(is.na(df))
missing_values

#balance of the target variable
class_counts <- table(df$loan_status)
print(class_counts)

barplot(class_counts, main="Class Distribution", xlab="Class", ylab="Count", col="skyblue")

#unique values of the column number of dependents
unique_values <- lapply(df, unique)
unique_column1 <- unique_values$ no_of_dependents
unique_column1

bin_breaks <- c(0, 6, 12, 18, Inf)
bin_labels <- c(">6 months", "7-12 months", "13-18months", "more than 1.5 yrs")

# Create a new column with the bin labels
df$loan_term <- cut(df$loan_term, breaks = bin_breaks, labels = bin_labels, right = FALSE)

# Print the resulting dataframe
print(df$loan_term)

duplicates <- sum(duplicated(df))

# Print the rows that are duplicates
print(duplicates)

# removing the column load_id
df <- df[, !colnames(df) %in% "loan_id"]
df
column_names <- colnames(df)
column_names


sapply(df, class)
df$loan_status_new <- as.numeric(df$loan_status)
numeric_df <- df[sapply(df, is.numeric)]
cor_matrix <- cor(numeric_df)

print(cor_matrix)

##VISUALISATION

library(ggplot2)

ggplot(df, aes(x = loan_amount, y = income_annum)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x') +
  labs(x = "loan amount", y = "income annum")


ggplot(df, aes(x = loan_amount, y = income_annum, color=education)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x') +
  labs(x = "loan amount", y = "income annum")

plot <- density(df$cibil_score)

# Plot the density
plot(plot,
     main = "Cibil Score Distribution",
     xlab = "Cibil Score",
     col = "blue")



ggplot(data = df, aes(x = education, fill = loan_status)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Education vs. Loan Approval",
    x = "Education Status",
    y = "Count"
  ) +
  scale_fill_manual(values = c("1" = "green", "0" = "red"))

#testing and training 
library(caTools)
set.seed(1)
split=sample.split(df,SplitRatio = 0.75)
View(df)
train_set<-subset(df,split==TRUE)
test_set<-subset(df,split==FALSE)
View(train_set)
View(test_set)

library(caret)
library(glmnet)

library(party)

tree_model <- ctree(loan_status~income_annum+loan_amount+cibil_score+ bank_asset_value, data = train_set)
summary(logistic_model)

predicted_probs <- predict(tree_model, newdata = test_set, type = "response")
mat<-table(test_set$loan_status,predicted_probs)
class<-ifelse(predicted_probs>0.5,1,0)
mean(test_set$loan_status==class)
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0) 

library(randomForest)

set.seed(120)
rf_model<-randomForest(x=train_set[-12],y=train_set$loan_status,ntree=500)
rf_model

predicted_prob <- predict(tree_model, newdata = test_set, type = "response")
mat<-table(test_set$loan_status,predicted_prob)
cls<-ifelse(predicted_prob>0.5,1,0)
mean(test_set$loan_status==cls)
predicted_classes <- ifelse(predicted_prob > 0.5, 1, 0) 



predictions <- predict(tree_model, newdata = test_set, type = "response")
print(predictions)

predictions1 <- predict(rf_model, newdata = test_set, type = "response")
print(predictions1)





