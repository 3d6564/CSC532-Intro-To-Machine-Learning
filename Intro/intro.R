# Creating vectors
subject_name <- c("John Doe", "Jane Doe", "Steve Graves")
temperature <- c(98.1, 98.6, 101.4)
flu_status <- c(FALSE, FALSE, TRUE)

# Creating factors from character vectors
gender <- factor(c("MALE", "FEMALE", "MALE"))
blood <- factor(c("O", "AB", "A"), levels = c("A", "B", "AB", "O"))
symptoms <- factor(c("SEVERE", "MILD", "MODERATE"),levels = c("MILD", "MODERATE", "SEVERE"),ordered = TRUE)

# Create a data frame from patient data vectors
pt_data <- data.frame(subject_name, temperature, flu_status, gender, blood, symptoms, stringsAsFactors= FALSE)

# Matrix with default column-major order
m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)

# Matrix with row first load order
m2 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)

# Functions in R
addArgs<-function(x,...)
{
  args<-c(...)
  x+sum(args)
}

# Testing sapply, applying a function to all elements in a vector
shiftSymptoms=function(symptom)
{
  s=tolower(symptom)
  if (s=="severe") {"mild"}
  else if (s=="mild") {"moderate"}
  else {"moderate"}
}
