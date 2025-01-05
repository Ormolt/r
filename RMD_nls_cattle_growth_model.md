# STEP 1 : Load necessary libraries
```
# Install the minpack.lm package if it's not already installed
if (!requireNamespace("minpack.lm", quietly = TRUE)) {
    install.packages("minpack.lm")  # Install the package
    install.packages("tidyverse")
}
install.packages("tidyverse")
install.packages("readxl")
```

# STEP 2 : Load necessary libraries
```
library(nlme)
library(minpack.lm)  # Now this should work without error
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
```

# Task 1: Data Preparation
# Load dataset
cattle_data <- read_excel("Documents/Org_Dataset.xlsx") 

# View the imported data
head(cattle_data) 


# Inspect data structure
str(cattle_data)

    tibble [80 C 4] (S3: tbl_df/tbl/data.frame)
     $ ID         : num [1:80] 1 2 3 4 5 6 7 8 9 10 ...
     $ Age_Months : num [1:80] 5.5 6 6.5 7 7.5 8 8.5 9 9.5 10 ...
     $ Breed_Group: chr [1:80] "HYBRID" "HYBRID" "HYBRID" "HYBRID" ...
     $ Weight_Kg  : num [1:80] 93.7 95.1 100 109.3 110.3 ...
    

cattle_data <- cattle_data %>%
  filter(is.finite(Age_Months) & is.finite(Weight_Kg) & !is.na(Age_Months) & !is.na(Weight_Kg))

# Display any rows with non-finite values for diagnostic purposes
non_finite_rows <- cattle_data %>%
  filter(
    !is.finite(Age_Months) |  # Check for non-finite Age
    !is.finite(Weight_Kg) # Check for non-finite Weight
  )

if (nrow(non_finite_rows) > 0) {
  message("Non-finite values detected. Below are the rows with issues:")
  print(non_finite_rows)
} else {
  message("No non-finite values detected.")
}

    No non-finite values detected.
    
    
# Task 2: Model Fitting
# Define growth model equations
brody_model <- function(Age_Months, A, B, k) A * (1 - B * exp(-k * Age_Months))
vonB_model <- function(Age_Months, A, B, k) A * (1 - exp(-k * Age_Months))^B
log_model <- function(Age_Months, A, B, k) A / (1 + B * exp(-k * Age_Months))
```

# Fit models for each group
fit_model <- function(model_func, data) {
  nlsLM(Weight_Kg ~ model_func(Age_Months, A, B, k), 
        data = cattle_data,
        start = list(A = 730, B = 0.5, k = 0.01),
        control = nls.lm.control(maxiter = 1000))
}


fits <- cattle_data %>% 
  group_by(Breed_Group) %>% 
  summarise(
    brody = list(fit_model(brody_model, pick(everything()))),
    vonB = list(fit_model(vonB_model, pick(everything()))),
    log = list(fit_model(log_model, pick(everything())))
  )
print(fits)


# Task 3: Parameter Estimation
extract_params <- function(model) {
  summary(model)$coefficients
}
params <- fits %>% 
  mutate(
    brody_params = lapply(brody, extract_params),
    vonB_params = lapply(vonB, extract_params),
    log_params = lapply(log, extract_params)
  )

print(params)

#str(fits$brody)  

# Task 4: Model Comparison
#Modify the compare_models() function to correctly retrieve residuals and fitted values:

compare_models <- function(model) {
  # Handle non-converged models
  if (!model$convInfo$isConv) {
    return(c(AIC = NA, BIC = NA, R2 = NA))
  }
  
  # Extract residuals and fitted values using functions
  residuals <- model$m$resid()  # Residuals
  fitted <- model$m$fitted()   # Fitted values
  
  # Calculate actual values as fitted + residuals
  actuals <- fitted + residuals

  # Compute metrics
  AIC_val <- AIC(model)
  BIC_val <- BIC(model)
  R2_val <- 1 - sum(residuals^2) / sum((actuals - mean(actuals))^2)

  return(c(AIC = AIC_val, BIC = BIC_val, R2 = R2_val))
}

model_metrics <- fits %>% 
  rowwise() %>% 
  mutate(
    brody_metrics = list(compare_models(brody)),
    vonB_metrics = list(compare_models(vonB)),
    log_metrics = list(compare_models(log))
  )

print(model_metrics)


# Function to check convergence and extract details
check_convergence <- function(model) {
  # Check if the model is NULL
  if (is.null(model)) {
    return(list(Status = "Error: Model is NULL"))
  }
  
  # Extract convergence info if available
  conv_info <- tryCatch(model$convInfo, error = function(e) NULL)
  
  # Handle missing convergence info
  if (is.null(conv_info)) {
    return(list(Status = "Error: No convergence info available"))
  }
  
  # Retrieve convergence details
  is_conv <- conv_info$isConv
  iterations <- conv_info$finIter
  tolerance <- conv_info$finTol
  stop_code <- conv_info$stopCode
  stop_message <- conv_info$stopMessage
  
  # Calculate RB2 if residuals and fitted values are available
  residuals <- tryCatch(residuals(model), error = function(e) NA)
  fitted <- tryCatch(fitted(model), error = function(e) NA)
  
  if (!anyNA(residuals) && !anyNA(fitted)) {
    actuals <- fitted + residuals
    R2 <- 1 - sum(residuals^2) / sum((actuals - mean(actuals))^2)
  } else {
    R2 <- NA
  }

  # Build output report
  result <- list(
    Status = if (is_conv) "Converged" else "Not Converged",
    Iterations = iterations,
    Tolerance = tolerance,
    StopCode = stop_code,
    StopMessage = stop_message,
    R2 = R2
  )
  return(result)
}

# Function to extract model details
extract_model_details <- function(model) {
  details <- list(
    Coefficients = tryCatch(as.list(coef(model)), error = function(e) NA),
    Residuals = tryCatch(residuals(model), error = function(e) NA),
    Fitted = tryCatch(fitted(model), error = function(e) NA)
  )
  return(details)
}

# Apply convergence checks and extract details for each model
convergence_results <- fits %>%
  mutate(
    brody_check = lapply(brody, check_convergence),
    vonB_check = lapply(vonB, check_convergence),
    log_check = lapply(log, check_convergence),
    brody_details = lapply(brody, extract_model_details),
    vonB_details = lapply(vonB, extract_model_details),
    log_details = lapply(log, extract_model_details)
  )

# View results
print(convergence_results)

# Task 5: Visualization
plot_growth <- function(model_func, model, data, title) {
  # Prepare prediction data
  pred_data <- data.frame(Age_Months = seq(min(data$Age_Months), max(data$Age_Months), length.out = 100))
  pred_data$Weight_Kg <- predict(model, newdata = pred_data)
  
  # Create plot
  ggplot(data, aes(x = Age_Months, y = Weight_Kg, color = Breed_Group)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, color = "grey", linetype = "dashed") +
    geom_line(data = pred_data, aes(x = Age_Months, y = Weight_Kg), linewidth = 1.2, color = "blue") +
    labs(
      title = title, 
      subtitle = "Fitted Growth Model with Observed Data",
      x = "Age (months)", 
      y = "Weight (kg)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}

plots <- fits %>% 
  rowwise() %>% 
  mutate(
    brody_plot = list(plot_growth(brody_model, brody, cattle_data, paste((Group = factor(Breed_Group)), "- Brody Model"))),
    vonB_plot = list(plot_growth(vonB_model, vonB, cattle_data, paste((Group = factor(Breed_Group)), "- Von Bertalanffy Model"))),
    log_plot = list(plot_growth(log_model, log, cattle_data, paste((Group = factor(Breed_Group)), "- Logistic Model")))
  )

# Output plots
for (plot_data in plots$brody_plot) print(plot_data)
for (plot_data in plots$vonB_plot) print(plot_data)
for (plot_data in plots$log_plot) print(plot_data)

# Task 6: Interpretation
# Compare parameters and visualize outputs
print("Parameter Estimates:")
params %>% select(Breed_Group, brody_params, vonB_params, log_params) %>% print()

print("Model Metrics:")
model_metrics %>% select(Breed_Group, brody_metrics, vonB_metrics, log_metrics) %>% print()

print("Model Comparison:")
model_metrics %>% unnest(cols = c(brody_metrics, vonB_metrics, log_metrics)) %>% print()
