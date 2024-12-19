# Machine Learning ------------------------------------------------------
pacman::p_load( tidyverse, tidymodels, caret, ISLR2, glmnet, boot, leaps,
               viridis, pls, car)

dataset <- read_rds("data/dataset.rds")

x <- model.matrix(antal_afhentede ~ . - 1, dataset)
y <- dataset$antal_afhentede

# Udtræk kun de numeriske kolonner
numeriske_variabler <- dataset[, sapply(dataset, is.numeric)]  

# Opret en korrelationsmatrix
cor_matrix <- cor(numeriske_variabler)

cor_matrix[is.na(cor_matrix)] <- 0

library(corrplot)
# Opret et varmekortgraf
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 100)

pairs(numeriske_variabler)

model <- lm(antal_afhentede ~ . , data = dataset)

vif(model)

summary(model)

cor_matrix <- cor(x)
cor_long <- as.data.frame(as.table(cor_matrix)) |> 
  rename(Korrelation = Freq) |> 
  filter(Var1 != Var2) |> 
  filter(abs(Korrelation) > 0.75) |> 
  arrange(desc(Korrelation))

# Ridge og Lasso --------------------------------------------
model <- lm(y ~ x)
grid <- 10^seq(10, -2, length = 100)

set.seed(123)
train <- sample(1:nrow(x), nrow(x)*2/3)
test <- (-train)
y.test <- y[test]

# Ridge
ridge.mod <- glmnet(
  x[train, ], 
  y[train], 
  alpha = 0,
  lambda = grid,
  thresh = 1e-12
)
set.seed(123)
cv.out <- cv.glmnet(
  x[train, ], 
  y[train], 
  alpha = 0,
  lambda = grid,
  nfolds = 5                    )

bestlam <- cv.out$lambda.min
rmse_ridge_cv <- sqrt(cv.out$cvm[cv.out$lambda == bestlam])

ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test, ]) 
rmse_ridge_test <- sqrt(mean((ridge.pred - y.test)^2))


# Lasso
lasso.mod <- glmnet(
  x[train, ], 
  y[train], 
  alpha = 1,
  lambda = grid,
  thresh = 1e-12
)
set.seed(123)
cv.out <- cv.glmnet(
  x[train, ], 
  y[train], 
  alpha = 1,
  lambda = grid,
  nfolds = 5                    )

bestlam <- cv.out$lambda.min
rmse_lasso_cv <- sqrt(cv.out$cvm[cv.out$lambda == bestlam])

lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test, ]) 
rmse_lasso_test <- sqrt(mean((lasso.pred - y.test)^2))


# 0 features
set.seed(123)
glm.fit <- glm(antal_afhentede ~ 1, data = dataset[train, ])
rmse_0_cv <- sqrt(cv.glm(dataset[train, ], glm.fit , K = 10)$delta[1])
rmse_0_test <- sqrt(mean((dataset[test, ]$antal_afhentede - predict(glm.fit, dataset[test, ]))^2))


# Best subset selection---------------------------------------------------------
library(leaps)   # Indlæs bibliotek til best subset regression
library(dplyr)   # Indlæs bibliotek til nem datamanipulation

# Definer predict-funktion for regsubsets
predict.regsubsets <- function(object, newdata, id, ...) {
  form  <- as.formula(object$call[[2]])                # Ekstraher modelens formel
  mat   <- model.matrix(form, newdata)                # Opret designmatrix til nye data
  coefi <- coef(object, id = id)                      # Hent koefficienter for modelstørrelsen
  xvars <- intersect(names(coefi), colnames(mat))     # Match variabler mellem model og data
  if (length(xvars) == 0) stop("Ingen matchende variabler i data!") # Stop, hvis ingen match
  mat[, xvars, drop = FALSE] %*% coefi                # Beregn forudsigelser
}

# Konverter kategoriske variabler til faktorer
dataset <- dataset %>%
  mutate(across(where(is.character), as.factor))      # Konverter alle character-kolonner til faktorer

# Split data i trænings- og testdatasæt
set.seed(123)                                         # Sæt seed for reproducerbarhed
train <- sample(seq_len(nrow(dataset)), size = 0.7 * nrow(dataset)) # Vælg 70% til træning
test  <- setdiff(seq_len(nrow(dataset)), train)       # Resterende 30% til test
samlet_train <- dataset[train, ]                     # Træningsdatasæt
samlet_test  <- dataset[test, ]                      # Testdatasæt

# Harmoniser faktorniveauer mellem trænings- og testdata
categorical_vars <- names(Filter(is.factor, dataset)) # Identificer faktorkolonner
for (col in categorical_vars) {
  samlet_test[[col]] <- factor(samlet_test[[col]], levels = levels(samlet_train[[col]])) # Ensret faktorniveauer
}

# Opsætning til K-fold Cross-Validation
k <- 10                                              # Antal fold til K-fold cross-validation
n <- nrow(samlet_train)                              # Antal observationer i træningsdata
set.seed(1)                                          # Sæt seed for reproducerbarhed
folds <- sample(rep(1:k, length = n))                # Opret fold

# Initialiser matrix til at gemme cross-validation fejl
cv.errors <- matrix(NA, k, 14, dimnames = list(NULL, paste(1:14))) # Gem fejl for hver fold og modelstørrelse

# Udfør K-fold Cross-Validation
for (j in 1:k) {                                     # Loop gennem fold
  best.fit <- regsubsets(antal_afhentede ~ ., data = samlet_train[folds != j, ], nvmax = 14) # Træn model
  for (i in 1:14) {                                  # Loop gennem kandidatmodelstørrelser
    pred <- tryCatch({
      predict.regsubsets(best.fit, samlet_train[folds == j, ], id = i) # Forudsig
    }, error = function(e) {
      return(rep(NA, sum(folds == j)))             # Returner NA ved fejl
    })
    cv.errors[j, i] <- mean((samlet_train$antal_afhentede[folds == j] - pred)^2, na.rm = TRUE) # Beregn MSE
  }
}

# Beregn gennemsnitlig cross-validation fejl for hver modelstørrelse
mean.cv.errors <- apply(cv.errors, 2, function(x) mean(x, na.rm = TRUE))

# Ignorer fejlende modeller (f.eks. model 14) ved beregning af minimum
valid_models <- which(is.finite(mean.cv.errors))    # Find gyldige modeller
mean.cv.errors <- mean.cv.errors[valid_models]      # Behold kun gyldige modeller
optimal_model_size <- valid_models[which.min(mean.cv.errors)] # Find optimal model

# Plot gennemsnitlige cross-validation fejl
par(mfrow = c(1, 1))                                # Konfigurer plottets layout
plot(mean.cv.errors, type = "b", xlab = "Antal prædiktorer", ylab = "Gns. CV Fejl",
     main = "K-fold Cross-Validation Fejl pr. Modelstørrelse") # Plot fejl pr. modelstørrelse
cat("Optimal modelstørrelse:", optimal_model_size, "\n") # Print resultat

# Træn den bedste model på alle træningsdata
reg.best <- regsubsets(antal_afhentede ~ ., data = samlet_train, nvmax = 14)
best_model_coefficients <- coef(reg.best, optimal_model_size) # Hent koefficienter
print(best_model_coefficients)                                # Print koefficienter

# Forudsig på testdata
pred_best_subset <- predict.regsubsets(reg.best, samlet_test, id = optimal_model_size)

# Beregn MSE og RMSE på testdata
mse_best_subset <- mean((samlet_test$antal_afhentede - pred_best_subset)^2) # Bestsubset MSE på testdata: 30583.84
rmse_best_subset <- sqrt(mse_best_subset)                                   # Bestsubset RMSE på testdata: 174.8824

# Beregn RMSE fra K-fold Cross-Validation
rmse_bestsubset_cv <- sqrt(min(mean.cv.errors))                  # Bestsubset CV RMSE: 154.7378




# Sammenligning: 3 og 13 prædiktorer--------------------------------------------

optimal_model_size <- 3

# For model 3
pred_model_3 <- predict.regsubsets(reg.best, samlet_test, id = 3)
mse_model_3 <- mean((samlet_test$antal_afhentede - pred_model_3)^2)
rmse_model_3 <- sqrt(mse_model_3) # Bestsubset RMSE med 3 prædiktorer på testdata: 138.2644

# For model 13
pred_model_13 <- predict.regsubsets(reg.best, samlet_test, id = 13)
mse_model_13 <- mean((samlet_test$antal_afhentede - pred_model_13)^2)
rmse_model_13 <- sqrt(mse_model_13) # Bestsubset RMSE med 13 prædiktorer på testdata: 174.8824

mean.cv.errors
cv_rmse_model_3 <- sqrt(mean.cv.errors[3]) #Bestsubset CV RMSE med 3 præditorer: 154.7378


# Hent koefficienterne for modelstørrelse 3
model_3_coefficients <- coef(reg.best, id = 1)

# Udskriv koefficienterne
print(model_3_coefficients)
