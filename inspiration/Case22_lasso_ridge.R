pacman::p_load(readxl, httr, jsonlite, tidyverse, rlist, rjstat, rjson, Rcrawler, 
               repurrrsive, tidymodels, caret, MASS, ISLR2, glmnet, boot, leaps,
               viridis, pls)

thise <- read_excel("stud_exam_data.xlsx") |> 
  mutate(date = date(date))

table(thise$forventet_l_lager)
table(thise$kammerjunkere)

helligdag <-ymd(c("2022-04-14", "2022-04-15", "2022-04-18",
                  "2022-05-13", "2022-05-26", "2022-06-05"))

base_url <- "https://dmigw.govcloud.dk/v2/"
info_url <- "metObs/collections/observation/items?"
req_url <- "stationId=06186&datetime=2022-04-01T12:00:00Z/2022-08-30T12:00:00Z&limit=100000"
api_key <- "&api-key=8119aa28-31a1-4cec-9ab7-549fb14e0090"

full_url <- base::paste0(base_url, info_url, req_url, api_key)

## call API
api_call <- httr::GET(full_url) # sender GET request med full_url
api_char <- base::rawToChar(api_call$content)
#data laves om til char med library (rjson)

api_JSON <- jsonlite::fromJSON(api_char, flatten = TRUE) # brug library(jsonlite) 
list_dmi <- api_JSON # lav JSON objekt om til en list
coldmi <- as.data.frame(do.call(cbind, list_dmi)) # lav listen om til en dataframe

# subset coldmi data: esktra her features.properties.observed, features.properties.parameterID og features.properties.value
# Lav elementerne om til vektorer (de er oprindeligt i data.frame format)
# Lav dataframe af at sammensætte vektorerne
col1 <- as.vector(coldmi[7]) 
col2 <- as.vector(coldmi[8]) 
col3 <- as.vector(coldmi[10])
add_cols <- c(col1, col2, col3)

# Lav dataframe om til en tibble
# Omdøb variablerne
# Gem tibble i objekt "dmi"
dmi <- as.tibble(add_cols) |> 
 dplyr::rename("Observationstidspunkt" =  "features.properties.observed", 
              "Observationer" = "features.properties.parameterId", 
              "Value" = "features.properties.value" ) |> 
  
  mutate(Observationstidspunkt = ymd_hms(Observationstidspunkt),
         weekend_helligdag = as_factor(if_else(weekdays(Observationstidspunkt) %in% 
         c("fredag", "lørdag", "søndag") | Observationstidspunkt %in% helligdag, 1, 0)),
         maaned = as_factor(month(Observationstidspunkt))) |> 
   
  filter(hour(Observationstidspunkt) == 12 & minute(Observationstidspunkt) == 0) |> 
  
    pivot_wider(names_from = Observationer, values_from = Value) |> 
     arrange(Observationstidspunkt) |> 
      mutate(date = date(Observationstidspunkt)) |> 

  
      left_join(thise, by = c("date"="date")) |> 
  
        mutate(temp1=if_else(is.na(lag(temp_max_past1h, 1)), 0, lag(temp_max_past1h, 1)),
               temp2=if_else(is.na(lag(temp_max_past1h, 2)), 0, lag(temp_max_past1h, 2)),
               temp3=if_else(is.na(lag(temp_max_past1h, 3)), 0, lag(temp_max_past1h, 3)),
               
               temp_gt25_3_dage_dummy=as_factor(if_else(temp1>=25 
                                                        & temp2>=25 & temp3>=25, 1, 0)),
               kammerjunkere = as_factor(kammerjunkere), forventet_l_lager = as_factor(forventet_l_lager),
               temp_max_past1h_i_anden = temp_max_past1h^2
              ) |> 
  
         dplyr::select(-temp1, -temp2, -temp3, -Observationstidspunkt, -date)
  
names(dmi)

table(dmi$weekend_helligdag)

# Ridge og Lasso  
x <- model.matrix(efterspørgsel ~ ., dmi)[, -1] 
y <- dmi$efterspørgsel

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
glm.fit <- glm(efterspørgsel ~ 1, data = dmi[train, ])
rmse_0_cv <- sqrt(cv.glm(dmi[train, ], glm.fit , K = 10)$delta[1])
rmse_0_test <- sqrt(mean((dmi[test, ]$efterspørgsel - predict(glm.fit, dmi[test, ]))^2))



# Best subset selection

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

dmi_train <- dmi[train,]
dmi_test <- dmi[test,]

k <- 10 # Vi danner 10 folds
n <- nrow(dmi_train) # registrerer hvor mange observationer, vi har.
set.seed(1) 
folds <- sample(rep(1:k, length = n)) #Vi tildeler en værdi mellem 1 og
dim(dmi_train)[2]  # Der er 14 variabler og dermed 13 prædiktorer

cv.errors <- matrix(NA, k, 13,
                    dimnames = list(NULL, paste(1:13)))
cv.errors


for (j in 1:k) { # her gennemløbes alle folds
  best.fit <- regsubsets(efterspørgsel ~ .,
                         data = dmi_train[folds != j, ],
                         nvmax = 13)
  for (i in 1:13) { # her gennemløbes alle kandidatmodeller
    pred <- predict(best.fit, dmi_train[folds == j, ], id = i)
    # predict-funktionen ovenfor kalder den funktion, vi har lavet tidligere. 
    cv.errors[j, i] <-
      mean((dmi_train$efterspørgsel[folds == j] - pred)^2) # Her udregnes MSE for hver 
    # fold for hver kandidatmodel 
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean) # apply er en smart funktion, der 
# gennemløber alle rækker og tager gennemsnittet henover hver søjle, som svarer 
# til hver kandidatmodel.
mean.cv.errors # Vi får altså en gennemsnitlig MSE for hver kandidatmodel.
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b") # Her plottes disse gennemsnit for hver størrelse,
which.min(mean.cv.errors)


# Her fittes modellen til ALLE træningsdata
reg.best <- regsubsets(efterspørgsel ~ ., data = dmi_train,
                       nvmax = 13)
coef(reg.best, 7)

pred_best_subset <- predict(reg.best, dmi_test, id = 7)


mse_best_subset <- mean((dmi[test,]$efterspørgsel - pred_best_subset)^2)
rmse_bestsubset_test <- sqrt(mse_best_subset)
rmse_bestsubset_cv <- sqrt(min(mean.cv.errors))


# Hvis der er forskellige resultater, tjek RStudio version (Help -> About RStudio) + R Version (Console)
# Tjek om variabler er ens
# Tjek om set.seed() er ens


# Hvis lasso eller ridge vinder, så kør regression igen med optimal lambda
# Hvis naiv model vinder, så hold weekend
# Hvis best subset vinder, skal i køre regression igen med det optimale antal værdier

# Daniels gruppe r^2 49%
# Bruge antal tilskuer data fra tidligere kampe der ikke er medtaget i guld for at få et mere præcist gennemsnitligt antal tilskuere
# Også bruge antal tilskuere fra sidste kamp mod bestemt hold
# Sørg for alle variabler er mulige at få før kampstart
