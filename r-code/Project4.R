pacman::p_load(tidyverse, tidymodels, textrecipes, doParallel)
# Read the data into R
data <- read.xlsx("WIF-tis4d.xlsx")

# Clean the data
data$cell_line <- gsub("CELL-TYPE 101", "Cell-type 101", data$cell_line)
data$cell_line <- gsub("WILD-TYPE", "Wild-type", data$cell_line)
data$treatment <- gsub("activating factor 42", "Activating factor 42", data$treatment)
data$treatment <- gsub("placebo", "Placebo", data$treatment)
data$name <- gsub("Gl-Rjs", "GL-rjS", data$name)
data$name <- gsub("Gl-Xib", "GL-XIb", data$name)
data$name <- gsub("Gl-Zhw", "GL-ZHw", data$name)
data$name <- gsub("Gl-Cwn", "GL-cwN", data$name)

# Spliting
set.seed(2023)
data_split <- initial_split(data, strata = gene_expression)
data_train <- training(data_split)
data_test <- testing(data_split)
data_cv <- vfold_cv(data_train)
data_cv
data_recipe <- 
  recipe(gene_expression ~ name, data = data_train) |>
  step_tokenize(name) |>
  step_tokenfilter(name, max_tokens = 100) |>
  step_tfidf(name)

data_model <- linear_reg(penalty = tune(), mixture = 1) |> 
  set_mode("regression") |> 
  set_engine("glmnet")


# Workflow
data_wf <- workflow(data_recipe, data_model)
doParallel::registerDoParallel()

data_grid <- grid_regular(penalty(), levels = 50)

# Tune model
data_tune <- tune_grid(
  data_wf, 
  resamples = data_cv, 
  grid = data_grid
)
collect_metrics(data_tune)

# Decide on best model
data_tune |> autoplot()
show_best(data_tune, metric = "rmse")
xtable(show_best(data_tune, metric = "rmse"))

penalty <- select_best(data_tune, metric = "rmse")
penalty


data_wf <- data_wf |> 
  finalize_workflow(penalty)
data_wf

# Fit final model
data_fit <- data_wf |> fit(data_train)
data_fit |> 
  extract_fit_parsnip() |> 
  vip::vi() |>
  filter(Importance > 0.2) |> 
  mutate(
    Variable = str_remove_all(Variable, "tfidf_review_"), 
    Variable = fct_reorder(Variable, Importance)
  ) |> 
  ggplot(aes(Importance, Variable, fill = Sign)) + 
  geom_col() + 
  harrypotter::scale_fill_hp_d("Ravenclaw")
data_fit |> extract_fit_engine() |> autoplot()

 # Last fit
last_fit(data_wf, data_split) |> collect_metrics()
xtable(last_fit(data_wf, data_split) |> collect_metrics())