# Final-Project
All completed assignments from karl.

```{r}
pacman::p_load(tidyverse, openxlsx, GauPro)

set.seed(2024)
```

Read the data into R
```{r}
data <- read.xlsx("2024-03-19-WIF-tis4d.xlsx")
```

Clean the data
```{r}
data$cell_line <- gsub("CELL-TYPE 101", "Cell-type 101", data$cell_line)
data$cell_line <- gsub("WILD-TYPE", "Wild-type", data$cell_line)
data$treatment <- gsub("activating factor 42", "Activating factor 42", data$treatment)
data$treatment <- gsub("placebo", "Placebo", data$treatment)
data$name <- gsub("Gl-Rjs", "GL-rjS", data$name)
data$name <- gsub("Gl-Xib", "GL-XIb", data$name)
data$name <- gsub("Gl-Zhw", "GL-ZHw", data$name)
data$name <- gsub("Gl-Cwn", "GL-cwN", data$name)
```

# Project1
Plot all the data
```{r}
x = 0:1:10
XIb = data$gene_expression[data$name == "GL-XIb"]
cDZ = data$gene_expression[data$name == "GL-cDZ"]
rjS = data$gene_expression[data$name == "GL-rjS"]
Xik = data$gene_expression[data$name == "GL-Xik"]
cwN = data$gene_expression[data$name == "GL-cwN"]
kYH = data$gene_expression[data$name == "GL-kYH"]
ZHw = data$gene_expression[data$name == "GL-ZHw"]
MFA = data$gene_expression[data$name == "GL-MFA"]
```


Plot the position of each sample and predict the trend (GL-XIb for example)
```{r}
kern <- Matern52$new(0)
gpk <- GauPro_kernel_model$new(matrix(x, ncol=1), XIb, kernel=kern, parallel=FALSE)
plot(gpk)
```
![GL-XIb](https://github.com/fzfzfzfzfs/Final-Project/assets/168513907/84ad75a3-6bc9-4b31-909b-b187ec47f637)

# Project2
```{r}
pacman::p_load(openxlsx, tidyverse, ggplot2, ggpubr, xtable, extrafont)
```

Create a tiff file
```{r}
data$name <- sub("^GL-", "", data$name)

last_observation <- data %>%
  group_by(name) %>%
  slice_tail(n=1)

data |> 
  ggplot(aes(conc, gene_expression, col = treatment)) + 
  geom_point() +
  scale_color_manual(values = c("#78a8d1", "#d4be97")) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  geom_label_repel(data = last_observation, aes(label = name), 
                   color="black", family = "serif", nudge_x = 0.5) +
  labs(x = "μg/ml", y = "Gene Expression", tag = "A") +
  facet_wrap(~cell_line, scales = "free") +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "white"), 
        strip.placement = "outside",
        strip.text.x = element_text(size = 12, angle = 0, hjust = 0),
        strip.text = element_text(color = "black")) +
  theme(text = element_text(family = "serif")) 


data_wild <- subset(data, data$cell_line == "Wild-type")
last_observation_wild <- data_wild %>%
  group_by(name) %>%
  slice_tail(n=1)

plot1 = data_wild |> 
  ggplot(aes(conc, gene_expression, col = treatment)) + 
  geom_point() +
  scale_color_manual(values = c("#78a8d1", "#d4be97")) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  geom_label_repel(data = last_observation_wild, aes(label = name), 
                   color="black", family = "serif") +
  labs(x = "μg/ml", y = "Gene Expression", tag = "A", title = "Wild-type") +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "white"), 
        strip.placement = "outside",
        strip.text.x = element_text(angle = 0, hjust = 0),
        strip.text = element_text(color = "black")) +
  theme(text = element_text(family = "serif")) 


data_cell <- subset(data, data$cell_line == "Cell-type 101")
last_observation_cell <- data_cell %>%
  group_by(name) %>%
  slice_tail(n=1)

plot2 = data_cell |> 
  ggplot(aes(conc, gene_expression, col = treatment)) + 
  geom_point() +
  scale_color_manual(values = c("#78a8d1", "#d4be97")) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  geom_label_repel(data = last_observation_cell, aes(label = name), 
                   color="black", family = "serif") +
  labs(x = "μg/ml", y = "Gene Expression", tag = "B", title = "Cell-type 101") +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "white"), 
        strip.placement = "outside",
        strip.text.x = element_text(size = 12, angle = 0, hjust = 0),
        strip.text = element_text(color = "black")) +
  theme(text = element_text(family = "serif")) 
```

Print a combined plot file
```{r}
plot = ggarrange(plot1, plot2, ncol = 2, common.legend = T, legend = "bottom")
```
![combine plot](https://github.com/fzfzfzfzfs/Final-Project/assets/168513907/723f5bd6-8d53-4834-bcec-a6c6666f1f7b)


Upload a tiff file
```{r}
tiff("plot.tiff", width = 9, height = 6, units = "in", res = 500)
print(plot)
dev.off()
```

# Project3
```{r}
pacman::p_load(pwr)
```

Calculate the sample size of linear regression
```{r}
pwr.r.test(r = sqrt(0.1), sig.level = 0.05, power = 0.9)
```
![Sample result](https://github.com/fzfzfzfzfs/Final-Project/assets/168513907/74945cf8-b87a-4293-bebe-d3d9468dd839)



# Project4
```{r}
pacman::p_load(tidyverse, tidymodels, textrecipes, doParallel)
```

Spliting
```{r}
set.seed(2023)
data_split <- initial_split(data, strata = gene_expression)
data_train <- training(data_split)
data_test <- testing(data_split)
data_cv <- vfold_cv(data_train)
data_cv
```
![Model performance](https://github.com/fzfzfzfzfs/Final-Project/assets/168513907/fda45ffe-5fa9-4629-ade2-b2df0c361d33)


Spliting
```{r}
data_recipe <- 
  recipe(gene_expression ~ name, data = data_train) |>
  step_tokenize(name) |>
  step_tokenfilter(name, max_tokens = 100) |>
  step_tfidf(name)

data_model <- linear_reg(penalty = tune(), mixture = 1) |> 
  set_mode("regression") |> 
  set_engine("glmnet")
```


Workflow
```{r}
data_wf <- workflow(data_recipe, data_model)
doParallel::registerDoParallel()

data_grid <- grid_regular(penalty(), levels = 50)
```


Tune model
```{r}
data_tune <- tune_grid(
  data_wf, 
  resamples = data_cv, 
  grid = data_grid
)
collect_metrics(data_tune)
```
![Tune model](https://github.com/fzfzfzfzfs/Final-Project/assets/168513907/f3f5e1e6-c191-419d-b2e0-d96fb97c8f5c)

Decide on best model
```{r}
data_tune |> autoplot()
show_best(data_tune, metric = "rmse")

data_wf <- data_wf |> 
  finalize_workflow(penalty)
data_wf
```
![Amount of regularization](https://github.com/fzfzfzfzfs/Final-Project/assets/168513907/80c2e74f-0ad5-42e0-93b9-5017e2f09d9d)
![Performan cemetrics of various models after tuning](https://github.com/fzfzfzfzfs/Final-Project/assets/168513907/fbbdf63f-ceae-41ed-87d6-5797115c1308)
![Workflow](https://github.com/fzfzfzfzfs/Final-Project/assets/168513907/d717da69-a178-48ca-9188-8edaeec41533)


Fit final model
```{r}
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
```
![Fit model 1](https://github.com/fzfzfzfzfs/Final-Project/assets/168513907/5e16cec0-db2b-4731-84ea-41d40faebc04)
![Fit model 2](https://github.com/fzfzfzfzfs/Final-Project/assets/168513907/921ca182-9376-47cd-92bb-796a4c3c2128)



Last fit
```{r}
last_fit(data_wf, data_split) |> collect_metrics()
```
![The results of the last model fit](https://github.com/fzfzfzfzfs/Final-Project/assets/168513907/c7534069-fca0-4b67-9a22-a550782248ba)

















