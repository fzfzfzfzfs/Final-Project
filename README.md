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
\begin{table}[h!]
\centering
\begin{tabular}{ccc}
\# A tibble: 10 $\times$ 2 & \\
& splits & id \\
1 & \verb|<split [57/7]>| &Fold01 \\
2 & \verb|<split [57/7]>| &Fold02 \\
3 & \verb|<split [57/7]>| &Fold03 \\
4 & \verb|<split [57/7]>| &Fold04 \\
5 & \verb|<split [58/6]>| &Fold05 \\
6 & \verb|<split [58/6]>| &Fold06 \\
7 & \verb|<split [58/6]>| &Fold07 \\
8 & \verb|<split [58/6]>| &Fold08 \\
9 & \verb|<split [58/6]>| &Fold09 \\
10 & \verb|<split [58/6]>| &Fold10 \\
\end{tabular}
\caption{The visualization of model performance.}
\label{tab:data}
\end{table}


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







```{r}

```



```{r}

```

































