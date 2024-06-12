pacman::p_load(openxlsx, tidyverse, ggplot2, ggpubr, xtable, extrafont)

# Read the data into R
data <- read.xlsx("2024-03-19-WIF-tis4d.xlsx")

# Clean the data
data$cell_line <- gsub("CELL-TYPE 101", "Cell-type 101", data$cell_line)
data$cell_line <- gsub("WILD-TYPE", "Wild-type", data$cell_line)
data$treatment <- gsub("activating factor 42", "Activating factor 42", data$treatment)
data$treatment <- gsub("placebo", "Placebo", data$treatment)
data$name <- gsub("Gl-Rjs", "GL-rjS", data$name)
data$name <- gsub("Gl-Xib", "GL-XIb", data$name)
data$name <- gsub("Gl-Zhw", "GL-ZHw", data$name)
data$name <- gsub("Gl-Cwn", "GL-cwN", data$name)

# Create a tiff file
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

# Print a combined plot file
plot = ggarrange(plot1, plot2, ncol = 2, common.legend = T, legend = "bottom")

# Upload a tiff file
tiff("plot.tiff", width = 9, height = 6, units = "in", res = 500)
print(plot)
dev.off()




















