library(corrplot)
library(visdat)
library(ggplot2)
library(gridExtra)
library(GGally)
library(fmsb)
library(dplyr)
library(tidyr)
library(hrbrthemes)
library(viridis)
library(ggplot2)


options(max.print = 10000) 



data <- read.csv("D:/data sci/cancer_classification.csv")
data
summary(data)

data$benign_0__mal_1 <- factor(data$benign_0__mal_1,levels = c(0,1),labels = c("benign_tumor","malignant_tumor"))
data

missing_values <- sum(is.na(data))
if (missing_values > 0) {
  print(paste("There are", missing_values, "missing values in the dataset."))
} else {
  print("There are no missing values in the dataset.")
}


correlation_matrix <- cor(data[,1:25])

print(correlation_matrix)


# Plot scatter plot
scatter_plot <- ggplot(data, aes(x = mean.radius, y = mean.texture, color = factor(benign_0__mal_1))) +
  geom_point() +
  labs(x = "Mean Radius", y = "Mean Texture", color = "Tumor Type") +
  theme_minimal()

print(scatter_plot)


scatter_matrix <- ggpairs(data, 
                          columns = 1:30,
                          mapping = ggplot2::aes(color = factor(benign_0__mal_1)), 
                          upper = list(continuous = "points"),
                          lower = list(continuous = "smooth"),
                          axisLabels = "internal")

print(scatter_matrix)




set1 <- names(data)[1:10]
set2 <- names(data)[11:20]

# Create violin plots for the first set of attributes
plots_set1 <- lapply(set1, function(attribute) {
  ggplot(data, aes(x = factor(benign_0__mal_1), y = !!sym(attribute))) +
    geom_violin(fill = "lightblue", color = "black") +
    labs(x = "Tumor Type", y = attribute) +
    theme_minimal()
})

plots_set2 <- lapply(set2, function(attribute) {
  ggplot(data, aes_string(x = "factor(benign_0__mal_1)", y = attribute)) +
    geom_violin(fill = "lightblue", color = "black") +
    labs(x = "Tumor Type", y = attribute) +
    theme_minimal()
})


for (plot in plots_set1) {
  print(plot)
}

for (plot in plots_set2) {
  print(plot)
}



#
data_numeric <- select(data, -benign_0__mal_1)


max_values <- apply(data_numeric, 2, max)


normalized_data <- data_numeric / max_values


normalized_data <- cbind(normalized_data, data$benign_0__mal_1)


plot_radar <- function(row) {
  radar_data <- as.data.frame(t(row[-length(row)]))
  names(radar_data) <- rownames(normalized_data)[-length(normalized_data)]
  radar_data$group <- factor(row[length(row)], levels = c(0, 1), labels = c("Benign", "Malignant"))
  
  radar_chart <- radarChart(radar_data,
                            axistype = 1,
                            title = paste("Tumor Type: ", levels(radar_data$group)),
                            # Customize other options as needed
                            pcol = "blue",
                            plwd = 2,
                            plty = 1,
                            cglcol = "grey",
                            cglty = 1,
                            axislabcol = "grey",
                            cglwd = 0.8)
  
  return(radar_chart)
}


plots <- lapply(seq_len(nrow(normalized_data)), function(i) plot_radar(normalized_data[i, ]))


for (plot in plots) {
  print(plot)
}


#histogram 

histograms <- list()


for(col in names(data)) {
  
  if(is.numeric(data[[col]])) {
    hist <- ggplot(data, aes(x = .data[[col]])) +
      geom_histogram(fill = "skyblue", color = "black", bins = 20) +
      labs(title = paste("Histogram of", col),
           x = col, y = "Frequency") +
      theme_minimal()
    
    histograms[[col]] <- hist
  }
}

grid.arrange(grobs = histograms, ncol = 5)


#line histograms

line_histograms <- list()


for(col in names(data)) {
  
  if(is.numeric(data[[col]])) {
    
    line_hist <- ggplot(data, aes(x = .data[[col]])) +
      geom_freqpoly(color = "skyblue", bins = 20) +
      labs(title = paste("Line Histogram of", col),
           x = col, y = "Frequency") +
      theme_minimal()
    
    line_histograms[[col]] <- line_hist
  }
}

grid.arrange(grobs = line_histograms, ncol = 5)

#bargraph

ggplot(data, aes(x = benign_0__mal_1)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of benign_0__mal_1 Column",
       x = "benign_0__mal_1",
       y = "Count") +
  theme_minimal()

#box plot
boxplots <- list()

for(col in names(data)) {
  
  if(is.numeric(data[[col]])) {
    
    boxplot <- ggplot(data, aes(y = .data[[col]])) +
      geom_boxplot(fill = "skyblue", color = "black") +
      labs(title = paste("Boxplot of", col),
           y = col) +
      theme_minimal()
    
    
    boxplots[[col]] <- boxplot
  }
}

grid.arrange(grobs = boxplots, ncol = 5)



# Create scatter plot
scatter_plot <- ggplot(data, aes(x = mean.perimeter, y = mean.area, color = benign_0__mal_1)) +
  geom_point(shape = 21, size = 3) +  # Use filled circles as points
  labs(title = "Scatter Plot of mean.perimeter vs mean.area (Colored by benign_0__mal_1)",
       x = "mean.perimeter",
       y = "mean.area",
       color = "benign_0__mal_1") +
  theme_minimal()

print(scatter_plot)


#scatter matrix
subset_data <- data[, 1:5]
pairs(subset_data)



# scatter matrix for the 1st 5 attribute without labeling color
benign_0__mal_1 <- data[, ncol(data)]

subset_data <- data[, 1:5]

pairs(subset_data, col = as.numeric(benign_0__mal_1))




#scatter matrix for the first 4attribute
benign_0__mal_1 <- data[, ncol(data)]

subset_data <- data[, 1:4]

pairs(subset_data, col = as.numeric(benign_0__mal_1))

legend("bottomright", legend = unique(benign_0__mal_1), col = unique(as.numeric(benign_0__mal_1)), pch = 1)


#scatter matrix for the 2nd 5 attribute
benign_0__mal_1 <- data[, ncol(data)]

subset_data <- data[, 5:9]

pairs(subset_data, col = as.numeric(benign_0__mal_1))

legend("bottomright", legend = unique(benign_0__mal_1), col = unique(as.numeric(benign_0__mal_1)), pch = 1)


#line graph for all attribute

line_plots <- list()

for (col in names(data)) {
  if (is.numeric(data[[col]])) {
    
    line_plot <- ggplot(data, aes(x = seq_along(.data[[col]]), y = .data[[col]])) +
      geom_line() +
      labs(title = paste("Line Plot of", col),
           x = "Index", y = col) +
      theme_minimal()
    
    line_plots[[col]] <- line_plot
  }
}

grid.arrange(grobs = line_plots, ncol = 5)



# line graph for Selected the first five attributes
subset_data <- data[, 1:5]

line_plots <- list()

for (col in names(subset_data)) {
  
  if (is.numeric(subset_data[[col]])) {
    
    line_plot <- ggplot(subset_data, aes(x = seq_along(.data[[col]]), y = .data[[col]])) +
      geom_line() +
      labs(title = paste("Line Plot of", col),
           x = "Index", y = col) +
      theme_minimal()
    
    line_plots[[col]] <- line_plot
  }
}

grid.arrange(grobs = line_plots, ncol = 3)



#violin plot for first 3 attributes

data_subset <- data[, 1:3]

melted_data <- reshape2::melt(data_subset)

ggplot(melted_data, aes(x = variable, y = value, fill = variable)) +
  geom_violin(width = 2.1, size = 0.2) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
  stat_summary(fun = median, geom = "point", shape = 21, size = 2, fill = "white", color = "black") +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum()


## Select only the 3rd 3 attributes
data_subset <- data[, 7:9]

melted_data <- reshape2::melt(data_subset)

ggplot(melted_data, aes(x = variable, y = value, fill = variable)) +
  geom_violin(width = 2.1, size = 0.2) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
  stat_summary(fun = median, geom = "point", shape = 21, size = 2, fill = "white", color = "black") +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum()



## skip the 4th column Select only the 3rd 3 attributes
data_subset <- data[, 5:7]

melted_data <- reshape2::melt(data_subset)

ggplot(melted_data, aes(x = variable, y = value, fill = variable)) +
  geom_violin(width = 2.1, size = 0.2) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
  stat_summary(fun = median, geom = "point", shape = 21, size = 2, fill = "white", color = "black") +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum()


#density graph
line_histograms <- list()

for(col in names(data)) {
  
  if(is.numeric(data[[col]])) {
    
    line_hist <- ggplot(data, aes(x = .data[[col]])) +
      geom_density(fill = "skyblue", color = "black", alpha = 0.7) +
      labs(title = paste("Density Plot of", col),
           x = col, y = "Density") +
      theme_minimal()
    
    line_histograms[[col]] <- line_hist
  }
}

grid.arrange(grobs = line_histograms, ncol = 5)


