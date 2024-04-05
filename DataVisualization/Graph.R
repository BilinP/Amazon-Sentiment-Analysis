library(ggplot2)


data <- read.csv("cleaned-11-200.csv")
data$Review_Length <- nchar(as.character(data$Reviews)) #gets amount of characters for each review

# Mean and median for data review for each star
mean_lengths <- tapply(data$Review_Length, data$Stars, mean)
median_lengths <- tapply(data$Review_Length, data$Stars, median)

# Bar plot for mean and median
barplot(rbind(mean_lengths, median_lengths), 
        beside = TRUE, 
        names.arg = unique(data$Stars), 
        col = c("red", "blue"),
        main = "Mean & Median Bar Plot by Star Category - Character count",
        xlab = "Star Category",
        ylab = "Review Length",
        legend.text = c("Mean", "Median"),
        args.legend = list(x = "topleft"))

# Density plot. All are positively skewed and can see more review characters for 5 star based on peak.  
ggplot(data, aes(x = Review_Length, fill = factor(Stars))) +
  geom_density(alpha = 1) +
  labs(title = "Density Plot of Review Lengths by Star Category",
       x = "Review Length",
       y = "Density",
       fill = "Star Category") +
  facet_wrap(~ Stars) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 1500))