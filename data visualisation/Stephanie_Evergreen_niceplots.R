
# Population Pyramid ------------------------------------------------------

# Load the necessary packages.
library(ggplot2)
library(plyr)
# Convert all male values to negative values.
cause$value[1:11] <- -1 * cause$value[1:11]
# Order the graph by descending values of male causes of death.
cause$condition <- factor(cause$condition, levels = cause$condition[order(cause$value[11:1])])
# Create, clean, and label the graph.
ggplot(cause, aes(x = condition, y = value, fill = variable)) + 
  geom_bar(subset = .(variable == "female"), stat = "identity", position = "identity") + 
  geom_bar(subset = .(variable == "male"), stat = "identity", position = "identity") + 
  scale_y_continuous(breaks = seq(-1,1,.1)) + 
  coord_flip() + 
  theme_bw() +
  theme(
    legend.position="none",
    plot.title=element_text(hjust = 0, vjust = 1, size = 11, face = "bold"),
    panel.border=element_blank(),
    panel.grid=element_blank(),
    axis.ticks=element_blank(),
    axis.text.x=element_blank()
  ) +
  labs(x = "", y = "") +
  scale_fill_manual(values = c(
    male = yellow,
    female = orange)
  ) +
  annotate("text", x = 7:11, y = -0.005, label = paste(cause$value[7:11]*-100, "%", sep = ""), color = "black", size = 3.2, hjust = 1) +
  annotate("text", x = 2:6, y = -0.005 + cause$value[2:6], label = paste(cause$value[2:6]*-100, "%", sep = ""), color = "black", size = 3.2, hjust = 1) +
  annotate("text", x = 1, y = -0.005, label = "not in top 10", color = yellow, size = 3.2, hjust = 1) +
  annotate("text", x = 1:3, y = 0.005 + cause$value[12:14], label = paste(cause$value[12:14]*100, "%", sep = ""), color = "black", size = 3.2, hjust = 0) +
  annotate("text", x = 4, y = 0.005, label = paste(cause$value[15]*100, "%", sep = ""), color = "black", size = 3.2, hjust = 0) +
  annotate("text", x = 5, y = 0.005, label = "not in top 10", color = orange, size = 3.2, hjust = 0) +
  annotate("text", x = 6, y = 0.005 + cause$value[17], label = paste(cause$value[17]*100, "%", sep = ""), color = "black", size = 3.2, hjust = 0) +
  annotate("text", x = 7:11, y = 0.005, label = paste(cause$value[18:22]*100, "%", sep = ""), color = "black", size = 3.2, hjust = 0) +
  annotate("text", x = 12, y = -0.005, label = "Male", color = yellow, size = 3.2, hjust = 1, vjust = 1.1, fontface = "bold") +
  annotate("text", x = 12, y = 0.005, label = "Female", color = orange, size = 3.2, hjust = 0, vjust = 1.1, fontface = "bold") +
  ggtitle("Leading causes of death for males and females are similar, with a few \nnotable exceptions.")


# Dot Plot ----------------------------------------------------------------

# Create vectors for Fall scores, Spring scores, and Subjects.
fall <- c(96,92,67,63,34)
spring <- c(100,98,75,77,69)
subjects <- c("Creative Arts","Science","Mathematics","Language","Literacy")
# Combine these into a data set.
data <- data.frame(subjects,fall,spring)
# Load the reshape2 package.
library(reshape2)
# Restructure the data into long-format.
scores <- melt(data = data, id = "subjects")
# Define / save colors.
grey <- "#bfbfbf"
grid <- "#dadada"
blue <- "#598fd5"
charcoal <- "#465966"
# Load the ggplot2 package.
library(ggplot2)
# Begin the visualization by mapping the variables appropriately.
# Save the graph as an object so that it can be added to or modified as needed.
plot <- ggplot(data = scores, aes(x = value, y = subjects, color = variable)) + 
  geom_point(size = 12, shape = 19) + 
  theme_bw() +
  labs(x = "", y = "") +
  scale_y_discrete(limits = subjects) +
  scale_x_continuous(limits = c(0,100), breaks = seq(0,100,100)) +
  scale_color_manual(values = c(fall = grey, spring = blue)) +
  annotate("text", x = scores$value, y = scores$subjects, label = scores$value, size = 4, fontface = "bold", color = "white") +
  annotate("text", x = scores$value[1:5] - 5, y = scores$subjects[1:5], label = scores$subjects[1:5], size = 5, color = charcoal, fontface = "bold", hjust = 1, vjust = -0.5) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = grid, size = 0.5),
    axis.line = element_line(color = grid, size = 0.5),
    axis.line.y = element_blank(),
    plot.title = element_text(size = 18, color = charcoal, hjust = 0)
  ) +
  ggtitle("Kindergarten readiness increased between Fall \nand Spring.")
# Export the visualization.
ggsave(file = "cleveland_dot_plot.pdf", dpi = 150)


# Bar Graph with Benchmark Line -------------------------------------------

# Create the data set.
groups <- c("Group A","Group B","Group C")
performance <- c(.55,.58,.67)
data <- data.frame(groups,performance)
# Organize colors by defining them as objects.
dark <- "#595959"
light <- "#d9d9d9"
accent <- "#eb7d2e"
# Load ggplot2 from the package library.
library(ggplot2)
# Create and clean the visual.
ggplot(data = data, aes(x = groups, y = performance), color = dark) +
  geom_bar(width = 0.4, stat = "identity") +
  scale_y_continuous(limits=c(0,0.8)) +
  annotate("text", x = data$groups, y = data$performance - .06, label = paste(data$performance*100, "%", sep = ""), size = 5, color = "white", fontface = "bold") + 
  ggtitle("Two of the three grantee groups did not meet \nperformance benchmarks this year.") +
  geom_hline(aes(yintercept = 0.65), color = accent, size = 1) +
  annotate("text", x = data$groups[1], y = .62, label = "Benchmark", size = 5, color = dark, fontface = "bold", hjust = 1) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 14, face = "bold", color = dark),
    axis.text.y = element_blank(),
    axis.line = element_blank()
  ) +
  geom_hline(yintercept = 0, size = 0.5, color = light)
# Save the visual as a PDF, PNG, JPG, or other file type as you wish.
ggsave(file = "bar_graph_benchmark_line.pdf", dpi = 150)

