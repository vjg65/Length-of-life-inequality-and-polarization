##################################################################
# Comparing our estimates of LE with those publised by UN-DESA
##################################################################
un.data <- read.csv("le_undesa.csv", header =T, sep = ";")
names(un.data)
our.data <- read.csv("ineq_pol_both.csv", header =T, sep = ";")
names(our.data)
our.data <- our.data[, c(2, 5, 6)]

data.m <- merge(our.data, un.data, by.y = c("Year", "ISO3"), by.x = c("year", "ISO3"))
names(data.m)
nrow(data.m)

plot(data.m$mean_c, data.m$le, pch = 20)
cor(data.m$mean_c, data.m$le)

mean(abs(data.m$mean_c - data.m$le))

cor(data.m$mean_c, data.m$le)

library(ggplot2)

ggplot(data.m, aes(x = mean_c, y = le, color = year)) + ylab ("UN data") +
  xlab("Our estimates") + theme(
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"), 
    axis.text.x= element_text(size=14),
    axis.text.y= element_text(size=14), 
    legend.title = element_text(size=14), #change legend title font size
    legend.text = element_text(size=12)) +
  geom_point()+scale_color_gradient(low = "darkred", high = "Yellow2")
ggsave("scatter.pdf")
