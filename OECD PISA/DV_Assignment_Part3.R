#Part 3

# library used.
library(dplyr)
library(ggplot2)

# Data Read in.
oecd <- read.csv("OECD_PISA.csv")

# Remove unwanted columns.
oecd %>% select(ï..LOCATION,SUBJECT,TIME,Value) -> oecd

# change location column name.
oecd$Location <- oecd$ï..LOCATION
oecd$ï..LOCATION <- NULL

# change location factor abv to full names.
countries_vector <-c("Australia", "Austria", "Belgium", "Brazil", "Canada", "Switzerland", "Chile", "Colombia",
                     "Costa Rica", "Czech Republic", "Germany", "Denmark", "ESP", "Estonia", "Finland", "France",
                     "United Kingdom", "Greece", "HKG", "Hungary", "Indonesia", "Ireland", "Iceland", "Israel",
                     "Italy", "Japan", "Korea", "Lithuania", "Luxembourg", "Latvia", "MAC", "Mexico",
                     "Netherlands", "Norway", "New Zealand", "OECD-Average","PER", "Poland", "Portugal", "Russia",
                     "SGP", "Slovak Republic", "Slovenia", "Sweden", "Turkey", "TWN", "United States")
levels(oecd$Location) <- countries_vector

# filter rows of G7 countries.
g7 <- c("Italy","France","Germany","Japan","United Kingdom", "United States", "Canada")
oecd <- oecd %>% filter(Location %in%  g7)

# filter rows with Tot values only.
oecd <- oecd %>% filter(SUBJECT == "TOT")

# Add missing columns with NA
gaps <- data.frame(SUBJECT="TOT",TIME=2006, Value=NA,Location="United States")
oecd <- rbind(oecd,gaps)
# plot
ggplot(data = oecd, aes(x = as.factor(TIME),y =Value, group=Location, color=Location )) +
  geom_line(size=1) +
  geom_point(size=2) +
  scale_color_brewer(type = "qual", palette = 3) +
  scale_y_continuous(limits = c(460, 540),
                     breaks = seq(from=460, to=540, by=10)) +
  scale_x_discrete(expand = c(0,0.3)) +
  theme_classic() +
  ggtitle("PISA Reading Performance of G7 Countries") +
  theme(
    panel.background = element_rect(fill = "#E2EDF3"),
    panel.grid.major.y = element_line(size = 0.4, linetype = 'solid',colour = "white"),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.direction="horizontal",
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.position = "bottom",
    axis.text.y = element_text(vjust = -0.5,margin = margin(r = -0.5,l = 0.2,unit = "cm"),size = 7)
  ) +
  guides(colour = guide_legend(nrow = 1))








