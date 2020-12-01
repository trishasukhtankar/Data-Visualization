#Part 2

# library used.
library(dplyr)
library(ggplot2)

# Data Read in.
oecd <- read.csv("OECD_PISA.csv")
oecd_2018 <- filter(oecd,TIME == 2018)

# Remove unwanted columns.
oecd_2018 %>% select(ï..LOCATION,SUBJECT,Value) -> oecd_2018

# Order Location based on ASC value of Subject BOY.
order_df_boy <- filter(oecd_2018,SUBJECT %in% c("BOY"))
ordered_loc <-order_df_boy[order(order_df_boy$Value),]

# create new data frame to keep tidy data.
new_oecd_2018 <- data.frame()
for (var in as.character(ordered_loc$ï..LOCATION)) {
  df <- filter(oecd_2018, ï..LOCATION == var)
  new_oecd_2018 <- rbind(new_oecd_2018,df)
}

# Remove Tot rows.
new_oecd_2018 %>% filter(SUBJECT %in% c("BOY","GIRL")) -> new_oecd_2018

# Change Location abbrevations.
loc_vector <- c("Australia", "Austria", "Belgium", "Brazil", "Canada", "Switzerland", "Chile", "Colombia",
                "Costa Rica", "Czech Republic", "Germany", "Denmark", "ESP", "Estonia", "Finland", "France",
                "United Kingdom", "Greece", "HKG", "Hungary", "Indonesia", "Ireland", "Iceland", "Israel",
                "Italy", "Japan", "Korea", "Lithuania", "Luxembourg", "Latvia", "MAC", "Mexico",
                "Netherlands", "Norway", "New Zealand", "OECD-Average","PER", "Poland", "Portugal", "Russia",
                "SGP", "Slovak Republic", "Slovenia", "Sweden", "Turkey", "TWN", "United States")

levels(new_oecd_2018$ï..LOCATION) <- loc_vector

# Adding new location column.
new_oecd_2018$Location <- as.character(new_oecd_2018[,"ï..LOCATION"])

# Adding new diff column.
segment_val <- filter(new_oecd_2018,SUBJECT=="BOY")$Value
new_oecd_2018$Segment <- rep(segment_val, each=2)

# changing factor ordering.
order_list <- unique(new_oecd_2018$Location)
new_oecd_2018 <- transform(new_oecd_2018, Location = factor(ï..LOCATION, levels = order_list))

# custom colouring.
custom_clr <- ifelse(new_oecd_2018$Location == "Ireland","#E8272C","#3F6C88")
custom_clr_fill <- ifelse(new_oecd_2018$Location == "Ireland","#E8272C",NA)
custom_clr_text <- ifelse(unique(new_oecd_2018$Location) == "Ireland","#E8272C","grey30")


# plot data.
ggplot(data = new_oecd_2018, aes(x = Location, y= Value, shape=SUBJECT)) +
  geom_point(color=custom_clr, size = 3, fill=custom_clr_fill) +
  scale_shape_manual(values = c(16,23), labels = c("Boys","Girls")) +
  geom_segment(aes(x = Location, y = Segment+3, xend = Location,yend = Value-3), size = 0.3, color = "grey") +
  geom_segment(aes(x = Location, y=340, xend = Location, yend = Segment-3), size = 0.3, color = "white") +
  scale_y_continuous(limits = c(340, 560),
                     breaks = seq(from=340, to=560, by=20)) +
  scale_x_discrete(expand = c(0,2)) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "#E2EDF3"),
    panel.grid.major.y = element_line(size = 0.4, linetype = 'solid',colour = "white"),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1,size = 7,family = "sans", colour = custom_clr_text),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.direction="horizontal",
    plot.margin = unit(c(0, 0, 0.9, 0), "cm"),
    legend.position = c(0.04,-0.2),
    axis.text.y = element_text(vjust = -0.5,margin = margin(r = -0.5,l = 0.2,unit = "cm"),size = 7)
  )