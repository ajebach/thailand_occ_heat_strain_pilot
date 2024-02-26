library(dplyr)
library(lubridate)
library(readr)
library(patchwork)
library(ggplot2)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

combined_df <- read.csv("./data/emu.csv")

combined_df <- combined_df %>% filter(location_code == 1)

combined_df$time <- as.POSIXct(combined_df$time, format = "%H:%M:%S")

is_within_five_minutes <- function(time) {
  minute <- as.integer(format(time, "%M"))
  hour <- as.integer(format(time, "%H"))
  (hour >= 9 & minute >= 55) | (hour <= 17 & minute <= 5)
}

combined_df <- combined_df %>% 
  filter(sapply(time, is_within_five_minutes))

grouped_df <- combined_df %>% 
  group_by(day, hour) %>% 
  summarise(
    mean_Tg = mean(Tg, na.rm = TRUE),
    sd_Tg = sd(Tg, na.rm = TRUE),
    se_Tg = sd_Tg / sqrt(n()),
    ci_Tg = qt(0.975, df=n()-1) * se_Tg,
    
    mean_RH = mean(RH, na.rm = TRUE),
    sd_RH = sd(RH, na.rm = TRUE),
    se_RH = sd_RH / sqrt(n()),
    ci_RH = qt(0.975, df=n()-1) * se_RH,
    
    mean_Tadb = mean(Tadb, na.rm = TRUE),
    sd_Tadb = sd(Tadb, na.rm = TRUE),
    se_Tadb = sd_Tadb / sqrt(n()),
    ci_Tadb = qt(0.975, df=n()-1) * se_Tadb,
    
    mean_Tnwb = mean(Tnwb, na.rm = TRUE),
    sd_Tnwb = sd(Tnwb, na.rm = TRUE),
    se_Tnwb = sd_Tnwb / sqrt(n()),
    ci_Tnwb = qt(0.975, df=n()-1) * se_Tnwb,
    
    mean_WBGT = mean(WBGT, na.rm = TRUE),
    sd_WBGT = sd(WBGT, na.rm = TRUE),
    se_WBGT = sd_WBGT / sqrt(n()),
    ci_WBGT = qt(0.975, df=n()-1) * se_WBGT
  )

print(grouped_df)

overall_df <- combined_df %>% 
  group_by(hour) %>% 
  summarise(
    mean_Tg = mean(Tg, na.rm = TRUE),
    sd_Tg = sd(Tg, na.rm = TRUE),
    se_Tg = sd_Tg / sqrt(n()),
    ci_Tg = qt(0.975, df=n()-1) * se_Tg,
    
    mean_RH = mean(RH, na.rm = TRUE),
    sd_RH = sd(RH, na.rm = TRUE),
    se_RH = sd_RH / sqrt(n()),
    ci_RH = qt(0.975, df=n()-1) * se_RH,
    
    mean_Tadb = mean(Tadb, na.rm = TRUE),
    sd_Tadb = sd(Tadb, na.rm = TRUE),
    se_Tadb = sd_Tadb / sqrt(n()),
    ci_Tadb = qt(0.975, df=n()-1) * se_Tadb,
    
    mean_Tnwb = mean(Tnwb, na.rm = TRUE),
    sd_Tnwb = sd(Tnwb, na.rm = TRUE),
    se_Tnwb = sd_Tnwb / sqrt(n()),
    ci_Tnwb = qt(0.975, df=n()-1) * se_Tnwb,
    
    mean_WBGT = mean(WBGT, na.rm = TRUE),
    sd_WBGT = sd(WBGT, na.rm = TRUE),
    se_WBGT = sd_WBGT / sqrt(n()),
    ci_WBGT = qt(0.975, df=n()-1) * se_WBGT
  )

print(overall_df)

plot_Tadb <- ggplot(overall_df, aes(x = hour, y = mean_Tadb)) +
  geom_linerange(aes(ymin = mean_Tadb - sd_Tadb, ymax = mean_Tadb + sd_Tadb), size = 0.5, colour = "#D53E4F") +
  geom_point(shape = 21, colour = "#D53E4F", fill = "white", size = 1, stroke = 1) +
  labs(x = "Hour", 
       y = "°C", 
       title = "Aspirated dry-bulb temperature") +
  theme_bw() +
  scale_x_continuous(breaks = seq(10, 17, 1), limits = c(9.9, 17.1)) +
  scale_y_continuous(breaks = seq(20, 32, 4), limits = c(20, 32)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

plot_Tg <- ggplot(overall_df, aes(x = hour, y = mean_Tg)) +
  geom_linerange(aes(ymin = mean_Tg - sd_Tg, ymax = mean_Tg + sd_Tg), size = 0.5, colour = "#525252") +
  geom_point(shape = 21, colour = "#525252", fill = "white", size = 1, stroke = 1) +
  labs(x = "Hour", 
       y = "°C", 
       title = "Globe temperature") +
  theme_bw() +
  scale_x_continuous(breaks = seq(10, 17, 1), limits = c(9.9, 17.1)) +
  scale_y_continuous(breaks = seq(20, 32, 4), limits = c(20, 32)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

plot_Tnwb <- ggplot(overall_df, aes(x = hour, y = mean_Tnwb)) +
  geom_linerange(aes(ymin = mean_Tnwb - sd_Tnwb, ymax = mean_Tnwb + sd_Tnwb), size = 0.5, colour = "#5E4FA2") +
  geom_point(shape = 21, colour = "#5E4FA2", fill = "white", size = 1, stroke = 1) +
  labs(x = "Hour", 
       y = "°C", 
       title = "Natural wet-bulb temperature") +
  theme_bw() +
  scale_x_continuous(breaks = seq(10, 17, 1), limits = c(9.9, 17.1)) +
  scale_y_continuous(breaks = seq(20, 32, 4), limits = c(20, 32)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

plot_WBGT <- ggplot(overall_df, aes(x = hour, y = mean_WBGT)) +
  geom_linerange(aes(ymin = mean_WBGT - sd_WBGT, ymax = mean_WBGT + sd_WBGT), size = 0.5, colour = "#F46D43") +
  geom_point(shape = 21, colour = "#F46D43", fill = "white", size = 1, stroke = 1) +
  labs(x = "Workday (hour)", 
       y = "°C", 
       title = "Wet-bulb globe temperature") +
  theme_bw() +
  scale_x_continuous(breaks = seq(10, 17, 1), limits = c(9.9, 17.1)) +
  scale_y_continuous(breaks = seq(20, 32, 4), limits = c(20, 32)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

plot_RH <- ggplot(overall_df, aes(x = hour, y = mean_RH)) +
  geom_linerange(aes(ymin = mean_RH - sd_RH, ymax = mean_RH + sd_RH), size = 0.5, colour = "#4292C6") +
  geom_point(shape = 21, colour = "#4292C6", fill = "white", size = 1, stroke = 1) +
  labs(x = "Hour", 
       y = "%", 
       title = "Relative Humidity") +
  theme_bw() +
  scale_x_continuous(breaks = seq(10, 17, 1), limits = c(9.9, 17.1)) +
  scale_y_continuous(breaks = seq(40, 60, 5), limits = c(40, 60)) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

overall_df$mean_AirSpeed <- rep(3.75, nrow(overall_df))
overall_df$sd_AirSpeed <- rep(0.25, nrow(overall_df))

plot_Vair <- ggplot(overall_df, aes(x = hour, y = mean_AirSpeed)) +
  geom_linerange(aes(ymin = mean_AirSpeed - sd_AirSpeed, ymax = mean_AirSpeed + sd_AirSpeed), size = 0.5, colour = "#66C2A5") +
  geom_point(shape = 21, colour = "#66C2A5", fill = "white", size = 1, stroke = 1) +
  labs(x = "Hour", 
       y = "m/s", 
       title = "Air Speed") +
  theme_bw() +
  scale_x_continuous(breaks = seq(10, 17, 1), limits = c(9.9, 17.1)) +
  scale_y_continuous(breaks = seq(3, 4.2, 0.4), limits = c(3, 4.2)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

combined_plot <- plot_Tadb / plot_Tg / plot_Tnwb / plot_WBGT / plot_Vair / plot_RH 

print(combined_plot)

ggsave(filename = "oasis.tiff", 
       plot = ggplot2::last_plot(), 
       width = 5, height = 7, units = "in",
       device='tiff', dpi=300)

summary_df <- combined_df %>% 
  filter(sapply(time, is_within_five_minutes)) %>% 
  summarise(
    mean_Tg = mean(Tg, na.rm = TRUE),
    sd_Tg = sd(Tg, na.rm = TRUE),
    se_Tg = sd_Tg / sqrt(n()),
    ci_Tg = qt(0.975, df = n() - 1) * se_Tg,
    
    mean_RH = mean(RH, na.rm = TRUE),
    sd_RH = sd(RH, na.rm = TRUE),
    se_RH = sd_RH / sqrt(n()),
    ci_RH = qt(0.975, df = n() - 1) * se_RH,
    
    mean_Tadb = mean(Tadb, na.rm = TRUE),
    sd_Tadb = sd(Tadb, na.rm = TRUE),
    se_Tadb = sd_Tadb / sqrt(n()),
    ci_Tadb = qt(0.975, df = n() - 1) * se_Tadb,
    
    mean_Tnwb = mean(Tnwb, na.rm = TRUE),
    sd_Tnwb = sd(Tnwb, na.rm = TRUE),
    se_Tnwb = sd_Tnwb / sqrt(n()),
    ci_Tnwb = qt(0.975, df = n() - 1) * se_Tnwb,
    
    mean_WBGT = mean(WBGT, na.rm = TRUE),
    sd_WBGT = sd(WBGT, na.rm = TRUE),
    se_WBGT = sd_WBGT / sqrt(n()),
    ci_WBGT = qt(0.975, df = n() - 1) * se_WBGT
  )

print(summary_df)

#OUTSIDE########################################################################

combined_df <- read.csv("./data/emu.csv")

combined_df <- combined_df %>% filter(location_code == 2)

combined_df$time <- as.POSIXct(combined_df$time, format = "%H:%M:%S")

combined_df$hour <- hour(combined_df$time) + ifelse(minute(combined_df$time) >= 30, 1, 0)
combined_df$hour[combined_df$hour == 24] <- 0

grouped_df <- combined_df %>% 
  group_by(day, hour) %>% 
  summarise(
    mean_Tg = mean(Tg, na.rm = TRUE),
    sd_Tg = sd(Tg, na.rm = TRUE),
    se_Tg = sd_Tg / sqrt(n()),
    ci_Tg = qt(0.975, df=n()-1) * se_Tg,
    
    mean_RH = mean(RH, na.rm = TRUE),
    sd_RH = sd(RH, na.rm = TRUE),
    se_RH = sd_RH / sqrt(n()),
    ci_RH = qt(0.975, df=n()-1) * se_RH,
    
    mean_Tadb = mean(Tadb, na.rm = TRUE),
    sd_Tadb = sd(Tadb, na.rm = TRUE),
    se_Tadb = sd_Tadb / sqrt(n()),
    ci_Tadb = qt(0.975, df=n()-1) * se_Tadb,
    
    mean_Tnwb = mean(Tnwb, na.rm = TRUE),
    sd_Tnwb = sd(Tnwb, na.rm = TRUE),
    se_Tnwb = sd_Tnwb / sqrt(n()),
    ci_Tnwb = qt(0.975, df=n()-1) * se_Tnwb,
    
    mean_WBGT = mean(WBGT, na.rm = TRUE),
    sd_WBGT = sd(WBGT, na.rm = TRUE),
    se_WBGT = sd_WBGT / sqrt(n()),
    ci_WBGT = qt(0.975, df=n()-1) * se_WBGT,
    
    mean_WBGT = mean(WBGT, na.rm = TRUE),
    sd_WBGT = sd(WBGT, na.rm = TRUE),
    se_WBGT = sd_WBGT / sqrt(n()),
    ci_WBGT = qt(0.975, df=n()-1) * se_WBGT,
    
    mean_Ws = mean(Ws, na.rm = TRUE),
    sd_Ws = sd(Ws, na.rm = TRUE),
    se_Ws = sd_Ws / sqrt(n()),
    ci_Ws = qt(0.975, df=n()-1) * se_Ws,
    
    mean_Ws = mean(Ws, na.rm = TRUE),
    sd_Ws = sd(Ws, na.rm = TRUE),
    se_Ws = sd_Ws / sqrt(n()),
    ci_Ws = qt(0.975, df=n()-1) * se_Ws,
  )

print(grouped_df)

overall_df <- combined_df %>% 
  group_by(hour) %>% 
  summarise(
    mean_Tg = mean(Tg, na.rm = TRUE),
    sd_Tg = sd(Tg, na.rm = TRUE),
    se_Tg = sd_Tg / sqrt(n()),
    ci_Tg = qt(0.975, df=n()-1) * se_Tg,
    
    mean_RH = mean(RH, na.rm = TRUE),
    sd_RH = sd(RH, na.rm = TRUE),
    se_RH = sd_RH / sqrt(n()),
    ci_RH = qt(0.975, df=n()-1) * se_RH,
    
    mean_Tadb = mean(Tadb, na.rm = TRUE),
    sd_Tadb = sd(Tadb, na.rm = TRUE),
    se_Tadb = sd_Tadb / sqrt(n()),
    ci_Tadb = qt(0.975, df=n()-1) * se_Tadb,
    
    mean_Tnwb = mean(Tnwb, na.rm = TRUE),
    sd_Tnwb = sd(Tnwb, na.rm = TRUE),
    se_Tnwb = sd_Tnwb / sqrt(n()),
    ci_Tnwb = qt(0.975, df=n()-1) * se_Tnwb,
    
    mean_WBGT = mean(WBGT, na.rm = TRUE),
    sd_WBGT = sd(WBGT, na.rm = TRUE),
    se_WBGT = sd_WBGT / sqrt(n()),
    ci_WBGT = qt(0.975, df=n()-1) * se_WBGT,
    
    mean_Ws = mean(Ws, na.rm = TRUE),
    sd_Ws = sd(Ws, na.rm = TRUE),
    se_Ws = sd_Ws / sqrt(n()),
    ci_Ws = qt(0.975, df=n()-1) * se_Ws
  )

print(overall_df)

filtered_df <- combined_df %>%
  filter(day %in% c(1, 2, 3),
         hour >= 13, hour <= 17)

summary_df <- filtered_df %>%
  summarise(
    mean_Tg = mean(Tg, na.rm = TRUE),
    sd_Tg = sd(Tg, na.rm = TRUE),
    se_Tg = sd_Tg / sqrt(n()),
    ci_Tg = qt(0.975, df = n() - 1) * se_Tg,
    mean_RH = mean(RH, na.rm = TRUE),
    sd_RH = sd(RH, na.rm = TRUE),
    se_RH = sd_RH / sqrt(n()),
    ci_RH = qt(0.975, df = n() - 1) * se_RH,
    mean_Tadb = mean(Tadb, na.rm = TRUE),
    sd_Tadb = sd(Tadb, na.rm = TRUE),
    se_Tadb = sd_Tadb / sqrt(n()),
    ci_Tadb = qt(0.975, df = n() - 1) * se_Tadb,
    mean_Tnwb = mean(Tnwb, na.rm = TRUE),
    sd_Tnwb = sd(Tnwb, na.rm = TRUE),
    se_Tnwb = sd_Tnwb / sqrt(n()),
    ci_Tnwb = qt(0.975, df = n() - 1) * se_Tnwb,
    mean_WBGT = mean(WBGT, na.rm = TRUE),
    sd_WBGT = sd(WBGT, na.rm = TRUE),
    se_WBGT = sd_WBGT / sqrt(n()),
    ci_WBGT = qt(0.975, df = n() - 1) * se_WBGT,
    mean_Ws = mean(Ws, na.rm = TRUE),
    sd_Ws = sd(Ws, na.rm = TRUE),
    se_Ws = sd_Ws / sqrt(n()),
    ci_Ws = qt(0.975, df = n() - 1) * se_Ws
  )

print(summary_df)

plot_Tadb <- ggplot(overall_df, aes(x = hour, y = mean_Tadb)) +
  geom_linerange(aes(ymin = mean_Tadb - sd_Tadb, ymax = mean_Tadb + sd_Tadb), size = 0.5, colour = "#D53E4F") +
  geom_line(colour = "#D53E4F", size = 0.3) +
  geom_point(shape = 21, colour = "#D53E4F", fill = "white", size = 1, stroke = 1) +
  labs(x = "Hour", 
       y = "°C", 
       title = "Aspirated dry-bulb temperature") +
  scale_y_continuous(limits = c(24, 38), breaks = seq(26, 38, by = 4)) +
  scale_x_continuous(breaks = seq(8, 17, 1)) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"),
        panel.grid.minor = element_blank())
print(plot_Tadb)

plot_Tg <- ggplot(overall_df, aes(x = hour, y = mean_Tg)) +  
  geom_linerange(aes(ymin = mean_Tg - sd_Tg, ymax = mean_Tg + sd_Tg), size = 0.5, colour = "#525252") +
  geom_line(colour = "#525252", size = 0.3) +
  geom_point(shape = 21, colour = "#525252", fill = "white", size = 1, stroke = 1) +
  labs(x = "Hour", 
       y = "°C", 
       title = "Globe temperature") +
  scale_y_continuous(limits = c(24, 38), breaks = seq(26, 38, by = 4)) +
  scale_x_continuous(breaks = seq(8, 17, 1)) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"),
        panel.grid.minor = element_blank())
print(plot_Tg)

plot_Tnwb <- ggplot(overall_df, aes(x = hour, y = mean_Tnwb)) +
  geom_linerange(aes(ymin = mean_Tnwb - sd_Tnwb, ymax = mean_Tnwb + sd_Tnwb), size = 0.5, colour = "#5E4FA2") +
  geom_line(colour = "#5E4FA2", size = 0.3) +
  geom_point(shape = 21, colour = "#5E4FA2", fill = "white", size = 1, stroke = 1) +
  labs(x = "Hour", 
       y = "°C", 
       title = "Natural wet-bulb temperature") +
  scale_y_continuous(limits = c(24, 38), breaks = seq(26, 38, by = 4)) +
  scale_x_continuous(breaks = seq(8, 17, 1)) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"),
        panel.grid.minor = element_blank())

plot_WBGT <- ggplot(overall_df, aes(x = hour, y = mean_WBGT)) +
  geom_linerange(aes(ymin = mean_WBGT - sd_WBGT, ymax = mean_WBGT + sd_WBGT), size = 0.5, colour = "#F46D43") +
  geom_line(colour = "#F46D43", size = 0.3) +
  geom_point(shape = 21, colour = "#F46D43", fill = "white", size = 1, stroke = 1) +
  labs(x = "Workday (hour)", 
       y = "°C", 
       title = "Wet-bulb globe temperature") + 
  scale_y_continuous(limits = c(24, 38), breaks = seq(26, 38, by = 4)) +
  scale_x_continuous(breaks = seq(8, 17, 1)) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"),
        panel.grid.minor = element_blank())

plot_Vair <- ggplot(overall_df, aes(x = hour, y = mean_Ws)) +
  geom_linerange(aes(ymin = mean_Ws - sd_Ws, ymax = mean_Ws + sd_Ws), size = 0.5, colour = "#66C2A5") +
  geom_line(colour = "#66C2A5", size = 0.3) +
  geom_point(shape = 21, colour = "#66C2A5", fill = "white", size = 1, stroke = 1) +
  labs(x = "Workday (hour)", 
       y = "m/s", 
       title = "Air Speed") +
  scale_y_continuous(limits = c(0, 0.83), breaks = seq(0, 0.80, by = 0.2)) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"),
        panel.grid.minor = element_blank())

plot_RH <- ggplot(overall_df, aes(x = hour, y = mean_RH)) +
  geom_linerange(aes(ymin = mean_RH - sd_RH, ymax = mean_RH + sd_RH), size = 0.5, colour = "#4292C6") +
  geom_line(colour = "#4292C6", size = 0.3) +
  geom_point(shape = 21, colour = "#4292C6", fill = "white", size = 1, stroke = 1) +
  labs(x = "Workday (hour)", 
       y = "%", 
       title = "Relative Humidity") +
  scale_y_continuous(limits = c(45, 90), breaks = seq(45, 90, by = 15)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(8, 17, 1)) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"),
        panel.grid.minor = element_blank())

combined_plot <- plot_Tadb / plot_Tg / plot_Tnwb / plot_WBGT / plot_Vair / plot_RH

print(combined_plot)

ggsave(filename = "outside.tiff", 
       plot = ggplot2::last_plot(), 
       width = 5, height = 7, units = "in",
       device='tiff', dpi=300)