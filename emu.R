library(dplyr)
library(lubridate)
library(readr)
library(patchwork)
library(ggplot2)
library(knitr)

#INSIDE#########################################################################

#WRANGLING

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
  filter(sapply(time, is_within_five_minutes)) %>%
  na.omit()

hourly_df <- combined_df %>% 
  group_by(day, hour) %>% 
  summarise(
    avg_Tg = mean(Tg),
    sd_Tg = sd(Tg),
    se_Tg = sd_Tg / sqrt(n()),
    ci_Tg = qt(0.975, df=n()-1) * se_Tg,
    
    avg_RH = mean(RH),
    sd_RH = sd(RH),
    se_RH = sd_RH / sqrt(n()),
    ci_RH = qt(0.975, df=n()-1) * se_RH,
    
    avg_Tadb = mean(Tadb),
    sd_Tadb = sd(Tadb),
    se_Tadb = sd_Tadb / sqrt(n()),
    ci_Tadb = qt(0.975, df=n()-1) * se_Tadb,
    
    avg_Tnwb = mean(Tnwb),
    sd_Tnwb = sd(Tnwb),
    se_Tnwb = sd_Tnwb / sqrt(n()),
    ci_Tnwb = qt(0.975, df=n()-1) * se_Tnwb,
    
    avg_WBGT = mean(WBGT),
    sd_WBGT = sd(WBGT),
    se_WBGT = sd_WBGT / sqrt(n()),
    ci_WBGT = qt(0.975, df=n()-1) * se_WBGT,
    
    avg_Ws = mean(Ws, na.rm = TRUE),
    sd_Ws = sd(Ws, na.rm = TRUE),
    se_Ws = sd_Ws / sqrt(n()),
    ci_Ws = qt(0.975, df=n()-1) * se_Ws,
    .groups = "drop"
  )

avg_hourly_df <- combined_df %>% 
  group_by(hour) %>% 
  summarise(
    avg_Tg = mean(Tg),
    sd_Tg = sd(Tg),
    se_Tg = sd_Tg / sqrt(n()),
    ci_Tg = qt(0.975, df=n()-1) * se_Tg,
    
    avg_RH = mean(RH),
    sd_RH = sd(RH),
    se_RH = sd_RH / sqrt(n()),
    ci_RH = qt(0.975, df=n()-1) * se_RH,
    
    avg_Tadb = mean(Tadb),
    sd_Tadb = sd(Tadb),
    se_Tadb = sd_Tadb / sqrt(n()),
    ci_Tadb = qt(0.975, df=n()-1) * se_Tadb,
    
    avg_Tnwb = mean(Tnwb),
    sd_Tnwb = sd(Tnwb),
    se_Tnwb = sd_Tnwb / sqrt(n()),
    ci_Tnwb = qt(0.975, df=n()-1) * se_Tnwb,
    
    avg_WBGT = mean(WBGT),
    sd_WBGT = sd(WBGT),
    se_WBGT = sd_WBGT / sqrt(n()),
    ci_WBGT = qt(0.975, df=n()-1) * se_WBGT,
    
    avg_Ws = mean(Ws, na.rm = TRUE),
    sd_Ws = sd(Ws, na.rm = TRUE),
    se_Ws = sd_Ws / sqrt(n()),
    ci_Ws = qt(0.975, df=n()-1) * se_Ws
  )

daily_df <- combined_df %>% 
  filter(sapply(time, is_within_five_minutes)) %>%
  group_by(day) %>%
  summarise(
    avg_Tg = mean(Tg),
    sd_Tg = sd(Tg),
    se_Tg = sd_Tg / sqrt(n()),
    ci_Tg = qt(0.975, df = n() - 1) * se_Tg,
    
    avg_RH = mean(RH),
    sd_RH = sd(RH),
    se_RH = sd_RH / sqrt(n()),
    ci_RH = qt(0.975, df = n() - 1) * se_RH,
    
    avg_Tadb = mean(Tadb),
    sd_Tadb = sd(Tadb),
    se_Tadb = sd_Tadb / sqrt(n()),
    ci_Tadb = qt(0.975, df = n() - 1) * se_Tadb,
    
    avg_Tnwb = mean(Tnwb),
    sd_Tnwb = sd(Tnwb),
    se_Tnwb = sd_Tnwb / sqrt(n()),
    ci_Tnwb = qt(0.975, df = n() - 1) * se_Tnwb,
    
    avg_WBGT = mean(WBGT),
    sd_WBGT = sd(WBGT),
    se_WBGT = sd_WBGT / sqrt(n()),
    ci_WBGT = qt(0.975, df = n() - 1) * se_WBGT,
    
    avg_Ws = mean(Ws, na.rm = TRUE),
    sd_Ws = sd(Ws, na.rm = TRUE),
    se_Ws = sd_Ws / sqrt(n()),
    ci_Ws = qt(0.975, df=n()-1) * se_Ws
  )

avg_daily_df <- combined_df %>% 
  filter(sapply(time, is_within_five_minutes)) %>% 
  summarise(
    avg_Tg = mean(Tg),
    sd_Tg = sd(Tg),
    se_Tg = sd_Tg / sqrt(n()),
    ci_Tg = qt(0.975, df = n() - 1) * se_Tg,
    
    avg_RH = mean(RH),
    sd_RH = sd(RH),
    se_RH = sd_RH / sqrt(n()),
    ci_RH = qt(0.975, df = n() - 1) * se_RH,
    
    avg_Tadb = mean(Tadb),
    sd_Tadb = sd(Tadb),
    se_Tadb = sd_Tadb / sqrt(n()),
    ci_Tadb = qt(0.975, df = n() - 1) * se_Tadb,
    
    avg_Tnwb = mean(Tnwb),
    sd_Tnwb = sd(Tnwb),
    se_Tnwb = sd_Tnwb / sqrt(n()),
    ci_Tnwb = qt(0.975, df = n() - 1) * se_Tnwb,
    
    avg_WBGT = mean(WBGT),
    sd_WBGT = sd(WBGT),
    se_WBGT = sd_WBGT / sqrt(n()),
    ci_WBGT = qt(0.975, df = n() - 1) * se_WBGT,
    
    avg_Ws = mean(Ws, na.rm = TRUE),
    sd_Ws = sd(Ws, na.rm = TRUE),
    se_Ws = sd_Ws / sqrt(n()),
    ci_Ws = qt(0.975, df=n()-1) * se_Ws
  )

#TABLES
kable(hourly_df, digits = 2, caption = "Hourly average each day")
kable(avg_hourly_df, digits = 2, caption = "Hourly average of all days")
kable(daily_df, digits = 2, caption = "Daily average each day")
kable(avg_daily_df, digits = 2, caption = "Daily average of all days")

inside_var <- hourly_df

#PLOTS

plot_Tadb <- ggplot(avg_hourly_df, aes(x = hour, y = avg_Tadb)) +
  geom_linerange(aes(ymin = avg_Tadb - sd_Tadb, ymax = avg_Tadb + sd_Tadb), linewidth = 0.5, colour = "#D53E4F") +
  geom_point(shape = 21, colour = "#D53E4F", fill = "white", size = 1, stroke = 1, na.rm = TRUE) +
  labs(x = "Hour", 
       y = "°C", 
       title = "Aspirated dry-bulb temperature") +
  theme_bw() +
  coord_cartesian(xlim = c(9.9, 17.1), ylim = c(20, 32)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  geom_line(data = hourly_df, aes(x = hour, y = Tadb, group = date), alpha = 0.5, color = "gray")
print(plot_Tadb)

plot_Tg <- ggplot(avg_hourly_df, aes(x = hour, y = avg_Tg)) +
  geom_linerange(aes(ymin = avg_Tg - sd_Tg, ymax = avg_Tg + sd_Tg), size = 0.5, colour = "#525252") +
  geom_point(shape = 21, colour = "#525252", fill = "white", size = 1, stroke = 1, na.rm = TRUE) +
  labs(x = "Hour", 
       y = "°C", 
       title = "Globe temperature") +
  theme_bw() +
  coord_cartesian(xlim = c(9.9, 17.1), ylim = c(20, 32)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
print(plot_Tg)

plot_Tnwb <- ggplot(avg_hourly_df, aes(x = hour, y = avg_Tnwb)) +
  geom_linerange(aes(ymin = avg_Tnwb - sd_Tnwb, ymax = avg_Tnwb + sd_Tnwb), size = 0.5, colour = "#5E4FA2") +
  geom_point(shape = 21, colour = "#5E4FA2", fill = "white", size = 1, stroke = 1, na.rm = TRUE) +
  labs(x = "Hour", 
       y = "°C", 
       title = "Natural wet-bulb temperature") +
  theme_bw() +
  coord_cartesian(xlim = c(9.9, 17.1), ylim = c(20, 32)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
print(plot_Tnwb)

plot_WBGT <- ggplot(avg_hourly_df, aes(x = hour, y = avg_WBGT)) +
  geom_linerange(aes(ymin = avg_WBGT - sd_WBGT, ymax = avg_WBGT + sd_WBGT), size = 0.5, colour = "#F46D43") +
  geom_point(shape = 21, colour = "#F46D43", fill = "white", size = 1, stroke = 1, na.rm = TRUE) +
  labs(x = "Workday (hour)", 
       y = "°C", 
       title = "Wet-bulb globe temperature") +
  theme_bw() +
  coord_cartesian(xlim = c(9.9, 17.1), ylim = c(20, 32)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
print(plot_WBGT)

plot_RH <- ggplot(avg_hourly_df, aes(x = hour, y = avg_RH)) +
  geom_linerange(aes(ymin = avg_RH - sd_RH, ymax = avg_RH + sd_RH), size = 0.5, colour = "#4292C6") +
  geom_point(shape = 21, colour = "#4292C6", fill = "white", size = 1, stroke = 1, na.rm = TRUE) +
  labs(x = "Hour", 
       y = "%", 
       title = "Relative Humidity") +
  theme_bw() +
  coord_cartesian(xlim = c(9.9, 17.1), ylim = c(40, 60)) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
print(plot_RH)

avg_hourly_df$avg_AirSpeed <- rep(3.75, nrow(avg_hourly_df))
avg_hourly_df$sd_AirSpeed <- rep(0.25, nrow(avg_hourly_df))

plot_Vair <- ggplot(avg_hourly_df, aes(x = hour, y = avg_AirSpeed)) +
  geom_linerange(aes(ymin = avg_AirSpeed - sd_AirSpeed, ymax = avg_AirSpeed + sd_AirSpeed), size = 0.5, colour = "#66C2A5") +
  geom_point(shape = 21, colour = "#66C2A5", fill = "white", size = 1, stroke = 1, na.rm = TRUE) +
  labs(x = "Hour", 
       y = "m/s", 
       title = "Air Speed") +
  theme_bw() +
  coord_cartesian(xlim = c(9.9, 17.1), ylim = c(3, 4.2)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
print(plot_Vair)

combined_plot <- plot_Tadb / plot_Tg / plot_Tnwb / plot_WBGT / plot_Vair / plot_RH 

print(combined_plot)

ggsave(filename = "oasis.tiff", 
       plot = ggplot2::last_plot(), 
       width = 5, height = 7, units = "in",
       device='tiff', dpi=300)

#OUTSIDE########################################################################

combined_df <- read.csv("./data/emu.csv")

combined_df <- combined_df %>% filter(location_code == 2)

combined_df$time <- as.POSIXct(combined_df$time, format = "%H:%M:%S")

combined_df$hour <- hour(combined_df$time) + ifelse(minute(combined_df$time) >= 30, 1, 0)
combined_df$hour[combined_df$hour == 24] <- 0

#TABLES

hourly_df <- combined_df %>% 
  group_by(day, hour) %>% 
  summarise(
    avg_Tg = mean(Tg, na.rm = TRUE),
    sd_Tg = sd(Tg, na.rm = TRUE),
    se_Tg = sd_Tg / sqrt(n()),
    ci_Tg = qt(0.975, df=n()-1) * se_Tg,
    
    avg_RH = mean(RH, na.rm = TRUE),
    sd_RH = sd(RH, na.rm = TRUE),
    se_RH = sd_RH / sqrt(n()),
    ci_RH = qt(0.975, df=n()-1) * se_RH,
    
    avg_Tadb = mean(Tadb, na.rm = TRUE),
    sd_Tadb = sd(Tadb, na.rm = TRUE),
    se_Tadb = sd_Tadb / sqrt(n()),
    ci_Tadb = qt(0.975, df=n()-1) * se_Tadb,
    
    avg_Tnwb = mean(Tnwb, na.rm = TRUE),
    sd_Tnwb = sd(Tnwb, na.rm = TRUE),
    se_Tnwb = sd_Tnwb / sqrt(n()),
    ci_Tnwb = qt(0.975, df=n()-1) * se_Tnwb,
    
    avg_WBGT = mean(WBGT, na.rm = TRUE),
    sd_WBGT = sd(WBGT, na.rm = TRUE),
    se_WBGT = sd_WBGT / sqrt(n()),
    ci_WBGT = qt(0.975, df=n()-1) * se_WBGT,
    
    avg_Ws = mean(Ws, na.rm = TRUE),
    sd_Ws = sd(Ws, na.rm = TRUE),
    se_Ws = sd_Ws / sqrt(n()),
    ci_Ws = qt(0.975, df=n()-1) * se_Ws,
    .groups = "drop"
  )

avg_hourly_df <- combined_df %>% 
  group_by(hour) %>% 
  summarise(
    avg_Tg = mean(Tg, na.rm = TRUE),
    sd_Tg = sd(Tg, na.rm = TRUE),
    se_Tg = sd_Tg / sqrt(n()),
    ci_Tg = qt(0.975, df=n()-1) * se_Tg,
    
    avg_RH = mean(RH, na.rm = TRUE),
    sd_RH = sd(RH, na.rm = TRUE),
    se_RH = sd_RH / sqrt(n()),
    ci_RH = qt(0.975, df=n()-1) * se_RH,
    
    avg_Tadb = mean(Tadb, na.rm = TRUE),
    sd_Tadb = sd(Tadb, na.rm = TRUE),
    se_Tadb = sd_Tadb / sqrt(n()),
    ci_Tadb = qt(0.975, df=n()-1) * se_Tadb,
    
    avg_Tnwb = mean(Tnwb, na.rm = TRUE),
    sd_Tnwb = sd(Tnwb, na.rm = TRUE),
    se_Tnwb = sd_Tnwb / sqrt(n()),
    ci_Tnwb = qt(0.975, df=n()-1) * se_Tnwb,
    
    avg_WBGT = mean(WBGT, na.rm = TRUE),
    sd_WBGT = sd(WBGT, na.rm = TRUE),
    se_WBGT = sd_WBGT / sqrt(n()),
    ci_WBGT = qt(0.975, df=n()-1) * se_WBGT,
    
    avg_Ws = mean(Ws, na.rm = TRUE),
    sd_Ws = sd(Ws, na.rm = TRUE),
    se_Ws = sd_Ws / sqrt(n()),
    ci_Ws = qt(0.975, df=n()-1) * se_Ws
  )

daily_df <- combined_df %>% 
  filter(sapply(time, is_within_five_minutes)) %>%
  group_by(day) %>%
  summarise(
    avg_Tg = mean(Tg),
    sd_Tg = sd(Tg),
    se_Tg = sd_Tg / sqrt(n()),
    ci_Tg = qt(0.975, df = n() - 1) * se_Tg,
    
    avg_RH = mean(RH),
    sd_RH = sd(RH),
    se_RH = sd_RH / sqrt(n()),
    ci_RH = qt(0.975, df = n() - 1) * se_RH,
    
    avg_Tadb = mean(Tadb),
    sd_Tadb = sd(Tadb),
    se_Tadb = sd_Tadb / sqrt(n()),
    ci_Tadb = qt(0.975, df = n() - 1) * se_Tadb,
    
    avg_Tnwb = mean(Tnwb),
    sd_Tnwb = sd(Tnwb),
    se_Tnwb = sd_Tnwb / sqrt(n()),
    ci_Tnwb = qt(0.975, df = n() - 1) * se_Tnwb,
    
    avg_WBGT = mean(WBGT),
    sd_WBGT = sd(WBGT),
    se_WBGT = sd_WBGT / sqrt(n()),
    ci_WBGT = qt(0.975, df = n() - 1) * se_WBGT,
    
    avg_Ws = mean(Ws, na.rm = TRUE),
    sd_Ws = sd(Ws, na.rm = TRUE),
    se_Ws = sd_Ws / sqrt(n()),
    ci_Ws = qt(0.975, df=n()-1) * se_Ws
  )

avg_daily_df <- combined_df %>% 
  filter(sapply(time, is_within_five_minutes)) %>% 
  summarise(
    avg_Tg = mean(Tg),
    sd_Tg = sd(Tg),
    se_Tg = sd_Tg / sqrt(n()),
    ci_Tg = qt(0.975, df = n() - 1) * se_Tg,
    
    avg_RH = mean(RH),
    sd_RH = sd(RH),
    se_RH = sd_RH / sqrt(n()),
    ci_RH = qt(0.975, df = n() - 1) * se_RH,
    
    avg_Tadb = mean(Tadb),
    sd_Tadb = sd(Tadb),
    se_Tadb = sd_Tadb / sqrt(n()),
    ci_Tadb = qt(0.975, df = n() - 1) * se_Tadb,
    
    avg_Tnwb = mean(Tnwb),
    sd_Tnwb = sd(Tnwb),
    se_Tnwb = sd_Tnwb / sqrt(n()),
    ci_Tnwb = qt(0.975, df = n() - 1) * se_Tnwb,
    
    avg_WBGT = mean(WBGT),
    sd_WBGT = sd(WBGT),
    se_WBGT = sd_WBGT / sqrt(n()),
    ci_WBGT = qt(0.975, df = n() - 1) * se_WBGT,
    
    avg_Ws = mean(Ws, na.rm = TRUE),
    sd_Ws = sd(Ws, na.rm = TRUE),
    se_Ws = sd_Ws / sqrt(n()),
    ci_Ws = qt(0.975, df=n()-1) * se_Ws
  )

#TABLES

kable(hourly_df, digits = 2, caption = "Hourly average each day")
kable(avg_hourly_df, digits = 2, caption = "Hourly average of all days")
kable(daily_df, digits = 2, caption = "Daily average each day")
kable(avg_daily_df, digits = 2, caption = "Daily average of all days")

outside_var <- hourly_df

#PLOTS

plot_Tadb <- ggplot(avg_hourly_df, aes(x = hour, y = avg_Tadb)) +
  geom_linerange(aes(ymin = avg_Tadb - sd_Tadb, ymax = avg_Tadb + sd_Tadb), size = 0.5, colour = "#D53E4F") +
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

plot_Tg <- ggplot(avg_hourly_df, aes(x = hour, y = avg_Tg)) +
  geom_linerange(aes(ymin = avg_Tg - sd_Tg, ymax = avg_Tg + sd_Tg), size = 0.5, colour = "#525252") +
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

plot_Tnwb <- ggplot(avg_hourly_df, aes(x = hour, y = avg_Tnwb)) +
  geom_linerange(aes(ymin = avg_Tnwb - sd_Tnwb, ymax = avg_Tnwb + sd_Tnwb), size = 0.5, colour = "#5E4FA2") +
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

plot_WBGT <- ggplot(avg_hourly_df, aes(x = hour, y = avg_WBGT)) +
  geom_linerange(aes(ymin = avg_WBGT - sd_WBGT, ymax = avg_WBGT + sd_WBGT), size = 0.5, colour = "#F46D43") +
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

plot_Vair <- ggplot(avg_hourly_df, aes(x = hour, y = avg_Ws)) +
  geom_linerange(aes(ymin = avg_Ws - sd_Ws, ymax = avg_Ws + sd_Ws), size = 0.5, colour = "#66C2A5") +
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

plot_RH <- ggplot(avg_hourly_df, aes(x = hour, y = avg_RH)) +
  geom_linerange(aes(ymin = avg_RH - sd_RH, ymax = avg_RH + sd_RH), size = 0.5, colour = "#4292C6") +
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