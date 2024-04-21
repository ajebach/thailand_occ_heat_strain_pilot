library(splines)
library(lme4)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(mgcv.helper)
library(mgcv)
library(dplyr)
library(emmeans)
library(patchwork)

#CORE TEMP######################################################################

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

df1 <- read.csv("./data/core.csv")

df1$time <- hms(df1$time)
df1$minutes <- hour(df1$time)*60 + minute(df1$time) + second(df1$time)/60

df1 <- df1 %>% filter(minutes >= 8*60)

df1$interval <- floor(df1$minutes / 5) * 5

df2 <- df1 %>%
  group_by(day, id, cond, interval) %>%
  summarise(avg_tc = mean(tc, na.rm = TRUE), .groups = "drop")

df3 <- df2 %>%
  group_by(cond, interval) %>%
  summarise(avg_tc = mean(avg_tc, na.rm = TRUE), .groups = "drop") %>%
  select(cond, interval, avg_tc)

df1 <- df1 %>%
  mutate(cond = relevel(as.factor(cond), ref = 'vest'))

fit <- gam(tc ~ s(interval, by = cond, k = 21) + cond + avg5_WBGT, data = df1)
summary(fit)

ref <- ref_grid(fit, at = list(interval = seq(min(df1$interval), max(df1$interval), by = 1),
                               cond = c('con','oasis','vest'), type = 'transform'))

plot_data_tc <- emmeans(ref, ~ interval|cond, conf.level = 0.95) %>%
  as_tibble()

emm_output <- emmeans(ref, ~ interval|cond)
conf_data <- as.data.frame(confint(emm_output, level = 0.95))

my_palette <- c("con" = "#D53E4F", "vest" = "#41AB5D", "oasis" = "#3288BD")
x_breaks <- seq(from = 480, to = 1020, by = 60)
x_limits <- c(480, 1020)
y_breaks <- seq(from = 36.8, to = 37.8, by = 0.2)
y_limits <- c(36.8, 37.85)
y_up <- y_limits[1] + 0.1 * (y_limits[2] - y_limits[1])

tc_all <- ggplot(data = plot_data_tc, aes(x = interval, y = emmean, colour = cond)) +
  geom_rect(aes(xmin = 12*60, xmax = 13*60, ymin = -Inf, ymax = Inf), fill = "#F0F0F0", alpha = 0.1, linetype = 'blank', show.legend = FALSE) +
  geom_line(data = df3, aes(y = avg_tc, colour = cond, group = cond), linetype = "solid", alpha = 0.3, linewidth = 0.4) +
  geom_point(data = df3, aes(y = avg_tc, colour = cond), size = 0.5) +
  labs(title = "A",
       x = "Time",
       y = "Core Temperature (°C)") +
  scale_x_continuous(expand = c(0, 0), breaks = x_breaks, limits = x_limits, labels = function(x) sprintf("%02d:%02d", x %/% 60, x %% 60)) +
  scale_y_continuous(expand = c(0, 0), breaks = y_breaks, limits = y_limits) +
  geom_segment(data = data.frame(xstart = seq(10*60, 16*60, by = 60), 
                                 ystart = y_limits[1], 
                                 xend = seq(10*60, 16*60, by = 60), 
                                 yend = y_up),
               aes(x = xstart, y = ystart, xend = xend, yend = yend), 
               linetype = "dashed", colour = "#525252") +
  annotate("text", x = seq(10*60, 16*60, by = 60) - 10, y = 37.2, label = "Cooling", angle = 45, vjust = 5.2, size = 3, colour = "#525252", fontface = "bold") +
  annotate("text", x = (12*60 + 13*60) / 2, y = 37.73, label = "Lunch Break", size = 3, colour = "#525252", fontface = "bold") +
  scale_colour_manual(aesthetics = c("colour"), 
                      values = c(my_palette), 
                      name = "", 
                      breaks = c("con", "vest", "oasis"),
                      labels = c("Control", "Vest", "Oasis")) +
  theme_bw() +
  theme(legend.position = c(0.01, 0.98),
        legend.justification = c(0, 0.98),
        legend.key.size = unit(0.5, "line"),
        legend.key.width = unit(1, "cm"),
        legend.direction = "horizontal",
        legend.box.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(hjust = -0.06),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        axis.text.x = element_text(angle = 45, hjust = 1))
print(tc_all)

dsub_tc <- df1 %>% 
  filter(interval > 775)

post_lunch_vest <- gam(tc ~ s(interval, by = cond, k = 14) + cond + avg5_WBGT, data = dsub_tc)
summary(post_lunch_vest)

dsub_tc <- dsub_tc %>%
  mutate(cond = relevel(cond, ref = 'oasis'))

post_lunch_oasis <- gam(tc ~ s(interval, by = cond, k = 14) + cond + avg5_WBGT, data = dsub_tc)
summary(post_lunch_oasis)

print(confint(post_lunch_vest, level = 0.95))
print(confint(post_lunch_oasis, level = 0.95))

ref <- ref_grid(post_lunch_oasis, at = list(interval = seq(min(dsub_tc$interval), max(dsub_tc$interval), by = 1),
                                            cond = c('con', 'oasis', 'vest')))

plot_data_tc_lunch <- emmeans(ref, ~ interval | cond) %>%
  as_tibble()

df3_lunch <- df3 %>% 
  filter(interval > 775)

x_breaks2 <- seq(from = 780, to = 1020, by = 60)
x_limits2 <- c(770, 1020)
mid_y <- mean(c(36.8, 37.85))

plot_data_tc_lunch$cond <- factor(plot_data_tc_lunch$cond, levels = c("con", "vest", "oasis"))
df3_lunch$cond <- factor(df3_lunch$cond, levels = c("con", "vest", "oasis"))

tc_lunch <- plot_data_tc_lunch %>%
  ggplot(aes(x = interval, y = emmean, colour = cond))+
  geom_rect(aes(xmin = 12*60+50, xmax = 13*60, ymin = -Inf, ymax = Inf), fill = "#F0F0F0", alpha = 0.1, linetype = 'blank', show.legend = FALSE) +
  geom_line()+
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = cond), alpha = 0.2, linetype = "blank", show.legend = FALSE) +
  geom_line(data = df3_lunch, aes(y = avg_tc, colour = cond, group = cond), linetype = "solid", alpha = 0.3, linewidth = 0.4) +
  geom_point(data = df3_lunch, aes(y = avg_tc, colour = cond), alpha = 0.6, size = 0.5) +
  labs(title = "C",
       x = "Time",
       y = "Core Temperature (°C)") +
  scale_x_continuous(expand = c(0, 0), breaks = x_breaks2, limits = x_limits2, labels = function(x) sprintf("%02d:%02d", x %/% 60, x %% 60)) +
  scale_y_continuous(expand = c(0, 0), breaks = y_breaks, limits = y_limits) +
  geom_segment(data = data.frame(xstart = seq(780, 960, by = 60),
                                 ystart = y_limits[1],
                                 xend = seq(780, 960, by = 60),
                                 yend = y_up),
               aes(x = xstart, y = ystart, xend = xend, yend = yend),
               linetype = "dashed", colour = "#525252") +
  annotate("text", x = seq(780, 960, by = 60) - 10, y = 37.2,
           label = "Cooling", angle = 45, vjust = 5.2, size = 3,
           colour = "#525252", fontface = "bold") +
  annotate("text", x = (13*60-10 + 13*60) / 2, y = mid_y, label = "Lunch Break", size = 2.8, colour = "#525252", fontface = "bold", angle = 90, hjust = 0.5) +
  scale_colour_manual(aesthetics = c("colour"),
                      values = c(my_palette),
                      name = "",
                      breaks = c("con", "vest", "oasis"),
                      labels = c("Control", "Vest", "Oasis")) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = -0.133),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        axis.text.x = element_text(angle = 45, hjust = 1))
print(tc_lunch)

#HEART RATE#####################################################################

df5 <- read.csv("./data/hr.csv")

df5_means <- df5 %>%
  group_by(interval, cond) %>%
  summarise(avg_hr = mean(avg_hr, na.rm = TRUE), .groups = "drop")

df5$cond = as.factor(df5$cond)

df5 <- df5 %>%
  mutate(cond = relevel(cond, ref = 'vest'))

fit <- gam(avg_hr ~ s(interval, by = cond, k = 24) + cond+ avg5_WBGT, data = df5)

summary(fit)

ref <- ref_grid(fit, at = list(interval = seq(min(df5$interval), max(df5$interval), by = 1),
                               cond = c('con', 'oasis', 'vest'), type = 'transform'))

plot_data_hr <- emmeans(ref, ~ interval|cond) %>%
  as_tibble()

y_breaks2 <- seq(from = 60, to = 120, by = 10)
y_limits2 <- c(60, 120)
y_up2 <- y_limits2[1] + 0.1 * (y_limits2[2] - y_limits2[1])

hr_all <-  ggplot(data = plot_data_hr, aes(x = interval, y = emmean, colour = cond)) +
  geom_rect(aes(xmin = 12*60, xmax = 13*60, ymin = -Inf, ymax = Inf), fill = "#F0F0F0", alpha = 0.1, linetype = 'blank', show.legend = FALSE) +
  geom_line(data = df5_means, aes(y = avg_hr, colour = cond, group = cond), linetype = "solid", alpha = 0.3, linewidth = 0.4) +
  geom_point(data = df5_means, aes(y = avg_hr, colour = cond), size = 0.5) +
  labs(title = "B",
       x = "Time",
       y = "Heart Rate (bpm)") +
  scale_x_continuous(expand = c(0, 0), breaks = x_breaks, limits = x_limits, labels = function(x) sprintf("%02d:%02d", x %/% 60, x %% 60)) +
  scale_y_continuous(expand = c(0, 0), breaks = y_breaks2, limits = y_limits2) +
  geom_segment(data = data.frame(xstart = seq(10*60, 16*60, by = 60), 
                                 ystart = y_limits2[1], 
                                 xend = seq(10*60, 16*60, by = 60), 
                                 yend = y_up2),
               aes(x = xstart, y = ystart, xend = xend, yend = yend), 
               linetype = "dashed", colour = "#525252") +
  annotate("text", x = seq(10*60, 16*60, by = 60) - 10, y = 83, label = "Cooling", angle = 45, vjust = 5.2, size = 3, colour = "#525252", fontface = "bold") +
  annotate("text", x = (12*60 + 13*60) / 2, y = 113, label = "Lunch Break", size = 3, colour = "#525252", fontface = "bold") +
  scale_colour_manual(aesthetics = c("colour"), 
                      values = c(my_palette), 
                      name = "", 
                      breaks = c("con", "vest", "oasis"),
                      labels = c("Control", "Vest", "Oasis")) +
  theme_bw() +
  theme(legend.position = c(0.01, 0.98),
        legend.justification = c(0, 0.98),
        legend.key.size = unit(0.5, "line"),
        legend.key.width = unit(1, "cm"),
        legend.direction = "horizontal",
        legend.box.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(hjust = -0.06),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        axis.text.x = element_text(angle = 45, hjust = 1))
print(hr_all)

dsub_hr <- df5 %>% 
  filter(interval > 775)

post_lunch_vest <- gam(avg_hr ~ s(interval, by = cond, k = 14) + cond + avg5_WBGT, data = dsub_hr)

summary(post_lunch_vest)

dsub_hr <- dsub_hr %>%
  mutate(cond = relevel(cond, ref = 'oasis'))

post_lunch_oasis <- gam(avg_hr ~ s(interval, by = cond, k = 18) + cond + avg5_WBGT, data = dsub_hr)
summary(post_lunch_oasis)

print(confint.gam(post_lunch_vest, level = 0.95))
print(confint.gam(post_lunch_oasis, level = 0.95))

ref <- ref_grid(post_lunch_oasis, at = list(interval = seq(min(dsub_hr$interval), max(dsub_hr$interval), by = 1),
                                      cond = c('con', 'oasis', 'vest'), type = 'transform'))

plot_data_hr_lunch <- emmeans(ref, ~ interval|cond) %>%
  as_tibble()

df5_means_lunch <- df5_means %>%
  filter(interval > 775)

mid_y <- mean(c(60, 120))

plot_data_hr_lunch$cond <- factor(plot_data_hr_lunch$cond, levels = c("con", "vest", "oasis"))
df5_means_lunch$cond <- factor(df5_means_lunch$cond, levels = c("con", "vest", "oasis"))


hr_lunch <- plot_data_hr_lunch %>%
  ggplot(aes(x = interval, y = emmean, colour = cond))+
  geom_rect(aes(xmin = 12*60+50, xmax = 13*60, ymin = -Inf, ymax = Inf), fill = "#F0F0F0", alpha = 0.1, linetype = 'blank', show.legend = FALSE) +
  geom_line()+
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = cond), alpha = 0.2, linetype = "blank")+
  geom_line(data = df5_means_lunch, aes(y = avg_hr, colour = cond, group = cond), linetype = "solid", alpha = 0.3, linewidth = 0.4) +
  geom_point(data = df5_means_lunch, aes(y = avg_hr, colour = cond), alpha = 0.6, size = 0.5) +
  labs(title = "D",
       x = "Time",
       y = "Heart Rate (bpm)") +
  scale_x_continuous(expand = c(0, 0), breaks = x_breaks2, limits = x_limits2, labels = function(x) sprintf("%02d:%02d", x %/% 60, x %% 60)) +
  scale_y_continuous(expand = c(0, 0), breaks = y_breaks2, limits = y_limits2) +
  geom_segment(data = data.frame(xstart = seq(780, 960, by = 60),  # Modified xstart values
                                 ystart = y_limits2[1], 
                                 xend = seq(780, 960, by = 60),  # Modified xend values
                                 yend = y_up2),
               aes(x = xstart, y = ystart, xend = xend, yend = yend), 
               linetype = "dashed", colour = "#525252") +
  annotate("text", x = seq(780, 960, by = 60) - 10, y = 83,  # Modified x values
           label = "Cooling", angle = 45, vjust = 5.2, size = 3,
           colour = "#525252", fontface = "bold") +
  annotate("text", x = (13*60-10 + 13*60) / 2, y = mid_y, label = "Lunch Break", size = 2.8, colour = "#525252", fontface = "bold", angle = 90, hjust = 0.5) +
  scale_colour_manual(aesthetics = c("colour"), 
                      values = c(my_palette), 
                      name = "", 
                      breaks = c("con", "vest", "oasis"),
                      labels = c("Control", "Vest", "Oasis")) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = -0.133),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        axis.text.x = element_text(angle = 45, hjust = 1))
print(hr_lunch)

#COMBINE PLOTS##################################################################

plot_data_tc_lunch <- plot_data_tc_lunch %>% filter(interval > 12*60)

tc_combined <- ggplot(data = plot_data_tc, aes(x = interval, y = emmean, colour = cond)) +
  geom_rect(aes(xmin = 12*60, xmax = 13*60, ymin = -Inf, ymax = Inf), fill = "#F0F0F0", alpha = 0.1, linetype = 'blank', show.legend = FALSE) +
  geom_line(data = df3, aes(y = avg_tc, colour = cond, group = cond), linetype = "solid", alpha = 0.3, linewidth = 0.4) +
  geom_point(data = df3, aes(y = avg_tc, colour = cond), size = 0.5) +
  geom_line(data = plot_data_tc_lunch, aes(y = emmean), show.legend = FALSE) +
  geom_ribbon(data = plot_data_tc_lunch, aes(ymin = lower.CL, ymax = upper.CL, fill = cond), alpha = 0.2, linetype = "blank", show.legend = FALSE) +
  labs(title = "A",
       x = "Time",
       y = "Core Temperature (°C)") +
  scale_x_continuous(expand = c(0, 0), breaks = x_breaks, limits = x_limits, labels = function(x) sprintf("%02d:%02d", x %/% 60, x %% 60)) +
  scale_y_continuous(expand = c(0, 0), breaks = y_breaks, limits = y_limits) +
  geom_segment(data = data.frame(xstart = seq(10*60, 16*60, by = 60), 
                                 ystart = y_limits[1], 
                                 xend = seq(10*60, 16*60, by = 60), 
                                 yend = y_up),
               aes(x = xstart, y = ystart, xend = xend, yend = yend), 
               linetype = "dotted", colour = "#525252") +
  annotate("text", x = seq(10*60, 16*60, by = 60) - 10, y = 37.1, label = "Cooling", angle = 45, vjust = 5.2, size = 3, colour = "#525252", fontface = "bold") +
  annotate("text", x = (12*60 + 13*60) / 2, y = 37.775, label = "Lunch Break", size = 3, colour = "#525252", fontface = "bold") +
  scale_colour_manual(aesthetics = c("colour"), 
                      values = c(my_palette), 
                      name = "", 
                      breaks = c("con", "vest", "oasis"),
                      labels = c("Control", "Vest", "Oasis")) +
  theme_bw() +
  theme(legend.position = c(0.01, 0.98),
        legend.justification = c(0, 0.98),
        legend.key.size = unit(0.5, "line"),
        legend.key.width = unit(1, "cm"),
        legend.direction = "horizontal",
        legend.box.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(hjust = -0.06),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        axis.text.x = element_text(angle = 45, hjust = 1))
print(tc_combined)

plot_data_hr_lunch <- plot_data_hr_lunch %>% filter(interval > 12*60)

hr_combined <- ggplot(data = plot_data_hr, aes(x = interval, y = emmean, colour = cond)) +
  geom_rect(aes(xmin = 12*60, xmax = 13*60, ymin = -Inf, ymax = Inf), fill = "#F0F0F0", alpha = 0.1, linetype = 'blank', show.legend = FALSE) +
  geom_line(data = plot_data_hr_lunch, aes(y = emmean), show.legend = FALSE) +
  geom_ribbon(data = plot_data_hr_lunch, aes(ymin = lower.CL, ymax = upper.CL, fill = cond), alpha = 0.2, linetype = "blank", show.legend = FALSE) +
  geom_line(data = df5_means, aes(y = avg_hr, colour = cond, group = cond), linetype = "solid", alpha = 0.3, linewidth = 0.4) +
  geom_point(data = df5_means, aes(y = avg_hr, colour = cond), size = 0.5) +
  labs(title = "B", 
       x = "Time", 
       y = "Heart Rate (bpm)") +
  scale_x_continuous(expand = c(0, 0), breaks = x_breaks, limits = x_limits, labels = function(x) sprintf("%02d:%02d", x %/% 60, x %% 60)) +
  scale_y_continuous(expand = c(0, 0), breaks = y_breaks2, limits = y_limits2) +
  geom_segment(data = data.frame(xstart = seq(10*60, 16*60, by = 60), 
                                 ystart = y_limits2[1], 
                                 xend = seq(10*60, 16*60, by = 60), 
                                 yend = y_up2),
               aes(x = xstart, y = ystart, xend = xend, yend = yend), 
               linetype = "dotted", colour = "#525252") +
  annotate("text", x = seq(10*60, 16*60, by = 60) - 10, y = 77, label = "Cooling", angle = 45, vjust = 5.2, size = 3, colour = "#525252", fontface = "bold") +
  annotate("text", x = (12*60 + 13*60) / 2, y = 116, label = "Lunch Break", size = 3, colour = "#525252", fontface = "bold") +
  scale_colour_manual(aesthetics = c("colour"), 
                      values = c(my_palette), 
                      name = "", 
                      breaks = c("con", "vest", "oasis"),
                      labels = c("Control", "Vest", "Oasis")) +
  theme_bw() +
  theme(legend.position = c(0.01, 0.98),
        legend.justification = c(0, 0.98),
        legend.key.size = unit(0.5, "line"),
        legend.key.width = unit(1, "cm"),
        legend.direction = "horizontal",
        legend.box.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(hjust = -0.06),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        axis.text.x = element_text(angle = 45, hjust = 1))
print(hr_combined)

print(hr_combined)

v1 <- (tc_all / hr_all) / (tc_lunch | hr_lunch)
print(v1)
ggsave(filename = "version1.png",
       plot = v1,
       device = "png",
       dpi = 600,
       width = 9,
       height = 8,
       units = "in")

v2 <- (tc_combined / hr_combined)
print(v2)
ggsave(filename = "version2.png", 
       plot = v2, 
       device = "png", 
       dpi = 600, 
       width = 8, 
       height = 7, 
       units = "in")
