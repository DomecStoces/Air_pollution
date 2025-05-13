#Plotting Year x Temperature
fig3 <- ggplot(filtered_data, aes(x = Date, y = T)) +
  geom_point(aes(size = Fitted), shape = 21, fill = "gray70", colour = "black", alpha = 0.7) +
  geom_smooth(data = format1, aes(x = Date, y = T),
              method = "gam", formula = y ~ s(x, bs = "cs"),
              colour = "black", fill = "gray50", alpha = 0.4,
              linewidth = 1, se = TRUE) +
  scale_size_continuous(
    range = c(0.1, 5),
    name = "Predicted abundance",
    breaks = c(3,4,5)
  ) +
  geom_vline(xintercept = policy1, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_vline(xintercept = policy2, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_vline(xintercept = policy3, linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = policy1, hjust = 1, y = max(format1$Immission, na.rm = TRUE) - 5,
           label = "Start of air policy (04/10/1991)", angle = 90, vjust = -0.5, size = 4, color = "black") +
  annotate("text", x = policy2, hjust = 1, y = max(format1$Immission, na.rm = TRUE) - 5,
           label = "Regulation update (01/06/2002)", angle = 90, vjust = -0.5, size = 4, color = "black") +
  annotate("text", x = policy3, hjust = 1, y = max(format1$Immission, na.rm = TRUE) - 5,
           label = "Air protection act (01/09/2012)", angle = 90, vjust = -0.5, size = 4, color = "black") +
  scale_y_continuous(
    limits = c(0, 150),
    breaks = seq(0, 150, by = 20),
    expand = expansion(add = c(5, 0))
  ) +
  scale_x_date(
    date_breaks = "3 years",
    date_labels = "%Y",
    expand = expansion(add = c(250, 0))
  ) +
  theme_minimal(base_size = 15, base_family = "Calibri") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(5, "pt")) +
  labs(
    x = "Year",
    y = "Temperature [°C]")
fig3
tiff("Temperatur_predictions.tiff", 
     width = 15, height = 10,     
     units = "in",                  
     res = 600,                     
     compression = "lzw")           
fig3
dev.off()

#Plotting Year x Precipitation 
fig4 <- ggplot(filtered_data, aes(x = Date, y = Precipitation)) +
  geom_point(aes(size = Fitted), shape = 21, fill = "gray70", colour = "black", alpha = 0.7) +
  geom_smooth(data = format1, aes(x = Date, y = Precipitation),
              method = "gam", formula = y ~ s(x, bs = "cs"),
              colour = "black", fill = "gray50", alpha = 0.4,
              linewidth = 1, se = TRUE) +
  scale_size_continuous(
    range = c(0.1, 5),
    name = "Predicted abundance",
    breaks = c(3,4,5)
  ) +
  geom_vline(xintercept = policy1, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_vline(xintercept = policy2, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_vline(xintercept = policy3, linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = policy1, hjust = 1, y = max(format1$Immission, na.rm = TRUE) - 5,
           label = "Start of air policy (04/10/1991)", angle = 90, vjust = -0.5, size = 4, color = "black") +
  annotate("text", x = policy2, hjust = 1, y = max(format1$Immission, na.rm = TRUE) - 5,
           label = "Regulation update (01/06/2002)", angle = 90, vjust = -0.5, size = 4, color = "black") +
  annotate("text", x = policy3, hjust = 1, y = max(format1$Immission, na.rm = TRUE) - 5,
           label = "Air protection act (01/09/2012)", angle = 90, vjust = -0.5, size = 4, color = "black") +
  scale_y_continuous(
    limits = c(0, 150),
    breaks = seq(0, 150, by = 20),
    expand = expansion(add = c(5, 0))
  ) +
  scale_x_date(
    date_breaks = "3 years",
    date_labels = "%Y",
    expand = expansion(add = c(250, 0))
  ) +
  theme_minimal(base_size = 15, base_family = "Calibri") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(5, "pt")) +
  labs(
    x = "Year",
    y = "Precipitation [mm]")
fig4
tiff("Precipitation_predictions.tiff", 
     width = 15, height = 10,     
     units = "in",                  
     res = 600,                     
     compression = "lzw")           
fig4
dev.off()
#Plotting Year x Wind
fig5 <- ggplot(filtered_data, aes(x = Date, y = Wind)) +
  geom_point(aes(size = Fitted), shape = 21, fill = "gray70", colour = "black", alpha = 0.7) +
  geom_smooth(data = format1, aes(x = Date, y = Wind),
              method = "gam", formula = y ~ s(x, bs = "cs"),
              colour = "black", fill = "gray50", alpha = 0.4,
              linewidth = 1, se = TRUE) +
  scale_size_continuous(
    range = c(0.1, 5),
    name = "Predicted abundance",
    breaks = c(3,4,5)
  ) +
  geom_vline(xintercept = policy1, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_vline(xintercept = policy2, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_vline(xintercept = policy3, linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = policy1, hjust = 1, y = max(format1$Immission, na.rm = TRUE) - 5,
           label = "Start of air policy (04/10/1991)", angle = 90, vjust = -0.5, size = 4, color = "black") +
  annotate("text", x = policy2, hjust = 1, y = max(format1$Immission, na.rm = TRUE) - 5,
           label = "Regulation update (01/06/2002)", angle = 90, vjust = -0.5, size = 4, color = "black") +
  annotate("text", x = policy3, hjust = 1, y = max(format1$Immission, na.rm = TRUE) - 5,
           label = "Air protection act (01/09/2012)", angle = 90, vjust = -0.5, size = 4, color = "black") +
  scale_y_continuous(
    limits = c(0, 15),
    breaks = seq(0, 150, by = 20),
    expand = expansion(add = c(5, 0))
  ) +
  scale_x_date(
    date_breaks = "3 years",
    date_labels = "%Y",
    expand = expansion(add = c(250, 0))
  ) +
  theme_minimal(base_size = 15, base_family = "Calibri") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(5, "pt")) +
  labs(
    x = "Year",
    y = "Wind [m/s]")
fig5
tiff("Wind_predictions.tiff", 
     width = 15, height = 10,     
     units = "in",                  
     res = 600,                     
     compression = "lzw")           
fig5
dev.off()
