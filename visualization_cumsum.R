library(dplyr)
library(scales)

# Step 1: Prepare cumulative data
cumulative_df <- format1 %>%
  arrange(Date) %>%
  mutate(Cumulative = cumsum(Number))

# Normalize cumulative data to fit the primary y-axis (0–150)
max_y <- 150
cumulative_df <- cumulative_df %>%
  mutate(Cumulative_scaled = Cumulative / max(Cumulative, na.rm = TRUE) * max_y)

# Step 2: Add to your existing plot
fig2 <- ggplot(format1 %>% filter(abs(Residual) >= 5), aes(x = Date, y = Immission)) +
  geom_point(aes(size = abs(Residual)), shape = 21, fill = "gray70", colour = "black", alpha = 0.7) +
  geom_smooth(data = format1, aes(x = Date, y = Immission), method = "gam",
              formula = y ~ s(x, bs = "cs"), colour = "black", fill = "gray50",
              alpha = 0.4, linewidth = 1, se = TRUE) +
  geom_line(data = cumulative_df, aes(x = Date, y = Cumulative_scaled), 
            color = "darkred", linewidth = 1) +
  scale_size_continuous(
    range = c(0.1, 6),
    name = "Abs(Residual)",
    breaks = c(5, 10, 25, 50, 75)
  ) +
  geom_vline(xintercept = policy1, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_vline(xintercept = policy2, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_vline(xintercept = policy3, linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = policy1, hjust = 1, y = max_y - 5,
           label = "Start of air policy (04/10/1991)", angle = 90, vjust = -0.5, size = 4, color = "black") +
  annotate("text", x = policy2, hjust = 1, y = max_y - 5,
           label = "Regulation update (01/06/2002)", angle = 90, vjust = -0.5, size = 4, color = "black") +
  annotate("text", x = policy3, hjust = 1, y = max_y - 5,
           label = "Air protection act (01/09/2012)", angle = 90, vjust = -0.5, size = 4, color = "black") +
  scale_y_continuous(
    name = "SO₂ pollution [μg·m⁻³]",
    limits = c(0, max_y),
    breaks = seq(0, max_y, by = 20),
    expand = expansion(add = c(5, 0)),
    sec.axis = sec_axis(~ . * max(cumulative_df$Cumulative, na.rm = TRUE) / max_y,
                        name = "Cumulative abundance")
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
    axis.ticks.length = unit(5, "pt")
  ) +
  labs(x = "Year")
