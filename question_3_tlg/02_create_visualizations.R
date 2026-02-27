# import libraries
library(pharmaverseadam)
library(ggplot2)
library(DescTools)

adae <- pharmaverseadam::adae

# filter for TEAEs and ensure AESEV is a factor for proper ordering
plot_data <- adae %>%
  filter(TRTEMFL == "Y") %>%
  mutate(
    # ordering severity so Mild is at the bottom and Severe is at the top
    AESEV = factor(AESEV, levels = c("MILD", "MODERATE", "SEVERE"))
  )

# bar plot creation
ae_plot <- ggplot(data = plot_data, aes(x = ACTARM, fill = AESEV)) +
  geom_bar(position = "stack", color = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = c("MILD" = "#d73027", "MODERATE" = "#228B22", "SEVERE" = "#74add1"),
    name = "Severity/Intensity"
  ) +
  # labels and formatting
  labs(
    title = "AE severity distribution by treatment",
    x = "Treatment Arm",
    y = "Count of AEs"
  ) +
  theme_minimal() +
  theme_minimal() + # Start with a clean base
  theme(
    # Set the area behind the bars to light grey
    panel.background = element_rect(fill = "#EDEDED", color = NA), 
    # Ensure the grid lines are still visible but subtle
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# ae event frequency plot
adsl <- pharmaverseadam::adsl

n_total <- n_distinct(adsl$USUBJID)

# calculate Incidence and 95% CI
ae_summary <- adae %>%
  filter(TRTEMFL == "Y") %>%
  group_by(AETERM) %>%
  summarise(n_ae = n_distinct(USUBJID)) %>%
  mutate(
    # calculate Clopper-Pearson 95% CI
    ci = BinomCI(n_ae, n_total, conf.level = 0.95, method = "clopper-pearson"),
    rate = ci[, 1] * 100,  
    # lower limit and upper limit
    lcl  = ci[, 2] * 100,      
    ucl  = ci[, 3] * 100       
  ) %>%
  # keep only the top 10, as needed
  slice_max(order_by = n_ae, n = 10) %>%
  # reorder AETERM for a descending plot
  mutate(AETERM = reorder(AETERM, rate))

ggplot(ae_summary, aes(x = rate, y = AETERM)) +
  # add the error bars first so they sit behind the points
  geom_errorbar(aes(xmin = lcl, xmax = ucl), width = 0.3) +
  geom_point(size = 3) +
  # labels and styling
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", n_total, " subjects; 95% Clopper-Pearson CIs"),
    x = "Percentage of Patients (%)",
    y = NULL
  ) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#F5F5F5", color = NA))

