# load libraries 
library(readxl) 
library(janitor)
library(tidyverse)

# load the data as diversity.df
diversity.df <- read_excel("data/bsc201_community_lab_data.xlsx") |> 
  clean_names()  |>
  mutate(aquatic_shannon_diversity = as.numeric(aquatic_shannon_diversity),
         terrestrial_shannon_diversity = as.numeric(terrestrial_shannon_diversity),
         plant_richness = as.numeric(plant_richness))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# AQUATIC DIVERISTY T-TEST---------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# T-Test for Aquatic Diversity in Restored and Unrestored Sites---------

aquatic_t_test.model <- t.test(aquatic_shannon_diversity ~ restoration_status, 
  data=diversity.df,
  var.equal = FALSE)
# what does the T-Test tell us - will show at the bottom - 
# We need to record the T Value, the P Value and the Degrees of Freedom
aquatic_t_test.model

# Extract the t-test results to use in the graph-----
t_stat_aquatic <- round(aquatic_t_test.model$statistic, 2)  # t-statistic
p_value_aquatic <- round(aquatic_t_test.model$p.value, 4)   # p-value
df_aquatic <- round(aquatic_t_test.model$parameter, 2)      # degrees of freedom

# Make the Aquatic T Test Plot ------
aquatic_diverity.plot <- diversity.df |> 
  ggplot(aes(restoration_status, aquatic_shannon_diversity)) +
    stat_summary(fun=mean, na.rm=TRUE,  geom = "bar",
                 fill="white", colour="black") +
      stat_summary(fun.data = mean_se, geom = "errorbar", width = .2) +
  labs(x="Restoration Status", y="Aquatic Shannon Diversity")+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  annotate("text", x = 1, 
          y = max(diversity.df$aquatic_shannon_diversity, na.rm=TRUE) * .8, # change the 0.8 to a value that allow syou to plot it
          label = paste("t =", t_stat_aquatic, "\n", "p =", p_value_aquatic, "\n", "df =", df_aquatic),
          size = 5, hjust = 0.5)

# Show the Aquatic T Test Plot
aquatic_diverity.plot

# Save the Aquatic T Test Plot-------
ggsave(aquatic_diverity.plot, file = "figures/aquatic_diverity_ttestplot.pdf", 
       width = 6, height = 6, units = "in", dpi = 300)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Terrestrail T - Test --------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# T-Test for Terrestrial Diveristy in Resotred and Unrestored Sites---------
terrestrial_t_test.model <- t.test(terrestrial_shannon_diversity ~ restoration_status, 
  data=diversity.df,
  var.equal = FALSE)

# What does the Terrestrial T - Test tell us
terrestrial_t_test.model

# Extract the t-test results from the Terrestrial T - Test
t_stat_terrestrial <- round(terrestrial_t_test.model$statistic, 2)  # t-statistic
p_value_terrestrial <- round(terrestrial_t_test.model$p.value, 4)   # p-value
df_terrestrial <- round(terrestrial_t_test.model$parameter, 2)      # degrees of freedom

# Make the Terrestrial T Test Plot
terrestrial_diverity.plot <- diversity.df |> 
  ggplot(aes(restoration_status, terrestrial_shannon_diversity)) +
    stat_summary(fun=mean, na.rm=TRUE,  geom = "bar",
                 fill="white", colour="black") +
      stat_summary(fun.data = mean_se, geom = "errorbar", width = .2) +
  labs(x="Restoration Status", y="Terrestrial Shannon Diversity")+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  annotate("text", x = 2, 
          y = max(diversity.df$terrestrial_shannon_diversity, na.rm=TRUE) * 0.9, # change the 0.9 to a value that allow syou to plot it
          label = paste("t =", t_stat_terrestrial, "\n", 
          "p =", p_value_terrestrial, "\n", 
          "df =", df_terrestrial),
          size = 5, hjust = 0.5)

# Show the tetterstiral T Test plot
terrestrial_diverity.plot

# Save the Terrestrial T Test Plot-------
ggsave(terrestrial_diverity.plot, file = "figures/terrestrial_diverity_ttest_plot.pdf", 
       width = 6, height = 6, units = "in", dpi = 300)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# REGRESSION OF TERRESTRIAL AND AQUATIC SHANNON DIVERSITY versus PLANT RICHNESS ----------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Run the Regression model for Aquatic Diversity versus Plant Richness -------
plant_aquatic_regressison.model <- lm(aquatic_shannon_diversity ~ plant_richness, diversity.df)
# Show the summary of the regression model
summary(plant_aquatic_regressison.model)

# Extracting coefficients - the slope and intercept
aq_linear_coefficients <- coef(plant_aquatic_regressison.model)
aq_intercept <- aq_linear_coefficients[1]
aq_slope <- aq_linear_coefficients[2]

# Create the line equation as a string
aq_line_equation <- sprintf("y = %.4fx + %.4f", aq_slope, aq_intercept)

# Print the line equation for aquatic diveristy versus plant richness
print(aq_line_equation)

# Create the Aquatic Diversity verse plant richness plot -------
aquatic_richness.plot <- diversity.df |> 
  ggplot(aes(plant_richness, aquatic_shannon_diversity)) +
    geom_point()+
    geom_smooth(method="lm", se=FALSE) +
  labs(x="Plant Species Richness", y="Aquatic Shannon Diversity")+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  annotate("text", x = min(diversity.df$plant_richness, na.rm=TRUE)+0.05, # adjust the 0.05 to move the the right with larger value 
          y = max(diversity.df$terrestrial_shannon_diversity, na.rm=TRUE) * 0.65, # change the 0.75 to a value that allow syou to plot it
          label = paste("Linear Regression Equation:", aq_line_equation, "\n", 
          "r^2 =", round(summary(plant_aquatic_regressison.model)$r.squared, 2), "\n", 
          "p =", round(summary(plant_aquatic_regressison.model)$coefficients[2,4], 4)),
          size = 4, hjust = 0)

# now to show the Aquatic Regression plot  ------
aquatic_richness.plot

# Save the Aquatic diversity versus plant richness plot-------
ggsave(aquatic_richness.plot, file = "figures/aquatic_diversity_plant_richness_regression.pdf", 
       width = 6, height = 6, units = "in", dpi = 300)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Run the Regression model for Terrestrial Diversity versus Plant Richness -------
terestrial_regressison.model <- lm(terrestrial_shannon_diversity ~ plant_richness, diversity.df)
# Show the summary of the regression model
summary(terestrial_regressison.model)

# Extracting coefficients - the slope and intercept
terr_linear_coefficients <- coef(terestrial_regressison.model)
terr_intercept <- terr_linear_coefficients[1]
terr_slope <- terr_linear_coefficients[2]

# Create the line equation as a string
terr_line_equation <- sprintf("y = %.4fx + %.4f", terr_slope, terr_intercept)

# Print the line equation for aquatic diveristy versus plant richness
print(terr_line_equation)

# Create the Terrestrial Diveristy verse plant richness plot -------
terrestrial_richness.plot <- diversity.df |> 
  ggplot(aes(plant_richness, terrestrial_shannon_diversity)) +
    geom_point()+
    geom_smooth(method="lm", se=FALSE) +
  labs(x="Plant Species Richness", y="Terrestrial Shannon Diversity")+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  annotate("text", x = min(diversity.df$plant_richness, na.rm=TRUE)+0.05, # adjust the 0.05 to move the the right with larger value 
          y = max(diversity.df$terrestrial_shannon_diversity, na.rm=TRUE) * 0.85, # change the 0.95 to a value that allow syou to plot it
          label = paste("Linear Regression Equation:", terr_line_equation, "\n", 
          "r^2 =", round(summary(terestrial_regressison.model)$r.squared, 2), "\n", 
          "p =", round(summary(terestrial_regressison.model)$coefficients[2,4], 4)),
          size = 4, hjust = 0)

# now to show the Aquatic Regression plot  ------
terrestrial_richness.plot

# Save the Aquatic diversity versus plant richness plot-------
ggsave(terrestrial_richness.plot, file = "figures/terrestrial_diversity_plant_richness_regresssion.pdf", 
       width = 6, height = 6, units = "in", dpi = 300)

