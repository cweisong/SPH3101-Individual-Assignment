# Load required libraries
library(tableone)
library(ggplot2)
library(gtsummary)
library(tidyverse)
library(flextable)
library(gt)
library(magick)


# ---------------------------
# INITIALIZE VARIABLES
# ---------------------------

# Continuous explanatory variables
exp_cont <- c("maternal_age", "first_birth", "maternal_BMI")

# Categorical explanatory variables
exp_fctr <- c(
  "birth_order",
  "diarrhea",
  "fever",
  "cough",
  "vit_a",
  "maternal_education",
  "maternal_job",
  "paternal_education",
  "residence",
  "region",
  "religion",
  "household_size",
  "wealth_index",
  "toilet"
)

# Combine explanatory vars
exp <- c(exp_cont, exp_fctr)

# Outcome (dependent) variables
dep_weight <- "child_weight_kg"
dep_waz <- "waz"
dep_uw_binary <- "underweight"

dep <- c(dep_weight, dep_waz, dep_uw_binary)

# ---------------------------
# PREPARE UNDERWEIGHT FACTOR
# ---------------------------
# bdhs_cl <- bdhs_cl %>%
#   mutate(
#     underweight = factor(
#       underweight,
#       levels = c(0, 1),
#       labels = c("Not underweight", "Underweight")
#     )
#   )
# 
# bdhs_cl

vars_left <- c("maternal_age", "first_birth", "maternal_BMI", "maternal_education", "maternal_job",
               "vit_a", "birth_order", "diarrhea", "fever", "cough", "paternal_education", "residence", 
               "toilet")

vars_right <- c( "religion", "region", "wealth_index", "toilet", "household_size")

# ---------------------------
# TABLE 1: BASELINE CHARACTERISTICS
# ---------------------------

# table_underweight <- bdhs_cl %>%
#   tbl_summary(
#     by = underweight,
#     statistic = list(
#       all_continuous() ~ "{median} [{p25}, {p75}]",
#       all_categorical() ~ "{n} ({p}%)"
#     ),
#     missing_text = "(Missing)"
#   ) %>%
#   add_p(
#     test = list(
#       all_continuous() ~ "t.test",   # Use t-test for continuous vars
#       all_categorical() ~ "chisq.test" # Chi-square for categorical
#     )
#   ) %>%
#   add_n() %>%
#   bold_labels() %>%
#   italicize_levels() %>%
#   modify_caption("**Table 1. Baseline characteristics by underweight status**")
# 
# # View in RStudio Viewer
# table_underweight
# 
# # Convert gtsummary table to gt
# gt_table <- as_gt(table_underweight)
# 
# # Save as PNG
# gtsave(gt_table, filename = "Table1.png")

table_left <- bdhs_cl %>%
  select(all_of(c("underweight", vars_left))) %>%
  tbl_summary(
    by = underweight,
    statistic = list(
      all_continuous() ~ "{median} [{p25}, {p75}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing_text = "(Missing)"
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ "t.test",
      all_categorical() ~ "chisq.test"
    )
  ) %>%
  bold_labels() %>%
  italicize_levels()

table_right <- bdhs_cl %>%
  select(all_of(c("underweight", vars_right))) %>%
  tbl_summary(
    by = underweight,
    statistic = list(
      all_continuous() ~ "{median} [{p25}, {p75}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing_text = "(Missing)"
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ "t.test",
      all_categorical() ~ "chisq.test"
    )
  ) %>%
  bold_labels() %>%
  italicize_levels()
gt_left <- as_gt(table_left)
gt_right <- as_gt(table_right)

# Save temporary PNGs
gtsave(gt_left, "temp_left.png")
gtsave(gt_right, "temp_right.png")
# Read images
img_left <- image_read("temp_left.png")
img_right <- image_read("temp_right.png")

# Append side by side
img_combined <- image_append(c(img_left, img_right))

# Save final PNG
image_write(img_combined, path = "Table1_split.png")


# ---------------------------
# VISUALIZATIONS
# ---------------------------

# Histograms of continuous variables
hist(bdhs_cl$maternal_age, main = "Maternal Age", xlab = "Age")
hist(bdhs_cl$first_birth, main = "Age at First Birth", xlab = "Years")
hist(bdhs_cl$maternal_BMI, main = "Maternal BMI", xlab = "BMI")
hist(bdhs_cl$waz, main = "Weight-for-Age Z-score", xlab = "WAZ")

# Violin plots for continuous vars vs. underweight
ggplot(bdhs_cl, aes(x = underweight, y = maternal_age)) + geom_violin()
ggplot(bdhs_cl, aes(x = underweight, y = first_birth)) + geom_violin()
ggplot(bdhs_cl, aes(x = underweight, y = maternal_BMI)) + geom_violin()

# ---------------------------
# BIVARIATE ANALYSIS
# ---------------------------

## Binary Underweight Outcome
# Continuous Explanatory Variables (Logistic Regression)
summary(glm(underweight ~ maternal_age, bdhs_cl, family = binomial))
summary(glm(underweight ~ first_birth, bdhs_cl, family = binomial))
summary(glm(underweight ~ maternal_BMI, bdhs_cl, family = binomial))

# Categorical Explanatory Variables (Chi-square Tests)
chisq.test(bdhs_cl$underweight, bdhs_cl$birth_order)
chisq.test(bdhs_cl$underweight, bdhs_cl$diarrhea)
chisq.test(bdhs_cl$underweight, bdhs_cl$fever)
chisq.test(bdhs_cl$underweight, bdhs_cl$cough)
chisq.test(bdhs_cl$underweight, bdhs_cl$vit_a)
chisq.test(bdhs_cl$underweight, bdhs_cl$maternal_education)
chisq.test(bdhs_cl$underweight, bdhs_cl$maternal_job)
chisq.test(bdhs_cl$underweight, bdhs_cl$paternal_education)
chisq.test(bdhs_cl$underweight, bdhs_cl$residence)
chisq.test(bdhs_cl$underweight, bdhs_cl$region)
chisq.test(bdhs_cl$underweight, bdhs_cl$religion)
chisq.test(bdhs_cl$underweight, bdhs_cl$household_size)
chisq.test(bdhs_cl$underweight, bdhs_cl$wealth_index)
chisq.test(bdhs_cl$underweight, bdhs_cl$toilet)

# ---------------------------
# INTERACTION ANALYSIS
# ---------------------------

# Likelihood Ratio Test (Maternal Education x Job)
model1 <- lm(waz ~ maternal_education + maternal_job, bdhs_cl)
model2 <- lm(waz ~ maternal_education * maternal_job, bdhs_cl)
anova(model1, model2)

# Interaction plot
with(bdhs_cl, interaction.plot(
  x.factor = education,
  trace.factor = job,
  response = waz,
  fun = mean,
  type = "b",
  pch = 19,
  col = c("red", "blue"),
  lty = 1,
  ylab = "Mean Weight-for-Age Z-score",
  xlab = "Maternal Education",
  main = "Interaction between Maternal Education and Maternal Job",
  legend = FALSE
))

legend(
  "bottomright",
  legend = c("No", "Yes"),
  col = c("red", "blue"),
  pch = 19,
  lty = 1,
  title = "Maternal Job",
  bty = "n"
)

# ---------------------------
# CONTINUOUS OUTCOME: WAZ
# ---------------------------

# Continuous Explanatory Variables (Linear Regression)
summary(lm(waz ~ maternal_age, bdhs_cl))
summary(lm(waz ~ first_birth, bdhs_cl))
summary(lm(waz ~ maternal_BMI, bdhs_cl))

# Binary Categorical Variables (t-tests)
t.test(waz ~ diarrhea, data = bdhs_cl)
t.test(waz ~ fever, data = bdhs_cl)
t.test(waz ~ cough, data = bdhs_cl)
t.test(waz ~ vit_a, data = bdhs_cl)
t.test(waz ~ maternal_job, data = bdhs_cl)
t.test(waz ~ residence, data = bdhs_cl)
t.test(waz ~ religion, data = bdhs_cl)
t.test(waz ~ toilet, data = bdhs_cl)

# Categorical Variables with >2 Levels (ANOVA)
summary(aov(waz ~ birth_order, bdhs_cl))
summary(aov(waz ~ maternal_education, bdhs_cl))
summary(aov(waz ~ paternal_education, bdhs_cl))
summary(aov(waz ~ region, bdhs_cl))
summary(aov(waz ~ household_size, bdhs_cl))
summary(aov(waz ~ wealth_index, bdhs_cl))
