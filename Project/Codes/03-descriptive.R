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

# ---- 1) Split your variables ----
vars_left_panel <- vars_left    # your predefined left vars
vars_right_panel <- vars_right  # your predefined right vars

# ---- 2) Function to create styled gt tables ----
create_panel <- function(vars, caption_text) {
  bdhs_cl %>%
    select(all_of(c("underweight", vars))) %>%
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
    modify_caption(caption_text) %>%
    modify_header(
      stat_1 ~"Not underweight (n={n})",
      stat_2 ~ "Underweight (n={n})",
    ) %>%
    as_gt() %>%
    opt_table_font(font = list(google_font("Arial"), default_fonts())) %>%
    tab_style(
      style = cell_borders(color = "white", sides = c("top", "bottom"), weight = px(0)),
      locations = cells_body()
    ) %>%
    tab_options(
      table.border.top.style = "solid",
      table.border.top.width = px(1),
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1),
      table.border.bottom.style = "solid",
      table.border.bottom.width = px(1),
      table.border.left.style = "none",
      table.border.right.style = "none",
      table.font.size = 10,
      data_row.padding = px(2),
      heading.align = "left"
    )
}

# ---- 3) Create left and right panels ----
table_left <- create_panel(vars_left_panel, "**Table 1a (Panel 1)**")
table_right<- create_panel(vars_right_panel, "**Table 1b (Panel 2)**")

# ---- 4) Save temporary images ----
gtsave(table_left, "left.png", zoom = 1.2)
gtsave(table_right, "right.png", zoom = 1.2)

# ---- 5) Combine side by side using magick ----
left_img <- image_read("left.png")
right_img <- image_read("right.png")

combined <- image_append(c(left_img, right_img))  # side-by-side
image_write(combined, "Table1.png")

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
  x.factor = maternal_education,
  trace.factor = maternal_job,
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
