# Load libraries
library(finalfit)

# ---------------------------
# DEFINE VARIABLES
# ---------------------------

# Child-level explanatory variables
exp_child <- c(
  "breastfeeding_duration",
  "birth_order",
  "twin",
  "diarrhea",
  "fever",
  "cough",
  "vit_a"
)

# Mother-level explanatory variables
exp_mother <- c(
  "maternal_age",
  "first_birth",
  "maternal_BMI",
  "maternal_education",
  "maternal_job",
  "c_section"
)

# Socio-demographic explanatory variables
exp_sd <- c(
  "paternal_education",
  "residence",
  "region",
  "religion",
  "household_size",
  "wealth_index",
  "toilet",
  "water"
)

# Combine all explanatory variables
exp <- c(exp_child, exp_mother, exp_sd)

# Dependent (outcome) variables
dep_weight <- "child_weight_kg"
dep_weight_zscore <- "waz"
dep <- c(dep_weight, dep_weight_zscore, "underweight")

# ---------------------------
# MISSINGNESS ANALYSIS
# ---------------------------

# Glimpse missingness
bdhs_cl |>
  ff_glimpse(exp, dep)

# Plot missingness overview
png("Plots/02-missingness-1.png", width = 800, height = 600, res = 100)
bdhs_cl |>
  missing_plot(exp, dep)
dev.off()
graphics.off()

# Compare missingness for each dependent variable
bdhs_cl |>
  missing_compare(dep_weight, exp)

bdhs_cl |>
  missing_compare(dep_weight_zscore, exp)

# ---------------------------
# DROP CASES WITH MISSING WAZ
# ---------------------------
bdhs_cl <- bdhs_cl[!is.na(bdhs_cl$waz), ]

bdhs_cl |>
  ff_glimpse(exp, dep)

# ---------------------------
# CHECK FOR VARIABLES WITH LOW COUNTS BY OUTCOME LEVEL
# ---------------------------
threshold <- 25
factor_cols <- sapply(bdhs_cl, is.factor)
uw_levels <- levels(bdhs_cl$underweight)

for (uw in uw_levels) {
  bdhs_uw <- bdhs_cl[bdhs_cl$underweight == uw, ]
  cat("\nResults for ", uw, "\n")
  
  for (col_name in names(bdhs_uw)[factor_cols]) {
    if (col_name == "underweight") next
    
    counts <- table(bdhs_uw[[col_name]])
    rare_levels <- names(counts[counts < threshold])
    
    if (length(rare_levels) > 0) {
      cat(col_name, ":", paste(rare_levels, collapse = ", "), "\n")
    }
  }
}

# ---------------------------
# REMOVE VARIABLES WITH LOW OR MISSING DATA
# ---------------------------
vars_to_remove <- c("breastfeeding_duration", "twin", "water")

bdhs_cl <- bdhs_cl |>
  select(-all_of(vars_to_remove))

exp <- exp[!exp %in% vars_to_remove]

bdhs_cl |>
  ff_glimpse(exp, dep)

# Updated missingness plot
png("Plots/02-missingness-2.png", width = 800, height = 600, res = 100)
bdhs_cl |>
  missing_plot(exp, dep)
dev.off()
graphics.off()

# ---------------------------
# REMOVE ROWS WITH SMALL AMOUNTS OF MISSINGNESS
# ---------------------------
vars_with_missing <- c("maternal_BMI", "diarrhea", "fever", "cough", "vit_a", "paternal_education", "toilet")

bdhs_cl <- bdhs_cl |>
  filter(if_all(all_of(vars_with_missing), ~ !is.na(.)))

# Remove variables with large missingness (39%)
vars_to_remove <- c("c_section")

bdhs_cl <- bdhs_cl |>
  select(-all_of(vars_to_remove))

exp <- exp[!exp %in% vars_to_remove]

# ---------------------------
# FINAL CHECK FOR MISSINGNESS
# ---------------------------
bdhs_cl |>
  ff_glimpse(exp, dep)

png("Plots/02-missingness-3.png", width = 800, height = 600, res = 100)
bdhs_cl |>
  missing_plot(exp, dep)
dev.off()
graphics.off()

# ---------------------------
# CHECK UNDERWEIGHT DISTRIBUTION
# ---------------------------
table(bdhs_cl$underweight, useNA = "ifany")

bdhs_cl

