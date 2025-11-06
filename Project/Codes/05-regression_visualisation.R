library(grid)
library(forestplot)

# -----------------------
# 1. Logistic Regression (exclude intercept, mark significant)
# -----------------------
coefs_log <- summary(logfit_final)$coefficients
ci_log <- confint(logfit_final)

# Remove intercept
coefs_log <- coefs_log[rownames(coefs_log) != "(Intercept)", ]
ci_log <- ci_log[rownames(ci_log) != "(Intercept)", ]

# Identify significant variables
pvals_log <- summary(logfit_final)$coefficients[-1, 4]  # exclude intercept
sig_vars_log <- names(pvals_log[pvals_log < 0.05])

# Add asterisk to significant variables
variable_names_log <- rownames(coefs_log)
variable_names_log[variable_names_log %in% sig_vars_log] <- paste0(variable_names_log[variable_names_log %in% sig_vars_log], "*")

# Dynamic tabletext
tabletext_log <- cbind(
  c("Variable", variable_names_log),
  c("OR (95% CI)", 
    paste0(round(exp(coefs_log[,1]), 2), " (", 
           round(exp(ci_log[,1]), 2), "-", 
           round(exp(ci_log[,2]), 2), ")"))
)

is_summary_log <- c(TRUE, rep(FALSE, nrow(coefs_log)))

# -----------------------
# 2. Linear Regression (exclude intercept, only significant, asterisk)
# -----------------------
coefs_lin <- summary(linfit_final)$coefficients
ci_lin <- confint(linfit_final)

# Remove intercept
coefs_lin <- coefs_lin[rownames(coefs_lin) != "(Intercept)", ]
ci_lin <- ci_lin[rownames(ci_lin) != "(Intercept)", ]

# Identify significant variables
pvals_lin <- summary(linfit_final)$coefficients[-1, 4]  # exclude intercept
sig_vars_lin <- names(pvals_lin[pvals_lin < 0.05])

# Keep only significant variables
coefs_lin <- coefs_lin[sig_vars_lin, , drop = FALSE]
ci_lin <- ci_lin[sig_vars_lin, , drop = FALSE]

# Add asterisk to significant variables
variable_names_lin <- paste0(rownames(coefs_lin), "*")

# Dynamic tabletext
tabletext_lin <- cbind(
  c("Variable", variable_names_lin),
  c("Coef (95% CI)", 
    paste0(round(coefs_lin[,1], 2), " (", 
           round(ci_lin[,1], 2), "-", 
           round(ci_lin[,2], 2), ")"))
)

is_summary_lin <- c(TRUE, rep(FALSE, nrow(coefs_lin)))

# -----------------------
# 3. Side-by-side forest plots
# -----------------------
grid.newpage()

# Top titles
vp_titles <- viewport(x = 0.5, y = 0.92, width = 1, height = 0.05)
pushViewport(vp_titles)
grid.text("Logistic Regression", x = 0.23, y = 0.8,
          gp = gpar(cex = 1.1, fontface = "bold"))
grid.text("(Odds Ratios with 95% CI)", x = 0.23, y = 0.3,
          gp = gpar(cex = 0.85, fontface = "italic"))

grid.text("Linear Regression", x = 0.77, y = 0.8,
          gp = gpar(cex = 1.1, fontface = "bold"))
grid.text("(Statistically Significant Variables Only)", x = 0.77, y = 0.3,
          gp = gpar(cex = 0.85, fontface = "italic"))
popViewport()

# Left forestplot (Logistic)
vp_left <- viewport(x = 0.25, y = 0.5, width = 0.44, height = 0.8)
pushViewport(vp_left)
forestplot(
  labeltext = tabletext_log,
  mean  = c(NA, exp(coefs_log[,1])),
  lower = c(NA, exp(ci_log[,1])),
  upper = c(NA, exp(ci_log[,2])),
  zero = 1,
  boxsize = 0.1,
  col = fpColors(box = "royalblue", line = "darkblue", summary = "royalblue"),
  xlab = "Odds Ratio",
  is.summary = is_summary_log,
  txt_gp = fpTxtGp(label = gpar(cex = 0.9),
                   ticks = gpar(cex = 0.8),
                   xlab = gpar(cex = 1)),
  new_page = FALSE,
  title = NULL
)
popViewport()

# Right forestplot (Linear)
vp_right <- viewport(x = 0.75, y = 0.5, width = 0.44, height = 0.8)
pushViewport(vp_right)
forestplot(
  labeltext = tabletext_lin,
  mean  = c(NA, coefs_lin[,1]),
  lower = c(NA, ci_lin[,1]),
  upper = c(NA, ci_lin[,2]),
  zero = 0,
  boxsize = 0.1,
  col = fpColors(box = "darkgreen", line = "forestgreen", summary = "darkgreen"),
  xlab = "Coefficient",
  is.summary = is_summary_lin,
  txt_gp = fpTxtGp(label = gpar(cex = 0.9),
                   ticks = gpar(cex = 0.8),
                   xlab = gpar(cex = 1)),
  new_page = FALSE,
  title = NULL
)
popViewport()
