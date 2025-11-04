grid.newpage()

# ---------- Top Titles (aligned across both plots) ----------
vp_titles <- viewport(x = 0.5, y = 0.9, width = 1, height = 0.05)
pushViewport(vp_titles)

# Logistic Regression Title
grid.text("Logistic Regression", x = 0.23, y = 0.8,
          gp = gpar(cex = 1.1, fontface = "bold"))
grid.text("(Odds Ratios with 95% CI)", x = 0.23, y = 0.3,
          gp = gpar(cex = 0.85, fontface = "italic"))

# Linear Regression Title
grid.text("Linear Regression", x = 0.77, y = 0.8,
          gp = gpar(cex = 1.1, fontface = "bold"))
grid.text("(Statistically Significant Variables Only)", x = 0.77, y = 0.3,
          gp = gpar(cex = 0.85, fontface = "italic"))

popViewport()

# ---------- Left viewport (Logistic Regression) ----------
vp_left <- viewport(x = 0.23, y = 0.5, width = 0.44, height = 0.8)
pushViewport(vp_left)
forestplot(
  labeltext = tabletext_log,
  mean  = c(NA, plot_coefs_log$coef),
  lower = c(NA, plot_coefs_log$l_conf_int),
  upper = c(NA, plot_coefs_log$u_conf_int),
  zero = 1,
  cex = 1,
  lineheight = "auto",
  fn.ci_norm = fpDrawNormalCI,
  boxsize = 0.1,
  col = fpColors(box = "royalblue", line = "darkblue", summary = "royalblue"),
  xlab = "Odds Ratio",
  is.summary = is_summary_log,
  txt_gp = fpTxtGp(
    label = gpar(cex = 0.9),
    ticks = gpar(cex = 0.8),
    xlab = gpar(cex = 1)
  ),
  new_page = FALSE,
  title = NULL   # remove internal title
)
popViewport()

# ---------- Right viewport (Linear Regression) ----------
vp_right <- viewport(x = 0.77, y = 0.5, width = 0.44, height = 0.8)
pushViewport(vp_right)
forestplot(
  labeltext = tabletext_lin,
  mean  = c(NA, plot_coefs_lin$coef),
  lower = c(NA, plot_coefs_lin$l_conf_int),
  upper = c(NA, plot_coefs_lin$u_conf_int),
  zero = 0,
  cex = 1,
  lineheight = "auto",
  fn.ci_norm = fpDrawNormalCI,
  boxsize = 0.1,
  col = fpColors(box = "darkgreen", line = "forestgreen", summary = "darkgreen"),
  xlab = "Coefficient",
  is.summary = is_summary_lin,
  txt_gp = fpTxtGp(
    label = gpar(cex = 0.9),
    ticks = gpar(cex = 0.8),
    xlab = gpar(cex = 1)
  ),
  new_page = FALSE,
  title = NULL
)
popViewport()
