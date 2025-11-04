library(car)

# list of vars used in the collinearity check
exp_overall <- exp_orig

# linear regression on weight-age z-score
lin_reg(exp_overall, purpose="collinearity")

# GVIF for m_age and m_first_birth are slightly higher
vars_to_test = c("maternal_age", "first_birth")
for (var in vars_to_test) {
  lin_reg(exp_overall, var_to_remove=var, purpose="collinearity")
}

# removing m_first_birth because current maternal age is more interesting than maternal age at first birth
vars_to_remove = c("first_birth")
lin_reg(exp_overall, var_to_remove=vars_to_remove, purpose="collinearity")
exp_overall <- exp_overall[!exp_overall %in% vars_to_remove]
lin_reg(exp_overall)

# next fit (removing sd_household_size)
exp_lin <- exp_overall
var_to_remove = "household_size"
lin_reg(exp_lin, var_to_remove)
exp_lin <- exp_lin[!exp_lin %in% var_to_remove]

# next fit (removing c_birth_order)
var_to_remove = "birth_order"
lin_reg(exp_lin, var_to_remove)
exp_lin <- exp_lin[!exp_lin %in% var_to_remove]

# next fit (removing sd_f_education)
var_to_remove = "paternaL_education"
lin_reg(exp_lin, var_to_remove)
exp_lin <- exp_lin[!exp_lin %in% var_to_remove]

# next fit (removing m_job)
var_to_remove = "maternal_job"
lin_reg(exp_lin, var_to_remove)
exp_lin <- exp_lin[!exp_lin %in% var_to_remove]

# next fit (removing c_cough)
var_to_remove = "cough"
lin_reg(exp_lin, var_to_remove)
exp_lin <- exp_lin[!exp_lin %in% var_to_remove]

# next fit (removing c_fever)
var_to_remove = "fever"
lin_reg(exp_lin, var_to_remove)
exp_lin <- exp_lin[!exp_lin %in% var_to_remove]

# next fit (removing sd_toilet)
var_to_remove = "toilet"
lin_reg(exp_lin, var_to_remove)
exp_lin <- exp_lin[!exp_lin %in% var_to_remove]

# check work with automated backwards elim
model <- paste("waz", "~", paste(exp_overall, collapse = " + "))
linfit_initial <- lm(as.formula(model), data = bdhs_cl)
auto_model <- step(linfit_initial, direction = "backward")
summary(auto_model)
AIC(auto_model)

# final model
model <- paste("waz", "~", paste(exp_lin, collapse = " + "))
linfit_final <- lm(as.formula(model), data = bdhs_cl)
summary(linfit_final)
AIC(linfit_final)

## model diagnostics
# influential plot
influencePlot(linfit_final, id=list(method="noteworthy", n=2))

# linearity plot
plot(linfit_final, which = 1)

# normality of residuals
plot(linfit_final, which = 2)

# homoscedasticity
plot(linfit_final, which = 3)

