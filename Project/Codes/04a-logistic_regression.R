# list of vars used in the collinearity check
exp_overall <- exp_orig

# logistic regression on binary underweight
log_reg(exp_overall, purpose="collinearity")

# GVIF for m_age and m_first_birth are slightly higher
vars_to_test = c("maternal_age", "first_birth")
for (var in vars_to_test) {
  log_reg(exp_overall, var_to_remove=var, purpose="collinearity")
}

# removing m_first_birth because current maternal age is more interesting than maternal age at first birth
vars_to_remove = c("first_birth")
log_reg(exp_overall, var_to_remove=vars_to_remove, purpose="collinearity")
exp_overall <- exp_overall[!exp_overall %in% vars_to_remove]
log_reg(exp_overall)

# next fit (removing sd_f_education due to highest p-value)
exp_log <- exp_overall
var_to_remove = "paternal_education"
log_reg(exp_log, var_to_remove)
exp_log <- exp_log[!exp_log %in% var_to_remove]

# next fit (removing sd_region due to highest p-value)
var_to_remove = "region"
log_reg(exp_log, var_to_remove)
exp_log <- exp_log[!exp_log %in% var_to_remove]

# next fit (removing c_fever due to highest p-value)
var_to_remove = "fever"
log_reg(exp_log, var_to_remove)
exp_log <- exp_log[!exp_log %in% var_to_remove]

# next fit (removing sd_toilet due to highest p-value)
var_to_remove = "toilet"
log_reg(exp_log, var_to_remove)
exp_log <- exp_log[!exp_log %in% var_to_remove]

# next fit (removing c_cough due to highest p-value)
var_to_remove = "cough"
log_reg(exp_log, var_to_remove)
exp_log <- exp_log[!exp_log %in% var_to_remove]

# next fit (removing sd_religion due to highest p-value)
var_to_remove = "religion"
log_reg(exp_log, var_to_remove)
exp_log <- exp_log[!exp_log %in% var_to_remove]

# next fit (removing c_birth_order due to highest p-value)
var_to_remove = "birth_order"
log_reg(exp_log, var_to_remove)
exp_log <- exp_log[!exp_log %in% var_to_remove]

# next fit (removing sd_household_size due to highest p-value)
var_to_remove = "household_size"
log_reg(exp_log, var_to_remove)
exp_log <- exp_log[!exp_log %in% var_to_remove]

# next fit (removing c_vit_a due to highest p-value)
var_to_remove = "vit_a"
log_reg(exp_log, var_to_remove)
exp_log <- exp_log[!exp_log %in% var_to_remove]

# next fit (removing c_diarrhea due to highest p-value)
var_to_remove = "diarrhea"
log_reg(exp_log, var_to_remove)
exp_log <- exp_log[!exp_log %in% var_to_remove]

# next fit (removing sd_residence due to highest p-value)
var_to_remove = "residence"
log_reg(exp_log, var_to_remove)
exp_log <- exp_log[!exp_log %in% var_to_remove]

# automated backwards elim?
model <- paste("underweight", "~", paste(exp_overall, collapse = " + "))
logfit_initial <- glm(as.formula(model), data = bdhs_cl, family = "binomial")
auto_model <- step(logfit_initial, direction = "backward")
summary(auto_model)

# final model
model <- paste("underweight", "~", paste(exp_log, collapse = " + "))
logfit_final <- glm(as.formula(model), data = bdhs_cl, family = "binomial")
summary(logfit_final)

