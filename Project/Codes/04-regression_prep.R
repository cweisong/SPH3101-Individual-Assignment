  # explanatory vars for general model
  exp_c = c("birth_order",
            "diarrhea",
            "fever",
            "cough",
            "vit_a")
  
  exp_m = c("maternal_age",
            "first_birth",
            "maternal_BMI",
            "maternal_education",
            "maternal_job")
  
  exp_sd = c("paternal_education",
             "residence",
             "region",
             "religion",
             "household_size",
             "wealth_index",
             "toilet")
  
  exp_orig = c(exp_c, exp_m, exp_sd)
  
  # function to help with logistic reg
  log_reg <- function(exp, var_to_remove="none", purpose="backwards_elim") {
    if (!"none" %in% var_to_remove) {
      exp <- exp[!exp %in% var_to_remove]
    }
    model <- paste("underweight", "~", paste(exp, collapse = " + "))
    logfit <- glm(as.formula(model), data = bdhs_cl, family = "binomial")
    if (purpose == "collinearity") {
      cat("\nVIF Values for Log Reg when Removing: ", var_to_remove, "\n")
      print(car::vif(logfit))
    }
    else if (purpose == "backwards_elim") {
      cat("\nRemoving: ", var_to_remove, "\n")
      print(summary(logfit))
    }
  }
  
  # function to help with linear reg
  lin_reg <- function(exp, var_to_remove="none", purpose="backwards_elim") {
    if (!"none" %in% var_to_remove) {
      exp <- exp[!exp %in% var_to_remove]
    }
    model <- paste("waz", "~", paste(exp, collapse = " + "))
    linfit <- lm(as.formula(model), data = bdhs_cl)
    if (purpose == "collinearity") {
      cat("\nVIF Values for Lin Reg when Removing: ", var_to_remove, "\n")
      print(car::vif(linfit))
    }
    else if (purpose == "backwards_elim") {
      cat("\nRemoving: ", var_to_remove, "\n")
      print(summary(linfit))
      print(AIC(linfit))
    }
  }