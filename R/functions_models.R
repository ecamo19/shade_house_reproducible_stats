#' @importFrom magrittr %>%
#' 
#' @title Model for determining the effects of treatments and N-fixing status
#' over traits and plant growth and biomass accumulation  
#' 
#' @author Erick Calderon-Morales 
#' 
#' @description This function was created for fitting a mixed model with 
#' treatment, nfixer as fixed effects and their interaction and 
#' the covariate initial height (which takes into account that each seedling 
#' had a different height at the beginning of the experiment), 
#' spcode as random effects
#' 
#' response_var_i = treatment x fixer + initial_height + random( 1|specie)
#' 
#' i = ith variable

#' 
#' 
#' @details This function is thought to be run along side with purrr::map 
#' for getting a list of models  
#'
#' @param data data frame with the response and dependent variables
#' @param response dependent variable that we want to model
#' 
#' @examples 
#' \dontrun{
#' map(setnames(names(data$response_var)), ~ mixed_model_1(response = .x, 
#'                                           data = data))
#'                                           }

# Model 1 ----------------------------------------------------------------------

mixed_model_1 <-  function(response, data) {    
    
    # Take each response variable and join it to the model formula
    formula = paste(response, " ~ treatment * nfixer + init_height + (1|spcode)")
    
    # Fit the model
    model_1_base <- lme4::lmer(as.formula(formula),
                            data = data) 
    return(model_1_base)
    
}

# Model 2: Three way interaction -----------------------------------------------

#' @importFrom magrittr %>%

#' @title Model formulas for mixed effects model  
#' 
#' @author Erick Calderon-Morales 
#' 
#' @description This function is for creating all the possible model formulas 
#' between response variables and independent variables in the shade house \
#' experiment  
#' 
#' @details This function is thought to be run along side with purrr::map 
#' for getting a list of model formulas 
#' 
#' Returns formula
#' response_var_i = treatment x fixer x trait_j + initial_height + random( 1|specie)
#' 
#' i = ith variable
#' j = jth trait  
#'
#' @param x_var  names of the traits that  we want to add to the 3-way 
#' interaction (nfixer x treatment x trait)
#'  
#' @param yvar names of dependent variables that we want to model
#' 
#' @examples 
#' \dontrun{
#' 
#' First Formulas
#' 
#' x_names <- 
#'     set_names(names(data))
#'
#' y_names <- set_names(names(data))
#'
#' formulas <- model_combinations_formulas(y_names, x_names)
#' 
#' Second Models
#' 
#' models_list <- map(formulas, ~ lme4::lmer(.x, data = data))
#'
#'}
#'@return list 

model_combinations_formulas <- function(y_var, x_var, nlme = FALSE){
    
    variables <- tidyr::crossing(y_var, x_var)
    # Strings to remove from original names
    pattern <- c('\\+.*|nfixer|treatment|[[:punct:]]| ')
    
    if(nlme == FALSE){
        
        # Model for lme4 
        models <- paste0(variables$y_var,"~treatment:nfixer:",
                         variables$x_var,"+init_height+(1|spcode)")
    }
    
    if (nlme == TRUE) {
        # Model for nlme
        models <- paste0(variables$y_var,"~treatment:nfixer:",
                         variables$x_var,"+init_height")
    }
        
        
    formulas <- purrr::map(models, as.formula)
    
    # Add name to the list 
    names(formulas) <- stringr::str_replace_all(formulas, pattern, replacement = '')
    return(formulas)
}


# ------------------------------------------------------------------------------
# nlme model
# ------------------------------------------------------------------------------

model_nlme <- function(formula, data){
    model_nlme <-lme(formula, 
                     random = ~1|spcode,
                     weights = varIdent(form = ~1|spcode),
                     data = data)
    
    # Step done for modifying the fixed term in the summary
    resp_var <- as.character(attr(terms(model_nlme), "variables"))[2]
    trait <- as.character(attr(terms(model_nlme), "variables"))[5]
    
    model_nlme$call$fixed <- as.formula(paste0(resp_var, " ~ init_height + treatment:nfixer:",trait))
    return(model_nlme)
}



#' @importFrom magrittr %>%
#' 
#' @title Model for determining the effects of treatments,N-fixing status and 
#' traits (3_way_interaction) over plant growth and biomass accumulation  
#' 
#' @author Erick Calderon-Morales 
#' 
#' @description This function was created for fitting a three way interaction
#' mixed model with treatment, nfixer and traits as fixed effects and 
#' the covariate initial height (which takes into account that each seedling 
#' had a different height at the beginning of the experiment), 
#' spcode as random effects
#' 
#' @details Get single model  
#' 
#' @param trait x_var
#' @param response dependent variable that we want to model
#' @param data data frame
#' 
#' @examples 
#' \dontrun{
#' models_1 <- mixed_model_2(response,trait, data)
#' }

mixed_model_2 <-  function(response, trait, data) {    
    
    # Take each response variable and join it to the model formula
    formula <- paste0(response, " ~ treatment * nfixer * ", trait, " + init_height + (1|spcode)")
    
    # Fit the model
    model_1_base <- lme4::lmer(as.formula(formula), 
                               data = data,
                               control = lmerControl(optCtrl = list(maxfun = 2e10))) 
    return(model_1_base)
    
}






