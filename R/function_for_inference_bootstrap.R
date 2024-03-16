
# Bootstrap function -----------------------------------------------------------
bootstrap_model_ci_df <- function(model, iter = 100, category = "all"){


    cli::cli_alert("Available options for category are norm, basic, perc or all")

    # get the response variable name from the model
    terms <- terms(model)
    response_var <- as.character(attr(terms, "variables"))[2]
    #trait_var <- as.character(attr(terms, "variables"))[5]
    colmn <- paste0("col_", 1:2)

    # Get 95% CIs

    # Bootstrap fixed effects
    #
    bootstrap_nlme <- lmeresampler::bootstrap(model, .f = fixef,
                                              type = "parametric",
                                              B = iter)

    stats::confint(bootstrap_nlme, type = category) %>%

        as.data.frame(.) %>%

        # create column with the name of the response variable
        tibble::add_column(response_var = response_var) %>%

        janitor::clean_names()  %>%

        filter(!term %in% c("(Intercept)","init_height", )) %>%

        tidyr::separate(data = ., col = term, sep = ":",into = colmn,
                        remove = T) %>%

        dplyr::mutate(col_1 = case_when(
            col_1 == "treatmentplus_nutrients" ~ "plus_nutrients",
            col_1 == "treatmentplus_water" ~ "plus_water",
            col_1 == "treatmentplus_water_nutrients" ~ "plus_water_nutrients",

            TRUE ~ col_1)) %>%

        dplyr::mutate(col_2 = case_when(
            col_2 == "nfixerfixer" ~ "fixer",
            col_2 == "nfixernonfixer" ~ "nonfixer",

            TRUE ~ col_2)) %>%

        #filter(!col_1 %in% "no_additions") %>%
        dplyr::arrange(col_1, col_2) %>%
        #unite("term", 1:3, sep = ":" #,remove = TRUE
        #      ) %>%
        #mutate(term = forcats::fct_inorder(term)) %>%

        # Create significance column
        dplyr::mutate(significance = if_else((lower > 0 & upper > 0 |
                                                  lower < 0 & upper < 0),
                                             TRUE, FALSE))

}
