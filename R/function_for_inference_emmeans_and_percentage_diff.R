
# Model inference: Percentage difference ----------------------------------------

## First; Get emmeans data frame ------------------------------------------------
emmeans_df <- function(model, formula, grouping_var = NULL){

    if (is_empty(grouping_var) == TRUE) {
        var = NULL
    } else (
        var <- ensym(grouping_var)
    )
    #var <- sym(ifelse(is_empty(grouping_var) == TRUE, NA, grouping_var))

    # get the response variable name from the model
    terms <- terms(model)
    response_var <- as.character(attr(terms, "variables"))[2]

    if(missing(formula)){
        stop("Error: At least one var should be specified! Example A, B or A|B")
    }
    else
        formula <- formula(paste0("pairwise ~ ", formula))

    print("Formula for pairwise comparisons: ")
    print(formula)

    # Get contrasts tukey
    # estimate: of the effect size, that is the difference
    # between the two emmeans (estimated marginal means)
    as.data.frame(emmeans::emmeans(model,
                                formula,
                                type = "response",
                                adjust = "tukey")$emmeans) %>%

        tibble::add_column(resp_var = response_var) %>%
        janitor::clean_names() %>%
        dplyr::select(resp_var, everything(),
                        # Remove variables
                      -c(df,lower_cl, upper_cl,se)) %>%

        # Rename response to emmean, this is done when models is log
        dplyr::rename_all(funs(stringr::str_replace_all(., "response", "emmean"))) %>%

        dplyr::group_by(!!(var)) %>%

        # Calculate % difference between control and variable, this assume that
        # that first name is the control

        dplyr::mutate(difference = ((emmean - first(emmean))),
               perc_difference =((emmean - first(emmean) )/first(emmean))*100) %>%

        dplyr::mutate_if(is.numeric, round, 3)

}


## Get tidy table ---------------------------------------------------------------
emmeans_table_tidy <- function(model, grouping_var = NULL, model_list = FALSE,
                                formula = NULL){
    if(missing(formula)){
        stop("Error: At least one var should be specified! Example A, B or A|B")
    }

    if(model_list == TRUE) {

        reactable::reactable(purrr::map_df(model, ~ emmeans_df(.x, formula, grouping_var)),

                  groupBy = "resp_var",

                  rowStyle = JS("function(rowInfo) {
                        if (rowInfo.level > 0) {
                        return { background: '#eee',
                        borderLeft: '2px solid #ffa62d' }}
                        else {return { borderLeft: '2px solid transparent' }}}"),

                  columns = list(

                      # Adjust columns width
                      resp_var = colDef(minWidth = 140),
                      treatment = colDef(minWidth = 185),
                      emmean = colDef(minWidth = 120),
                      difference = colDef(minWidth = 120),

                      # Color p_value if it is less than 0.05
                      perc_difference = colDef(minWidth = 175,
                                               style = function(value) {
                                                   if (value <= 0 ) {color <- "red"}
                                                   else {color <- "#008000"}
                                                   list(color = color)})))
    }

    else
        reactable::reactable(emmeans_df(model, formula, grouping_var),

                  groupBy = "resp_var",

                  rowStyle = JS("function(rowInfo) {
                        if (rowInfo.level > 0) {
                        return { background: '#eee',
                        borderLeft: '2px solid #ffa62d' }}
                        else {return { borderLeft: '2px solid transparent' }}}"),

                  columns = list(

                      # Adjust columns width
                      resp_var = colDef(minWidth = 140),
                      treatment = colDef(minWidth = 185),
                      emmean = colDef(minWidth = 120),
                      difference = colDef(minWidth = 120),

                      # Color p_value if it is less than 0.05
                      perc_difference = colDef(minWidth = 175,
                                               style = function(value) {
                                                   if (value <= 0 ) {color <- "red"}
                                                   else {color <- "#008000"}
                                                   list(color = color)})))
    }


