# Model inference: Multiple comparisons table -----------------------------------

## First, get multiple comparisons as data frame --------------------------------
tukey_table_df <- function(model, formula){

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

    # Get contrasts
    # estimate: of the effect size, that is the difference
    # between the two emmeans (estimated marginal means)
    as.data.frame(emmeans::emmeans(model,
                              formula,
                              type = "response",
                              adjust ="tukey")$contrast) %>%
        janitor::clean_names() %>%

        # create column with the name of the response variable
        tibble::add_column(response_variable = response_var) %>%

        dplyr::mutate(response_variable = factor(response_variable),
                    contrast = factor(contrast)) %>%

        dplyr::select(response_variable, everything(),-c(df)) %>%

        dplyr::mutate_if(is.numeric, round, 4)

}


## Second, tidy table -----------------------------------------------------------
tukey_table_tidy <- function(model, model_list = FALSE, formula = NULL){

    if(missing(formula)){
        stop("Error: At least one var should be specified! Example A, B or A|B")
    }

    if(model_list == TRUE) {

        reactable::reactable(purrr::map_df(model, ~tukey_table_df(.x, formula)),

                  groupBy = "response_variable",

                  rowStyle = JS("function(rowInfo) {
                        if (rowInfo.level > 0) {
                        return { background: '#eee',
                        borderLeft: '2px solid #ffa62d' }}
                        else {return { borderLeft: '2px solid transparent' }}}"),

                  columns = list(

                      # Adjust columns width
                      response_variable = colDef(minWidth = 165),
                      contrast = colDef(minWidth = 185),

                      se = colDef(minWidth = 55),

                      # Color p_value if it is less than 0.05
                      p_value = colDef(minWidth = 60,
                          style = function(value) {
                            if (value >= 0.05) {color <- "black"}
                            else {color <- "#008000"}
                            list(color = color)})))
    }

    else
        reactable::reactable(tukey_table_df(model, formula),

                  groupBy = "response_variable",

                  rowStyle = JS("function(rowInfo) {
                        if (rowInfo.level > 0) {
                        return { background: '#eee',
                        borderLeft: '2px solid #ffa62d' }}
                        else {return { borderLeft: '2px solid transparent' }}}"),

                  columns = list(

                      # Adjust columns width
                      response_variable = colDef(minWidth = 165),
                      contrast = colDef(minWidth = 185),
                      se = colDef(minWidth = 55),

                      # Color p_value if it is less than 0.05
                      p_value = colDef(minWidth = 60,
                          style = function(value) {
                            if (value >= 0.05) {color <- "black"}
                            else {color <- "#008000"}
                            list(color = color)})))
    }

