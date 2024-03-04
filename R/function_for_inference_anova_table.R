# Model inference: Anova tidy table ---------------------------------------------

## First, get table as dataframe ------------------------------------------------
anova_table_df <- function(model){

    # get the response variable name from the model
    terms <- terms(model)
    response_var <- as.character(attr(terms, "variables"))[2]

    #trait_var <- as.character(attr(terms, "variables"))[5]

    # Generate anova table
    car::Anova(model, type = "III", test.statistic = c("F")) %>%

        data.frame() %>%
        tibble::rownames_to_column("fixed_effects") %>%

        # create column with the name of the response variable
        tibble::add_column(response_variable = response_var) %>%
        janitor::clean_names() %>%

        # Round p and f values
        dplyr::mutate(f = round(f, 4),
               pr_f =     round(pr_f, 20)) %>%
        dplyr::select(response_variable, fixed_effects, everything())
}


## Second, generate final table --------------------------------------------------
anova_table_tidy <- function(model, model_list = FALSE){

    if(model_list == TRUE) {

       reactable::reactable(purrr::map_df(model, anova_table_df),

                  groupBy = "response_variable",

                  rowStyle = JS("function(rowInfo) {
                        if (rowInfo.level > 0) {
                        return { background: '#eee',
                        borderLeft: '2px solid #ffa62d' }}
                        else {return { borderLeft: '2px solid transparent' }}}"),

                  columns = list(

                      # Adjust columns width
                      response_variable = colDef(minWidth = 165),
                      fixed_effects = colDef(minWidth = 125),
                      f = colDef(minWidth = 70),
                      df = colDef(minWidth = 70),

                      # Color p_value if it is less than 0.05
                      pr_f = colDef(style = function(value) {
                          if (value >= 0.05) {color <- "black"}
                          else {color <- "#008000"}
                          list(color = color)})))
    } else
        reactable::reactable(anova_table_df(model),

                  groupBy = "response_variable",

                  rowStyle = JS("function(rowInfo) {
                        if (rowInfo.level > 0) {
                        return { background: '#eee',
                        borderLeft: '2px solid #ffa62d' }}
                        else {return { borderLeft: '2px solid transparent' }}}"),

                  columns = list(

                      # Adjust columns width

                      response_variable = colDef(minWidth = 165),
                      fixed_effects = colDef(minWidth = 125),
                      f = colDef(minWidth = 70),
                      df = colDef(minWidth = 70),

                      # Color p_value if it is less than 0.05
                      pr_f = colDef(style = function(value) {
                          if (value >= 0.05) {color <- "black"}
                          else {color <- "#008000"}
                          list(color = color)})))

}
