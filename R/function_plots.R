#' @importFrom magrittr %>%

#' @title Box plot for visualizing the data in the exploratory data analysis
#'
#' @author Erick Calderon-Morales
#'
#' @description This function is used for the EDA and is though for running along
#' with purrr::pmap for creating plots of each variable with a line of code
#'
#' @details Color scheme of the treatments in the experiment
#'
#' Harvestatthebegging = Black ("#000000")
#' ambientrain = Yellow ("#F0E442")
#' ambientrain_nutrients = Green ("#009E73")
#' ambientrain_water = Light blue ("#56B4E9")
#' ambientrain_water_nutrients = Dark blue ("#0072B2")
#'
#' @examples
#' \dontrun{
#' pmap( ~ boxplot_plot_pmap(data = data_complete,y = !!sym(..1), x = !!sym(..2),
#'                            fill = !!sym(..3)))
#'}
#'
#' @export


# Box plot for using it with pmap ----------------------------------------------

boxplot_plot_pmap <-  function(x, y, fill = NULL, data) {

    xvar <- rlang::enquo(x)
    yvar <- rlang::enquo(y)
    fill <- rlang::enquo(fill)

    ggplot2::ggplot(data, ggplot2::aes(fill = !!fill, x = !!xvar, y = !!yvar )) +
        ggplot2::geom_boxplot() +
        ggplot2::scale_fill_manual(values = c("#F0E442","#009E73",
                                              "#56B4E9","#0072B2")) +

        ggplot2::theme_classic() +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::theme(axis.text=element_text(size = 21),
                       axis.title=element_text(size = 21,face = "bold"))
}


# Cleveland plot ---------------------------------------------------------------

cleveland_plot <-function(x, y, color = NULL, shape  = NULL ,
                          ci_lower, ci_upper, data){

    xvar <- rlang::enquo(x)
    yvar <- rlang::enquo(y)
    color <- rlang::enquo(color)
    shape <- rlang::enquo(shape)
    upper <- rlang::enquo(ci_upper)
    lower <- rlang::enquo(ci_lower)

    ggplot2::ggplot(data = data,

                # Highlight significant terms
                aes(x = !!xvar, y = !!yvar,
                    color = !!color, shape = !!shape)) +


    ggplot2::geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
    ggplot2::geom_point(size = 6, position = position_dodge(width = .9)) +

    # 95% C.I
    ggplot2::geom_linerange(aes(ymin = !!lower, ymax = !!upper),
                            lwd = 1, position = position_dodge(width = .9)) +
    ggplot2::theme_bw() +

    ggplot2::theme(legend.position = "none",
                   strip.text.x = element_text(size = 25),
                   axis.text.y   = element_text(size = 25),
                   axis.text.x   = element_text(size = 25),
                   axis.title.y  = element_text(size = 25),
                   axis.title.x  = element_text(size = 25),
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(size = .4, colour = "black"),
                   panel.border = element_rect(colour = "black", fill = NA,
                                               size = 1.3)) +
    # Add name
    #labs(title = paste0(response_variable)) +
    ggplot2::facet_wrap(~ response_var, scales = "free", ncol = 2) +
    # Significance colors
    ggplot2::scale_color_manual(values = c("#F0E442","#009E73",
                                           "#56B4E9","#0072B2")) +

    scale_shape_manual(values = c(1, 16)) +
    ggplot2::ylab("Estimated coefficients (median +/- 95CI)") +
    ggplot2::xlab("") +
    ggplot2::coord_flip()

}






#  Simulate --------------------------------------------------------------------

#' @importFrom magrittr %>%
#'
#' @title Simulate models coefficient values of a model for obtaining the
#' posterior probability distribution
#'
#' @author Erick Calderon-Morales
#'
#' @description This function takes a model and returns a data frame with the
#' median and upper and lower confidence intervals.
#'
#' @param model  lme4 or lm object
#' @param iter number of simulations
#' @param interaction_3way Does the model contains a 3-way interaction?
#'
#' @examples
#' \dontrun{
#' simulate_coefs(model,iter, interaction_3way = FALSE )
#'}
#'
#' @export

simulate_coefs <- function(model,iter, interaction_3way = FALSE ){

    # Recognize 3way interaction
    if (interaction_3way == TRUE) {

        # Words to remove from labels
        #pattern <- c('\\+.*|nfixer|treatment|_')

        # Get response variable name for add it as title
        response_variable <- as.character(attr(terms(model), "variables"))[2]
        response_variable <- stringr::str_to_title(stringr::str_replace_all(response_variable,
                                                                            pattern = '_',
                                                                            replacement = ' '))

        merTools::FEsim(model, iter) %>%

            # Get CIs
            dplyr::mutate(lower =  median - 1.96 * sd, upper =  median + 1.96 * sd) %>%

            # Remove unnecessary terms
            dplyr::filter(!term %in% c("(Intercept)","init_height" )) %>%

            # Create significance column
            dplyr::mutate(significance = if_else((lower > 0 & upper > 0 | lower < 0 & upper < 0),
                                          TRUE, FALSE)) %>%

            # Filter unwanted treatments
            # dplyr::filter(!term %in% c("treatmentplus_nutrients","treatmentplus_water",
            #                        "treatmentplus_water_nutrients", "nfixerfixer",
            #
            #                        "treatmentplus_nutrients:nfixerfixer",
            #                        "treatmentplus_water:nfixerfixer",
            #                        "treatmentplus_water_nutrients:nfixerfixer",
            #
            #                        "amax_log", "d13c", "d15n", "gs_sqrt", "wue_log",
            #                        "pnue_log",
            #
            #                        "nfixerfixer:amax_log","nfixerfixer:d13c",
            #                        "nfixerfixer:d15n","nfixerfixer:gs_sqrt",
            #                        "nfixerfixer:wue_log", "nfixerfixer:pnue_log",
            #
            #                        "treatmentplus_nutrients:amax_log","treatmentplus_nutrients:d13c",
            #                        "treatmentplus_nutrients:d15n","treatmentplus_nutrients:gs_sqrt",
            #                        "treatmentplus_nutrients:wue_log", "treatmentplus_nutrients:pnue_log",
            #
            #                        "treatmentplus_water:amax_log","treatmentplus_water:d13c",
            #                        "treatmentplus_water:d15n","treatmentplus_water:gs_sqrt",
            #                        "treatmentplus_water:wue_log", "treatmentplus_water:photo_nitrogen_use_log",
            #
            #                        "treatmentplus_water_nutrients:amax_log","treatmentplus_water_nutrients:d13c",
            #                        "treatmentplus_water_nutrients:d15n","treatmentplus_water_nutrients:gs_sqrt",
            #                        "treatmentplus_water_nutrients:wue_log", "treatmentplus_water_nutrients:pnue_log"
            #

            #)) %>%

            #arrange(term) %>%

            # Edit factor levels
            #dplyr::mutate(term = stringr::str_replace_all(term, #pattern,
            #                                              replacement = ' ')) %>%

            #dplyr::mutate(term = stringr::str_replace_all(term, pattern = ' ',
            #                                              replacement = '')) %>%
            tibble::add_column(response_variable) %>%
            dplyr::select(response_variable, term, everything(), -mean)


    } else if(interaction_3way == FALSE){

        # Words to remove from labels
        pattern <- c('\\+.*|nfixer|treatment|_')

        response_variable <- as.character(attr(terms(model), "variables"))[2]

        merTools::FEsim(model, iter) %>%

            # Get CIs
            dplyr::mutate(lower =  median - 1.96 * sd,
                   upper =  median + 1.96 * sd) %>%

            # Remove unnecessary terms
            dplyr::filter(!term %in% c("(Intercept)","init_height" )) %>%

            # Create significance column
            dplyr::mutate(significance = if_else((lower_bound > 0 & upper__bound > 0 | lower_bound < 0 & upper_bound < 0),
                                          TRUE, FALSE)) %>%

            # Filter unwanted terms

            arrange(term) %>%

            # Edit factor levels
            dplyr::mutate(term = stringr::str_replace_all(term, pattern,
                                                          replacement = ' ')) %>%

            dplyr::mutate(term = stringr::str_replace_all(term, pattern = ' ',
                                                          replacement = '')) %>%
            tibble::add_column(response_variable) %>%
            dplyr::select(response_variable,term, everything(), -mean)

    }
}

# Plot simulated data frame ----------------------------------------------------

#' @importFrom magrittr %>%

#' @title Coefficient plots
#'
#' @author Erick Calderon-Morales
#'
#' @description This function takes the data frame generated with the simulate_coefs
#' and returns a ggplot
#'
#' @param model  lme4 or lm object
#' @param iter number of simulations
#' @param interaction_3way Does the model contains a 3-way interaction?
#'
#' @examples
#' \dontrun{
#' plot_simulate_conf_int(model, iter, interact_3way = F)
#'}
#'
#' @export

plot_simulate_conf_int <- function(model, iter, interact_3way = F ){

    if(interact_3way == F) {

        # Plot
        ggplot2::ggplot(data = purrr::map_dfr(model, ~ simulate_coefs(.,iter ,
                                                                      interaction_3way = F)),
               # Highlight significant terms
               aes(x = reorder(term, -median), y = median, color = significance)) +


            ggplot2::geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
            ggplot2::geom_point(position = position_dodge(width = .9)) +

            # 95% C.I
            ggplot2::geom_linerange(aes(ymin = lower,ymax = upper),
                                    lwd = 1, position = position_dodge(width = .9)) +

            # 99% C.I
            #geom_linerange(aes(ymin = !!yvar - !!se * interval2,
            #                   ymax = !!yvar + !!se * interval2),
            #               lwd = 1/2, position = position_dodge(width = .9)) +

            ggplot2::theme_bw() +

            ggplot2::theme(legend.position = "none",
                           axis.text.y   = element_text(size= 15),
                           axis.text.x   = element_text(size= 15),
                           axis.title.y  = element_text(size= 15),
                           axis.title.x  = element_text(size= 15),
                           panel.grid.major.y = element_blank(),
                           panel.grid.minor = element_blank(),
                           axis.line = element_line(size = .4,colour = "black"),
                           panel.border = element_rect(colour = "black", fill= NA,
                                                       size = 1.3)) +
            # Add name
            #labs(title = paste0(response_variable)) +

            # Significance colors
            ggplot2::scale_colour_manual(values = c("gray","black")) +
            ggplot2::ylab("Estimated effects of treatments (median +/- CI)") +
            ggplot2::xlab("") +

            ggplot2::facet_wrap(~ response_variable, scales = "free_x", ncol = 2) +


            ggplot2::coord_flip()

    } else if (interact_3way == T) {

        # Plot
        ggplot2::ggplot(data = purrr::map_dfr(model, ~ simulate_coefs(., iter,
                                                                      interaction_3way = T)),

                        # Highlight significant terms
                        aes(x = reorder(term, -median), y = median,color = significance)) +

            ggplot2::geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
            ggplot2::geom_point(position = position_dodge(width = .9)) +

            # 95% C.I
            ggplot2::geom_linerange(aes(ymin = lower,
                                        ymax = upper),
                                    lwd = 1, position = position_dodge(width = .9)) +

            # 99% C.I
            #geom_linerange(aes(ymin = !!yvar - !!se * interval2,
            #                   ymax = !!yvar + !!se * interval2),
            #               lwd = 1/2, position = position_dodge(width = .9)) +

            ggplot2::theme_bw() +

            ggplot2::theme(legend.position = "none",
                           axis.text.y   = element_text(size= 15),
                           axis.text.x   = element_text(size= 15),
                           axis.title.y  = element_text(size= 15),
                           axis.title.x  = element_text(size= 15),
                           panel.grid.major.y = element_blank(),
                           panel.grid.minor = element_blank(),
                           axis.line = element_line(size = .4,colour = "black"),
                           panel.border = element_rect(colour = "black", fill= NA,
                                                       size = 1.3)) +
            # Add name
            #labs(title = paste0(response_variable)) +

            # Significance colors
            ggplot2::scale_colour_manual(values = c("gray","black")) +
            ggplot2::ylab("Estimated effects of treatments (median +/- CI)") +
            ggplot2::xlab("") +

            ggplot2::facet_wrap(~ response_variable, scales = "free_x", nrow = 2) +

            ggplot2::coord_flip()
        }
}

