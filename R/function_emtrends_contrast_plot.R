# Create dataframe ------------------------------------------------------------


get_emtreds_contrast_df <- function(model, trait){

                        # get the response variable name from the model
                        terms <- terms(model)
                        response_var <- as.character(attr(terms, "variables"))[2]

                        # Get contrast
                        emmeans::emtrends(model,
                                pairwise ~ treatment,
                                var = trait)$contrast %>%

                                # Transform object to a tibble
                                tibble::as_tibble() %>%

                                # Clean column names
                                janitor::clean_names() %>%

                                # Add new column with the trait
                                tibble::add_column(trait = trait) %>%

                                # Add new column with the response var
                                tibble::add_column(response_var = response_var) %>%

                                # Replace characters
                                dplyr::mutate(contrast = base::gsub('-', 'vs.',
                                                                    .$contrast),
                                            # Invert estimates to improve
                                            # readability
                                            estimate =  estimate * -1,
                                            lower_cl =  lower_cl * -1,
                                            upper_cl =  upper_cl * -1,

                                            # Create new column to highlight
                                            # significant terms
                                            significant = ifelse(p_value < 0.05,
                                                                    TRUE, FALSE)) %>%
                                # Replace characters
                                dplyr::mutate(contrast = base::gsub('_', ' ',
                                                                    .$contrast))
                        }

# Create plot -----------------------------------------------------------------

plot_emtrents_contrast <- function(model, trait, filter_treatments = TRUE) {

    # Map dataframe in case several traits are given
    data_emtrents_contrast <- purrr::map_dfr(trait,
                                            ~get_emtreds_contrast_df(trait = .x,
                                            model = model)) %>%
                        dplyr::mutate(response_var = factor(response_var))

    # Create a cleaner dataset for the plot
    if(filter_treatments == TRUE){
        data_emtrents_contrast <-
                        data_emtrents_contrast %>%

                            # Get the contrasts that contain no_additions
                            filter(stringr::str_detect(contrast,
                                                        "no additions"))
    }else{
       data_emtrents_contrast
    }

    # Cleveland plot
    ggplot2::ggplot(data = data_emtrents_contrast, aes(x = contrast,
                                                        y = estimate,
                                                        color = significant,
                                                        shape = trait)) +

    # Highlight significant terms
    ggplot2::scale_color_manual(values = c("grey", "black")) +

    # Add horizontal line
    ggplot2::geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
    ggplot2::geom_point(size = 6, position = position_dodge(width = .9)) +

    # add 95% Confidence intervals
    ggplot2::geom_linerange(aes(ymin = lower_cl	, ymax = upper_cl),
                            lwd = 1, position = position_dodge(width = .9)) +

    # Choose the theme
    ggplot2::theme_bw() +

    # Axis labels
    ggplot2::ylab("Estimated coefficients (median +/- 95CI)") +
    ggplot2::xlab("") +

    # Flip plot
    ggplot2::coord_flip() +

    # Custumize legend
    ggplot2::theme(legend.position = "bottom",
                    strip.text.x  = ggplot2::element_text(size = 25),
                    axis.text.y   = ggplot2::element_text(size = 25),
                    axis.text.x   = ggplot2::element_text(size = 25),
                    axis.title.y  = ggplot2::element_text(size = 25),
                    axis.title.x  = ggplot2::element_text(size = 25),
                    panel.grid.major.y = ggplot2::element_blank(),
                    panel.grid.minor   = ggplot2::element_blank(),
                    axis.line    = ggplot2::element_line(size = .4,
                                                         colour = "black"),
                    panel.border = ggplot2::element_rect(colour = "black",
                                                        fill = NA,
                                                        size = 1.3)) +

    # Add the response variable as title
    ggplot2::facet_wrap(~response_var, scales = "free_x", ncol = 2) +

    # Select the shapes for each trait
    ggplot2::scale_shape_manual(values = c(15, 16, 17, 18,19))

}