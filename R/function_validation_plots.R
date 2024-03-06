validation_plots <- function(model, data, group, variables = c()) {

    a <- plot(model, type = c("p", "smooth"))

    ## heteroscedasticity
    b <-  plot(model, sqrt(abs(resid(.))) ~ fitted(.), type = c("p", "smooth"))

    c <- plot(model, resid(., scaled = TRUE) ~ fitted(.), abline = 0, pch = 16,
              xlab = "Fitted values", ylab = "Standardised residuals")

    d <- plot(model, resid(., scaled = TRUE) ~ fitted(.),
              abline = 0, pch = 16, col = data[,group],
              xlab = "Fitted values", ylab = "Standardised residuals")

    e <- plot(model, formula(paste0("resid(., scaled = TRUE) ~ fitted(.) |", group)),
              abline = 0, pch = 16, xlab = "Fitted values",
              ylab = "Standardised residuals")

    f <- plot(model, formula(paste0(group, "~ resid(., scaled=TRUE)")),
              abline = 0, pch = 16, xlab = "Standardised residuals", ylab = "spcode")


    (g <- car::qqPlot(resid(model)))

    (h <- plot(resid(model, type = "pearson") ~ hat(model.matrix(model)),
               las = 1,
               ylab = "Standardised residuals",
               xlab = "Leverage"))

    (i <- hist(resid(model), xlab = "Residuals", main = ""))

    if (length(variables) > 0){
        for (each_variable in variables) {

            plot(data[each_variable],
                 resid(model),
                 xlab = each_variable,
                 ylab = "Residuals")}} else{


                    print("No variable specified inthe variables argument")

                 }


    return(cowplot::plot_grid(a,b,c,d,e,f, ncol = 3))

}
