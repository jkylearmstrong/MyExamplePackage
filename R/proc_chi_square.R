proc_chi_square <- function(data, factor, feature){

  enquo_feature <- enquo(feature)
  enquo_factor <- enquo(factor)

  tmp <- data %>%
    select(!!enquo_feature , !!enquo_factor) %>%
    collect()

  table2 <- table(tmp)

  plot_chi_square_residuals <-  vcd::mosaic(table2, gp = vcd::shading_max)

  plot_balloon <- gplots::balloonplot(table2,
                                      main ="Balloon Plot \n Area is proportional to Freq.")

  return( list(Frequency_Table = table2,
               plot_chi_square_residuals = plot_chi_square_residuals,
               plot_balloon = plot_balloon))

}
