StatRollapplyr <- ggproto("StatRollapplyr", Stat, 
                          required_aes = c("x", "y"),
                          
                          compute_group = function(data, scales, width, align, FUN, 
                                                   index.ref, index.basis, ...) {
                            data <- data[order(data$x), ]
                            if(!is.null(index.ref)){
                              data$y <- index_help(data$y, ref = index.ref, 
                                                   basis = index.basis)
                            }
                            y_ra <- zoo::rollapplyr(data$y, FUN = FUN, width = width, 
                                                    fill = NA, align = align, ...)
                            
                            result <- data.frame(x = data$x, y = as.numeric(y_ra))
                            return(result)
                          }
)

stat_rollapplyr <- function(mapping = NULL, data = NULL, geom = "line",
                            position = "identity", show.legend = NA, 
                            inherit.aes = TRUE, width, align = "right", 
                            FUN = mean, index.ref = NULL, index.basis = 100, ...) {
  ggplot2::layer(
    stat = StatRollapplyr, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(width = width, align = align, na.rm = FALSE, FUN = FUN, 
                  index.ref = index.ref, index.basis = index.basis, ...)
    # note that this function is unforgiving of NAs.
  )
}