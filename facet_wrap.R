facet_wrap <- function(p, pad=0.05) { 
  # get facet font attributes
  th = theme_get()
  if (length(p$theme) > 0L)
    th = th + p$theme
  
  require("grid")
  grobs <- ggplotGrob(p)
  
  # wrap strip x text
  if ((class(p$facet)[1] == "grid" && !is.null(names(p$facet$cols))) ||
      class(p$facet)[1] == "wrap")
  {
    ps = calc_element("strip.text.x", th)[["size"]]
    family = calc_element("strip.text.x", th)[["family"]]
    face = calc_element("strip.text.x", th)[["face"]]
    
    if (class(p$facet)[1] == "wrap") {
      nm = names(p$facet$facets)
    } else {
      nm = names(p$facet$cols)
    }
    
    # get number of facet columns
    levs = levels(factor(p$data[[nm]]))
    npanels = length(levs)
    if (class(p$facet)[1] == "wrap") {
      cols = n2mfrow(npanels)[1]
    } else {
      cols = npanels
    }
    
    # get plot width
    sum = sum(sapply(grobs$width, function(x) convertWidth(x, "in")))
    panels_width = par("din")[1] - sum  # inches
    # determine strwrap width
    panel_width = panels_width / cols
    mx_ind = which.max(nchar(levs))
    char_width = strwidth(levs[mx_ind], units="inches", cex=ps / par("ps"), 
                          family=family, font=gpar(fontface=face)$font) / 
      nchar(levs[mx_ind])
    width = floor((panel_width - pad)/ char_width)  # characters
    
    # wrap facet text
    p$data[[nm]] = unlist(lapply(strwrap(p$data[[nm]], width=width, 
                                         simplify=FALSE), paste, collapse="\n"))
  }
  
  if (class(p$facet)[1] == "grid" && !is.null(names(p$facet$rows))) {  
    ps = calc_element("strip.text.y", th)[["size"]]
    family = calc_element("strip.text.y", th)[["family"]]
    face = calc_element("strip.text.y", th)[["face"]]
    
    nm = names(p$facet$rows)
    
    # get number of facet columns
    levs = levels(factor(p$data[[nm]]))
    rows = length(levs)
    
    # get plot height
    sum = sum(sapply(grobs$height, function(x) convertWidth(x, "in")))
    panels_height = par("din")[2] - sum  # inches
    # determine strwrap width
    panels_height = panels_height / rows
    mx_ind = which.max(nchar(levs))
    char_height = strwidth(levs[mx_ind], units="inches", cex=ps / par("ps"), 
                           family=family, font=gpar(fontface=face)$font) / 
      nchar(levs[mx_ind])
    width = floor((panels_height - pad)/ char_height)  # characters
    
    # wrap facet text
    p$data[[nm]] = unlist(lapply(strwrap(p$data[[nm]], width=width, 
                                         simplify=FALSE), paste, collapse="\n"))
  }
  
  invisible(p)
}
