#' PCA plot based on ggplot2 using the stats::prcomp function
#'
#' @param data A data frame containing the first collumn as the grouping information
#' @param pc Which PC to present, one of 12, 13, 23, the default value is 12
#' @param level default value is 0.68
#'
#' @return
#' @export
#'
#' @author ZhonghuiGai
#' @examples
#' data <- iris
#' data <- data[, c(5, 1:4)]
#' colnames(data)[1] <- "group"
#' ggPCA(data)
ggPCA <- function(data, pc = 12,
                   level = 0.68){
  pca <- stats::prcomp(data[, -1], scale. = TRUE)
  site <- pca$x[, 1:3]
  eig <- summary(pca)$importance[2,]*100
  p.v <- vegan::adonis(data[, -1]~group,
                       data = data)$aov.tab$`Pr(>F)`[1]
  if(all(rownames(site) == rownames(data))){
    pca.data <- data.frame(group = data$group, site)
    colnames(pca.data)[2:4] <- paste0("PC", 1:3)
  }
  label <- data.frame(min = apply(pca.data[, 2:4], 2, min),
                      max = apply(pca.data[, 2:4], 2, max))
  label$mean <- (label$min + label$max)/2
  if (pc == 12) {
    x <- "PC1"
    y <- "PC2"
    x.posi <- label[1, 3]
    y.posi <- label[2, 2]
    x.lab <- paste0(x, ": ", eig[1], "%")
    y.lab <- paste0(y, ": ", eig[2], "%")
  }else if (pc == 13) {
    x <- "PC1"
    y <- "PC3"
    x.posi <- label[1, 3]
    y.posi <- label[3, 2]
    x.lab <- paste0(x, ": ", eig[1], "%")
    y.lab <- paste0(y, ": ", eig[3], "%")
  }else if (pc == 23) {
    x <- "PC2"
    y <- "PC3"
    x.posi <- label[2, 3]
    y.posi <- label[3, 2]
    x.lab <- paste0(x, ": ", eig[2], "%")
    y.lab <- paste0(y, ": ", eig[3], "%")
  }
  p <- ggplot(data = pca.data, aes_string(x = x, y = y, color = "group")) +
    geom_point(aes(color = group, shape = group),
               size = 1.5, alpha = 1) +
    stat_ellipse(level = level,  linetype = 3,
                 geom = "polygon", alpha = 0.02,
                 aes(fill = group), show.legend = FALSE) +
    xlab(x.lab) + ylab(y.lab) +
    annotate(geom = "text", x = x.posi, y = y.posi*1.25,
             label = paste0("adonis: p.value =  ", p.v),
             size = 4.5, fontface = "bold.italic",
             colour = ifelse(p.v < 0.05, "red1", "black"))
  p <- p + theme(panel.grid = element_line(color = 'gray90', size = 0.1),
                 panel.background = element_rect(color = 'gray60',
                                                 fill = 'transparent', size = 1),
                 axis.text = element_text(size = 12, face = "bold", color = "black"),
                 axis.text.x = element_text(colour = "black", size = 12, face = "bold"),
                 axis.title = element_text(size = 12, face = "bold"),
                 legend.text = element_text(size = 10, face = "bold"),
                 legend.title = element_blank(),
                 legend.position = "right",
                 panel.border = element_rect(colour = "black", fill = "transparent"),
                 legend.background = element_rect(fill = "transparent"),
                 legend.key = element_rect(fill = "transparent"))
  return(p)
}
