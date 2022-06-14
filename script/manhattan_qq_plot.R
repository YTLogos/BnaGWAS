# ====================manhattan plot========================
# global variables to escape r cmd check
utils::globalVariables(c("index", "marker", "chrom_alt", "xbreaks"))
#' Make manhattan plot with full ggplot customizability
#'
#' This function is provided to make manhattan plot with full ggplot customizability. So next
#' we can customize the manhattan plot with kinds of functions of ggplot2 and add additional layers.
#' @param gwasres a data frame of gwas results.
#' @param snp Name of the column containing SNP identifiers; default is NA.
#' @param bp Name of the column containing the SNP positions; default is NA.
#' @param chrom Name of the column containing the chromosome identifers; default is NA.
#' @param pvalue Name of the column containing the p values; default is NA.
#' @param vlinetype the type of vline (geom_vline()). The default is "solid".
#' @param vlinesize the size of the vline. The default is 0.75.
#' @param title the title of manhattan plot. The default is "Manhattan Plot".
#' @param color the colors of alternate chromosome. The default is "#FF8C00" and "#556B2F"
#' @param pointsize the size of point. The default is 1.25.
#' @param file.output a logical, if file.output=TRUE, the result will be saved.
#' if file.output=FALSE, the result will be printed. The default is TRUE.
#' @param file a character, users can choose the different output formats of plot,
#' so far, "jpeg", "pdf", "png", "tiff" can be selected by users. The default is "png".
#' @param dpi a number, the picture element for .jpeg, .png and .tiff files. The default is 300.
#' @param output a character, the name of your trait. The default is "Trait"
#'
#' @author Tao Yan <\email{tyan@zju.edu.cn}> |
#' <\href{https://taoyan.netlify.com/}{https://taoyan.netlify.com/}>
#'
#' @import ggplot2
#' @import gtools
#' @return a manhattan plot based on ggplot2.
#'
#' @export
#'
#' @examples
#' ggmanhattan(gwas_test)
#'
#'
#' search.names
#' @keywords internal
#'
#' @return Nothing; internal function
#'
search.names <- function(term, dfnames) {
  for (i in 1:length(term)) {
    res <- grep(paste0("\\b", term[i], "\\b"), dfnames, ignore.case = TRUE)
    if (length(res) > 0) {
      if (length(res) == 1) {
        return(dfnames[res])
      }
    }
  }
}

ggmanhattan <- function(gwasres,
                        snp = NA,
                        bp = NA,
                        chrom = NA,
                        pvalue = NA,
                        index = NA,
                        ref="Darmor-bzh",
                        file.output = FALSE,
                        file = "png",
                        output = "Trait",
                        dpi = 300,
                        vlinetype = "solid",
                        vlinesize = 0.75,
                        title = "Manhattan Plot",
                        color = c("#FF8C00", "#556B2F"),
                        pointsize = 0.75,
                        p_select = 6,
                        verbose = TRUE, ...) {
  dfnames <- names(gwasres)
  if (is.na(chrom)) {
    chrom <- search.names(c("chr", "chrom", "chromosome"), dfnames)
    if (is.null(chrom)) {
      stop("Couldn't find the chromosome column. Please specify the name of the column with chromosome ids(chr,chrom or chromosome)")
    }
  }
  if (is.na(snp)) {
    snp <- search.names(c("snp", "snpid", "rs", "rsid"), dfnames)
    if (is.null(snp)) {
      stop("Couldn't find the snp column. Please specify the name of the column with snp ids(snp, snpid, rs or rsid)")
    }
  }
  if (is.na(bp)) {
    bp <- search.names(c("bp", "pos", "position"), dfnames)
    if (is.null(bp)) {
      stop("Couldn't find the bp column. Please specify the name of the column with bp ids(bp, pos or position)")
    }
  }
  if (is.na(pvalue)) {
    pvalue <- search.names(c("p", "p-value", "pvalue", "pval"), dfnames)
    if (is.null(chrom)) {
      stop("Couldn't find the pvalue column. Please specify the name of the column with pvalue ids(p, pvalue, pval or p-value)")
    }
  }
  df <- as.data.frame(gwasres)
  df$chrom <- df[, chrom]
  df$chrom <- as.character(df$chrom)
  df$bp <- as.numeric(as.character(df[, bp]))
  df$pvalue <- as.numeric(as.character(df[, pvalue]))
  df$snp <- df[, snp]

  if (is.na(index)) {
    df <- df[order(df$bp), ]
    df <- df[mixedorder(df$chrom), ]
    df$index <- 1:nrow(df)
  }else {
    df$index <- df[, index]
    df <- df[order(df$index), ]
    df <- df[mixedorder(df$chrom), ]
  }

  # calculate the numbers of chromosome
  chrnum <- data.frame(table(df$chrom))
  chrnum$Var1 <- as.character(chrnum$Var1)
  chrnum <- chrnum[mixedorder(chrnum$Var1), ]

  # marker the odd and even chromosome
  chrom_odd <- as.character(chrnum$Var1[seq(1, nrow(chrnum), 2)])
  df$chrom_alt <- replace(df$chrom, df$chrom %in% chrom_odd, 0)
  df$chrom_alt <- replace(df$chrom_alt, df$chrom_alt != 0, 1)

  #-Log10 transform the pvalue

  df$marker <- -log10(df$pvalue)

  # specify the y limit
  ymax <- max(df$marker) + 0.1

  # specify x axis tick points

  df_split <- split(df, df$chrom)
  xbreaks <- sapply(df_split, function(x) {
    midpoint <- length(x$index) / 2
    if (midpoint < 1) midpoint <- 1
    return(x$index[midpoint])
  })

  # calculate the number of SNPs

  snp_num <- cumsum(chrnum$Freq)

  # make the manhattan plot
  if (ref=="ZS11"){
    p1 <- ggplot(df, aes(x = index, y = marker, colour = as.factor(chrom_alt))) +
      geom_point(size = pointsize) +
      scale_x_continuous(breaks = xbreaks, labels = c("A01", "A10", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09"), expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0), limits = c(1, ymax), breaks = seq(1, ymax, 2)) +
      guides(colour = "none") +
      labs(x = "Chromosomes", y = expression(bold(-log[10] ~ (pvalue))), title = title) +
      geom_vline(xintercept = snp_num-200, colour = "#E8E8E8", size = vlinesize, linetype = vlinetype) +
      geom_hline(yintercept = p_select, color = "blue", linetype="dashed") +
      scale_color_manual(values = color) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "black", size = 1),
        axis.line.y = element_line(color = "black", size = 1),
        axis.ticks.length = unit(.25, "cm"),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 12, colour = "black", family = "Times New Roman"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold", family = "Times New Roman")
      )
    class(p1) <- append(class(p1), "ggman")
  }else{
    p1 <- ggplot(df, aes(x = index, y = marker, colour = as.factor(chrom_alt))) +
      geom_point(size = pointsize) +
      scale_x_continuous(breaks = xbreaks, labels = c("A01", "A10", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "A02", "An", "Cn", "A03", "A04", "A05", "A06", "A07", "A08", "A09"), expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0), limits = c(1, ymax), breaks = seq(1, ymax, 2)) +
      guides(colour="none") +
      labs(x = "Chromosomes", y = expression(bold(-log[10] ~ (pvalue))), title = title) +
      geom_vline(xintercept = snp_num-200, colour = "#E8E8E8", size = vlinesize, linetype = vlinetype) +
      geom_hline(yintercept = p_select, color = "blue", linetype="dashed") +
      scale_color_manual(values = color) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "black", size = 1),
        axis.line.y = element_line(color = "black", size = 1),
        axis.ticks.length = unit(.25, "cm"),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 12, colour = "black", family = "Times New Roman"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold", family = "Times New Roman")
      )
    class(p1) <- append(class(p1), "ggman")
  }
  

  # whether to save the plot or not

  if (!file.output) {
    if (verbose) cat("The Manhattan Plot is Plotting! Please wait for a moment...", sep = "\n")
    return(p1)
  }
  if (file.output) {
    if (verbose) cat("The Manhattan Plot is Plotting! Please wait for a moment...", sep = "\n")
    if (file == "jpg") jpeg(paste("The_Manhattan_Plot_of_", output, ".jpg", sep = ""), width = 15 * dpi, height = 7 * dpi, res = dpi, quality = 100)
    if (file == "pdf") pdf(paste("The_Manhattan_Plot_of_", output, ".pdf", sep = ""), width = 15, height = 7)
    if (file == "tiff") tiff(paste("The_Manhattan_Plot_of_", output, ".tiff", sep = ""), width = 15 * dpi, height = 7 * dpi, res = dpi)
    if (file == "png") png(paste("The_Manhattan_Plot_of_", output, ".png", sep = ""), width = 15 * dpi, height = 7 * dpi, res = dpi)
    par(xpd = TRUE)
  } else {
    if (is.null(dev.list())) dev.new(width = 9, height = 7)
    par(xpd = TRUE)
  }
  print(p1)
  if (file.output) dev.off()
  if (file.output & verbose) cat(paste("The Manhattan Plot is stored in: ", getwd(), sep = ""), sep = "\n")
}

# ============================gwas result manipulation====================

# splite_chr <- function(data, c) {
#   c(str_split(data[c], pattern = "_")[[1]][1])
# }
# splite_pos <- function(data, c) {
#   c(str_split(data[c], pattern = "_")[[1]][2])
# }

manhattan_data_prepare <- function(gwas_res_emmax) {
  colnames(gwas_res_emmax) <- c("SNP", "Beta", "SE", "P")
  # plan(multiprocess)
  # gwas_res_emmax$CHR <- future.apply::future_apply(gwas_res_emmax, 1, splite_chr, c = "SNP")
  # gwas_res_emmax$BP <- future.apply::future_apply(gwas_res_emmax, 1, splite_pos, c = "SNP")
  chr_pos <- colsplit(string = gwas_res_emmax$SNP, pattern = "_", names = c("CHR", "BP"))
  gwas_res_emmax <- cbind(gwas_res_emmax, chr_pos)
  gwas_res_emmax <- gwas_res_emmax %>% dplyr::select(SNP, CHR, BP, P)
  gwas_res_emmax$CHR <- as.integer(gwas_res_emmax$CHR)
  gwas_res_emmax$BP <- as.integer(gwas_res_emmax$BP)
  gwas_res_emmax["P"][gwas_res_emmax["P"] == 0] <- NA
  gwas_res_emmax <- gwas_res_emmax %>% tidyr::drop_na()
  return(gwas_res_emmax)
}




myqq = function(pvector, ...) {
  
  # Check for sensible input
  if (!is.numeric(pvector)) stop("Input must be numeric.")
  
  # limit to not missing, not nan, not null, not infinite, between 0 and 1
  pvector <- pvector[!is.na(pvector) & !is.nan(pvector) & !is.null(pvector) & is.finite(pvector) & pvector<1 & pvector>0]
  
  # Observed and expected
  o = -log10(sort(pvector,decreasing=FALSE))
  e = -log10( ppoints(length(pvector) ))
  
  
  #     # The old way
  #     plot(e, o, pch=20, 
  #          xlab=expression(Expected~~-log[10](italic(p))), 
  #          ylab=expression(Observed~~-log[10](italic(p))), 
  #          ...)
  
  # The new way to initialize the plot.
  ## See http://stackoverflow.com/q/23922130/654296
  ## First, define your default arguments
  def_args <- list(pch=20, xlim=c(0, max(e)), ylim=c(1, max(o)), 
                   xlab=expression(Expected~~-log[10](italic(p))), 
                   ylab=expression(Observed~~-log[10](italic(p)))
  )
  ## Next, get a list of ... arguments
  #dotargs <- as.list(match.call())[-1L]
  dotargs <- list(...)
  ## And call the plot function passing NA, your ... arguments, and the default
  ## arguments that were not defined in the ... arguments.
  tryCatch(do.call("plot", c(list(x=e, y=o), def_args[!names(def_args) %in% names(dotargs)], dotargs)), warn=stop)
  
  # Add diagonal
  abline(1,1,col="red")
  
}

