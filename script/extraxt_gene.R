#' Get the candidate genes in regions based on significant SNPs of GWAS results.
#'
#' This function is developed to get the candidate genes in regions based on significant SNPs of GWAS results.
#' @param gff a data frame of all the gene (transcript), must have column names.
#' @param sig.snp a data frame of significant SNPs.
#' @param distance numeric (bp), it is to define the region. The default is 50000, you need to choose it based on the LD distance in your study.
#' @param geneid Name of the column containing the geneid in gff file; default is NA.
#'
#' @param gff.chrom Name of the column containing the chromosome identifers in the gff file; default is NA.
#' @param snp.chrom Name of the column containing the chromosome identifers in the snp.sig file; default is NA.
#' @param pvalue Name of the column containing the p values in snp.sig file; default is NA.
#' @param snp_location Name of the column containing the snp position in snp.sig file; default is NA.
#' @param file.save a logical, if file.output=TRUE, the result will be saved.
#' if file.output=FALSE, the result will be printed. The default is TRUE.
#' @param file.type a character, users can choose the different output formats,
#' so far, "csv", "txt", "xlsx" can be selected by users. The default is "csv".
#'
#' @author Tao Yan <\email{tyan@zju.edu.cn}> |
#' <\href{https://taoyan.netlify.com/}{https://taoyan.netlify.com/}>
#'
#' @import writexl
#' @return a data.frame contain the candidate genes with start,end,genid etc
#'
#' @export
#'
#' @examples
#' get_gene_from_snp(gff, sig.snp)
get_gene_from_snp <- function(
                              gff,
                              sig.snp,
                              distance = NA,
                              ...) {
  distance <- 1000 * distance
  colnames(gff) <- c("chr", "start", "end", "gene")
  r1 <- vector(mode = "numeric")
  r2 <- vector(mode = "numeric")
  for (i in 1:nrow(sig.snp)) {
    if (sig.snp[i, ][["BP"]] >= distance) {
      r <- c(sig.snp[i, ][["BP"]] - distance, sig.snp[i, ][["BP"]] + distance)
    } else {
      r <- c(sig.snp[i, ][["BP"]], sig.snp[i, ][["BP"]] + distance)
    }
    r1 <- append(r1, r[1])
    r2 <- append(r2, r[2])
  }
  sig.snp$r1 <- r1
  sig.snp$r2 <- r2
  ## search genes
  findgene <- function(i) {
    snp_gene <- gff[gff$start > sig.snp$r1[i] & gff$end < sig.snp$r2[i] & gff$chr == sig.snp$CHR[i], ]
    if (nrow(snp_gene) == 0) {
      return(NULL)
    } else {
      snp_gene$snp_location <- sig.snp$BP[i]
      snp_gene$p_value <- sig.snp$P[i]
      return(snp_gene)
    }
  }

  plan(multiprocess)
  test <- future.apply::future_lapply(seq(1, nrow(sig.snp)), findgene)
  gene_snp <- do.call(rbind, test)
  return(gene_snp)
}

