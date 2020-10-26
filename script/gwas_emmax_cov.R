# GWAS with EMMAX model

emmax <- "./biosoft/emmax-intel64"
genotype <- "./data/Bna.core.maf0.05.int0.9"
k <- "./data/Bna.core.maf0.05.int0.9.BN.kinf"
cov <- "./data/Bna.core.emmax.cov.txt"
tfam_path <- "./data/Bna.core.maf0.05.int0.9.tfam"
Bna_darmor_geneid <- "./data/Bna_darmor_gene_id_info.txt"
load("./data/Bna_gene_anno.RData")
global_theme <- theme(
  axis.text.y = element_text(size = 13, face = "bold", colour = "black"),
  axis.text.x = element_text(size = 13, face = "bold", colour = "black"),
  axis.title.x = element_text(size = 13, face = "bold"),
  axis.title.y = element_text(size = 13, face = "bold"),
  panel.background = element_rect(fill = "white"),
  axis.ticks.length = unit(.25, "cm"),
  plot.title = element_text(lineheight = .8, face = "bold", hjust = 0.5),
  plot.caption = element_text(size = 10, face = "bold", ),
  plot.subtitle = element_text(hjust = 0.5, size = 10),
  strip.background = element_rect(fill = "white"),
  strip.text = element_text(size = 18, hjust = 0, colour = "black", face = "bold"),
  legend.position = "none",
  legend.title = element_text(size = 13, face = "bold"),
  legend.text = element_text(size = 13),
  panel.border = element_rect(color = "black", fill = NA, size = 1),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_blank()
)


tfam <- read.table(tfam_path, header = FALSE, stringsAsFactors = FALSE)

output <- function(trait) {
  result_path <- paste0("./tmp/", Sys.Date(), ".", trait, ".GWAS.EMMAX.cov")
  return(result_path)
}

gwas_emmax <- function(phenotype, out) {
  cmd <- sprintf("%s -v -d 10 -t %s -o %s -p %s -k %s -c %s", emmax, genotype, out, phenotype, k, cov)
  gwas_res <- system(cmd, intern = TRUE)
  return(gwas_res)
}
