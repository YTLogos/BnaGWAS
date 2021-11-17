# GWAS with EMMAX model

emmax <- "./biosoft/emmax-intel64"
genotype.darmor <- "./data/Bna.Darmor.Core.maf0.05.int0.9"
k.darmor <- "./data/Bna.Darmor.Core.maf0.05.int0.9.BN.kinf"
cov.darmor <- "./data/Bna.darmor.core.emmax.cov.txt"
tfam_path.darmor <- "./data/Bna.Darmor.Core.maf0.05.int0.9.tfam"

genotype.zs11 <- "./data/Bna.ZS11.Core.maf0.05.int0.9"
k.zs11 <- "./data/Bna.ZS11.Core.maf0.05.int0.9.BN.kinf"
cov.zs11 <- "./data/Bna.zs11.core.emmax.cov.txt"
tfam_path.zs11 <- "./data/Bna.ZS11.Core.maf0.05.int0.9.tfam"
#Bna_darmor_geneid <- "./data/Bna_darmor_gene_id_info.txt"
#load("./data/Bna_gene_anno.RData")
global_theme <- theme(
  axis.text.y = element_text(size = 13, face = "bold", colour = "black", family = "Times New Roman"),
  axis.text.x = element_text(size = 13, face = "bold", colour = "black", family = "Times New Roman"),
  axis.title.x = element_text(size = 13, face = "bold", family = "Times New Roman"),
  axis.title.y = element_text(size = 13, face = "bold", family = "Times New Roman"),
  panel.background = element_rect(fill = "white"),
  axis.ticks.length = unit(.25, "cm"),
  plot.title = element_text(lineheight = .8, face = "bold", hjust = 0.5, family = "Times New Roman"),
  plot.caption = element_text(size = 15, face = "bold", family = "Times New Roman", colour = "red"),
  plot.subtitle = element_text(hjust = 0.5, size = 10, family = "Times New Roman"),
  strip.background = element_rect(fill = "white"),
  strip.text = element_text(size = 18, hjust = 0, colour = "black", face = "bold", family = "Times New Roman"),
  legend.position = "none",
  legend.title = element_text(size = 13, face = "bold", family = "Times New Roman"),
  legend.text = element_text(size = 13, family = "Times New Roman"),
  panel.border = element_rect(color = "black", fill = NA, size = 1),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_blank()
)


#tfam <- read.table(tfam_path, header = FALSE, stringsAsFactors = FALSE)

tfam <- function(ref){
  if(ref=="Darmor-bzh"){
    tfam_path <- tfam_path.darmor 
  }else{
    tfam_path <- tfam_path.zs11
  }
  tfam_data <- read.table(tfam_path, header = FALSE, stringsAsFactors = FALSE)
  return(tfam_data)
}

output <- function(trait,ref) {
  result_path <- paste0("./tmp/", Sys.Date(), ".", trait, ".", ref, ".GWAS.EMMAX.cov")
  return(result_path)
}

gwas_emmax <- function(phenotype, out, ref) {
  if(ref=="Darmor-bzh"){
    genotype=genotype.darmor
    k=k.darmor
    cov=cov.darmor
  }
  else{
    genotype=genotype.zs11
    k=k.zs11
    cov=cov.zs11
  }
  cmd <- sprintf("%s -v -d 10 -t %s -o %s -p %s -k %s -c %s", emmax, genotype, out, phenotype, k, cov)
  gwas_res <- system(cmd, intern = TRUE)
  return(gwas_res)
}
