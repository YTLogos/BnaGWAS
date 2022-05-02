# <font face="Time" color=green size=6>Introduction</font>

<font face="Time" size=4>Rapeseed (***Brassica napus*** L.) is an important source of edible oil and protein-rich livestock feed in the world. ***B. napus*** (AACC) was ancestrally originated from an interspecific hybridization between two diploid progenitors, ***B. rapa*** (AA) (n = 10) and ***B. oleracea*** (CC) (n = 9), less than 7500 years ago. In our previous study, we resequenced a world-wide collection of `991` ***B. napus*** gerplasm accessions, including 658 winter types, 145 semi-winter types and 188 spring types, from 39 countries (<a href="https://www.sciencedirect.com/science/article/pii/S1674205218303435?via%3Dihub" target="_blank">**Wu et al., 2019**</a>).

<p>&nbsp;</p>
<p align="center">
<img src="img/Bna_map.png" width="800" hegiht="1000">
</p>
<p style="text-align:center">The geographic distribution of rapeseed accessions </p>


In genetics, a genome-wide association study (**GWAS**), also known as whole genome association study (WGAS), is an observational study of a genome-wide set of genetic variants in different individuals to see if any variant is associated with a trait. **GWAS** typically focus on associations between single-nucleotide polymorphisms (**SNPs**) and traits like major agronomic traits.

In order to make better use of this huge ***B. napus*** gerplasm accessions, we develop this interactive application ([**BnaGWAS**](https://bnapus-zju.com/gwas)) in <a href="https://www.r-project.org/" target="_blank">**R**</a> with <a href="https://shiny.rstudio.com/" target="_blank">**`Shiny`**</a>. This application can conduct **GWAS**, visualization of **GWAS** results (Manhattan plot and QQ plot), extraction of significant genes and annotation of genes</font>.

# <font face="Time" color=green size=6>Data input</font>

## <font color=green size=5 face="Time">phenotype data (.txt)</font>

> **<font color=red size=4 face="Time"> Noted: Your Samples Uploaded MUST Be The 300 Core Collection Samples Used Here ! So If Some Samples Are Not In Your LIST, You Need Add Them In Your List, And Set The Value NA. If Some Samples In Your List Are Not In The 300 Core Collection Samples Here, JUST REMOVE THEM!</font>**

> <font color=green size=4 face="Time">I highly recommended first download the example of the expected input phenotype dataset below, and then replace the phenotype values with you own data</font>.

<font size=4 face="Time">You just need upload your phenotype data to run **GWAS**. Here we just use the **`300`** core collection gerplasm which represent the most of genetic resources of 1000 ***B. napus*** gerplasm accessions. an example of the expected input data format is present as below:

<style>
table th:first-of-type {
        width: 100px;
}
</style>


|       |       |
| ------------- | -------------- |
|R4157|0.859791123|
|R4158|0.87369142|
|R4163|0.842593709|
|R4168|0.884782609|
|R4171|NA|
|R4176|0.885619807|
|R4177|0.885884455|
|R4179|0.879374612|
|R4180|0.878567797|
|R4182|0.868825911|
|...|...|

Where, column one correspond to samples, column two correspond to phenotype values.

An example of the expected input phenotype dataset can be accessible <a href="https://bnapus-zju.com/sample_phenotype.txt" target="_blank"> **here**</a>.</font>

## <font color=green szie=5 face="Time">Other parameters</font>

<font size=4 face="Time">Next you need enter your trait name (`recommended`) (default: Bna_trait). Now just support the <a href="https://genome.sph.umich.edu/wiki/EMMAX" target="_blank">**EMMAX**</a> model. After all the prepared works are ready, then clink **Run Analysis** to start GWAS.</font>

# <font color=green size=6 face="Time">Visualization</font>

<font size=4 face="Time">For the `Visualization` section in this App, it is aiming to visualize the Manhattan plot and QQ plot. You can choose the alternate colors for alternate chromosomes and p-value threshold (default: p-value=5).</font>

# <font color=green size=6 face="Time">Extraction</font>

<font size=4 face="Time">The extraction of significant genes is based on the significant p-value of SNPs. So here you need choose the p-value threshold and the distance up/down-stream of SNPs (`recommended`) (default: 75kb).</font>

# <font color=green size=6 face="Time">Annotation</font>

<font size=4 face="Time">This section is designed for gene annotation based on different databases (`eggNOG`, `GO`, `KEGG`, `NR`, etc.).</font>
