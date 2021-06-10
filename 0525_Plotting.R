#May 25, 2021
#Plotting VCF data

#install package to read VCF files
install.packages("vcfR")
library(vcfR)
#reading VCF file (using truncated file for my ease of understanding)
clarkiamodified <- read.vcfR("Data/clarkiamodified.vcf", verbose = FALSE)

#(note-- recommendation from vcfR documentation: work on individual chromosomes, not whole datasets, to avoid memory issues)

#make chrom file. chrom file represents CHROMosomes (vcf data is input)
chrom <- create.chromR(name='SuperScaffold10095', vcf=clarkiamodified)
chrom <- proc.chromR(chrom, verbose=TRUE)
plot(chrom)
#plotting
plot(chrom)
#only the variant count was plotted?
head(chrom)
#looks like an issue with INFO column? not showing the whole af/dp/etc?
chromoqc(chrom)
save(vcf, file="practicevcf")
data(vcfR_example)
save(vcf_file, file="practicevcf")
clarkia <- read.vcfR("Data/clarkia.vcf", verbose=FALSE)
clarkia2 <- read.vcfR("Data/clarkiamodified copy 2.vcf", verbose=FALSE)
chrom <- create.chromR(name='practice', vcf=clarkia2)
plot(chrom)
clarkia3 <- read.vcfR("Data/clarkiamodified copy.vcf")
chrom3 <- create.chromR(name='testchrom', vcf=clarkia3)
plot(chrom)
head(chrom)
chromoqc(chrom3)
