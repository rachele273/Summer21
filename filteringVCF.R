#filtering VCF names to filter w vcftools. there is probably an easier way
library(tidyverse)
all_id <- read.delim(file.choose(), header=FALSE)
parviflora <- all_id %>% 
  select(V1) %>%
  filter(str_detect(V1, "p"))
xantiana <- all_id %>% 
  select(V1) %>% 
  filter(str_detect(V1, "x"))
write.table(x=parviflora, file="parviflora.txt")
write.table(x=xantiana, file="xantiana.txt")
