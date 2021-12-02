library(dplyr)
library(tidyr)
library(readxl)

# for i in *.tif; do convert $i ${i%.tif}.png; done

# Also https://talks.cpsievert.me/20180202/#11

summary_table <- read_excel("summary-table_linux_AN_tg.xlsx")

regions <- c("TM1","TM2","TM3","TM4","TM5","TM6","TM7","ECL1","ECL3")

st <- summary_table %>% 
    mutate(`Length of segment before-Cys45.50` = as.integer(`Lenght of segment before-Cys45.50`),
           `Length of segment after-Cys45.50` = as.integer(`Lenght of segment after-Cys45.50`))

st$Structure[st$Structure=="B"] <- "B_AT2"

for (r in regions) {
    c.orig <- sprintf("Contacts with %s",r)
    c.out1 <- sprintf("%s contacts min",r)
    c.out2 <- sprintf("%s contacts max",r)
    st <- st %>% separate(!!sym(c.orig), 
                          into=c(c.out1,c.out2),
                          convert=TRUE,
                          remove=FALSE,
                          fill="right")
    same.be <- is.na(st[[c.out2]])
    st[[c.out2]][same.be] <- st[[c.out1]][same.be]
}

st <- st %>% select(-c(`Lenght of segment before-Cys45.50`, 
                       `Lenght of segment after-Cys45.50`,
                       `Structures within this structure's cluster`)) %>%
    mutate(Structure=str_replace(Structure,".tif","")) %>%
    separate("Structure",c(NA,"ID"),remove=FALSE,sep=2) %>%
    rename(Cluster=`Cluster (based on Volume overlaps)`) %>%
    relocate(Cluster, GPCR, Structure, ECL2) %>%
    arrange(Cluster)



save(st, file="ecl2/data/summary-table.RData")
