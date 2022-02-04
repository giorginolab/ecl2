library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(stringr)

# for i in *.tif; do convert $i ${i%.tif}.png; done

# Also https://talks.cpsievert.me/20180202/#11

summary_table <- read_excel("original-data/summary-table_linux_AN_CORR.xlsx")

regions <- c("TM1","TM2","ECL1","TM3","TM4","TM5","TM6","ECL3","TM7")

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


clusterOfID<-setNames(st$Cluster, st$ID)


dfXl <- list()
dfFl <- list()

for (reg in regions) {
    tmp <- read_csv(sprintf("original-data/MD_ECL2_contacts/%s.csv", reg), 
                    skip = 1, show_col_types = FALSE)
    tmpX <- head(tmp,1)
    dfX <- gather(tmpX, ID, Contacts)
    dfX$Region <- reg
    dfX$Frame <- -1
    dfXl[[reg]] <- dfX
    
    tmpF <- tmp[-1,]
    tmpF$Frame <- seq_along(tmpF[[1]])
    dfF <- gather(tmpF,ID, Contacts, -Frame)
    dfF$Region <- reg
    dfFl[[reg]] <- dfF
}

contactsF <- do.call("rbind",dfFl)
contactsX <- do.call("rbind",dfXl)

contacts <- rbind(na.omit(contactsF),contactsX)
contacts$Cluster <- clusterOfID[contacts$ID]


save(st, contacts, file="ecl2/data/summary-table.RData")



