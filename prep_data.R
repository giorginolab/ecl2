library(dplyr)
library(tidyr)
library(readxl)

# for i in *.tif; do convert $i ${i%.tif}.png; done


summary_table <- read_excel("data/summary-table_linux_AN_tg.xlsx")

regions <- c("TM1","TM2","TM3","TM4","TM5","TM6","TM7","ECL1","ECL3")

st <- summary_table %>% 
    mutate(`Length of segment before-Cys45.50` = as.integer(`Lenght of segment before-Cys45.50`),
           `Length of segment after-Cys45.50` = as.integer(`Lenght of segment after-Cys45.50`))


for (r in regions) {
    c.orig <- sprintf("Contacts with %s",r)
    c.out1 <- sprintf("%s contacts begin",r)
    c.out2 <- sprintf("%s contacts end",r)
    st <- st %>% separate(!!sym(c.orig), 
                          into=c(c.out1,c.out2),
                          convert=TRUE,
                          fill="right")
    same.be <- is.na(st[[c.out2]])
    st[[c.out2]][same.be] <- st[[c.out1]][same.be]
}

save(st, file="ecl2app/data/summary-table.RData")
