# 13. Saeimas vēlēšanas

# Reset
rm(list = ls())
gc()


# Packages
require(data.table)
require(stringi)


# Kandidātu saraksts

dat_kand <- fread("data/dat_kand.csv", quote = "")
tmp <- dat_kand[, .N, keyby = .(`Kandidātu saraksts`)]

dat_kand[, c("Nr", "Saraksts") := data.table(stri_split_fixed(`Kandidātu saraksts`,
                                                              pattern = " ", n = 2,
                                                              simplify = T))]
dat_kand[, Nr := as.integer(Nr)]

dat_kand_2 <- dat_kand[, .(Apgabals = `Vēlēšanu apgabals`, Nr, Saraksts)]

setorder(dat_kand_2, Apgabals, Nr)

dat_kand_2[, Dalitajs := 2 * (1:.N) - 1, by = .(Apgabals, Nr)]

dat_kand_2[, .N, keyby = .(Nr, Saraksts, Apgabals)][grep("kons|KPV", Saraksts)]



# Partiju rezultāti
dat_rez_part <- fread("data/dat_rez_part.csv", quote = "")

setnames(dat_rez_part, c("Nr", "Saraksts", "Zīmes", "Procenti", "Vietas"))

dat_rez_part[, Procenti := sub(",", ".", Procenti)]
dat_rez_part[, Procenti := sub("%", "", Procenti)]
dat_rez_part[, Procenti := as.numeric(Procenti)]
dat_rez_part
dat_rez_part[, sum(Procenti)]

dat_rez_part[Procenti > 5, Nr]

partijas_in <- dat_rez_part[Procenti > 5, Nr]
partijas_in



dat_kand_2 <- dat_kand_2[Nr %in% partijas_in]
dat_kand_2


# Partiju rezultāti pa apgabaliem
dat_rez_part_apg <- fread("data/dat_rez_part_apg.csv")
setnames(dat_rez_part_apg, "Pašvaldība", "Apgabals")

dat_rez_part_apg[, Kopā := NULL]
dat_rez_part_apg <- dat_rez_part_apg[Apgabals != "Kopā"]

dat_rez_part_apg <- melt(dat_rez_part_apg, id.vars = "Apgabals",
                         variable.name = "Nr", value.name = "Balsis",
                         variable.factor = F)

dat_rez_part_apg[, Nr := as.integer(Nr)]

dat_rez_part_apg <- dat_rez_part_apg[Nr %in% partijas_in]

dat_rez_part[, .(Nr, Balsis = Zīmes)]


# Deputātu skaits

dat_kand_2[, .N, keyby = .(Apgabals)]

dat_dep <- data.table(Apgabals = c("Rīga", "Vidzeme", "Latgale", "Zemgale", "Kurzeme"),
                      Deputati = c(29, 27, 16, 15, 13))
dat_dep
dat_dep[, sum(Deputati)]


# Apvieno balsis ar kandidātiem

sapply(dat_rez_part_apg, class)

dat <- merge(dat_rez_part_apg, dat_kand_2, by = c("Apgabals", "Nr"))
dat <- merge(dat, dat_dep, by = c("Apgabals"))

dat[, Dalijums := Balsis / Dalitajs]

setorder(dat, Apgabals, -Dalijums)

anyDuplicated(dat, by = c("Apgabals", "Dalijums"))

dat[, i := 1:.N, by = .(Apgabals)]

dat[i <= Deputati]

sapply(dat_rez_part, class)
sapply(dat, class)

tab_dep <- merge(dat[i <= Deputati, .(Deputati = .N), by = .(Nr, Saraksts)],
                 dat_rez_part[, .(Nr, Balsis = Zīmes)],
                 by = "Nr")

setorder(tab_dep, -Deputati, -Balsis)
setcolorder(tab_dep, c("Nr", "Saraksts", "Balsis"))

tab_dep