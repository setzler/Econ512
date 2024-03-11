#####################
# 1. Prepare the data
#####################

# prepare the workspace
rm(list = ls())
library(data.table)
library(ggplot2)

# download the data
url = "https://raw.githubusercontent.com/setzler/EconData/master/DataRepo/CensusCBP/CBP_national_supersector.csv"
destfile = "~/Downloads/CBP_national_ supersector.csv"
download.file(url = url, destfile = destfile)

# read the data
cbp = setDT(read.csv(destfile))
cbp[, share := payroll_quarter1 / sum(payroll_quarter1), year]

###################################
# 2. From Basic plot to Nice plot
###################################

gg = ggplot(data = cbp, aes(x = year, y = share, color = supersector)) + geom_line()
ggsave(filename = "supersectors_basic.png", gg)

gg = ggplot(data = cbp, aes(x = year, y = share, color = supersector)) + 
    geom_line() +
    theme_gray(base_size = 16)
ggsave(filename = "supersectors_biggerfont.png", gg)

cbp[, share := 100*share]
gg = ggplot(data = cbp, aes(x = year, y = share, color = supersector)) + 
    geom_line() +
    theme_gray(base_size = 16) +
    labs(x = "Year", y = "Share of Payroll, Q1 (%)", color = "NAICS Supersector") 
ggsave(filename = "supersectors_clearerfont.png", gg)

library(stringr)
cbp[, supersector := str_replace(supersector, " and ", " & ")]
gg = ggplot(data = cbp, aes(x = year, y = share, color = supersector)) + 
    geom_line() +
    theme_gray(base_size = 16) +
    labs(x = "Year", y = "Share of Payroll, Q1 (%)", color = "NAICS\nSupersector") +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(ncol = 2))
ggsave(filename = "supersectors_betterlegend.png", gg, width = 8, height = 5)


residual_sectors = cbp[share < 5, unique(supersector)]
cbp[supersector %in% residual_sectors, supersector := "Residual"]
cbp = cbp[, list(share = sum(share)), list(year, supersector)]
gg = ggplot(data = cbp, aes(x = year, y = share, color = supersector, shape = supersector)) + 
    geom_line() +
    geom_point() +
    theme_bw(base_size = 16) +
    labs(x = "Year", y = "Share of Payroll, Q1 (%)", color = "NAICS\nSupersector", shape = "NAICS\nSupersector") +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(ncol = 2))
ggsave(filename = "supersectors_bettercolors.png", gg, width = 7.5, height = 5)


###################################
# 3. Tables
###################################

library(textab)

E <- function(){
    return("$\\mathbb{E}$")
}

tt = TexRow(c("", "Sunny Days", "Rainy Days"), space = 3) 
tt = tt + TexMidrule(list(c(2, 3)))
tt = tt + TexRow(sprintf("Hiking ($\\alpha$) %s", E())) / TexRow(c(20, 0.5), dec = 1, percentage = TRUE, pvalues = c(0, 1))
tt = tt + TexRow("{\\color{green} Reading} ($\\frac{\\gamma}{2}$)") / TexRow(c(5, 90), dec = 1, percentage = TRUE, pvalues = c(1, 0))

TexSave(tab = tt, filename = "weather", positions = c("l", "c", "c"), stand_alone = TRUE, compile_tex = TRUE, pretty_rules = TRUE)



