# make appendix
#system("sudo apt-get install pdftk")

setwd("/home/a/projects/seed-bank")
library(rmarkdown)

rmarkdown::render("Appendix_figures.Rmd")
rmarkdown::render("Appendix_tables.Rmd")

# getting today's data to add to the filename
x=date()
x=strsplit(x," ")[[1]]

system("pdftk Appendix_figures.pdf Appendix_tables.pdf cat output Appendix_S1.pdf")

# make full paper

system("pdftk sb_manuscript.pdf Appendix_S1.pdf cat output full_manuscript.pdf")
