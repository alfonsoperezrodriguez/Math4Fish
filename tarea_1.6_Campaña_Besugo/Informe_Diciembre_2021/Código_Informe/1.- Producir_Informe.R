

install.packages("tinytex")
tinytex::install_tinytex()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Producir el informe
rmarkdown::render(
  'Besugo_Informe_1.6_V4_AC_AP.Rmd', output_file = 'Informe_Campaña_Besugo_Diciembre_2021.pdf'
  )

# Producir las tablas con CPUE por especie, tallas, lance y barco
source("save_cpue_output.R")

