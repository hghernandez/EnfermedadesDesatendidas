##%######################################################%##
#                                                          #
####             Envío el reporte por Email             ####
#                                                          #
##%######################################################%##

library(rmarkdown)
library(blastula)

ruta <- getwd()

email <- blastula::render_email(paste0(ruta,"/Evaluacion/ReporteEval.Rmd"))

create_smtp_creds_file(
  file = "gmail_creds",
  user = "hernan.hernandez@uner.edu.ar",
  host = "smtp.gmail.com",
  port = 465,
  use_ssl = TRUE
)


email %>%
  smtp_send(
    from = "hernan.hernandez@uner.edu.ar",
    to = c("hernan.hernandez@uner.edu.ar"),
    subject = "Evaluación Webinar Enfermedades Desatendidas",
    
    credentials =
      creds_file(file = "gmail_creds")
    
  )


