#SHORT DEF:   Function to send SMS report.
#RETURNS:     Nothing. SMS report are sent.
#             TODO: build in checks to log if SMS report was successfully sent.
#DESCRIPTION: Function using Plivo service to send SMS texts to phonenumber specified.
#             Note: Plivo credentials are hardcoded! Do not share!!!
#             TODO: use scan function to read credentials from csv input file.
#INPUT:       text: Vector of body text to be sent by SMS. Elements should not exceed 1600 character limit!
#             src: source phone number, starting with country code, default 254727876796
#             dst: destination phone number, starting with country code, e.g., 234789123456


sendSMSReport <- function(SMStext, src = "254727876796", dst, userField) {
  if (is.list(res)) {
    #plivio account details
	pw <- readRDS("passwords.rds")
    AUTH_ID = plivio$AUTH_ID
    AUTH_TOKEN = plivio$AUTH_TOKEN
    url = paste0("https://api.plivo.com/v1/Account/", AUTH_ID, "/Message/")

    for (i in SMStext) {
      if (nchar(i) <= 1600) {
        POST(url,
             authenticate(AUTH_ID, AUTH_TOKEN),
             body = list(src = src, dst = dst, text = i))
      }else {
        print("Text message exceeds 1600 character limit. Message not sent")
      }
    }
  }
}



#' function to send mail
sendEmailReport <- function(userEmail, FR, IC, PP, SP, FRrecom, ICrecom, country, PPrecom, SPrecom, userPhoneNr) {

  print(paste("Running email generation FR=", FR, "IC=", IC, "PP=", PP, "SP=", SP, "FRrecom=", FRrecom, "ICrecom=", ICrecom))
  if (FR == TRUE & 
    IC == FALSE &
    FRrecom == TRUE &
    country == "NG") {
    print("Generating PDF file")
    if (file.exists(paste("fertilizer_advice_", userPhoneNr, ".pdf", sep = "")))
      webshot::rmdshot('FR_markdown_VFT.Rmd', file = paste0('fertilizer_advice_', userPhoneNr, ".pdf"), delay = 3)
  }

  if (FR == TRUE &
    IC == FALSE &
    FRrecom == TRUE &
    country == "GH") {
    if (file.exists(paste("fertilizer_advice_", userPhoneNr, ".pdf", sep = "")))
      webshot::rmdshot('FR_markdown_VFT.Rmd', file = paste0('fertilizer_advice_', userPhoneNr, ".pdf"), delay = 3)
  }

  if (FR == TRUE &
    IC == FALSE &
    FRrecom == TRUE &
    country == "TZ") {
    #if (file.exists("fertilizer_advice_swa.pdf"))
    webshot::rmdshot('FR_markdown_swa.Rmd', file = paste0('fertilizer_advice_swa_', userPhoneNr, ".pdf"), delay = 3)
  }

  if (IC == TRUE &
    FR == FALSE &
    country == "NG" &
    ICrecom == TRUE) {
    if (file.exists("intercrop_advice_VFT.pdf"))
      webshot::rmdshot('IC_markdown_VFT.Rmd', file = paste0('intercrop_advice_', userPhoneNr, ".pdf"), delay = 3)
  }

  if (IC == TRUE &
    FR == FALSE &
    country == "TZ" &
    ICrecom == TRUE) {
    if (file.exists("CIS_VFT.pdf"))
      webshot::rmdshot('CIS_markdown_swa.Rmd', file = paste0('CIS_advice_', userPhoneNr, ".pdf"), delay = 3)
  }

  if (PP == TRUE & PPrecom == TRUE & country == "NG") {
    if (file.exists("PP_advice_VFT.pdf"))
      webshot::rmdshot('PP_markdownVFT.Rmd', file = paste0('PP_advice_', userPhoneNr, ".pdf"), delay = 3)
  }

  if (PP == TRUE & PPrecom == TRUE & country == "TZ") {
    if (file.exists("PP_advice_swa.pdf"))
      webshot::rmdshot('PP_markdown_swa.Rmd', file = paste0('PP_advice_swa_', userPhoneNr, ".pdf"), delay = 3)
  }

  if (SP == TRUE & SPrecom == TRUE & country == "NG") {
    if (file.exists("SP_advice_VFT.pdf"))
      webshot::rmdshot('SP_markdownVFT.Rmd', file = paste0('SP_advice_', userPhoneNr, ".pdf"), delay = 3)
    if (file.exists("spgg.png")) file.remove("spgg.png")
  }
  if (SP == TRUE & SPrecom == TRUE & country == "GH") {
    if (file.exists("SP_advice_VFT.pdf"))
      webshot::rmdshot('SP_markdownVFT.Rmd', file = paste0('SP_advice_', userPhoneNr, ".pdf"), delay = 3)
    if (file.exists("spgg.png")) file.remove("spgg.png")
  }

  if (SP == TRUE & SPrecom == TRUE & country == "TZ") {
    if (file.exists("SP_advice_swa.pdf"))
      webshot::rmdshot('SP_markdown_swa.Rmd', file = paste0('SP_advice_swa_', userPhoneNr, ".pdf"), delay = 3)
    if (file.exists("spgg.png")) file.remove("spgg.png")
  }

  listofPDFs <- NULL
  if (FR == TRUE & FRrecom == TRUE & country == "NG") { listofPDFs <- c(listofPDFs, paste("fertilizer_advice_", userPhoneNr, ".pdf", sep = "")) }
  if (FR == TRUE & FRrecom == TRUE & country == "GH") { listofPDFs <- c(listofPDFs, paste("fertilizer_advice_", userPhoneNr, ".pdf", sep = "")) }


  if (FR == TRUE & FRrecom == TRUE & country == "TZ") { listofPDFs <- c(listofPDFs, paste("fertilizer_advice_swa_", userPhoneNr, ".pdf", sep = "")) }

  if (PP == TRUE & PPrecom == TRUE & country == "NG") { listofPDFs <- c(listofPDFs, paste("PP_advice_", userPhoneNr, ".pdf", sep = "")) }

  if (PP == TRUE & PPrecom == TRUE & country == "TZ") { listofPDFs <- c(listofPDFs, paste("PP_advice_swa_", userPhoneNr, ".pdf", sep = "")) }

  if (SP == TRUE & SPrecom == TRUE & country == "NG") { listofPDFs <- c(listofPDFs, paste("SP_advice_", userPhoneNr, ".pdf", sep = "")) }
  if (SP == TRUE & SPrecom == TRUE & country == "GH") { listofPDFs <- c(listofPDFs, paste("SP_advice_", userPhoneNr, ".pdf", sep = "")) }

  if (SP == TRUE & SPrecom == TRUE & country == "TZ") { listofPDFs <- c(listofPDFs, paste("SP_advice_swa_", userPhoneNr, ".pdf", sep = "")) }

  if (IC == TRUE & country == "NG" & ICrecom == TRUE) { listofPDFs <- c(listofPDFs, paste("intercrop_advice_", userPhoneNr, ".pdf", sep = "")) }

  if (IC == TRUE & country == "TZ" & ICrecom == TRUE) { listofPDFs <- c(listofPDFs, paste("CIS__advice", userPhoneNr, ".pdf", sep = "")) }


  if (!is.null(listofPDFs)) {
	pwd <- readRDS("passwords.rds")
    send.mail(from = pwd$email$user.name,
              to = as.character(userEmail),
              subject = "AKILIMO recommendation",
              body = "Please find attached the recommendation. \n Best Regards, \n AKILIMO",
              authenticate = TRUE,
              attach.files = dput(as.character(listofPDFs)),
              smtp = list(host.name = pwd$email$host.name, port = 587,
                          user.name = pwd$email$user.name, passwd = pwd$email$password, tls = TRUE))
    if (file.exists(listofPDFs)) file.remove(listofPDFs)
  }

}

