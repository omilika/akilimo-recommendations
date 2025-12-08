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

	listofPDFs <- NULL
	add_pdf <- function(f) {listofPDFs <<- c(listofPDFs, f); f}

	if (FR & (!IC) & FRrecom) {
	# is that the correct file exists? Or is that the file that should be generated?
		if (country %in% c("NG", "GH") & file.exists(paste0("fertilizer_advice_", userPhoneNr, ".pdf"))) {
			fname <- add_pdf(paste0('fertilizer_advice_', userPhoneNr, ".pdf"))
			webshot::rmdshot('FR_markdown_VFT.Rmd', file=fname, delay = 3)
		} else if (country == "TZ") {
			#if (file.exists("fertilizer_advice_swa.pdf"))
			fname <- add_pdf(paste0('fertilizer_advice_swa_', userPhoneNr, ".pdf"))
			webshot::rmdshot('FR_markdown_swa.Rmd', file = fname, delay = 3)
		}
	}

	if (FR & IC & ICrecom) {
		if (country == "NG" & file.exists("intercrop_advice_VFT.pdf")) {
			fname <- add_pdf(paste0('intercrop_advice_', userPhoneNr, ".pdf"))
			webshot::rmdshot('IC_markdown_VFT.Rmd', file = fname, delay = 3)
		} else if (country == "TZ" & file.exists("CIS_VFT.pdf")) {
			fname <- add_pdf(paste0('CIS_advice_', userPhoneNr, ".pdf"))
			webshot::rmdshot('CIS_markdown_swa.Rmd', file = fname, delay = 3)
		}
	}
	
	if (PP & PPrecom) {
		if (country == "NG" & file.exists("PP_advice_VFT.pdf")) {
			fname <- add_pdf(paste0('PP_advice_', userPhoneNr, ".pdf"))
			webshot::rmdshot('PP_markdownVFT.Rmd', file = fname, delay = 3)
		} else if (country == "TZ" & file.exists("PP_advice_swa.pdf")) {
			fname <- add_pdf(paste0('PP_advice_swa_', userPhoneNr, ".pdf"))
			webshot::rmdshot('PP_markdown_swa.Rmd', file = fname, delay = 3)
		} 
	}

	if (SP & SPrecom) {
		if (country %in% c("NG", "GH") & file.exists("SP_advice_VFT.pdf")) {
			fname <- add_pdf(paste0('SP_advice_', userPhoneNr, ".pdf"))
			webshot::rmdshot('SP_markdownVFT.Rmd', file = fname, delay = 3)
			if (file.exists("spgg.png")) file.remove("spgg.png")
		} else if (country == "TZ" & file.exists("SP_advice_swa.pdf")) {
			fname <- add_pdf(paste0('SP_advice_swa_', userPhoneNr, ".pdf"))
			webshot::rmdshot('SP_markdown_swa.Rmd', file = fname, delay = 3)
			if (file.exists("spgg.png")) file.remove("spgg.png")
		}
	}

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
		try(file.remove(listofPDFs))
	}

}

