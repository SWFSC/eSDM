### Sending user feedback to esdm.feedback@gmail.com

###############################################################################
### Generate warning message if no internet connection is detected
feedback_internet_connection <- reactive({
  # Invalidate and thus rerun this reactive every 3 seconds
  invalidateLater(millis = 3000, session)
  
  ifelse(isTruthy(try(getURL("www.google.com"), silent = TRUE)), 
         "",
         paste("Warning: no internet connection is detected.", 
               "You must be connected to the internet to submit feedback."))
})


###############################################################################
### Send feedback and give user confirmation message
feedback_submit <- eventReactive(input$feedback_submit_event, {
  #####################################
  ### Ensure that all field have an entry
  list.in.feedback <- list(input$feedback_name, input$feedback_email, 
                           input$feedback_tab, input$feedback_comment)
  validate(
    need(all(sapply(list.in.feedback, isTruthy)), 
         "Please ensure that all fields have an entry")
  )
  
  
  #####################################
  # Process user inputs to prepare for email
  
  ### User name and email
  f.name    <- paste("Name:", input$feedback_name)
  f.email   <- paste("Email:", input$feedback_email)
  
  ### Selected tab(s)
  f.tab <- paste(ifelse(length(input$feedback_tab) > 1, "Tabs:", "Tab:"), 
                 paste(input$feedback_tab, collapse = "; "))
  
  ### Actual comment
  f.comment <- unlist(strsplit(paste("Comments:",  input$feedback_comment), 
                               "\n"))
  
  ### Email details
  from    <- sprintf("<sendmailR@\\%s>", "eSDM")
  to      <- "<esdm.feedback@gmail.com>"
  subject <- paste0("eSDM feedback from ", input$feedback_name, "; ", 
                    Sys.Date())
  body    <- list(f.name, f.email, f.tab, "", f.comment)

  #####################################
  ### Send email
  mail.out <- try(suppressWarnings(
    sendmailR::sendmail(from, to, subject, body, 
                        control = list(smtpServer="ASPMX.L.GOOGLE.COM"))), 
    silent = TRUE)
  
  validate(
    need(isTruthy(mail.out), 
         paste("Feedback could not be submitted; please check your", 
               "internet connection. If this problem persists,", 
               "you can email your feedback to", 
               "Sam Woodman (sam.woodman@noaa.gov) and", 
               "Karin Forney (karin.forney@noaa.gov)."))
  )
  
  "Feedback submitted successfully. Thank you!"
})

###############################################################################
