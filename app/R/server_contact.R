server_contact <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      
      observeEvent(input$send, {
        
        iv <- shinyvalidate::InputValidator$new()
        iv$add_rule("name", shinyvalidate::sv_required())
        iv$add_rule("email", shinyvalidate::sv_email())
        iv$add_rule("org", shinyvalidate::sv_required())
        iv$add_rule("message", shinyvalidate::sv_required())
        iv$enable()
        
        if(iv$is_valid()){ # verify if inputs are filled
          
          # email content
          email <- emayili:: envelope(
            from = input$email,
            to = "basinscoutsupport@thefreshwatertrust.org",
            subject = "PROJECT NAME - Contact"
          ) %>%
            html(
              paste(
                "<b>Name:</b>", input$name, "<br>",
                "<b>Organization:</b>", input$org, "<br>",
                "<b>Message:</b>", input$message
              )
            )
          
          # configure server
          smtp <- emayili::server(host = "thefreshwatertrust-org.mail.eo.outlook.com",
                                  port = 25,
                                  username = "", 
                                  password = "")
          
          # send email
          send <- smtp(email, verbose = TRUE)
          
          if(send$status_code == 250){ 
            
            # if sent, show notifications and clean the inputs
            
            showNotification(
              type = "default", 
              id = "contact-message-sent-success",
              "Email sent!", 
              duration = NULL
            )
            
            # updateTextInput(session, "name", value = "", placeholder = "Full Name")
            # updateTextInput(session, "email", value = "", placeholder = "Business Email")
            # updateTextInput(session, "org", value = "", placeholder = "Organization")
            # updateTextAreaInput(session, "message", value = "", placeholder = "How can we help?")
            
          }else{
            
            # if not sent, show error message
            showNotification(
              type = "error", 
              id = "contact-message-sent-error",
              "Oops! Something went wrong. Wait a few minutes and try again or
              email us at <basinscoutsupport@thefreshwatertrust.org>.",
              duration = NULL
            )
            
          }
          
        }
        
      })
      
      
    })
}
