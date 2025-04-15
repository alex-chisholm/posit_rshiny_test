# UI module for contact page

ui_contact <- function(id){
  
  ns <- NS(id)
  
  div(
    class = "container",
    
    div(
      class = "d-flex justify-content-center",
      
      wellPanel(
        class = "bg-light",
        style = "border-radius: 0 0 3px 3px; max-width:500px;",
        
        div(
          style = "font-size:24px; font-weight: bold;",
          class = "text-center",
          img(src = "img/TFT_logo.png", height = "110px")
        ),
      
       
        div(
          style = "width:400px;",
          class = "text-justified p-1 pb-4 pt-3",
          "If you have any questions, encounter any issues, or simply want to share your thoughts,
              please don't hesitate to reach out to us."
          ),
        
        div(
          class = "d-flex justify-content-center",
          div(
            textInput(ns("name"), label = NULL, placeholder = "Full Name", width = 400),
            textInput(ns("email"), label = NULL, placeholder = "Business Email", width = 400),
            textInput(ns("org"), label = NULL, placeholder = "Organization", width = 400),
            textAreaInput(ns("message"), label = NULL, placeholder = "How can we help?", width = 400, height = 200),
            div(
              class = "pt-2",
              actionBttn(
                inputId = ns("send"),
                label = "Send",
                size = "sm",
                style = "simple",
                color = "success"
              )
            )
          )
          
        )
      )
      
    )
  )
  
}
