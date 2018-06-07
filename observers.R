observeEvent(input$demovars, 
             # Observe changes in input$text1, and change text2 on event
             updateCheckboxInput(session, "demovars2", NULL, input$demovars)
)

observeEvent(input$demovars2, 
             # Observe changes in input$text2, and change text1 on event
             updateCheckboxInput(session, "demovars", NULL, input$demovars2)
)

observeEvent(input$subs, 
             # Observe changes in input$text1, and change text2 on event
             updateCheckboxInput(session, "subs2", NULL, input$subs)
)

observeEvent(input$subs2, 
             # Observe changes in input$text2, and change text1 on event
             updateCheckboxInput(session, "subs", NULL, input$subs2)
)




