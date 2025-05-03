s=input$Spot
k=input$Strike
v=input$Sigma
r=input$RF_rate
tt=input$Time
d=input$Div
nstep=input$Steps

Option_Data_2=binomopt(s, k, v, r, tt, d, nstep, american=FALSE, putopt=FALSE,
                       returntrees = TRUE, crr = TRUE, returngreeks = TRUE)