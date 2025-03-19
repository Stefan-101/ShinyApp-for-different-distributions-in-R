#instalarea pachetului shiny (daca este necesar)
if (!require(shiny)) 
  install.packages("shiny") 
#incarcare pachet shiny
library(shiny)

# PARTEA 1: INTERFATA 

ui <- fluidPage(
  #titlul aplicatiei
  titlePanel("REPREZENTAREA GRAFICA A FUNCTIILOR DE REPARTITIE ~ Shiny App"),
  
  sidebarLayout(
    #partea de selectare distributie si introducere de date
    sidebarPanel(
      #selectarea distributiei
      selectInput( 
        "distributie", 
        "Alegeti distributia dorita din cele de mai jos:",
        choices = c("N(0, 1)", 
                    "N(μ, σ^2)", 
                    "Exp(λ)"),
        #by default este selectata prima distributie (nu necesita campuri aditionale)
        selected = "N(0, 1)"
      ),
      
      #cazul in care se selecteaza a doua distributie
      conditionalPanel( 
        condition = "input.distributie=='N(μ, σ^2)'", 
        numericInput("miu", "Care doriti sa fie valoarea mediei(μ)?:", value = 0, step = 0.1), #by default valoarea e 0, pot sa o incrementez/decrementez din sageti cu 0.1
        numericInput("sigma", "Care doriti sa fie valoarea deviatiei standard(σ)?:", value = 1, min = 0.1, step = 0.1)  #by default valoarea e 1, pot sa o incrementez/decrementez din sageti cu 0.1
                                                                                                                        #min=0.1 pentru ca deviatia e mereu pozitiva, iar 0.1 este minimul practic posibil pentru generarea de rezultate concludente
      ),
      
      #cazul in care se selecteaza a treia distributie
      conditionalPanel(
        condition = "input.distributie=='Exp(λ)'",
        numericInput("lambda", "Care doriti sa fie valoarea parametrului λ?:", value = 1, min = 0.1, step = 0.1) #by default valoarea e 1, pot sa o incrementez/decrementez din sageti cu 0.1
                                                                                                                 #min=0.1 pentru generarea de rezultate concludente; parametrul lambda trebuie sa fie intotdeauna pozitiv 
      ),
      
      #label pentru introducerea numarului de variabile in cazul sumelor
      numericInput("n", "Care doriti sa fie numarul de variabile(n) in cazul sumelor?:", value = 10, min = 1, step = 1), #by default valoarea e 10, pot sa o incrementez/decrementez din sageti cu 1
                                                                                                                         #min=1 pentru ca vrem sa avem cel putin o variabila in cadrul sumelor
      
      #graficul pentru repartitia selectata se genereaza si afiseaza odata cu apasarea butonului de catre utilizator
      actionButton("butonGenerare", "Genereaza graficul cu repartitiile selectate")
    ),
    
    #partea de afisare a rezultatului(graficului)
    mainPanel(plotOutput("graficDistributie"))
  )
)

# PARTEA 2: GENERAREA REPARTITIILOR <-> LOGICA APLICATIEI
#intr-o aplicatie Shiny, logica se defineste in functia server
server <- function(input, output) 
{
  #realizarea graficului ce va fi trimis ca output
  output$graficDistributie <- renderPlot(
    {
      input$butonGenerare  #declanseaza logica doar dupa ce utilizatorul a apasat butonul din interfata
      isolate( #prevenim modificarea graficului daca utilizatorul introduce dupa o generare alte date, dar nu apasa pe buton
        {
          n<-input$n #preluare numar variabile din input
          x<-if(input$distributie=="Exp(λ)") seq(0, 20, length.out=2000) else seq(-5, 5, length.out=2000) #vector pentru generare precisa a punctelor pe grafic
          
          #initializare variabile aleatoare
          Var_X<-NULL
          Var_X_inmultit_adunat<-NULL 
          Var_X_patrat<-NULL 
          Var_X_suma<-NULL 
          Var_X_suma_patrat<-NULL
          
          #N(0, 1)
          if(input$distributie=="N(0, 1)") 
          {
            Var_X<-pnorm(x, mean=0, sd=1)
            Var_X_inmultit_adunat<-pnorm((x-3)/2, mean=0, sd=1) #(x-3)/2 pentru a aduce Y=3+2X la distributie N(0,1)
            Var_X_patrat<-pnorm(sqrt(x), mean=0, sd=1) - pnorm(-sqrt(x), mean=0, sd=1) #[-|x|,|x|]
            Var_X_suma<-pnorm(x, mean=0, sd=sqrt(n)) #sd=rad(var)(var=n*1=n pentru suma)
            
            #suma patratelor - simulare cu n variabile
            suma_patrat<-replicate(10000, sum(rnorm(n, mean = 0, sd = 1)^2)) #generam 10000 valori pentru suma de patrate de v.a.
            Var_X_suma_patrat<-ecdf(suma_patrat)(x) #functia de repartitie empirica a sumelor anterior simulate
            
            #setez legenda pentru grafic
            legenda_ok<- c(expression(F[X](x)), expression(F[3+2*X](x)), expression(F[X^2](x)), expression(F[sum(X[i])](x)), expression(F[sum(X[i]^2)](x)))
          }
          
          #N(μ, σ^2)
          if (input$distributie=="N(μ, σ^2)") 
          {
            miu<-input$miu #preluare medie din input
            sigma<-input$sigma #preluare deviatie standard din input
            
            Var_X<-pnorm(x, mean=miu, sd=sigma)
            Var_X_inmultit_adunat<-pnorm((x-(3+2*miu))/(2*sigma), mean=miu, sd=sigma) #medie=3+2miu, var=(2*sigma)^2=>abatere=2*sigma, unde Z=X-medie/abatere 
            Var_X_patrat<-pnorm(sqrt(x), mean=miu, sd=sigma) - pnorm(-sqrt(x), mean=miu, sd =sigma) #[-|x|,|x|]
            Var_X_suma<-pnorm(x, mean=n*miu, sd=sqrt(n)*sigma) #sd=rad(var)(var=n*sigma^2 pentru suma)
            
            #suma patratelor - simulare cu n variabile
            suma_patrat<-replicate(10000, sum(rnorm(n, mean=miu, sd=sigma)^2)) #generam 10000 valori pentru suma de patrate de v.a.
            Var_X_suma_patrat<- ecdf(suma_patrat)(x) #functia de repartitie empirica a sumelor anterior simulate
            
            #setez legenda pentru grafic
            legenda_ok<- c(expression(F[X](x)), expression(F[3+2*X](x)), expression(F[X^2](x)), expression(F[sum(X[i])](x)), expression(F[sum(X[i]^2)](x)))
          }
          
          #Exp(λ)
          if(input$distributie=="Exp(λ)") 
          {
            lambda<-input$lambda #preluare lambda din input
            
            Var_X<-pexp(x, rate=lambda)
            Var_X_inmultit_adunat<-pexp((x-2)/(-5), rate=lambda) #(x-2)/(-5) din Y=2-5X 
            Var_X_patrat<-pexp(sqrt(x), rate=lambda) #[-|x|,|x|]
            Var_X_suma<-pgamma(x, shape=n, rate=lambda) #distributie speciala (Erlang-tip de distributie Gamma) pentru n v.a. Xi
            #suma de v.a. distribuite Exp = distributie Erlang!!!
            
            #suma patratelor - simulare cu n variabile
            suma_patrat<-replicate(10000, sum(rexp(n, rate=lambda)^2)) #generam 10000 valori pentru suma de patrate de v.a.
            Var_X_suma_patrat<-ecdf(suma_patrat)(x) #functia de repartitie empirica a sumelor anterior simulate
            
            #setez legenda pentru grafic
            legenda_ok<- c(expression(F[X](x)), expression(F[2-5*X](x)), expression(F[X^2](x)), expression(F[sum(X[i])](x)), expression(F[sum(X[i]^2)](x)))
          }
          
          #GRAFICUL
          plot(x, Var_X, type ="l", col="darksalmon", lwd=2, ylim=c(0, 1), xlab="x", ylab="F(x)", main=paste("FUNCTII DE REPARTITIE PENTRU", input$distributie))
          lines(x, Var_X_inmultit_adunat, col="darkcyan", lwd=2)
          lines(x, Var_X_patrat, col="red", lwd=2)
          lines(x, Var_X_suma, col="blueviolet", lwd=2)
          lines(x, Var_X_suma_patrat, col="gold", lwd=2)
          
          #LEGENDA
          legend("bottomright", 
                 legend = legenda_ok,
                 col = c("darksalmon", "darkcyan", "red", "blueviolet", "gold"), lwd = 2)
        })
    })
}

#LANSAREA APLICATIEI
shinyApp(ui=ui, server=server)