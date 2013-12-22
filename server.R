library(shiny)
library(shinyAce)
library(psych)
library(car)


shinyServer(function(input, output) {


    bs <- reactive({
            x <- read.csv(text=input$text, sep="\t")
            describe(x)[2:13]
    })
    
    
    correl <- reactive({
            x <- read.csv(text=input$text, sep="\t")
            
            round(cor(cbind(x)),3)
    })
    
    
    
    makecorPlot <- function(){
        x <- read.csv(text=input$text, sep="\t")
        pairs.panels(x)
    }
    
    output$corPlot <- renderPlot({
        print(makecorPlot())
    })
    
    
    
    reg <- reactive({
        dat <- read.csv(text=input$text, sep="\t")
        
        colnames(dat) <- c("Outcome", c(colnames(dat)[2:ncol(dat)]))
        result <- lm(Outcome ~., dat)
        
        reslt <- summary(result)
        print(reslt)
        cnfi <- confint(result)
        
        cat("---", "\n", "95% CI of B (Unstandardized beta):", "\n")
        print(cnfi)
        
        z <- scale(dat)     # standardize the data
        z <- data.frame(z)
        z.res <- summary(lm(Outcome ~ ., z))
        
        stdb <- data.frame(round((z.res$coefficients[,1][-1]),3))
        colnames(stdb)[1] <- "Standardized beta"
        
        cat("\n", "---", "\n", "Standardized beta estimates:", "\n")
        print(stdb)
        
    if (ncol(dat) >= 3) {

        VIF <- vif(result)
        Tolerance <- 1/VIF
        
        vif.res <- round(data.frame(VIF, Tolerance),3)
        
        cat("\n", "---", "\n", "VIF and tolerance statistic (1/VIF):", "\n")
        print(vif.res)
        cat("\n", "VIF should be smaller than 10 (clozer to 1 better);", "\n",
        "tolerance statistic (1/VIF) should be greater than 0.2.", "\n")
    }
    })
    
    
    
    aic <- reactive({
        dat <- read.csv(text=input$text, sep="\t")
        
        colnames(dat) <- c("Outcome", c(colnames(dat)[2:ncol(dat)]))
        result <- lm(Outcome ~., dat)
        
        model <- step(result)
        print(model)
        
        cat("\n", "---", "\n", "Regression analysis with the best fitting model:", "\n")
        
        summary(model)
        
    })
    
    
    info <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")# バージョン情報
        info2 <- paste("It was executed on ", date(), ".", sep = "")# 実行日時
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    output$info.out <- renderPrint({
        info()
    })



    output$textarea.out <- renderPrint({
        bs()
    })

    output$correl.out <- renderPrint({
        correl()
    })
    
    output$reg.out <- renderPrint({
        reg()
    })
    
    output$aic.out <- renderPrint({
        aic()
    })
    
    output$downloadCorPlot <- downloadHandler( # 名前変更
    filename = function() {
        paste('Corplot-', Sys.Date(), '.pdf', sep='') # 名前変更
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
		print(makecorPlot()) # 名前変更
		dev.off()
	})
    

})
