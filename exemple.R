library(cartography)
data(nuts2006)
nuts2.df$cagr <- (((nuts2.df$pop2008 / nuts2.df$pop1999)^(1/9)) - 1) * 100
cols <- carto.pal(pal1 = "green.pal", n1 = 2, pal2 = "red.pal", n2 = 4) 
plot(nuts0.spdf, border = NA, col = NA, bg = "#A6CAE0")
plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)
choroLayer(spdf = nuts2.spdf, df = nuts2.df, 
           var = "cagr", 
           breaks = c(-2.43,-1,0,0.5,1,2,3.1), 
           col = cols, border = "grey40", lwd = 0.5, 
           legend.pos = "right", 
           legend.title.txt = "Compound Annual\nGrowth Rate", 
           legend.values.rnd = 2, add = TRUE) 
plot(nuts0.spdf,border = "grey20", lwd=0.75, add=TRUE)
layoutLayer(title = "Demographic Trends", author = "cartography", 
            sources = "Eurostat, 2008", frame = TRUE, col = NA, 
            scale = NULL, coltitle = "black",
            south = TRUE) 
barscale(size = 400)