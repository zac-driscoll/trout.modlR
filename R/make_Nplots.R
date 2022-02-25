#' Graph Nplot
#'
#' A wrapper function used to create Nplot. The type arguement determines
#' which Nplot graphing function will be used ("total","recruit",or "naa").
#'
#'
#' @param type Graph Type("total","recruit",or "naa")
#' @param dat ADMB data output
#' @param title Include graph title ("Y" or "N")
#' @param ymax Y Axis Maximum
#'
#' @return A ggplot object
#' @export
#'
#' @noRd
#'
#' @examples graph_nplot()
graph_Nplot <-
  function(data,type,title = "Y",ymax = 0) {
    #set parameters
    params <-
      get_parameters(dat, "mfage", "mlage", "Nage", "years", "mu", "spp")
    #assign parameters to environment
    for (i in 1:length(params)) {
      assign(
        unlist(names(params[i])),
        unlist(params[i]))
    }
    #colnames(Nage) <- paste0("c", ages) #"ages" is defined in UseRPlotter_mjs.R
    #plotting defaults
    ym <- find_Nplot_ymax(type,ymax,Nage)
    gtitle <- make_Nplot_title(title,type,mfage,mlage,mu,spp)
    #pass parameter to graphing code
    if(type == "total"){
      p <- Nplot.total(mfage,mlage,years,spp,mu,Nage,title,ym)
    }
    if(type == "recruit"){
      p <- Nplot.recruit(mfage,Nage,mu,title,ym)
    }
    if(type == "naa"){
      p <- Nplot.naa(mu,Nage,mfage,mlage,title)
    }
    p
  }

Nplot.total <- function(mfage,mlage,years,spp,mu,Nage,title) {
  c.ages <- paste0("c", mfage:mlage)
  Nage$sum <- (rowSums(Nage[, c(c.ages)])) / 1000
  Nage <- cbind(years, Nage) #first column is years
  Nage.plot <- Nage
  #Line plot of total abundance
  p <-
    ggplot2::ggplot(Nage, ggplot2::aes(x = years, y = sum)) +
    ggplot2::labs(x = "Year", y = "Number of Fish (x 1,000)", title = gtitle) +
    ggplot2::geom_line(colour = "black") +
    ggplot2::geom_point(colour = "black") +
    ggplot2::ylim(0, ym) +
    ggplot2::scale_x_continuous(breaks = seq(min(years),max(years), 5)) +
    ggplot::theme_bw()
  p <- p + theme_bw() + axis.form
  p
}

Nplot.recruit <- function(mfage,Nage,mu,gtitle){
    #Line Plot of first age (recruitment)
    p <-
      ggplot2::ggplot(Nage,ggplot2::aes(x=years,y=rec))+
      ggplot2::labs(x="Year",y="Recruitment (x 1,000)",title=gtitle) +
      ggplot2::scale_x_continuous(breaks=seq(min(years),max(years),5))+
      ggplot2::geom_line(colour="black") +
      ggplot2::ylim(0,ym) +
      ggplot2::geom_point(colour="black")
      ggplot2::theme_bw() +
      axis.form
    p
}

Nplot.naa <- function(mu,Nage,mfage,mlage,gtitle){
  naa <- data.frame(melt(fit$Nage/1000))
    colnames(naa) <- c("year","age","value")
    my.max <- max(naa$value)
    naa.1 <- dplyr::filter(naa, age>=mfage,age<=mlage)
    p <-
      ggplot2::ggplot(naa.1,
                         ggplot2::aes(as.factor(age),
                                      as.factor(year),
                                      z=value)) +
      ggplot2::geom_tile(ggplot2::aes(fill=value), colour = "grey60")+
      ggplot2::scale_fill_gradientn(colours = c("white", "green", "navyblue"),
                                    values = c(0,0.05,1),
                                    name="Number\nof Fish\n(x 1000)") +
      ggplot2::labs(x="Age",y="Year",title=gtitle,legend) +
      ggplot2::theme_bw() +
      axis.form
    p

  }

make_Nplot_title <- function(title,type,mfage,mlage,mu,spp){
  gtitle <- switch(
    type,
    "total" = paste0("Estimated Abundance at Age in ", mu),
    "recruit" = paste0("Number of Age-", mfage, " Recruits in ", mu),
    "naa" = paste0("Estimated ",spp," Abundance ","(Age ",mfage," to ",mlage,
                   "+) in ",mu)
  )
  if (title != "Y") { gtitle <- ""}
  gtitle
}

find_Nplot_ymax <- function(type,ymax,Nage){
  ifelse(ymax == 0,
         switch(type,
                "total" = max(Nage$sum),
                "recruit" = max(Nage$rec)),
         ymax)
}


