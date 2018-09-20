rm(list=ls())

siteCoor <- readRDS("stageTrack/siteCoords.rds")


iconFun <- function(site) {

    svg(height=3, width=5,
        filename=paste0("stageTrack/www/icon", site, ".svg"))
    par(mai=rep(0.45,4), xaxs="i", yaxs="i")
    plot(x=seq(1,14), y=c(3,2,5,13,8,4,5,4,7,5,3,2,3,1),
         type="l", ylim=c(0,14), lwd=20, col="red",
         axes=FALSE, xlab=NA, ylab=NA)
    axis(1, at=seq(1,14),labels=NA, lwd.ticks=0, lwd=20)
    axis(2, labels=NA, lwd.ticks=0, lwd=20)
    graphics.off()
}

## Run fun
invisible(sapply(X=siteCoor$SiteCode, FUN=iconFun))



#############################################################################
## PNG version

iconFun <- function(site) {

    png(height=3, width=5, units="in", res=100,
        filename=paste0("stageTrack/www/icon", site, ".png"))
    par(mai=rep(0.45,4), xaxs="i", yaxs="i")
    plot(x=seq(1,14), y=c(3,2,5,13,8,4,5,4,7,5,3,2,3,1),
         type="l", ylim=c(0,14), lwd=20, col="red",
         axes=FALSE, xlab=NA, ylab=NA)
    axis(1, at=seq(1,14),labels=NA, lwd.ticks=0, lwd=20)
    axis(2, labels=NA, lwd.ticks=0, lwd=20)
    graphics.off()
}

## Run fun
invisible(sapply(X=siteCoor$SiteCode, FUN=iconFun))

