


col.2.rgb.fun<-function(col.pal, alpha.val=.5) {
	col.rgb<-col2rgb(col.pal)/255
	
	new.pal<-c()
	for(i in 1:ncol(col.rgb)) {
		new.pal[i]<-rgb(col.rgb[1,i], col.rgb[2,i], col.rgb[3,i], alpha=alpha.val)
	}
	return(new.pal)
}
##### Function to process output of stan model and plot fit

plot.stan.fun<-function(exob,
						count.data, # Count data used for the fit
						mid_age,  # mid_points of age categories in data, used for fit
						col.plot="royalblue",
						title=NULL,
						year.counts=NA,
						ind.keep
						) {

pred.rate<-exob$poi_rate_g
							
if(length(ind.keep)<4){
	pred.rate<-pred.rate[c(sapply(ind.keep, function(x) seq(x*1000-999, x*1000, by=1))),,]
}							  
#### Calculate number of plots needed
nrow_plots<-ceiling(ncol(count.data)/3)
		
							 
par(mfrow=c(nrow_plots, 3), mar=c(0.5, 0.5, 0.5, 0.5), oma=c(5,5,5,3 ))
for(j in 1:ncol(count.data)) {
	
plot(mid_age, count.data[,j], pch=19, cex=.3, ylim=c(0, max(count.data)), ylab="", xlab="", yaxt="n", xaxt="n")
for(i in 1:200) {
	lines(mid_age, pred.rate[floor(runif(1, 1, dim(pred.rate)[[1]])),, j], col=col.2.rgb.fun(col.plot, .2), lwd=.05)
}
points(mid_age, count.data[,j], pch=19, cex=.3)
if(c(j-1)%%3==0) {
	axis(2)
}
if(j > c(ncol(count.data)-ncol(count.data)%%3)) {
	axis(1)
}
axis(1, labels=F, tick=T)
axis(2, labels=F, tick=T)

if(!is.na(year.counts[j])) {legend("topright", legend=c(year.counts[j]), bty="n", cex=.7)}
}
mtext("Age", 1, line=3, cex=1, outer=T)
mtext("Dengue counts", 2, line=3, cex=1, outer=T)
mtext(title, 3, line=2, cex=1.2, outer=T)
				  
					
						}






