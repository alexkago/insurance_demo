system('wget http://cran.r-project.org/src/contrib/Archive/Rcpp/Rcpp_0.10.4.tar.gz')
system('R CMD INSTALL -c Rcpp_0.10.4.tar.gz')
system('rm Rcpp_0.10.4.tar.gz')

install.packages("colorspace",clean=T)
install.packages("stringr",clean=T)
install.packages("RColorBrewer",clean=T)
install.packages("dichromat",clean=T)
install.packages("labeling",clean=T)

system('wget http://cran.r-project.org/src/contrib/Archive/plyr/plyr_1.7.1.tar.gz')
system('R CMD INSTALL -c plyr_1.7.1.tar.gz')
system('rm plyr_1.7.1.tar.gz')

system('wget http://cran.r-project.org/src/contrib/Archive/httpuv/httpuv_1.2.1.tar.gz')
system('R CMD INSTALL -c httpuv_1.2.1.tar.gz')
system('rm httpuv_1.2.1.tar.gz')

system('wget http://cran.r-project.org/src/contrib/Archive/reshape2/reshape2_1.2.2.tar.gz')
system('R CMD INSTALL -c reshape2_1.2.2.tar.gz')
system('rm reshape2_1.2.2.tar.gz')

install.packages("ggplot2",clean=T)
install.packages("shiny",clean=T)
install.packages("randomForest",clean=T)
install.packages("Rook",clean=T)
print("All packages installed...")