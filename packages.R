#Installing via github; no cran; nobioconductor


library(devtools)

install_github('duncantl/SVGAnnotation')
--------

## Getting all installed packages

ip <- as.data.frame(installed.packages()[,c(1,3:4)])
rownames(ip) <- NULL
-------

##Force install a version

library(devtools)
install_version('rmdformats', version = '0.3.6')

-------

## Check if the packages are installed or not

c("Rserve", "ggplot2", "devtools", "RCurl", "RJSONIO") %in% installed.packages()[, 1]


------
## Configuring R
./configure --prefix=/apps/R/4.0.0 --enable-R-shlib --with-x --with-cairo --with-jpeglib --with-readline --with-tcltk --with-blas --with-lapack --enable-R-profiling --enable-memory-profiling

> writeLines(head(readLines(R.home('etc/Makeconf')), n = 8))
# etc/Makeconf. Generated from Makeconf.in by configure.
#
# ${R_HOME}/etc/Makeconf
#
# R was configured using the following call
# (not including env. vars and site configuration)
# configure '--prefix=/MG/SHARED/PIPELINES/RESEARCH/RH7/1.0/R-4.0.5' '-enable-R-shlib' '--with-blas' '--with-cairo' '--with-x' '--with-libpng' '--with-jpeglib' '--with-pcre1' '--x-libraries=/usr/include/X11'

