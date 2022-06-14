FROM rocker/verse:4.1.2
## tag cryanking/cognitioncheck:1.1

ARG R_VERSION
ARG BUILD_DATE
ARG CRAN
ENV BUILD_DATE ${BUILD_DATE:-2022-02-12}
ENV R_VERSION=${R_VERSION:-4.1.2} \
    CRAN=${CRAN:-https://cran.rstudio.com} \ 
    TERM=xterm

# Set the locale
#RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
#    locale-gen
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8   


RUN useradd docker \
	&& mkdir /home/docker \
	&& chown docker:docker /home/docker \
	&& addgroup docker staff

RUN  apt-get update \
	&& DEBIAN_FRONTEND="noninteractive" apt-get install -y --no-install-recommends \
  apt-utils \
  gpg gpg-agent
  
RUN install2.r --error \
    janitor \
    tableone \
    foreach \
    doParallel \
    nonnest2 \
    pROC

RUN install2.r --error kableExtra
     
# fixes a wierd error
RUN install2.r --error Rcpp
# fix the repo for tex to the historical version
RUN tlmgr option repository http://ftp.math.utah.edu/pub/tex/historic/systems/texlive/2021/tlnet-final
# install a package to fore tlmgr to update
RUN R -e 'tinytex::tlmgr_install(pkgs="amsmath")' 
# install almost all the packages needed to make the report generate
COPY ./dummy_report.rmd /code/dummy_report.rmd
RUN R -e 'rmarkdown::render("/code/dummy_report.rmd")'

RUN chmod -R 777 /root ; chmod -R 777 /usr/local/lib/R/site-library /usr/local/lib/R/library /usr/local/texlive/
