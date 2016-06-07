FROM phusion/baseimage
RUN echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list
RUN gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
RUN gpg -a --export E084DAB9 | sudo apt-key add -
RUN apt-get update
RUN apt-get -y install r-base
RUN R -e "install.packages(c('shiny','dplyr','tidyr','stringi','stringr','xtable'), repos='https://cran.rstudio.com/')"
RUN curl https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.4.2.786-amd64.deb -o shiny.deb && dpkg -i shiny.deb
RUN rm -r /srv/shiny-server/*
COPY ./R /srv/shiny-server
COPY count.jpg /srv/shiny-server
COPY robots.txt /srv/shiny-server
COPY ./shiny-server/templates /opt/shiny-server/templates
expose 3838
CMD /usr/bin/shiny-server