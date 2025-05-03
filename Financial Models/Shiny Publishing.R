library(rsconnect)

rsconnect::setAccountInfo(name='talgatzharkynbay',
                          token='A0E74CDF5F98B5B731359329D90CA555',
                          secret='MI0ZBAarp16jFw0cS0Ml2oPngLp889Ww5+FjzTVS')
deployApp("Construction App.Rmd")
