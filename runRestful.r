library(plumber)
r <- plumb("myfile.R")  # Where 'myfile.R' is the location of the file shown above
r$run(port=8000)

#then run in browser:
#   http://localhost:8000/mean
#   http://localhost:8000/mean?samples=100
# curl --data "a=4&b=3" "http://localhost:8000/sum"
