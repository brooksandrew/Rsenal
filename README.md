
## Rsenal R package

This is mainly a collection of random, or more accurately, clusters of random R functions I've written for personal and work projects over the years. I don't expect it to be immediately useful to the general population of R users. For now it's mostly my cloud storage (with nice documentation) for R functions that I use repeatedly. Some functions are fairly general purpose, while others were developed for specific problems and are not super generalizeable in the current state. 

Current clusters of functionality: 

* manipulating igraph objects 
* modeling and model validation 
* cleaning data 
* scaling and transforming data
* manipulating/summarizing outputs from models and association rules

### How to download

`install.packages('devtools') # if devtools not already installed  
require('devtools')  
install_gitub('brooksandrew/Rsenal')  
require('Rsenal')`  

That's it, you got it.

List all functions

`library(help='Rsenal')`

OR 

`?Rsenal` and then click the `Index` link at the bottom.
