## Compile Under Mac OSX

It is a bit tricky to install python correctly. My general suggestion 
is to use [Anaconda's version](https://store.continuum.io/cshop/anaconda/). 
It includes most things about scientific computing for Mac OSX. 

Also you may have to install maven to manage scala codes. 
```
$ brew install maven
```

Then proceed to compile codes. 
```
$ easy_install Pygments # maybe the root user: sudo
$ cd color; mvn install; cd .. # build scala to jar
$ python colorai.py # get the code run
```


