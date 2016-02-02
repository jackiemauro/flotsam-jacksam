library("tuneR")

# this is a silly file that takes random distributions and plays sounds based off of them

jazz <- function(n = 10,
                 t.dist = "normal", 
                 d.dist = "", 
                 t.mean = 261.63,
                 t.sd = 50,
                 d.mean = 44100,
                 d.sd = sqrt(44100),
                 play = F){
  
  # if you don't change anything, this will create
  # 10 normally distributed tones centered at C4 with 
  # SD=50, each for the same amount of time.
  # i recommend using an exponential distribution
  # for durations, since that seems most natural.
  # pick play = T if you want it to play the tones after
  # running
  
  # for reference (so you aren't making a terrible sound most of the time):
  #   G3 = 196
  #   D3 = 146.83
  #   A3 = 220
  #   B3 = 246.94
  #   C4 = 261.63
  #   A4 = 440
  # http://www.phy.mtu.edu/~suits/notefreqs.html for others
  
  
  # pick the tone distribution
  if(t.dist == "normal"){
    x <- rnorm(n, t.mean, t.sd)
  }
  else if(t.dist == "poisson"){
    x <- rpois(n, t.mean)
  }
  else if(t.dist == "uniform"){
    x <- runif(n, t.mean - t.sd, t.mean + t.sd)
  }
  else{
    print("i only know a few, stop being greedy")
  }
  
  # pick the duration distribution
  if(d.dist != ""){
    if(d.dist == "normal"){
      y <- rnorm(n, d.mean, d.sd)
    }
    else if(d.dist == "poisson"){
      y <- rpois(n, d.mean)
    }
    else if(d.dist == "exp"){
      y <- rexp(n, 1/d.mean)
    }
    else if(t.dist == "uniform"){
      x <- runif(n, d.mean - d.sd, d.mean + d.sd)
    }
    else{
      print("i only know a few, just use exponential gahd")
    }
    com = NULL
    for(ii in 2:n){
      temp = paste(",sine(x[",ii,"], duration = y[",ii,"])")
      com = paste(com,temp)
    }
    com = paste("bind(sine(x[1],duration = y[1])", com, ")")
    jazzy = eval(parse(text = com))
  }
  
  # with no duration given, defaults to equal time for each
  else{
    com = NULL
    for(ii in 2:n){
      temp = paste(",sine(x[",ii,"])")
      com = paste(com,temp)
    }
    com = paste("bind(sine(x[1])", com, ")")
    jazzy = eval(parse(text = com))
  }
  
  # if they want it to play straight away
  if(play == T){
    play(jazzy)
  }
  return(jazzy)
}


# tdir <- "C:\\Users\\jackie\\Desktop\\own research"
# tfile <- file.path(tdir, "forDaph.wav")
# writeWave(ls, filename = tfile)
# 
# ## harmonies!
# ce = sine(261.63) + sine(329.63)
# majthird <- normalize(ce)
# 
# eg = sine(329.63) + sine(392)
# minthird <- normalize(eg)
# 
# da = sine(293.66) + sine(220)
# fifth <- normalize(da)
