###########################################
#### APPENDIX B -  Overview Literature ####
###########################################

#--------------#
#### Set up ####
#--------------#
rm(list = ls())
library(DiagrammeR)

#---------------------------------------------------#
#### Draw diagram lit. review validation efforts ####
#---------------------------------------------------#
diagram1 <- "
  digraph dot {

  graph [layout = dot]
  node [shape = circle,
        style = filled,
        color = grey,
        label = '']

  node[fillcolor = white]
  a[label = 1]
  b[label = 2]
  c[label = 3]
  d[label = 4]
  e[label = 5]
  f[label = 6]
  g[label = 7]
  h[label = 8]
  i[label = 9]
  j[label = 10]
  k[label = 11]
  l[label = 12]
  m[label = 13]
  n[label = 14]
  o[label = 15]
  p[label = 16]
  q[label = 17]
  r[label = 18]


  edge [color = grey]
  a -> {b c d e f g h i j k l m n o}
  {p q r} -> a
}
"
grViz(diagram1)

#-----------------------------------------------#
#### Draw diagram lit. review metamodel uses ####
#-----------------------------------------------#
diagram2 <- "
  digraph dot {

  graph [layout = dot]
  node [shape = circle,
        style = filled,
        color = grey,
        label = '']

  node[fillcolor = white]
  a[label = 1]
  b[label = 2]
  c[label = 3]
  d[label = 4]
  e[label = 5]
  f[label = 6]
  g[label = 7]
  h[label = 8]
  i[label = 9]
  j[label = 10]
  k[label = 11]
  l[label = 12]
  m[label = 13]
  n[label = 14]
  o[label = 15]
  p[label = 16]
  q[label = 17]
  r[label = 18]


  edge [color = grey]
  a -> {b c d e f g h i j k l m n o}
  {p q r} -> a
}
"
grViz(diagram2)
