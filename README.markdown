Last Updated : 2012/05/09 20:56:52 tkych

# Donuts: Graph DSL for Common Lisp

## Introduction

By donuts, the graph represented by s-expression is converted to image file.
How to use donuts is easy.
<> creates a node.
-> puts an edge between two nodes.
And && makes a graph by bundling some nodes, edges and graphs.
$$ outputs an image of a graph.


## Version

The Current version of the donuts is 0.2.6 (beta).

## Dependency

* [Graphviz](http://www.graphviz.org/) by AT&T Research Labs

* [cl-ppcre](http://weitz.de/cl-ppcre/) by Dr. Edmund Weitz

* [trivial-shell](http://www.quicklisp.org/) by Gary Warren King


## Installation

1.  CL-REPL> `(push #P"/path-to-your-donuts-directory/" asdf:*central-registry*)`
2.  CL-REPL> `(ql:quickload :donuts)`
3.  CL-USER> `(in-package :donuts)`
4.  DONUTS> `($$ (&& (-> 1 2)))`  ;output graph image to viewer

 [quicklisp]: http://www.quicklisp.org/


## Usage

* Node-Constructor, <> makes node from node's identity.   `(<> label) => node`
* Edge-Constructor, -> makes node with nodes.   `(-> node1 node2) => edge`
* Graph-Constructor, && makes graph with nodes, edges, graphs.   `(&& . nodes-edges-graphs) => graph`
* Shell-Interface, $$ outputs graph to viewer.   `($$ graph) => NIL ;output image to viewer`

For more details, see index.html(under translation) or index-j.html in doc directory.



## Examples

      CL-USER> (dot-output
                 (& (:label "example")
                   (-> (<> "a" :shape :box) "b" :color :red)))

      digraph graph_T41 {
        label="example";
        node_T39 [label="a",shape=box];
        node_T39 -> "b" [color=red];
      }
      NIL

      CL-USER> ;; from http://graphviz.org/content/cluster
               ($ (:outfile "cluster.pdf")
                  (&& ([&] (:label "process #1" :style :filled :color :lightgrey)
                        (with-node (:style :filled :color :white)
                          (--> "a0" "a1" "a2" "a3")))
                      ([&] (:label "process #2" :color :blue)
                        (with-node (:style :filled)
                          (--> "b0" "b1" "b2" "b3")))
                      (->> (<> "start" :shape :Mdiamond) "a0" "b0")
                      (==> "a3" "b3" (<> "end" :shape :Msquare))
                      (->  "a1" "b3")
                      (->  "a3" "a0")
                      (->  "b2" "a3")))

       ;; make cluster.pdf and show it in viewer.
       NIL


## Author, License, Copyright

* Takaya OCHIAI <tkych.repl@gmail.com>

* MIT License

* Copyright (C) 2012 Takaya OCHIAI