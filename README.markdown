Last Updated : 2012/05/12 09:59:30 tkych

# Donuts: Graph DSL for Common Lisp

## Introduction

By donuts, the graph represented by s-expression is converted to image file.
How to use donuts is easy.
<> creates a node.
-> puts an edge between two nodes.
&& makes a graph by bundling some nodes, edges and graphs.
$$ outputs an image of a graph.

For more details, see [index.html(Under translation)](./donuts/doc/index.html) or [index-j.html(Japanease)](./donuts/doc/index-j.html) in doc directory.


## Version

The Current version of the donuts is 0.2.7 (beta).

## Dependencies

* [Graphviz](http://www.graphviz.org/) by AT&T Research Labs

* [cl-ppcre](http://weitz.de/cl-ppcre/) by Dr. Edmund Weitz

* [trivial-shell](http://www.quicklisp.org/) by Gary Warren King


## Installation & Start

1.  CL-REPL> `(push #P"/path-to-your-donuts-directory/" asdf:*central-registry*)`
2.  CL-REPL> `(ql:quickload :donuts)`
3.  CL-USER> `(in-package :donuts)`
4.  DONUTS> `(dot-output (&& (-> 1 2)))`  ;output dot code in standard-output
5.  DONUTS> `($$ (&& (-> 1 2)))`  ;output graph image to viewer


## Usage

* Node-Constructor, <> makes node from node's identity.   `(<> label) => node`
* Edge-Constructor, -> makes node with nodes.   `(-> node1 node2) => edge`
* Graph-Constructor, && makes graph with nodes, edges, graphs.   `(&& . nodes-edges-graphs) => graph`
* Shell-Interface, $$ outputs graph to viewer.   `($$ graph) => NIL ;output image to viewer`
* dot-output outputs dot code in standart-output.   `(dot-output graph) => NIL ;output dot code`


## Examples

      DONUTS> (dot-output
                (& (:label "example")
                  (-> (<> "a" :shape :box) "b" :color :red)))

      digraph graph_ID_41 {
        label="example";
        node_ID_39 [label="a",shape=box];
        node_ID_39 -> "b" [color=red];
      }
      NIL

      DONUTS> ;; example from http://graphviz.org/content/cluster
              ;; create cluster.pdf and show it in viewer
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
       NIL


## Author, License, Copyright

* Takaya OCHIAI <tkych.repl@gmail.com>

* MIT License

* Copyright (C) 2012 Takaya OCHIAI