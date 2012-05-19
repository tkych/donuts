Last Updated : 2012/05/19 12:36:50 tkych

# Donuts: Graph DSL for Common Lisp


## Introduction

By donuts, the graph represented by s-expression is converted to image.
How to use donuts is easy.
<> creates a node.
-> puts an edge between two nodes.
&& makes a graph by bundling some nodes, edges and graphs.
$$ outputs an image of the graph.

The goal of donuts is to draw graphs in lispic way of thinking (REPL, macro, CLOS ,multi-paradigm style and so on). 

For more details, see index(Under Translation) or index-ja(Japanease) in doc directory.


## Version

The Current version of the donuts is 0.3.0 (beta).

## Dependencies

* [Graphviz](http://www.graphviz.org/) by AT&T Labs

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
* Graph-Constructor, &&, & makes graph with nodes, edges, graphs.   `(&& . nodes-edges-graphs) => graph`
* Shell-Interface, $$, $ outputs graph to viewer.   `($$ graph) => NIL ;output image to viewer`
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

       DONUTS> 
       ;; example from http://www.linuxjournal.com/article/7275
       ;; num-day: total number of days in month
       ;; starting-day: 0 as Sun, 1 as Mon, ... , 6 as Sat
       (defun generate-monthly-calendar (month year num-days starting-day)
         (let ((month (<> (format nil "~@(~A~)\\n~D" month year) :shape :Msquare))
               (luminary7 (loop :for day :in '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
                                :collect (<> day :shape :egg :style :filled :color :lightgray)))
               (days (loop :for day :in (nconc (loop :repeat starting-day :collect "")
                                               (loop :for d :from 1 :to num-days :collect d)
                                               (loop :repeat (- (* 7 (if (and (= 28 num-days) (= 0 starting-day))
                                                                         4 5)) ;for Feb starting Sun in common year 
                                                                starting-day num-days)
                                                     :collect ""))
                           :collect (<> day :shape :box))))
           ;; fn group from On Lisp, ex. (group '(1 2 3 4) 2) => ((1 2) (3 4))
           (apply #'&& (loop :for week :in (cons luminary7 (group days 7)) 
                             :collect (apply #'--> month week)))))

       GENERATE-MONTHLY-CALENDAR

       DONUTS> ($$ (& (:size "8,6":rankdir :LR)
                     (generate-monthly-calendar 'may 2012 31 2)))

       ; Output Calendar to Viewer
       NIL


## Author, License, Copyright

* Takaya OCHIAI <tkych.repl@gmail.com>

* MIT License

* Copyright (C) 2012 Takaya OCHIAI
