Last Updated : 2012/05/29 12:23:51 tkych


# Donuts: Graph DSL for Common Lisp


## Introduction

By donuts, the graph represented by s-expression is converted to image.
How to use donuts is easy.
<> creates a node.
-> puts an edge between two nodes.
&& makes a graph by bundling some nodes, edges and graphs.
$$ outputs an image of the graph.

For more details, please see index(Under Translation) or index-ja(Japanease) in doc directory.


## The Goal of Donuts

[Graphviz][] is a collection of libraries and utilities for drawing a graph.
[Dot language][] is description language, used in Graphviz.
Graphviz is very useful.
However, I (as a lisp programmer) think there are some points to do kaizen.

1.  Since dot language is not turing-complete,
    when we draw a graph, we don't take full advantage of the pattern in the graph.

2.  Because dot language is so-called compiled language,
    development cycle is inconvenient.

3.  Plain common lisp does not have ability to draw graph.

The goal of donuts is to draw graph in lispic way of thinking 
(REPL, macro, CLOS, multi-paradigm style, and so on). 

  [Graphviz]: http://www.graphviz.org/
  [Dot language]: http://www.graphviz.org/dot-language.html


## Current Version of Donuts

0.3.1 (beta)


## Dependencies

* [Graphviz][] by AT&T Labs

* [cl-ppcre](http://weitz.de/cl-ppcre/) by Dr. Edmund Weitz

* [trivial-shell](http://common-lisp.net/project/trivial-shell/) by Gary Warren King


## Installation & Start

1.  CL-REPL> `(ql:quickload :donuts)`
2.  CL-USER> `(in-package :donuts)`
3.  DONUTS> `(dot-output (&& (-> 1 2)))`  ;output dot code in standard-output
4.  DONUTS> `($$ (&& (-> 1 2)))`  ;output graph image to viewer


## Usage

* Node-Constructor, <> makes node from node's identity.   `(<> label) => node`
* Edge-Constructor, -> makes node with nodes.   `(-> node1 node2) => edge`
* Graph-Constructor, &&, &, [&] makes graph with nodes, edges, graphs.   `(&& . nodes-edges-graphs) => graph`
* Shell-Interface, $$, $ outputs graph to viewer.   `($$ graph) => NIL ;output image to viewer`
* DOT-OUTPUT outputs dot code in standart-output.   `(DOT-OUTPUT graph) => NIL ;output dot code`


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

      DONUTS> ;; Example from http://graphviz.org/content/cluster
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

       ; Create cluster.pdf & Output image to viewer as shown on the right
       NIL

       DONUTS> 
       ;; Example from http://www.linuxjournal.com/article/7275
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

       DONUTS> 
       ;; Example from http://www.graphviz.org/doc/info/html2.gv
       ($$ (& (:rankdir :LR)
         (with-node (:shape :plaintext)
           (let ((a (<> (html (table :border 0 :cellborder 1 :cellspacing 0
                                     (tr (td :rowspan 3 :bgcolor :yellow "class"))
                                     (tr (td :port "here" :bgcolor :lightblue "qualfier"))))))
                 (b (<> (html (table :bgcolor :bisque
                                     (tr (td :colspan 3 "elephant")
                                         (td :rowspan 2 :bgcolor :chartreuse
                                             :valign :bottom :align :right "two"))
                                     (tr (td :colspan 2 :rowspan 2
                                             (table :bgcolor :grey
                                                    (tr (td "corn"))
                                                    (tr (td :bgcolor :yellow "c"))
                                                    (tr (td "f"))))
                                         (td :bgcolor :white "penguin"))
                                     (tr (td :colspan 2 :border 4 :align :right :port "there" "4"))))
                         :shape :ellipse :style :filled))
                 (c (<> (html "long line 1" (br) "line 2" (br :align :left) "line 3" (br :align :right))))
                 (d (<> "d" :shape :triangle)))
             (&&
               (~ b c)
               (-> (@ a :here) (@ b :there) :dir :both :arrowtail :diamond)
               (-> c b)
               (-> d c :label (html (table (tr (td :bgcolor :red :width 10)
                                               (td "Edge labels" (br) "also")
                                               (td :bgcolor :blue :width 10))))))))))

       ; Output example to Viewer
       NIL


## Author, License, Copyright

* Takaya OCHIAI <tkych.repl@gmail.com>

* MIT License

* Copyright (C) 2012 Takaya OCHIAI
