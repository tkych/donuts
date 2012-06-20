Last Updated : 2012/06/16 19:54:15 tkych

Current Version of Donuts is 0.3.1 (beta)


Donuts: Graph DSL (or Graphviz Interface) for Common Lisp
=========================================================


Introduction
------------

By donuts, the graph represented by S-expression is converted to image. 
How to use donuts is simple. 
<> creates a node. 
-> puts an edge two nodes. 
&& makes a graph by bundling some nodes, edges and graphs. 
$$ outputs an image of the graph.

For further details, please see index (Under Translation) or 
index-ja (Japanease) in doc directory.


The Goal of Donuts
------------------

[Graphviz][] is a collection of libraries and utilities for drawing a
graph.  [Dot language][] is description language, used in Graphviz.
Graphviz is very useful.  However, I (as a lisp programmer) think
there are some points to do kaizen.

1.  Since dot language is not Turing-complete, when we draw a graph,
    we don't take full advantage of the pattern in the graph.

2.  Because dot language is so-called compiled language,
    development cycle is inconvenient.

3.  Plain Common Lisp does not have ability to draw graph.

The goal of donuts is to draw graph in lispic way of thought 
(REPL, macro, CLOS, multi-paradigm style, and so on). 

  [Graphviz]: http://www.graphviz.org/
  [Dot language]: http://www.graphviz.org/content/dot-language


Dependencies
------------

* [Graphviz][] by AT&T Labs

* [CL-PPCRE](http://weitz.de/cl-ppcre/) by Dr. Edmund Weitz

* [Trivial-Shell](http://common-lisp.net/project/trivial-shell/) by Gary Warren King


Installation & Start
--------------------

1.  CL-REPL> `(ql:quickload :donuts)`
2.  CL-REPL> `(in-package :donuts)`
3.  DONUTS> `(dot-output (&& (-> 1 2)))`  ;output dot code in standard-output
4.  DONUTS> `($$ (&& (-> 1 2)))`  ;output graph image to viewer


Usage
-----

* Node-Constructor, <> makes node from node's identity.   `(<> label) => node`
* Edge-Constructor, -> makes edge between nodes.   `(-> node1 node2) => edge`
* Graph-Constructor, &&, &, [&] makes graph with nodes, edges, graphs.   `(&& . nodes-edges-graphs) => graph`
* Shell-Interface, $$, $ outputs graph to viewer.   `($$ graph) => NIL ;output image to viewer`
* DOT-OUTPUT outputs dot code in standart-output.   `(DOT-OUTPUT graph) => NIL ;output dot code`


Examples
--------

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

       ; Create cluster.pdf & Output image to viewer
       NIL

       DONUTS> 
       ;; Example from http://www.linuxjournal.com/article/7275
       ;; num-day: total number of days in month
       ;; starting-day: 0 as Sun, 1 as Mon, ... , 6 as Sat
       (defun generate-monthly-calendar (month year num-days starting-day)
         (let ((month     (generate-month-nodes month year))
               (luminary7 (generate-luminary7-nodes))
               (days      (generate-day-nodes num-days starting-day)))
           (apply #'&& (loop :for week :in (cons luminary7 (group days 7)) 
                             :collect (apply #'--> month week)))))

       (defun generate-month-nodes (month year)
         (<> (format nil "~@(~A~)\\n~D" month year) :shape :Msquare))

       (defun generate-luminary7-nodes ()
         (loop :for day :in '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
               :collect (<> day :shape :egg :style :filled :color :lightgray)))

       (defun generate-day-nodes (num-days starting-day)
         (loop :for day :in (nconc (loop :repeat starting-day :collect "")
                                   (loop :for d :from 1 :to num-days :collect d)
                                   (loop :repeat (- (* 7 (if (and (= 28 num-days) (= 0 starting-day))
                                                             4 5)) ;for Feb starting Sun in common year 
                                                         starting-day num-days)
                                         :collect ""))
               :collect (<> day :shape :box)))

       ;; from On Lisp, e.g. (group '(1 2 3 4) 2) => ((1 2) (3 4))
       (defun group (lst n)
         (if (zerop n) (error "zero length"))
         (labels ((rec (lst acc)
                    (let ((rest (nthcdr n lst)))
                      (if (consp rest)
                          (rec rest (cons (subseq lst 0 n)
                                          acc))
                          (nreverse (cons lst acc))))))
           (if lst (rec lst nil) nil)))

       ($$ (& (:size "8,6":rankdir :LR)
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


Grammar
-------

      <donuts-code> ::= '('<output-op> <graph>')'|<node>|<edge>|<graph>|<html-like-label>|<tag>|<common-lisp-code>
      <output-op> ::= 'dot-output'|'dot-pprint'|'$$'|'$' <attr-list>

      <attr-list> ::= '('{<attr>}')'
      <attr> ::= <attr-keyword> <attr-value>

      <graph> ::= '(&&' <graph-elts>')'|'(&' <attr-list> <graph-elts>')'|<cluster>
      <graph-elts> ::= nil|<pre-node>|<node>|<edge>|<graph>|<cluster>|<rank>|<with>|<graph-elts>{ <graph-elts>}
      <cluster> ::= '([&]' <attr-list> <graph-elts>')'

      <pre-node> ::= <number>|<string>
      <node> ::= <pre-node>|'(<>' (<pre-node>|<html-like-label>){ <attr>}')'|<record>|'(@'<node> <port>[ <port>]')'

      <record> ::= '([] "'<record-label>'"'{ <attr>}')'
      <record-label> ::= <field>{'|'<field>}
      <field> ::= [<filed-port> ]{char}|'{'<record-label>'}'
      <filed-port> ::= <keyword>

      <port> ::= <compass-port>|<filed-port>
      <compass-port> ::= :n|:ne|:e|:se|:s|:sw|:w|:nw|:c|:_

      <edge> ::= '('<edge-cons> <node> <node>{ <attr>}')'|'('<multi-edge-cons>{ <node>}')'|'(?' <node>{ <attr>}')'
      <edge-cons> ::= '->'|'--'
      <multi-edge-cons> ::= '-->'|'->>'|'---'|'-<'|'O'

      <rank> ::= '(rank' <rank-keyword>{ <node>}')'|'(~'{ <node>}')'
      <rank-keyword> ::= :same|:min|:max|:source|:sink

      <with> ::= '('<with-op> <attr-list> <graph-elts> ')'
      <with-op> ::= 'with-node'|'with-edge'

      <html-like-label> ::= '(html'{ <tag>| <txt>}')'
      <txt> ::= <string>|<number>
      <tag> ::= '('<tag-cons> <tag-body>')'
      <tag-cons> ::= 'table'|'font'|'i'|'b'|'u'|'sub'|'sup'|'br'|'hr'|'tr'|'vr'|'td'|'img'
      <tag-body> ::= <tag>|<attr>|<txt>|<tag-body>{ <tag-body>}


Author, License, Copyright
--------------------------

* Takaya OCHIAI <tkych.repl@gmail.com>

* MIT License

* Copyright (C) 2012 Takaya OCHIAI
