;;
;; Copyright © 2017 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.numerical.unimin
  "`unimin` is a module of functions and methods designed to find minimal (or
  maximal) values of single variable functions.")

(defn local-maxima
  " Given a function f on [a, b] and N > 0, examine f at the endpoints a, b, and
  at N equally-separated interior points. From this form a list of brackets (p
  q) in each of which a local maximum is trapped. Then apply Brent to all these
  brackets and return a list of pairs (x fx) representing the local maxima.
  "
  [f a b n ftol])

(defn local-minima [f a b n ftol])

(defn estimate-global-max
  "Refer to the previous two functions and find the max of all of those."
  [f a b n ftol])

(defn estimate-global-min
  "Refer to the previous two functions and find the min."
  [f a b n ftol])
