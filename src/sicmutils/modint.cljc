;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.modint
  "This namespace contains an implementation of a [[ModInt]] datatype and various
  operations for creating and working with [[ModInt]] instances. See [\"Modular
  Arithmetic\"](https://en.wikipedia.org/wiki/Modular_arithmetic) on Wikipedia
  for more details about modular arithmetic.

  [[sicmutils.modint]] also extends many SICMUtils generic operations
  to the [[ModInt]] datatype."
  (:require [sicmutils.euclid :as e]
            [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

(defrecord ModInt [i m]
  v/Numerical
  (numerical? [_] true)

  v/Value
  (zero? [_] (v/zero? i))
  (one? [_] (v/one? i))
  (identity? [_] (v/one? i))
  (zero-like [_] (ModInt. (v/zero-like i) m))
  (one-like [_] (ModInt. (v/one-like i) m))
  (identity-like [_] (ModInt. (v/one-like i) m))
  (freeze [_] (list 'modint i m))
  (exact? [_] true)
  (kind [_] ::modint))

(defn modint?
  "Returns true if `x` is an instance of [[ModInt]], false otherwise."
  [x]
  (instance? x ModInt))

(defn make [i m]
  (->ModInt (g/modulo i m) m))

(defn- modular-binop [op]
  (fn [a b]
    (if-not (= (:m a) (:m b))
      (u/arithmetic-ex "unequal moduli")
      (make (op (:i a) (:i b)) (:m a)))))

(defn- invert
  "Modular inverse. JVM implementation uses the native BigInt implementation."
  ([m] (invert (:i m) (:m m)))
  ([i modulus]
   #?(:clj
      (try (-> (biginteger i)
               (.modInverse (biginteger modulus))
               (int)
               (->ModInt modulus))
           (catch ArithmeticException _
             (u/arithmetic-ex (str i " is not invertible mod " modulus))))

      :cljs
      (let [[g a _] (e/extended-gcd i modulus)]
        (if (< g 2)
          (make a modulus)
          (u/arithmetic-ex (str i " is not invertible mod " modulus)))))))

(defn- mod-expt
  "Modular exponentiation, more efficient on the JVM."
  [base pow modulus]
  #?(:clj (let [base (if (neg? pow)
                       (:i (invert base modulus))
                       base)]
            (-> (.modPow (biginteger base)
                         (.abs (biginteger pow))
                         (biginteger modulus))
                (int)
                (->ModInt modulus)))

     :cljs (-> (g/expt (u/bigint base)
                       (u/bigint pow))
               (g/modulo modulus)
               (js/Number)
               (->ModInt modulus))))

(defn chinese-remainder
  "[Chinese Remainder Algorithm](https://en.wikipedia.org/wiki/Chinese_remainder_theorem).

  Accepts a sequence of [[ModInt]] instances (where the modulus `:m` of
  all [[ModInt]] instances are relatively prime), and returns a [[ModInt]] `x`
  such that `(:i input) == (mod x (:m input))`.

  For example:

  ```clojure
  (let [a1 (m/make 2 5)
        a2 (m/make 3 13)]
    [(= 42 (chinese-remainder a1 a2))
     (= (:i a1) (mod cr (:m a1)))
     (= (:i a2) (mod cr (:m a2)))])
  ;;=> [true true true]
  ```"
  [& modints]
  (let [prod  (transduce (map :m) g/* modints)
        xform (map (fn [{:keys [i m]}]
		                 (let [c (g/quotient prod m)]
                       (g/* i c (:i (invert c m))))))]
    (-> (transduce xform g/+ modints)
        (g/modulo prod))))

(def ^:private add (modular-binop g/add))
(def ^:private sub (modular-binop g/sub))
(def ^:private mul (modular-binop g/mul))
(def ^:private remainder (modular-binop g/remainder))
(def ^:private modulo (modular-binop g/modulo))

(g/defmethod g/add [::modint ::modint] [a b] (add a b))
(g/defmethod g/mul [::modint ::modint] [a b] (mul a b))
(g/defmethod g/sub [::modint ::modint] [a b] (sub a b))
(g/defmethod g/negate [::modint] [a] (make (g/negate (:i a)) (:m a)))
(g/defmethod g/invert [::modint] [a] (invert a))
(g/defmethod g/magnitude [::modint] [{:keys [i m] :as a}] (g/modulo i m))
(g/defmethod g/abs [::modint] [{:keys [i m] :as a}]
  (if (g/negative? i)
    (make i m)
    a))
(g/defmethod g/quotient [::modint ::modint] [a b] (mul a (invert b)))
(g/defmethod g/remainder [::modint ::modint] [a b] (remainder a b))
(g/defmethod g/modulo [::modint ::modint] [a b] (modulo a b))
(g/defmethod g/exact-divide [::modint ::modint] [a b] (mul a (invert b)))
(g/defmethod g/negative? [::modint] [a] (g/negative? (:i a)))

;; Methods that allow interaction with other integral types. The first block is
;; perhaps slightly more efficient:
(doseq [op [g/add g/mul g/sub]]
  (g/defmethod op [::v/integral ::modint] [a b] (make (op a (:i b)) (:m b)))
  (g/defmethod op [::modint ::v/integral] [a b] (make (op (:i a) b) (:m a))))

;; A more efficient exponent implementation is available on the JVM.
(g/defmethod g/expt [::v/integral ::modint] [a b](mod-expt a (:i b) (:m b)))
(g/defmethod g/expt [::modint ::v/integral] [a b] (mod-expt (:i a) b (:m a)))

;; The second block promotes any integral type to a ModInt before operating.
(doseq [op [g/quotient g/remainder g/exact-divide]]
  (g/defmethod op [::v/integral ::modint] [a b] (op (make a (:m b)) b))
  (g/defmethod op [::modint ::v/integral] [a b] (op a (make b (:m a)))))
