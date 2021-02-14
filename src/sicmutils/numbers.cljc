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

(ns sicmutils.numbers
  "This namespace extends of all appropriate SICMUtils generic operations
  from [[sicmutils.generic]] and [[sicmutils.value]] to the Clojure(script)
  numeric tower.

  For other numeric extensions, see [[sicmutils.ratio]]
  and [[sicmutils.complex]]."
  (:refer-clojure :rename {zero? core-zero?
                           / core-div
                           + core-plus
                           - core-minus
                           * core-times}
                  #?@(:cljs [:exclude [zero? / + - *]]))
  (:require [sicmutils.complex :refer [complex]]
            [sicmutils.ratio :as r]

            ;; Required to enable the generic gcd implementation.
            [sicmutils.euclid]
            [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.value :as v]
            #?(:cljs [goog.math.Long :as Long])
            #?(:cljs [goog.math.Integer :as Integer]))
  #?(:cljs
     (:import (goog.math Long Integer))
     :clj
     (:import [clojure.lang BigInt Ratio]
              [java.math BigInteger])))

;; "Backstop" implementations that apply to anything that descends from
;; ::v/real.
(g/defmethod g/add [::v/real ::v/real] [a b] (#?(:clj +' :cljs core-plus) a b))
(g/defmethod g/mul [::v/real ::v/real] [a b] (#?(:clj *' :cljs core-times) a b))
(g/defmethod g/sub [::v/real ::v/real] [a b] (#?(:clj -' :cljs core-minus) a b))
(g/defmethod g/negate [::v/real] [a] (core-minus a))
(g/defmethod g/negative? [::v/real] [a] (neg? a))
(g/defmethod g/expt [::v/real ::v/real] [a b] (u/compute-expt a b))
(g/defmethod g/abs [::v/real] [a] (u/compute-abs a))
(g/defmethod g/magnitude [::v/real] [a] (u/compute-abs a))
(g/defmethod g/div [::v/real ::v/real] [a b] (core-div a b))
(g/defmethod g/invert [::v/real] [a] (core-div a))

;; ## Complex Operations
(g/defmethod g/real-part [::v/real] [a] a)
(g/defmethod g/imag-part [::v/real] [a] 0)

(g/defmethod g/angle [::v/real] [a]
  (if (neg? a)
    Math/PI
    (v/zero-like a)))

(g/defmethod g/conjugate [::v/real] [a] a)

;; ## Trig Operations

(g/defmethod g/sin [::v/real] [a] (Math/sin a))
(g/defmethod g/cos [::v/real] [a] (Math/cos a))
(g/defmethod g/tan [::v/real] [a] (Math/tan a))

(g/defmethod g/cosh [::v/real] [a] (Math/cosh a))
(g/defmethod g/sinh [::v/real] [a] (Math/sinh a))
(g/defmethod g/tanh [::v/real] [a] (Math/tanh a))

(g/defmethod g/atan [::v/real] [a] (Math/atan a))
(g/defmethod g/atan [::v/real ::v/real] [a b] (Math/atan2 a b))

;; Operations which allow promotion to complex numbers when their
;; arguments would otherwise result in a NaN if computed on the real
;; line

(g/defmethod g/asin [::v/real] [a]
  (if (> (g/abs a) 1)
    (g/asin (complex a))
    (Math/asin a)))

(g/defmethod g/acos [::v/real] [a]
  (if (> (g/abs a) 1)
    (g/acos (complex a))
    (Math/acos a)))

#?(:cljs
   (do
     ;; JS makes these available natively.
     (g/defmethod g/acosh [::v/real] [a]
       (if (>= a 1)
         (Math/acosh a)
         (g/acosh (complex a))))

     (g/defmethod g/asinh [::v/real] [a]
       (if (>= a 1)
         (Math/asinh a)
         (g/asinh (complex a))))

     (g/defmethod g/atanh [::v/real] [a]
       (if (>= (g/abs a) 1)
         (g/atanh (complex a))
         (Math/atanh a)))))

(g/defmethod g/sqrt [::v/real] [a]
  (if (neg? a)
    (g/sqrt (complex a))
    (u/compute-sqrt a)))

(g/defmethod g/log [::v/real] [a]
  (if (neg? a)
    (g/log (complex a))
    (Math/log a)))

;; Specialized methods provided by the host platforms.

#?(:clj  (g/defmethod g/log10 [Double] [x]
           (if (neg? x)
             (g/log10 (complex x))
             (Math/log10 x)))

   :cljs (g/defmethod g/log10 [js/Number] [x]
           (if (neg? x)
             (g/log10 (complex x))
             (Math/log10 x))))

#?(:cljs (g/defmethod g/log2 [js/Number] [x]
           (if (neg? x)
             (g/log2 (complex x))
             (Math/log2 x))))

(g/defmethod g/exp [::v/real] [a]
  (Math/exp a))

(defn ^:private exact-divide
  "Checked implementation of g/exact-divide general enough to use for any type
  that defines g/remainder and g/quotient."
  [a b]
  {:pre [(v/zero? (g/remainder a b))]}
  (g/quotient a b))

(g/defmethod g/exact-divide [::v/integral ::v/integral] [b a] (exact-divide b a))

;; All JVM and JS types that respond to ::native-integral behave correctly with
;; Clojure's native `quot`, `rem`, `mod`.
(g/defmethod g/quotient [::v/native-integral ::v/native-integral] [a b] (quot a b))
(g/defmethod g/remainder [::v/native-integral ::v/native-integral] [a b] (rem a b))
(g/defmethod g/modulo [::v/native-integral ::v/native-integral] [a b] (mod a b))

;; This section defines methods that act differently between Clojurescript and
;; Clojure. The clojure methods are all slightly more refined based on Java's
;; type system.
#?(:clj
   ;; Efficient, native GCD on the JVM.
   (g/defmethod g/gcd [BigInteger BigInteger] [a b] (.gcd a b)))

#?(:cljs
   (do (g/defmethod g/expt [::v/native-integral ::v/native-integral] [a b]
         (if (neg? b)
           (g/invert (u/compute-expt a (core-minus b)))
           (u/compute-expt a b)))

       (g/defmethod g/div [::v/integral ::v/integral] [a b]
         (let [rem (g/remainder a b)]
           (if (v/zero? rem)
             (g/quotient a b)
             (r/rationalize a b))))

       (g/defmethod g/invert [::v/integral] [a]
         (if (v/one? a)
           a
           (r/rationalize 1 a)))))

;; Clojurescript and Javascript have a number of numeric types available that
;; don't respond true to number? These each require their own block of method
;; implementations.
#?(:cljs
   (do
     ;; native BigInt type in JS.
     (g/defmethod g/add [js/BigInt js/BigInt] [a b] (core-plus a b))
     (g/defmethod g/mul [js/BigInt js/BigInt] [a b] (core-times a b))
     (g/defmethod g/sub [js/BigInt js/BigInt] [a b] (core-minus a b))
     (g/defmethod g/negate [js/BigInt] [a] (core-minus a))

     (g/defmethod g/expt [js/BigInt js/BigInt] [a b]
       (if (g/negative? b)
         (g/invert (js* "~{} ** ~{}" a (core-minus b)))
         (js* "~{} ** ~{}" a b)))

     ;; Not ideal; TODO find a better way to calculate this without the
     ;; downcast.
     (g/defmethod g/sqrt [js/BigInt] [a]
       (Math/sqrt (js/Number a)))

     (g/defmethod g/abs [js/BigInt] [a] (if (neg? a) (core-minus a) a))
     (g/defmethod g/quotient [js/BigInt js/BigInt] [a b] (core-div a b))
     (g/defmethod g/remainder [js/BigInt js/BigInt] [a b] (js* "~{} % ~{}" a b))
     (g/defmethod g/magnitude [js/BigInt] [a b]
       (if (neg? a) (core-minus a) a))


     (doseq [op [g/add g/mul g/sub g/expt g/remainder g/quotient]]
       ;; Compatibility between js/BigInt and the other integral types.
       (g/defmethod op [js/BigInt ::v/integral] [a b]
         (op a (u/bigint b)))

       (g/defmethod op [::v/integral js/BigInt] [a b]
         (op (u/bigint a) b))

       ;; For NON integrals, we currently have no choice but to downcast the
       ;; BigInt to a floating point number. TODO if we introduce BigDecimal
       ;; support this could get cleaner.
       (g/defmethod op [js/BigInt ::v/floating-point] [a b]
         (op (js/Number a) b))

       (g/defmethod op [::v/floating-point js/BigInt] [a b]
         (op a (js/Number b)))

       ;; BigInt can't handle these operations natively, so we override with a
       ;; downcast to number for now.
       (doseq [op [g/cos g/sin g/tan
                   g/asin g/acos g/atan
                   g/cosh g/sinh g/tanh
                   g/asinh g/acosh g/acosh
                   g/cot g/sec g/csc g/sech g/csch]]
         (g/defmethod op [js/BigInt] [a]
           (op (js/Number a))))

       (g/defmethod g/atan [js/BigInt ::v/real] [l r] (g/atan (js/Number l) r))
       (g/defmethod g/atan [::v/real js/BigInt] [l r] (g/atan l (js/Number r)))
       (g/defmethod g/atan [js/BigInt js/BigInt] [l r] (g/atan (js/Number l) (js/Number r))))

     ;; Google Closure library's 64-bit Long:
     (g/defmethod g/add [Long Long] [a b] (.add a b))
     (g/defmethod g/mul [Long Long] [a b] (.multiply a b))
     (g/defmethod g/sub [Long Long] [^Long a ^Long b] (.subtract a b))
     (g/defmethod g/negate [Long] [^Long a] (.negate a))
     (g/defmethod g/abs [Long] [^Long a] (if (.isNegative a) (.negate a) a))
     (g/defmethod g/remainder [Long Long] [^Long a ^Long b] (.modulo a b))
     (g/defmethod g/magnitude [Long] [^Long a] (if (.isNegative a) (.negate a) a))

     ;; Implementation of exponent taken from Clojure's numeric tower's
     ;; expt-int:
     ;; https://github.com/clojure/math.numeric-tower/blob/master/src/main/clojure/clojure/math/numeric_tower.clj#L72
     (letfn [(long-expt [base pow]
               (loop [^Long n pow
                      ^Long y (.getOne Long)
                      ^Long z base]
                 (let [t (not (.isOdd n))
                       n ^Long (.shiftRight n 1)]
                   (cond
                     t (recur n y (.multiply z z))
                     (.isZero n) (.multiply z y)
                     :else (recur n (.multiply z y) (.multiply z z))))))]
       (g/defmethod g/expt [Long Long] [a ^Long b]
         (if (.isNegative b)
           (g/invert (long-expt a (.negate b)))
           (long-expt a b))))

     ;; Compatibility between basic number type and the google numeric types.
     ;; Any operation between a number and a Long or Integer will promote the
     ;; number.
     (doseq [op [g/add g/mul g/sub g/gcd g/lcm g/expt g/remainder g/quotient]]
       (g/defmethod op [Long ::v/native-integral] [a b]
         (op a (.fromNumber Long b)))

       (g/defmethod op [::v/native-integral Long] [a b]
         (op (.fromNumber Long a) b))

       ;; If this type encounters a floating point type it should lose
       ;; precision.
       (g/defmethod op [Long ::v/floating-point] [a b]
         (op (js/Number a) b))

       (g/defmethod op [::v/floating-point Long] [a b]
         (op a (js/Number b))))

     ;; Google Closure's arbitrary-precision Integer:
     (g/defmethod g/add [Integer Integer] [a b] (.add a b))
     (g/defmethod g/mul [Integer Integer] [a b] (.multiply a b))
     (g/defmethod g/sub [Integer Integer] [^Integer a ^Integer b] (.subtract a b))
     (g/defmethod g/negate [Integer] [^Integer a] (.negate a))
     (g/defmethod g/abs [Integer] [^Integer a] (if (.isNegative a) (.negate a) a))
     (g/defmethod g/remainder [Integer Integer] [^Integer a ^Integer b] (.modulo a b))
     (g/defmethod g/magnitude [Integer] [^Integer a] (if (.isNegative a) (.negate a) a))

     (letfn [(int-expt [base pow]
               (loop [^Integer n pow
                      ^Integer y (.-ONE Integer)
                      ^Integer z base]
                 (let [t (not (.isOdd n))
                       ^Integer n (.shiftRight n 1)]
                   (cond
                     t (recur n y (.multiply z z))
                     (.isZero n) (.multiply z y)
                     :else (recur n (.multiply z y) (.multiply z z))))))]
       (g/defmethod g/expt [Integer Integer] [a ^Integer b]
         (if (.isNegative b)
           (g/invert (int-expt a (.negate b)))
           (int-expt a b))))

     ;; Compatibility between basic number type and the google numeric types.
     ;; Any operation between a number and a Long or Integer will promote the
     ;; number.
     (doseq [op [g/add g/mul g/sub g/gcd g/lcm g/expt g/remainder g/quotient]]
       (g/defmethod op [Integer ::v/native-integral] [a b]
         (op a (.fromNumber Integer b)))

       (g/defmethod op [::v/native-integral Integer] [a b]
         (op (.fromNumber Integer a) b))

       ;; If this type encounters a floating point type it should lose
       ;; precision.
       (g/defmethod op [Integer ::v/floating-point] [a b]
         (op (js/Number a) b))

       (g/defmethod op [::v/floating-point Integer] [a b]
         (op a (js/Number b)))

       ;; When they encounter each other in binary operations, Long is coerced
       ;; to Integer.
       (g/defmethod op [Integer Long] [a b]
         (op a (.fromNumber Integer b)))

       (g/defmethod op [Long Integer] [a b]
         (op (.fromNumber Integer a) b)))

     ;; These names are slightly different between the two types.
     (g/defmethod g/quotient [Long Long] [^Long a ^Long b] (.div a b))
     (g/defmethod g/quotient [Integer Integer] [^Integer a ^Integer b] (.divide a b))))
