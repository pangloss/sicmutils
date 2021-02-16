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

(ns sicmutils.complex
  "This namespace provides a number of functions and constructors for working
  with [[Complex]] numbers in Clojure and Clojurescript, and
  installs [[Complex]] into the SICMUtils generic arithmetic system.

  For other numeric extensions, see [[sicmutils.ratio]]
  and [[sicmutils.numbers]]."
  (:require [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.value :as v]
            #?(:cljs ["complex.js" :as Complex]))
  #?(:clj
     (:import [org.apache.commons.math3.complex Complex ComplexFormat])))

(def ^{:doc "A [[Complex]] value equal to 0 (south pole on the Riemann Sphere)."}
  ZERO
  #?(:clj Complex/ZERO :cljs (.-ZERO Complex)))

(def ^{:doc "A [[Complex]] value equal to 1."}
  ONE #?(:clj Complex/ONE :cljs (.-ONE Complex)))

(def  ^{:doc "A [[Complex]] value equal to `i`."}
  I
  #?(:clj Complex/I :cljs (.-I Complex)))

(def ^:no-doc complextype Complex)

(derive ::complex ::v/number)

(defn complex
  "Returns a [[Complex]] number with the supplied real part `re` and imaginary
  part `im`. `im` defaults to 0."
  ([re]
   (Complex. (u/double re)))
  ([re im]
   (Complex. (u/double re)
             (u/double im))))

(defn complex?
  "Returns true if `a` is an instance of [[Complex]], false otherwise."
  [a]
  (instance? Complex a))

(g/defmethod g/make-rectangular [::v/real ::v/real] [re im]
  (if (v/exact-zero? im)
    re
    (complex re im)))

(g/defmethod g/make-polar [::v/real ::v/real] [radius angle]
  (cond (v/exact-zero? radius) radius
        (v/exact-zero? angle)  radius
        :else
        #?(:cljs (Complex. #js {:abs (js/Number radius)
                                :arg (js/Number angle)})
           :clj (let [angle (u/double angle)]
                  (Complex. (* radius (Math/cos angle))
                            (* radius (Math/sin angle)))))))

(g/defmethod g/real-part [::complex] [^Complex a] (#?(:clj .getReal :cljs .-re) a))
(g/defmethod g/imag-part [::complex] [^Complex a] (#?(:clj .getImaginary :cljs .-im) a))
(g/defmethod g/magnitude [::complex] [^Complex a] (.abs a))
(g/defmethod g/angle [::complex] [^Complex a] (#?(:clj .getArgument :cljs .arg) a))
(g/defmethod g/conjugate [::complex] [^Complex a] (.conjugate a))

(def ^{:doc "Parser that converts a string representation of a complex number,
  like `1 + 3i`, into a [[Complex]] number object in clj or cljs."}
  parse-complex
  #?(:clj (let [cf (ComplexFormat.)]
            (fn [s]
              (let [v (.parse cf s)]
                `(complex ~(g/real-part v)
                          ~(g/imag-part v)))))

     :cljs (fn [s] `(complex ~s))))

#?(:cljs
   (extend-type Complex
     IEquiv
     (-equiv [this other]
       (.equals this other))

     IPrintWithWriter
     (-pr-writer [x writer opts]
       (write-all writer "#sicm/complex \"" (.toString x) "\""))))

#?(:clj
   ;; Clojure implementation of a printer that will emit items that can
   ;; round-trip via #sicm/complex.
   (let [cf (ComplexFormat.)]
     (defmethod print-method Complex [^Complex v ^java.io.Writer w]
       (.write w (str "#sicm/complex \""
                      (.format cf v)
                      "\"")))))

(extend-type Complex
  v/Numerical
  (numerical? [_] true)

  v/Value
  (zero? [c] #?(:clj (= ZERO c) :cljs (.isZero c)))
  (one? [c] (= ONE c))
  (identity? [c] (= ONE c))
  (zero-like [_] ZERO)
  (one-like [_] ONE)
  (identity-like [_] ONE)
  (freeze [c] (let [re (g/real-part c)
                    im (g/imag-part c)]
                (if (v/zero? im)
                  re
                  (list 'complex re im))))
  (exact? [c] (and (v/exact? (g/real-part c))
                   (v/exact? (g/imag-part c))))
  (kind [_] ::complex))

(g/defmethod g/add [::complex ::complex] [^Complex a ^Complex b] (.add a b))
(g/defmethod g/add [::complex ::v/real] [^Complex a n] (.add a (u/double n)))
(g/defmethod g/add [::v/real ::complex] [n ^Complex a] (.add a (u/double n)))

(g/defmethod g/expt [::complex ::complex] [^Complex a ^Complex b] (.pow a b))
(g/defmethod g/expt [::complex ::v/real] [^Complex a n] (.pow a (u/double n)))
(g/defmethod g/expt [::v/real ::complex] [n ^Complex a] (.pow ^Complex (complex n) a))

(g/defmethod g/abs [::complex] [^Complex a] (.abs a))
(g/defmethod g/exp [::complex] [^Complex a] (.exp a))
(g/defmethod g/log [::complex] [^Complex a] (.log a))
(g/defmethod g/sqrt [::complex] [^Complex a] (.sqrt a))

(g/defmethod g/sin [::complex] [^Complex a] (.sin a))
(g/defmethod g/cos [::complex] [^Complex a] (.cos a))
(g/defmethod g/tan [::complex] [^Complex a] (.tan a))
(g/defmethod g/asin [::complex] [^Complex a] (.asin a))
(g/defmethod g/acos [::complex] [^Complex a] (.acos a))
(g/defmethod g/atan [::complex] [^Complex a] (.atan a))
(g/defmethod g/cosh [::complex] [^Complex a] (.cosh a))
(g/defmethod g/sinh [::complex] [^Complex a] (.sinh a))
(g/defmethod g/tanh [::complex] [^Complex a] (.tanh a))
(g/defmethod g/simplify [::complex] [a] (v/freeze a))

#?(:cljs
   ;; These are all defined explicitly in Complex.js.
   (do
     (g/defmethod g/cot [::complex] [^Complex a] (.cot a))
     (g/defmethod g/sec [::complex] [^Complex a] (.sec a))
     (g/defmethod g/csc [::complex] [^Complex a] (.csc a))
     (g/defmethod g/tanh [::complex] [^Complex a] (.tanh a))
     (g/defmethod g/sech [::complex] [^Complex a] (.sech a))
     (g/defmethod g/csch [::complex] [^Complex a] (.csch a))
     (g/defmethod g/acosh [::complex] [^Complex a] (.acosh a))
     (g/defmethod g/asinh [::complex] [^Complex a] (.asinh a))
     (g/defmethod g/atanh [::complex] [^Complex a] (.atanh a))))

;;The remaining methods have different names in the Clojure vs JS
;;implementations.
#?(:clj
   (do
     (g/defmethod g/sub [::complex ::complex] [^Complex a ^Complex b] (.subtract a b))
     (g/defmethod g/sub [::complex ::v/real] [^Complex a n] (.subtract a (double n)))
     (g/defmethod g/sub [::v/real ::complex] [n ^Complex a] (.add (.negate a) (double n)))

     (g/defmethod g/mul [::complex ::complex] [^Complex a ^Complex b] (.multiply a b))
     (g/defmethod g/mul [::complex ::v/real] [^Complex a n] (.multiply a (double n)))
     (g/defmethod g/mul [::v/real ::complex] [n ^Complex a] (.multiply a (double n)))

     (g/defmethod g/div [::complex ::complex] [^Complex a ^Complex b] (.divide a b))
     (g/defmethod g/div [::complex ::v/real] [^Complex a n] (.divide a (double n)))
     (g/defmethod g/div [::v/real ::complex] [n ^Complex a] (.multiply (.reciprocal a) (double n)))

     (g/defmethod g/negate [::complex] [^Complex a] (.negate a))
     (g/defmethod g/invert [::complex] [^Complex a] (.reciprocal a))
     (g/defmethod g/square [::complex] [^Complex a] (.multiply a a))
     (g/defmethod g/cube [::complex] [^Complex a] (.pow a 3.0)))

   :cljs
   (do
     (g/defmethod g/sub [::complex ::complex] [^Complex a ^Complex b] (.sub a b))
     (g/defmethod g/sub [::complex ::v/real] [^Complex a n] (.sub a (u/double n)))
     (g/defmethod g/sub [::v/real ::complex] [n ^Complex a] (.add (.neg a) (u/double n)))

     (g/defmethod g/mul [::complex ::complex] [^Complex a ^Complex b] (.mul a b))
     (g/defmethod g/mul [::complex ::v/real] [^Complex a n] (.mul a (u/double n)))
     (g/defmethod g/mul [::v/real ::complex] [n ^Complex a] (.mul a (u/double n)))

     (g/defmethod g/div [::complex ::complex] [^Complex a ^Complex b] (.div a b))
     (g/defmethod g/div [::complex ::v/real] [^Complex a n] (.div a (u/double n)))
     (g/defmethod g/div [::v/real ::complex] [n ^Complex a] (.mul ^Complex (.inverse a) (u/double n)))

     (g/defmethod g/negate [::complex] [^Complex a] (.neg a))
     (g/defmethod g/invert [::complex] [^Complex a] (.inverse a))
     (g/defmethod g/square [::complex] [^Complex a] (.mul a a))
     (g/defmethod g/cube [::complex] [^Complex a] (.pow a 3.0))))
