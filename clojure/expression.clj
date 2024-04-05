(load-file "proto.clj")
(load-file "parser.clj")

(def __exprs (field :exprs))

(defn constructor [ctor prototype]
  (fn [& args] (apply ctor prototype args)))

(def evaluate (method :evaluate))
(def diff (method :diff))
(def toString (method :toString))

(def ExprObjPrototype
  {
   :evaluate (fn [distributeFunction]
               (fn [this vars]
                 (apply distributeFunction
                        (map (fn [expr] (evaluate expr vars)) (__exprs this)))))
   :diff (fn [diffFunction]
           (fn [this var]
              (diffFunction
               (__exprs this)
               (map (fn [expr] (diff expr var)) (__exprs this)))))
   :toString (fn [nameObj]
               (fn [this] 
               (if (empty? (__exprs this))
                (str "(" nameObj " )")
                (str "(" nameObj
                  (apply str (map (fn [expr] (str " " (toString expr))) (__exprs this)))
                  ")"))))
   })

(defn BaseExprObj [evFunction nameObj diffFunction]
  (fn [prototype & exprs]
    {:prototype prototype
     :evaluate ((prototype :evaluate) evFunction)
     :toString ((prototype :toString) nameObj)
     :diff ((prototype :diff) diffFunction)
     :exprs exprs}))

(defn createConstructor [evFunction nameObj diffFunction]
  (constructor (BaseExprObj evFunction nameObj diffFunction) ExprObjPrototype))

(declare Add)
(declare Subtract)
(declare Multiply)
(declare Divide)
(declare Negate)
(declare Constant)
(declare Variable)
(declare Sumexp)
(declare LSE)

(defn constDiff [& all] (Constant 0))

(defn baseDiff [f]
  (fn [exprs diffExprs]
    (apply f diffExprs)))

(defn mulDiff [exprs diffExprs]
  (if (== (count exprs) 1)
    (first diffExprs)
    (Add
      (apply Multiply (first diffExprs) (rest exprs))
      (Multiply (first exprs) (mulDiff (rest exprs) (rest diffExprs))))))

(defn divDiff [exprs diffExprs]
  (if (== (count exprs) 1)
    (Negate (Divide (first diffExprs) (Multiply (first exprs) (first exprs))))
    (let [numerator (first exprs)
          numeratorDiff (first diffExprs)
          denominator (apply Multiply (rest exprs))
          denominatorDiff (mulDiff (rest exprs) (rest diffExprs))]
        (Divide (Subtract
            (Multiply numeratorDiff denominator)
            (Multiply numerator denominatorDiff))
          (Multiply denominator denominator)))))

(defn divisionByZeroSafeDivide [& args]
  (if (= (count args) 1)
    (apply divisionByZeroSafeDivide (conj args 1))
    (reduce #(/ %1 (double %2)) args)))

(defn sumexpCalc [& args]
  (apply + (map #(Math/pow Math/E %) args)))

(defn sumexpDiff [exprs diffExprs]
  (apply Add (map #(Multiply %2 (Sumexp %1)) exprs diffExprs)))

(defn lseDiff [exprs diffExprs]
  (Divide (sumexpDiff exprs diffExprs) (apply Sumexp exprs)))

(def Add (createConstructor + "+" (baseDiff #'Add)))
(def Subtract (createConstructor - "-" (baseDiff #'Subtract)))
(def Multiply (createConstructor * "*" mulDiff))
(def Divide (createConstructor divisionByZeroSafeDivide "/" divDiff))
(def Negate (createConstructor - "negate" (baseDiff #'Negate)))
(def Sumexp (createConstructor sumexpCalc "sumexp" sumexpDiff))
(def LSE (createConstructor #(Math/log (apply sumexpCalc %&)) "lse" lseDiff))
(defn Constant [value]
  {:evaluate (fn [this vars] value)
   :diff constDiff
   :toString (constantly (str value))})
(def ConstZero (Constant 0))
(def ConstOne (Constant 1))
(defn Variable [name]
  {:evaluate (fn [this vars] (vars name))
   :diff (fn [this var] (if (= var name) ConstOne ConstZero))
   :toString (constantly name)})

(defn baseOp [distributeFunction]
    (fn [& args]
      (fn [var]
        (apply distributeFunction (mapv (fn [func] (func var)) args)))))

(def constant constantly)

(defn variable [name]
  (fn [var] 
    (get var name)))

(defn negate [arg]
  (fn [vars]
    (- (arg vars))))

(defn meansqCount [& args]
  (/ (apply + (mapv #(* % %) args)) (count args)))

(defn rmsCount [& args]
  (Math/sqrt (apply meansqCount args)))

(def divide (baseOp divisionByZeroSafeDivide))
(def add (baseOp +))
(def subtract (baseOp -))
(def multiply (baseOp *))
(def meansq (baseOp meansqCount))
(def rms (baseOp rmsCount))

(def objList {
  'constant Constant
  'variable Variable
  '+ Add
  '- Subtract
  '* Multiply
  '/ Divide
  'negate Negate
  'lse LSE
  'sumexp Sumexp
})

(def funcList {
  'constant constant
  'variable variable
  '+ add
  '- subtract
  '* multiply
  '/ divide
  'negate negate
  'meansq meansq
  'rms rms
  })

(defn parseExpression [parseList expr]
  (cond 
    (list? expr) (apply (get parseList (first expr)) (mapv (partial parseExpression parseList) (rest expr)))
    (number? expr) ((parseList 'constant) expr)
    (symbol? expr) ((parseList 'variable) (name expr))
    :else (throw (Exception. "Invalid symbol"))))

(defn parseWitnList [parseList] (comp (partial parseExpression parseList) read-string))

(def parseFunction (parseWitnList funcList))
(def parseObject (parseWitnList objList))

(defparser parseObjectInfix
    *all-chars (mapv char (range 0 128))
    (*chars [p] (+char (apply str (filter p *all-chars))))
    *letter (*chars #(Character/isLetter %))
    *digit (*chars #(Character/isDigit %))
    *space (*chars #(Character/isWhitespace %))
    *ws (+ignore (+star *space))
    *number (+map read-string (+str (+plus *digit)))
    *identifier (+str (+seqf cons *letter (+star (+or *letter *digit))))
    (*seq [begin p end]
          (+seqn 1 begin (+opt (+seqf cons *ws p (+star (+seqn 1 *ws \, *ws p)))) *ws end))
    *array (+map vec (*seq \[ (delay *value) \]))
    *member (+seq *identifier *ws (+ignore \:) *ws (delay *value))
    *object (+map (partial reduce #(apply assoc %1 %2) {}) (*seq \{ *member \}))
    *value (+or *null *number *string *object *array)
    *json (+seqn 0 *ws *value *ws)

    *op-char ["+", "-", "/", "*"]
    (*op [begin p end]
          (+seqn 1 begin (+opt (+seqf cons *ws p (+star (+seqn 1 *ws \, *ws p)))) *ws end))
    *parseAddSub (+seqf cons   *ws *op-char )
    *parseObjectInfix (+seqn 0 *ws *parseAddSub *ws))