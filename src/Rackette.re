open CS17SetupRackette;
open Read;
open Read.Reader;
open Types;

/* TODO: fill this with your initial top level environment,
 * consisting of built-in procedures like + or empty? */
let initialTle: environment = [
  [
    (
      Name("+"),
      BuiltinV({
        printedRep: "<builtin-proc-+>",
        bProc: {
          let plusProc: list(value) => value =
            fun
            | [NumV(n1), NumV(n2)] => NumV(n1 + n2)
            | [_, _] => failwith("Incorrect input type: expected numbers")
            | _ =>
              failwith("incorrect number of inputs: expected 2 arguments");
          plusProc;
        },
      }),
    ),
    (
      Name("-"),
      BuiltinV({
        printedRep: "<builtin-proc-->",
        bProc: {
          let minusProc: list(value) => value =
            fun
            | [NumV(n1), NumV(n2)] => NumV(n1 - n2)
            | [_, _] => failwith("Incorrect input type: expected numbers")
            | _ =>
              failwith("incorrect number of inputs: expected 2 arguments");
          minusProc;
        },
      }),
    ),
    (
      Name("*"),
      BuiltinV({
        printedRep: "<builtin-proc-*>",
        bProc: {
          let timesProc: list(value) => value =
            fun
            | [NumV(n1), NumV(n2)] => NumV(n1 * n2)
            | [_, _] => failwith("Incorrect input type: expected numbers")
            | _ =>
              failwith("incorrect number of inputs: expected 2 arguments");
          timesProc;
        },
      }),
    ),
    (
      Name("/"),
      BuiltinV({
        printedRep: "<builtin-proc-/>",
        bProc: {
          let quoProc: list(value) => value =
            fun
            | [NumV(n1), NumV(n2)] => NumV(n1 / n2)
            | [_, _] => failwith("Incorrect input type: expected numbers")
            | _ =>
              failwith("incorrect number of inputs: expected 2 arguments");
          quoProc;
        },
      }),
    ),
    (
      Name("remainder"),
      BuiltinV({
        printedRep: "<builtin-proc-remainder>",
        bProc: {
          let remProc: list(value) => value =
            fun
            | [NumV(n1), NumV(n2)] => NumV(n1 mod n2)
            | [_, _] => failwith("Incorrect input type: expected numbers")
            | _ =>
              failwith("incorrect number of inputs: expected 2 arguments");
          remProc;
        },
      }),
    ),
    (
      Name("="),
      BuiltinV({
        printedRep: "<builtin-proc-=>",
        bProc: {
          let numEqualProc: list(value) => value =
            fun
            | [NumV(n1), NumV(n2)] => BoolV(n1 == n2)
            | [_, _] => failwith("Incorrect input type: expected numbers")
            | _ =>
              failwith("incorrect number of inputs: expected 2 arguments");
          numEqualProc;
        },
      }),
    ),
    (
      Name("<"),
      BuiltinV({
        printedRep: "<builtin-proc-<>",
        bProc: {
          let lessThanProc: list(value) => value =
            fun
            | [NumV(n1), NumV(n2)] => BoolV(n1 < n2)
            | [_, _] => failwith("Incorrect input type: expected numbers")
            | _ =>
              failwith("incorrect number of inputs: expected 2 arguments");
          lessThanProc;
        },
      }),
    ),
    (
      Name("<="),
      BuiltinV({
        printedRep: "<builtin-proc-<=>",
        bProc: {
          let lessThanEqualProc: list(value) => value =
            fun
            | [NumV(n1), NumV(n2)] => BoolV(n1 <= n2)
            | [_, _] => failwith("Incorrect input type: expected numbers")
            | _ =>
              failwith("incorrect number of inputs: expected 2 arguments");
          lessThanEqualProc;
        },
      }),
    ),
    (
      Name(">"),
      BuiltinV({
        printedRep: "<builtin-proc->>",
        bProc: {
          let greaterThanProc: list(value) => value =
            fun
            | [NumV(n1), NumV(n2)] => BoolV(n1 > n2)
            | [_, _] => failwith("Incorrect input type: expected numbers")
            | _ =>
              failwith("incorrect number of inputs: expected 2 arguments");
          greaterThanProc;
        },
      }),
    ),
    (
      Name(">="),
      BuiltinV({
        printedRep: "<builtin-proc->=>",
        bProc: {
          let greaterThanEqualProc: list(value) => value =
            fun
            | [NumV(n1), NumV(n2)] => BoolV(n1 >= n2)
            | [_, _] => failwith("Incorrect input type: expected numbers")
            | _ =>
              failwith("incorrect number of inputs: expected 2 arguments");
          greaterThanEqualProc;
        },
      }),
    ),
    (
      Name("equal?"),
      BuiltinV({
        printedRep: "<builtin-proc-equal?>",
        bProc: {
          let anyEqualProc: list(value) => value =
            fun
            | [NumV(n1), NumV(n2)] => BoolV(n1 == n2)
            | [BoolV(b1), BoolV(b2)] => BoolV(b1 == b2)
            | [ListV(l1), ListV(l2)] => BoolV(l1 == l2)
            | [ClosureV(c1), ClosureV(c2)] => BoolV(c1 == c2)
            | [BuiltinV(bi1), BuiltinV(bi2)] => BoolV(bi1 == bi2)
            | [_, _] => BoolV(false)
            | _ =>
              failwith("incorrect number of inputs: expected 2 arguments");
          anyEqualProc;
        },
      }),
    ),
    (
      Name("number?"),
      BuiltinV({
        printedRep: "<builtin-proc-number?>",
        bProc: {
          let isNumberProc: list(value) => value =
            fun
            | [NumV(_)] => BoolV(true)
            | [_] => BoolV(false)
            | _ =>
              failwith("incorrect number of inputs: expected 1 argument");
          isNumberProc;
        },
      }),
    ),
    (
      Name("zero?"),
      BuiltinV({
        printedRep: "<builtin-proc-zero?>",
        bProc: {
          let isZeroProc: list(value) => value =
            fun
            | [NumV(n)] => BoolV(n == 0)
            | [_] => failwith("incorrect input type; expected a number")
            | _ => failwith("incorrect number of inputs");
          isZeroProc;
        },
      }),
    ),
    (
      Name("cons"),
      BuiltinV({
        printedRep: "<builtin-proc-cons>",
        bProc: {
          let consProc: list(value) => value =
            fun
            | [elem, ListV(l)] => ListV([elem, ...l])
            | _ =>
              failwith("incorrect number of inputs or incorrect input type");
          consProc;
        },
      }),
    ),
    (
      Name("first"),
      BuiltinV({
        printedRep: "<builtin-proc-first>",
        bProc: {
          let firstProc: list(value) => value =
            fun
            | [ListV(l)] =>
              switch (l) {
              | [] =>
                failwith("this procedure expects a non-empty list input")
              | [hd, ..._] => hd
              }
            | [_] => failwith("incorrect input type: expected a list")
            | _ =>
              failwith("incorrect number of inputs: expected 1 argument");
          firstProc;
        },
      }),
    ),
    (
      Name("rest"),
      BuiltinV({
        printedRep: "<builtin-proc-rest>",
        bProc: {
          let restProc: list(value) => value =
            fun
            | [ListV(l)] =>
              switch (l) {
              | [] =>
                failwith("this procedure expects a non-empty list input")
              | [_, ...tl] => ListV(tl)
              }
            | [_] => failwith("incorrect input type: expected a list")
            | _ =>
              failwith("incorrect number of inputs: expected 1 argument");
          restProc;
        },
      }),
    ),
    (
      Name("empty?"),
      BuiltinV({
        printedRep: "<builtin-proc-empty?>",
        bProc: {
          let isEmptyProc: list(value) => value =
            fun
            | [ListV([])] => BoolV(true)
            | [ListV([_, ..._])]
            | [_] => BoolV(false)
            | _ =>
              failwith("incorrect number of inputs: expected 1 argument");
          isEmptyProc;
        },
      }),
    ),
    (
      Name("cons?"),
      BuiltinV({
        printedRep: "<builtin-proc-cons?>",
        bProc: {
          let isConsProc: list(value) => value =
            fun
            | [ListV([_, ..._])] => BoolV(true)
            | [ListV([])]
            | [_] => BoolV(false)
            | _ =>
              failwith("incorrect number of inputs: expected 1 argument");
          isConsProc;
        },
      }),
    ),
    (
      Name("not"),
      BuiltinV({
        printedRep: "<builtin-proc-not>",
        bProc: {
          let isNotProc: list(value) => value =
            fun
            | [BoolV(b)] => b ? BoolV(false) : BoolV(true)
            | [_] => failwith("incorrect input type: expected a boolean")
            | _ =>
              failwith("incorrect number of inputs: expected 2 arguments");
          isNotProc;
        },
      }),
    ),
  ],
];

/* containsDuplicates is a helper procedure to check if a list contains any
 * duplicate items
 *
 * Input: a list of items of type arbitrary element 'a
 * Output: a boolean that is true if the list contains any duplicates and
 *         false otherwise
 *
 * RECURSION DIAGRAMS
 *
 * Original Input: [1, 2, 3, 4]
 *    Recursive Input: [2, 3, 4]
 *    Recursive Output: false
 *      Ideation: Since RO is false, and 1 is not present in the list,
 *                return false
 * Original Output: false
 *
 * Original Input: [1, 2, 2, 4]
 *    Recursive Input: [2, 2, 4]
 *    Recursive Output: true
 *      Ideation: Since RO is true, duplicate is found in the list, so return
 *                true
 * Original Output: true
 *
 * Original Input: [1, 1, 3, 4]
 *    Recursive Input: [1, 3, 4]
 *    Recursive Output: false
 *      Ideation: Since 1 is present in the list and is the first element
 *                of Original Input, duplicate is found so return true
 * Original Output: true
 *
 * */

let rec containsDuplicates: list('a) => bool =
  input =>
    switch (input) {
    | [] => false
    | [firstItem, ...restItems] =>
      let rec containedIn = lst =>
        switch (lst) {
        | [] => false
        | [hd, ...tl] => hd == firstItem || containedIn(tl)
        };
      containedIn(restItems) || containsDuplicates(restItems);
    };

/* Test cases for containsDuplicates */

checkExpect(
  containsDuplicates([]),
  false,
  "no duplicates found in empty list",
);
checkExpect(containsDuplicates([1, 2, 3]), false, "no duplicates found");
checkExpect(containsDuplicates([1, 2, 3, 2]), true, "duplicates found");
checkExpect(containsDuplicates(["a", "b"]), false, "no duplicates found");
checkExpect(containsDuplicates(["a", "a"]), true, "duplicates found");
checkExpect(
  containsDuplicates(["a", "a", "b", "b"]),
  true,
  "duplicates found",
);

/* First we will define some general helper procedures */

/* lookup is a helper procedure that looks up a name in a given environment, in
 * order to check if it already exits
 *
 * Input: an environment, env, which is a list of bindingLists, where each
 *        bindingList is a list of bindings
 * Output: an option, Some(y), if found and None otherwise
 *
 * RECURSION DIAGRAMS:
 *
 * Original Input: ([[x, 2]], y)
 *    Recursive Input: ([], y)
 *    Recursive Output: None
 *      Ideation: Since Recursive Ouput is None (ie no matching binding for 'y')
 *                was found in the rest of the environment and y is not present
 *                in the first binding list, Original Output is None
 * Original Output: None
 *
 * Original Input: ([[y, 2]],  y)
 *    Recursive Input: ([], y)
 *    Recursive Output: None
 *      Ideation: Recursive Ouput is None (ie no matching binding for 'y')
 *                was found in the rest of the environment, but y is found in
 *                the first binding list, so we return value found, which is
 *                Some(2)
 * Original Output: Some(2)
 *
 * */

let rec lookup: (environment, name) => option(value) =
  (env, n) =>
    switch (env) {
    | [] => None
    | [firstBindingList, ...restBindingLists] =>
      let rec lookupWithin: (bindingList, name) => option(value) = (
        (bl, lookingFor) =>
          switch (bl) {
          | [] => None
          | [firstBinding, ...restBindings] =>
            switch (firstBinding) {
            | (x, y) =>
              if (x == lookingFor) {
                Some(y);
              } else {
                lookupWithin(restBindings, lookingFor);
              }
            }
          }
      );
      switch (lookupWithin(firstBindingList, n)) {
      | None => lookup(restBindingLists, n)
      | Some(y) => Some(y)
      };
    };

/* Test cases for lookup */

let test: environment = [
  [(Name("x"), NumV(1))],
  [(Name("y"), NumV(2))],
];
checkExpect(lookup(initialTle, Name(")")), None, "return None");
checkExpect(
  lookup(test, Name("x")),
  Some(NumV(1)),
  "returns value of x (exists in environment)",
);

/* Design Recipe
   Data Definitions:
   concreteProgramPiece:
     a concreteProgramPiece is a part of Rackette code which is represented in
     3 possible ways: NumberC(int), SymbolC(string), and
     ListC(list(concreteProgramPiece)). NumberC represents an integer, SymbolC
     usually represents any word usage in Rackette, and ListC represents a
     larger list of NumberCs and SymbolCs.

   expression:
    a series of possible expressions that a Rackette program can take.


   Example Data:
   concreteProgramPiece: NumberC(5), SymbolC("define"),
   ListC(SymbolC("define"), SymbolC("x"), NumberC(5))
   expression: NumE(5), AndE(BoolE(false), BoolE(true)), EmptyE

   Input:
   input: a concreteProgramPiece which represents an expression
   Output: the concreteProgramPiece outputted in the form of an expression

   Recursion Diagram #1:
   Original Input: NumberC(5)
      Recursive Input: NA
      Recursive Output: NA
        Ideation: NA
   Original Output: NumE(5)

   Recursion Diagram #2:
   Original Input: ListC([SymbolC("if"), SymbolC("true"), NumberC(5), NumberC(4)])
        Ideation1: parse the boolean expression (second element) of OI
      Recursive Input1: SymbolC("true")
      Recursive Output1: true
        Ideation2: Since RO1 resulted in true, parse the true expression
      Recursive Input2: NumberC(5)
      RecursiveOutput2: NumE(5)
        Ideation: Copy RO2 to OO
   Original Output: NumE(5)
   */
let rec parseExpression: concreteProgramPiece => expression =
  input =>
    switch (input) {
    | NumberC(x) => NumE(x)
    | SymbolC("true") => BoolE(true)
    | SymbolC("false") => BoolE(false)
    | SymbolC("empty") => EmptyE
    | SymbolC(x) => NameE(Name(x))
    | ListC([]) => failwith("empty parentheses are not allowed as an input")
    | ListC([SymbolC("if"), x, y, z]) =>
      IfE({
        boolExpr: parseExpression(x),
        trueExpr: parseExpression(y),
        falseExpr: parseExpression(z),
      })
    | ListC([SymbolC("and"), x, y]) =>
      AndE(parseExpression(x), parseExpression(y))
    | ListC([SymbolC("or"), x, y]) =>
      OrE(parseExpression(x), parseExpression(y))
    | ListC([SymbolC("lambda"), ListC(parameters), body]) =>
      LambdaE({
        nameList:
          List.map(
            p =>
              switch (p) {
              | SymbolC(s) => Name(s)
              | _ => failwith("Incorrect Syntax")
              },
            parameters,
          ),
        lambdaBody: parseExpression(body),
      })
    | ListC([SymbolC("cond"), ...conditions]) =>
      CondE(
        List.map(
          c =>
            switch (c) {
            | ListC([condExpr, resultExpr]) => {
                conditionExpr: parseExpression(condExpr),
                resultExpr: parseExpression(resultExpr),
              }
            | _ => failwith("Cond clause expects: [test, expr]")
            },
          conditions,
        ),
      )
    | ListC([SymbolC("let"), ListC(pairs), body]) =>
      LetE({
        letPairs:
          List.map(
            p =>
              switch (p) {
              | ListC([SymbolC(x), y]) => {
                  pairName: Name(x),
                  pairExpr: parseExpression(y),
                }
              | _ => failwith("Syntax Error")
              },
            pairs,
          ),
        letBody: parseExpression(body),
      })
    | ListC([SymbolC("if"), ..._]) =>
      failwith("if requires 3 subexpressions")
    | ListC([SymbolC("lambda"), ..._]) => failwith("invalid lambda syntax")
    | ListC([SymbolC("let"), ..._]) => failwith("invalid let syntax")
    | ListC([SymbolC("and"), ..._]) =>
      failwith("and requires 2 subexpressions")
    | ListC([SymbolC("or"), ..._]) =>
      failwith("or requires 2 subexpressions")
    | ListC(l) => ApplicationE(List.map(parseExpression, l))
    };

/* Test Cases for parseExpression */

checkError(
  () => parseExpression(read("(lambda (x 3) x)")),
  "Incorrect Syntax",
);

checkError(
  () => parseExpression(read("(cond ((> 1 0)))")),
  "Cond clause expects: [test, expr]",
);

checkError(() => parseExpression(read("(let ((x)) x)")), "Syntax Error");

checkExpectExpression(
  parseExpression(SymbolC("empty")),
  EmptyE,
  "parse empty expression",
);

checkExpectExpression(
  parseExpression(read("empty")),
  EmptyE,
  "read and parse empty expression",
);

checkExpectExpression(
  parseExpression(read("5")),
  NumE(5),
  "Parses number",
);
checkExpectExpression(
  parseExpression(read("true")),
  BoolE(true),
  "Parses true",
);
checkExpectExpression(
  parseExpression(read("false")),
  BoolE(false),
  "Parses false",
);
checkExpectExpression(
  parseExpression(read("empty")),
  EmptyE,
  "Parses empty",
);
checkExpectExpression(
  parseExpression(read("x")),
  NameE(Name("x")),
  "Parses variable name",
);

checkExpectExpression(
  parseExpression(read("(if true 1 2)")),
  IfE({boolExpr: BoolE(true), trueExpr: NumE(1), falseExpr: NumE(2)}),
  "Parses if-expression correctly",
);

checkExpectExpression(
  parseExpression(read("(and true false)")),
  AndE(BoolE(true), BoolE(false)),
  "Parses and-expression correctly",
);

checkExpectExpression(
  parseExpression(read("(or false true)")),
  OrE(BoolE(false), BoolE(true)),
  "Parses or-expression correctly",
);

checkExpectExpression(
  parseExpression(read("(lambda (x y) (+ x y))")),
  LambdaE({
    nameList: [Name("x"), Name("y")],
    lambdaBody:
      ApplicationE([
        NameE(Name("+")),
        NameE(Name("x")),
        NameE(Name("y")),
      ]),
  }),
  "Parses lambda-expression correctly",
);

checkExpectExpression(
  parseExpression(read("(lambda (x) (lambda (y) (+ x y)))")),
  LambdaE({
    nameList: [Name("x")],
    lambdaBody:
      LambdaE({
        nameList: [Name("y")],
        lambdaBody:
          ApplicationE([
            NameE(Name("+")),
            NameE(Name("x")),
            NameE(Name("y")),
          ]),
      }),
  }),
  "Parses nested lambda inside another lambda",
);

checkExpectExpression(
  parseExpression(read("(((lambda (x) (lambda (y) (+ x y))) 5) 10)")),
  ApplicationE([
    ApplicationE([
      LambdaE({
        nameList: [Name("x")],
        lambdaBody:
          LambdaE({
            nameList: [Name("y")],
            lambdaBody:
              ApplicationE([
                NameE(Name("+")),
                NameE(Name("x")),
                NameE(Name("y")),
              ]),
          }),
      }),
      NumE(5),
    ]),
    NumE(10),
  ]),
  "Parses nested lambda applications correctly",
);

checkExpectExpression(
  parseExpression(read("(cond ((> 3 2) 1) ((< 3 2) 0))")),
  CondE([
    {
      conditionExpr: ApplicationE([NameE(Name(">")), NumE(3), NumE(2)]),
      resultExpr: NumE(1),
    },
    {
      conditionExpr: ApplicationE([NameE(Name("<")), NumE(3), NumE(2)]),
      resultExpr: NumE(0),
    },
  ]),
  "Parses cond-expression correctly",
);

checkExpectExpression(
  parseExpression(read("(let ((x 5) (y 7)) (+ x y))")),
  LetE({
    letPairs: [
      {pairName: Name("x"), pairExpr: NumE(5)},
      {pairName: Name("y"), pairExpr: NumE(7)},
    ],
    letBody:
      ApplicationE([
        NameE(Name("+")),
        NameE(Name("x")),
        NameE(Name("y")),
      ]),
  }),
  "Parses let-expression correctly",
);

checkExpectExpression(
  parseExpression(read("(+ 1 2)")),
  ApplicationE([NameE(Name("+")), NumE(1), NumE(2)]),
  "Parses addition application",
);

checkExpectExpression(
  parseExpression(read("(* 3 4)")),
  ApplicationE([NameE(Name("*")), NumE(3), NumE(4)]),
  "Parses multiplication application",
);

checkExpectExpression(
  parseExpression(read("(* (+ 1 2) (- 5 3))")),
  ApplicationE([
    NameE(Name("*")),
    ApplicationE([NameE(Name("+")), NumE(1), NumE(2)]),
    ApplicationE([NameE(Name("-")), NumE(5), NumE(3)]),
  ]),
  "Parses nested applications correctly",
);

checkExpectExpression(
  parseExpression(read("(cons 1 empty)")),
  ApplicationE([NameE(Name("cons")), NumE(1), EmptyE]),
  "Parses cons expression correctly",
);

checkExpectExpression(
  parseExpression(read("(first (cons 1 empty))")),
  ApplicationE([
    NameE(Name("first")),
    ApplicationE([NameE(Name("cons")), NumE(1), EmptyE]),
  ]),
  "Parses first expression correctly",
);

checkExpectExpression(
  parseExpression(read("(empty? empty)")),
  ApplicationE([NameE(Name("empty?")), EmptyE]),
  "Parses empty? correctly",
);

checkExpectExpression(
  parseExpression(read("(not true)")),
  ApplicationE([NameE(Name("not")), BoolE(true)]),
  "Parses not-expression correctly",
);

checkExpectExpression(
  parseExpression(SymbolC("empty")),
  EmptyE,
  "parse empty expression",
);

checkExpectExpression(
  parseExpression(SymbolC("empty")),
  EmptyE,
  "Parses empty",
);

checkExpectExpression(
  parseExpression(NumberC(5)),
  NumE(5),
  "Parses number",
);

checkExpectExpression(
  parseExpression(SymbolC("true")),
  BoolE(true),
  "Parses true",
);

checkExpectExpression(
  parseExpression(SymbolC("false")),
  BoolE(false),
  "Parses false",
);

checkExpectExpression(
  parseExpression(SymbolC("x")),
  NameE(Name("x")),
  "Parses variable name",
);

checkExpectExpression(
  parseExpression(
    ListC([SymbolC("if"), SymbolC("true"), NumberC(1), NumberC(2)]),
  ),
  IfE({boolExpr: BoolE(true), trueExpr: NumE(1), falseExpr: NumE(2)}),
  "Parses if-expression correctly",
);

checkExpectExpression(
  parseExpression(
    ListC([SymbolC("and"), SymbolC("true"), SymbolC("false")]),
  ),
  AndE(BoolE(true), BoolE(false)),
  "Parses and-expression correctly",
);

checkExpectExpression(
  parseExpression(
    ListC([SymbolC("or"), SymbolC("false"), SymbolC("true")]),
  ),
  OrE(BoolE(false), BoolE(true)),
  "Parses or-expression correctly",
);

checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("lambda"),
      ListC([SymbolC("x"), SymbolC("y")]),
      ListC([SymbolC("+"), SymbolC("x"), SymbolC("y")]),
    ]),
  ),
  LambdaE({
    nameList: [Name("x"), Name("y")],
    lambdaBody:
      ApplicationE([
        NameE(Name("+")),
        NameE(Name("x")),
        NameE(Name("y")),
      ]),
  }),
  "Parses lambda-expression correctly",
);

checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("lambda"),
      ListC([SymbolC("x")]),
      ListC([
        SymbolC("lambda"),
        ListC([SymbolC("y")]),
        ListC([SymbolC("+"), SymbolC("x"), SymbolC("y")]),
      ]),
    ]),
  ),
  LambdaE({
    nameList: [Name("x")],
    lambdaBody:
      LambdaE({
        nameList: [Name("y")],
        lambdaBody:
          ApplicationE([
            NameE(Name("+")),
            NameE(Name("x")),
            NameE(Name("y")),
          ]),
      }),
  }),
  "Parses nested lambda inside another lambda",
);

checkExpectExpression(
  parseExpression(
    ListC([
      ListC([
        ListC([
          SymbolC("lambda"),
          ListC([SymbolC("x")]),
          ListC([
            SymbolC("lambda"),
            ListC([SymbolC("y")]),
            ListC([SymbolC("+"), SymbolC("x"), SymbolC("y")]),
          ]),
        ]),
        NumberC(5),
      ]),
      NumberC(10),
    ]),
  ),
  ApplicationE([
    ApplicationE([
      LambdaE({
        nameList: [Name("x")],
        lambdaBody:
          LambdaE({
            nameList: [Name("y")],
            lambdaBody:
              ApplicationE([
                NameE(Name("+")),
                NameE(Name("x")),
                NameE(Name("y")),
              ]),
          }),
      }),
      NumE(5),
    ]),
    NumE(10),
  ]),
  "Parses nested lambda applications correctly",
);

checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("let"),
      ListC([
        ListC([SymbolC("x"), NumberC(5)]),
        ListC([SymbolC("y"), NumberC(7)]),
      ]),
      ListC([SymbolC("+"), SymbolC("x"), SymbolC("y")]),
    ]),
  ),
  LetE({
    letPairs: [
      {pairName: Name("x"), pairExpr: NumE(5)},
      {pairName: Name("y"), pairExpr: NumE(7)},
    ],
    letBody:
      ApplicationE([
        NameE(Name("+")),
        NameE(Name("x")),
        NameE(Name("y")),
      ]),
  }),
  "Parses let-expression correctly",
);

checkExpectExpression(
  parseExpression(ListC([SymbolC("+"), NumberC(1), NumberC(2)])),
  ApplicationE([NameE(Name("+")), NumE(1), NumE(2)]),
  "Parses addition application",
);

checkExpectExpression(
  parseExpression(ListC([SymbolC("*"), NumberC(3), NumberC(4)])),
  ApplicationE([NameE(Name("*")), NumE(3), NumE(4)]),
  "Parses multiplication application",
);

checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("*"),
      ListC([SymbolC("+"), NumberC(1), NumberC(2)]),
      ListC([SymbolC("-"), NumberC(5), NumberC(3)]),
    ]),
  ),
  ApplicationE([
    NameE(Name("*")),
    ApplicationE([NameE(Name("+")), NumE(1), NumE(2)]),
    ApplicationE([NameE(Name("-")), NumE(5), NumE(3)]),
  ]),
  "Parses nested applications correctly",
);

checkExpectExpression(
  parseExpression(ListC([SymbolC("cons"), NumberC(1), SymbolC("empty")])),
  ApplicationE([NameE(Name("cons")), NumE(1), EmptyE]),
  "Parses cons expression correctly",
);

checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("first"),
      ListC([SymbolC("cons"), NumberC(1), SymbolC("empty")]),
    ]),
  ),
  ApplicationE([
    NameE(Name("first")),
    ApplicationE([NameE(Name("cons")), NumE(1), EmptyE]),
  ]),
  "Parses first expression correctly",
);

checkExpectExpression(
  parseExpression(ListC([SymbolC("empty?"), SymbolC("empty")])),
  ApplicationE([NameE(Name("empty?")), EmptyE]),
  "Parses empty? correctly",
);

checkExpectExpression(
  parseExpression(ListC([SymbolC("not"), SymbolC("true")])),
  ApplicationE([NameE(Name("not")), BoolE(true)]),
  "Parses not-expression correctly",
);

/* Design Recipe
    Data Definitions:
    concreteProgramPiece:
     a concreteProgramPiece is a part of Rackette code which is represented in
     3 possible ways: NumberC(int), SymbolC(string), and
     ListC(list(concreteProgramPiece)). NumberC represents an integer, SymbolC
     usually represents any word usage in Rackette, and ListC represents a
     larger list of NumberCs and SymbolCs.
   definition:
      a defintion is a tuple which contains a name and an
      expression, all enclosed in parentheses. In a definition, the name is
      associated to an expression or procedure.


    Example Data:
    concreteProgramPiece: NumberC(5), SymbolC("define"),
    ListC(SymbolC("define"), SymbolC("x"), NumberC(5))
    definition: (Name("x"), NumE(4)), (Name("basket"), EmptyE),
    (Name("check"),
    IfE({boolExpr: BoolE(true), trueExpr: NumE(4), falseExpr: NumE(3)}))


    Input:
    input: a concreteProgramPiece whose ListC starts with SymbolC("define")
    Output: the concreteProgramPiece in the form of a definition, following the
    tuple format of (name, expression)

     */
let parseDefinition: concreteProgramPiece => definition =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), SymbolC(x), y]) => (
        Name(x),
        parseExpression(y),
      )
    | _ => failwith("missing a name and/or expression")
    };

/* Test Cases for parseDefinition: */

checkError(
  () => parseDefinition(read("(+ 1 2)")),
  "missing a name and/or expression",
);

checkExpectDefinition(
  parseDefinition(read("(define x 10)")),
  (Name("x"), NumE(10)),
  "Parses simple numeric define correctly",
);

checkExpectDefinition(
  parseDefinition(read("(define flag true)")),
  (Name("flag"), BoolE(true)),
  "Parses boolean definition correctly",
);

checkExpectDefinition(
  parseDefinition(read("(define lst empty)")),
  (Name("lst"), EmptyE),
  "Parses empty list definition correctly",
);

checkExpectDefinition(
  parseDefinition(read("(define y (+ 5 3))")),
  (Name("y"), ApplicationE([NameE(Name("+")), NumE(5), NumE(3)])),
  "Parses arithmetic define correctly",
);

checkExpectDefinition(
  parseDefinition(read("(define add1 (lambda (x) (+ x 1)))")),
  (
    Name("add1"),
    LambdaE({
      nameList: [Name("x")],
      lambdaBody:
        ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(1)]),
    }),
  ),
  "Parses define of a simple lambda function correctly",
);

checkExpectDefinition(
  parseDefinition(read("(define add (lambda (x y) (+ x y)))")),
  (
    Name("add"),
    LambdaE({
      nameList: [Name("x"), Name("y")],
      lambdaBody:
        ApplicationE([
          NameE(Name("+")),
          NameE(Name("x")),
          NameE(Name("y")),
        ]),
    }),
  ),
  "Parses define of multi-parameter lambda correctly",
);

checkExpectDefinition(
  parseDefinition(ListC([SymbolC("define"), SymbolC("x"), NumberC(10)])),
  (Name("x"), NumE(10)),
  "Parses simple numeric define correctly",
);

checkExpectDefinition(
  parseDefinition(
    ListC([SymbolC("define"), SymbolC("cs17"), SymbolC("true")]),
  ),
  (Name("cs17"), BoolE(true)),
  "Parses boolean definition correctly",
);

checkExpectDefinition(
  parseDefinition(
    ListC([SymbolC("define"), SymbolC("lst"), SymbolC("empty")]),
  ),
  (Name("lst"), EmptyE),
  "Parses empty list definition correctly",
);

checkExpectDefinition(
  parseDefinition(
    ListC([
      SymbolC("define"),
      SymbolC("y"),
      ListC([SymbolC("+"), NumberC(5), NumberC(3)]),
    ]),
  ),
  (Name("y"), ApplicationE([NameE(Name("+")), NumE(5), NumE(3)])),
  "Parses arithmetic define correctly",
);

checkExpectDefinition(
  parseDefinition(
    ListC([
      SymbolC("define"),
      SymbolC("add1"),
      ListC([
        SymbolC("lambda"),
        ListC([SymbolC("x")]),
        ListC([SymbolC("+"), SymbolC("x"), NumberC(1)]),
      ]),
    ]),
  ),
  (
    Name("add1"),
    LambdaE({
      nameList: [Name("x")],
      lambdaBody:
        ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(1)]),
    }),
  ),
  "Parses define of a simple lambda function correctly",
);

checkExpectDefinition(
  parseDefinition(
    ListC([
      SymbolC("define"),
      SymbolC("add"),
      ListC([
        SymbolC("lambda"),
        ListC([SymbolC("x"), SymbolC("y")]),
        ListC([SymbolC("+"), SymbolC("x"), SymbolC("y")]),
      ]),
    ]),
  ),
  (
    Name("add"),
    LambdaE({
      nameList: [Name("x"), Name("y")],
      lambdaBody:
        ApplicationE([
          NameE(Name("+")),
          NameE(Name("x")),
          NameE(Name("y")),
        ]),
    }),
  ),
  "Parses define of multi-parameter lambda correctly",
);

/* Design Recipe
   Data Definitions:
   concreteProgramPiece:
     a concreteProgramPiece is a part of Rackette code which is represented in
     3 possible ways: NumberC(int), SymbolC(string), and
     ListC(list(concreteProgramPiece)). NumberC represents an integer, SymbolC
     usually represents any word usage in Rackette, and ListC represents a
     larger list of NumberCs and SymbolCs.

   abstractProgramPiece:
     an abstractProgramPiece classifies Rackette code into two groups:
     Definition(definition) and Expression(expression).


   Example Data:
   concreteProgramPiece: NumberC(5), SymbolC("define"),
   ListC(SymbolC("define"), SymbolC("x"), NumberC(5))
   abstractProgramPiece: Expression(NumE(5)), Definition(("x", BoolE(true)))

   Input:
   input: a concreteProgramPiece which needs to be parsed
   Output: an abstractProgramPiece which is equivalent to the Rackette code
   represented by concreteProgramPiece, so either a definition or an expression.
     */
let parsePiece: concreteProgramPiece => abstractProgramPiece =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), ..._]) =>
      Definition(parseDefinition(input))
    | _ => Expression(parseExpression(input))
    };

/* Test cases for parsePiece */
checkExpectAbstractProgramPiece(
  parsePiece(read("(define x 5)")),
  Definition((Name("x"), NumE(5))),
  "Produces a parsed Definition",
);

checkExpectAbstractProgramPiece(
  parsePiece(read("(define add (lambda (a b) (+ a b)))")),
  Definition((
    Name("add"),
    LambdaE({
      nameList: [Name("a"), Name("b")],
      lambdaBody:
        ApplicationE([
          NameE(Name("+")),
          NameE(Name("a")),
          NameE(Name("b")),
        ]),
    }),
  )),
  "Parses a Lambda Definition Correctly",
);

checkExpectAbstractProgramPiece(
  parsePiece(read("(+ 1 2)")),
  Expression(ApplicationE([NameE(Name("+")), NumE(1), NumE(2)])),
  "Parses an Expression Correctly",
);

checkExpectAbstractProgramPiece(
  parsePiece(read("((lambda (x) (lambda (y) (+ x y))) 3)")),
  Expression(
    ApplicationE([
      LambdaE({
        nameList: [Name("x")],
        lambdaBody:
          LambdaE({
            nameList: [Name("y")],
            lambdaBody:
              ApplicationE([
                NameE(Name("+")),
                NameE(Name("x")),
                NameE(Name("y")),
              ]),
          }),
      }),
      NumE(3),
    ]),
  ),
  "Parses a nested lambda expression correctly",
);

checkExpectAbstractProgramPiece(
  parsePiece(read("(if true 1 0)")),
  Expression(
    IfE({boolExpr: BoolE(true), trueExpr: NumE(1), falseExpr: NumE(0)}),
  ),
  "Parses boolean expression correctly",
);

checkExpectAbstractProgramPiece(
  parsePiece(ListC([SymbolC("define"), SymbolC("x"), NumberC(5)])),
  Definition((Name("x"), NumE(5))),
  "Produces a parsed Definition",
);

checkExpectAbstractProgramPiece(
  parsePiece(
    ListC([
      SymbolC("define"),
      SymbolC("add"),
      ListC([
        SymbolC("lambda"),
        ListC([SymbolC("a"), SymbolC("b")]),
        ListC([SymbolC("+"), SymbolC("a"), SymbolC("b")]),
      ]),
    ]),
  ),
  Definition((
    Name("add"),
    LambdaE({
      nameList: [Name("a"), Name("b")],
      lambdaBody:
        ApplicationE([
          NameE(Name("+")),
          NameE(Name("a")),
          NameE(Name("b")),
        ]),
    }),
  )),
  "Parses a Lambda Definition Correctly",
);

checkExpectAbstractProgramPiece(
  parsePiece(ListC([SymbolC("+"), NumberC(1), NumberC(2)])),
  Expression(ApplicationE([NameE(Name("+")), NumE(1), NumE(2)])),
  "Parses an Expression Correctly",
);

checkExpectAbstractProgramPiece(
  parsePiece(
    ListC([
      ListC([
        SymbolC("lambda"),
        ListC([SymbolC("x")]),
        ListC([
          SymbolC("lambda"),
          ListC([SymbolC("y")]),
          ListC([SymbolC("+"), SymbolC("x"), SymbolC("y")]),
        ]),
      ]),
      NumberC(3),
    ]),
  ),
  Expression(
    ApplicationE([
      LambdaE({
        nameList: [Name("x")],
        lambdaBody:
          LambdaE({
            nameList: [Name("y")],
            lambdaBody:
              ApplicationE([
                NameE(Name("+")),
                NameE(Name("x")),
                NameE(Name("y")),
              ]),
          }),
      }),
      NumE(3),
    ]),
  ),
  "Parses a nested lambda expression correctly",
);

checkExpectAbstractProgramPiece(
  parsePiece(
    ListC([SymbolC("if"), SymbolC("true"), NumberC(1), NumberC(0)]),
  ),
  Expression(
    IfE({boolExpr: BoolE(true), trueExpr: NumE(1), falseExpr: NumE(0)}),
  ),
  "Parses boolean expression correctly",
);

/* Design Recipe
    Data Definitions:
    concreteProgram:
     a concreteProgram is a list of concreteProgramPieces, representing the
     Rackette program string after the read function is applied.
   abstractProgram:
     an abstractProgram is a list of abstractProgramPieces, representing a list
     of definitons and expressions which describe the Rackette code.

    Example Data:
    concreteProgram: [NumberC(5), NumberC(4)],
    [ListC([SymbolC("apple")]), NumberC(4)]
    abstractProgram:  [Definition(("x", NumE(4))), Expression(NumE(3))],
    [Definition(("a", NamE("apple")))]

    Input:
    input: a concreteProgram which needs to be parsed by the procedure
    Output: an abstractProgram which represents the parsed version of the
    concreteProgram inputted.
     */
let parse: concreteProgram => abstractProgram =
  input =>
    /* this will parse all of the pieces of this program,
     * giving us a list of pieces, our abstract syntax */
    List.map(parsePiece, input);

/* Test cases for parse: */
checkExpectAbstractProgram(
  parse(readAll("(define x 5) (define y 10)")),
  [Definition((Name("x"), NumE(5))), Definition((Name("y"), NumE(10)))],
  "Parses Definitions",
);

checkExpectAbstractProgram(
  parse(readAll("(+ 1 2) (* 3 4)")),
  [
    Expression(ApplicationE([NameE(Name("+")), NumE(1), NumE(2)])),
    Expression(ApplicationE([NameE(Name("*")), NumE(3), NumE(4)])),
  ],
  "Parses expressions correctly",
);

checkExpectAbstractProgram(
  parse(readAll("(define square (lambda (x) (* x x))) (square 5)")),
  [
    Definition((
      Name("square"),
      LambdaE({
        nameList: [Name("x")],
        lambdaBody:
          ApplicationE([
            NameE(Name("*")),
            NameE(Name("x")),
            NameE(Name("x")),
          ]),
      }),
    )),
    Expression(ApplicationE([NameE(Name("square")), NumE(5)])),
  ],
  "Parses mix of definition and expression correctly",
);

checkExpectAbstractProgram(
  parse([
    ListC([SymbolC("define"), SymbolC("x"), NumberC(5)]),
    ListC([SymbolC("define"), SymbolC("y"), NumberC(10)]),
  ]),
  [Definition((Name("x"), NumE(5))), Definition((Name("y"), NumE(10)))],
  "Parses Definitions",
);

checkExpectAbstractProgram(
  parse([
    ListC([SymbolC("+"), NumberC(1), NumberC(2)]),
    ListC([SymbolC("*"), NumberC(3), NumberC(4)]),
  ]),
  [
    Expression(ApplicationE([NameE(Name("+")), NumE(1), NumE(2)])),
    Expression(ApplicationE([NameE(Name("*")), NumE(3), NumE(4)])),
  ],
  "Parses expressions correctly",
);

checkExpectAbstractProgram(
  parse([
    ListC([
      SymbolC("define"),
      SymbolC("square"),
      ListC([
        SymbolC("lambda"),
        ListC([SymbolC("x")]),
        ListC([SymbolC("*"), SymbolC("x"), SymbolC("x")]),
      ]),
    ]),
    ListC([SymbolC("square"), NumberC(5)]),
  ]),
  [
    Definition((
      Name("square"),
      LambdaE({
        nameList: [Name("x")],
        lambdaBody:
          ApplicationE([
            NameE(Name("*")),
            NameE(Name("x")),
            NameE(Name("x")),
          ]),
      }),
    )),
    Expression(ApplicationE([NameE(Name("square")), NumE(5)])),
  ],
  "Parses mix of definition and expression correctly",
);

/* Testing for parse errors */

checkError(
  () => parsePiece(ListC([])),
  "empty parentheses are not allowed as an input",
);

checkError(
  () => parseDefinition(ListC([SymbolC("define")])),
  "missing a name and/or expression",
);

checkError(
  () => parseDefinition(ListC([SymbolC("define"), SymbolC("a")])),
  "missing a name and/or expression",
);

checkError(
  () => parseDefinition(ListC([SymbolC("define"), NumberC(12)])),
  "missing a name and/or expression",
);

checkError(
  () => parseExpression(ListC([SymbolC("if"), NumberC(5)])),
  "if requires 3 subexpressions",
);

checkError(
  () =>
    parseExpression(
      ListC([SymbolC("lambda"), SymbolC("a"), SymbolC("b")]),
    ),
  "invalid lambda syntax",
);

checkError(
  () =>
    parseExpression(ListC([SymbolC("let"), SymbolC("a"), NumberC(5)])),
  "invalid let syntax",
);

checkError(
  () => parseExpression(ListC([SymbolC("and"), NumberC(5)])),
  "and requires 2 subexpressions",
);

checkError(
  () => parseExpression(ListC([SymbolC("or"), SymbolC("true")])),
  "or requires 2 subexpressions",
);

/* Design Recipe
 * Data Definitions:
 * environment:
 * a list of binding lists which represents all the given built-in procedures.

 * expression:
 * many possible expressions that a Rackette program can take.

 * Example Data:
 * environment: [[("name", NumV(4)), ("ball", NumV(5))], ["smart", NumV(3)]],
 * [[("yes", BoolV(true))]]
 * expression: NumE(4), EmptyE, OrE(BoolE(true), BoolE(false))
 *
 *
 * Input:
 * tle: the top-level form of the environment
 * env: the local environment
 * expr: the expression inputted that needs to be evaluated
 * Output:
 * the value of the expression inputted, derived from using tle and env
 *
 * Recursion Diagram #1:
 * Original Input: (initialTle, [], NumV(1))
 *   Recursive Input: NA!
 *   Recursive Output: NA!
 *   Ideation: convert OI to a number value (no recursion)
 * Original Output: NumV(1)
 *
 * Recursion Diagram #2:
 * Original Input: (initialTle, [], OrE(BoolE(false), BoolE(false)))
 *  Ideation1: Evaluate the first boolean expression in the Or Expression
 *    Recursive Input1: (initialTle, [], BoolE(false))
 *    Recursive Output1: BoolV(false)
 *  Ideation2: Since RO1 is false, evaluate the second boolean expression in
 *             the Or Expression
 *    Recursive Input2: (initialTle, [], BoolE(false))
 *    Recursive Output2: BoolV(false)
 *      Ideation: copy RO2 to OO
 * Original Output: BoolV(false)
 *
 * */

let rec eval: (environment, environment, expression) => value =
  (tle, env, expr) =>
    switch (expr) {
    | NumE(x) => NumV(x)
    | BoolE(x) => BoolV(x)
    | EmptyE => ListV([])
    | NameE(name) =>
      switch (lookup(env, name)) {
      | None =>
        switch (lookup(tle, name)) {
        | None => failwith("the name is not associated to any environment")
        | Some(v) => v
        }
      | Some(v) => v
      }
    | LambdaE({nameList: lst, lambdaBody: body}) =>
      if (containsDuplicates(lst)) {
        failwith("lambda has duplicate argument in argument list");
      } else {
        ClosureV({cNameList: lst, cExpr: body, cEnv: env});
      }
    | LetE({letPairs: letPairsList, letBody: exp}) =>
      let namesList = List.map(pairList => pairList.pairName, letPairsList);
      if (containsDuplicates(namesList)) {
        failwith("let has duplicate variable in binding");
      } else {
        let lis = [];
        let rec letEval: (list(letPair), environment) => environment = (
          (lpList, l) =>
            switch (lpList) {
            | [] => l
            | [firstLP, ...restLPs] =>
              switch (firstLP) {
              | {pairName: n, pairExpr: expr} =>
                let newBinding: binding = (n, eval(tle, env, expr));
                switch (l) {
                | [] => letEval(restLPs, [List.cons(newBinding, [])])
                | [bindingList] =>
                  letEval(restLPs, [List.cons(newBinding, bindingList)])
                | _ => failwith("error")
                };
              }
            }
        );
        eval(tle, letEval(letPairsList, lis) @ env, exp);
      };
    | OrE(x1, x2) =>
      switch (eval(tle, env, x1)) {
      | BoolV(true) => BoolV(true)
      | BoolV(false) =>
        switch (eval(tle, env, x2)) {
        | BoolV(b) => BoolV(b)
        | _ =>
          failwith(
            "A boolean value wasn't entered. Therefore, OrE cannot be executed.",
          )
        }
      | _ =>
        failwith(
          "A boolean value wasn't entered. Therefore, OrE cannot be executed.",
        )
      }
    | CondE(l) =>
      let rec evalCondE: list(condData) => value = (
        lst =>
          switch (lst) {
          | [] =>
            failwith(
              "All conditions resulted in false, so nothing was executed.",
            )
          | [firstCD, ...restCD] =>
            switch (firstCD) {
            | {conditionExpr: expr1, resultExpr: expr2} =>
              switch (eval(tle, env, expr1)) {
              | BoolV(b) =>
                if (b) {
                  eval(tle, env, expr2);
                } else {
                  evalCondE(restCD);
                }
              | _ => failwith("condition expression must evaluate to boolean")
              }
            }
          }
      );
      evalCondE(l);
    | IfE({boolExpr: expr1, trueExpr: expr2, falseExpr: expr3}) =>
      switch (eval(tle, env, expr1)) {
      | BoolV(b) => b ? eval(tle, env, expr2) : eval(tle, env, expr3)
      | _ => failwith("if condition must evaluate to boolean")
      }
    | AndE(x1, x2) =>
      switch (eval(tle, env, x1)) {
      | BoolV(false) => BoolV(false)
      | BoolV(true) =>
        switch (eval(tle, env, x2)) {
        | BoolV(b) => BoolV(b)
        | _ =>
          failwith(
            "A boolean value wasn't entered. Therefore, AndE cannot be executed.",
          )
        }
      | _ =>
        failwith(
          "A boolean value wasn't entered. Therefore, AndE cannot be executed.",
        )
      }
    | ApplicationE(expressionList) =>
      switch (expressionList) {
      | [] =>
        failwith(
          "cannot be empty, this error will already be detected by parse",
        )
      | [procedure, ...restExps] =>
        let z = eval(tle, env, procedure);
        switch (z) {
        | BuiltinV(b) =>
          let listValues = List.map(x => eval(tle, env, x), restExps);
          b.bProc(listValues);
        | ClosureV(closure) =>
          let listValues = List.map(x => eval(tle, env, x), restExps);
          let listNames = closure.cNameList;
          let rec makeNewEnv:
            (environment, list(name), list(value)) => environment = (
            (env, nameList, valueList) =>
              switch (nameList, valueList) {
              | ([], []) => env
              | ([firstName, ...restNames], [firstValue, ...restValues]) =>
                switch (env) {
                | [bindingList] =>
                  let newBinding: binding = (firstName, firstValue);
                  let updatedEnv: environment = [
                    List.cons(newBinding, bindingList),
                  ];
                  makeNewEnv(updatedEnv, restNames, restValues);
                | _ => failwith("error")
                }
              | _ => failwith("Mismatch in # of formals and actuals")
              }
          );
          let newEnvL = makeNewEnv([[]], listNames, listValues);
          let newExtendedEnv = newEnvL @ closure.cEnv;
          eval(tle, newExtendedEnv, closure.cExpr);
        | _ =>
          failwith(
            "cannot perform procecure-application on non-procedure values",
          )
        };
      }
    };

/* Test cases for eval */
checkError(
  () => eval(initialTle, [], parseExpression(read("y"))),
  "the name is not associated to any environment",
);

checkError(
  () => eval(initialTle, [], parseExpression(read("(and 677777 true)"))),
  "A boolean value wasn't entered. Therefore, AndE cannot be executed.",
);

checkError(
  () => eval(initialTle, [], parseExpression(read("(or 12342 false)"))),
  "A boolean value wasn't entered. Therefore, OrE cannot be executed.",
);

checkError(
  () => eval(initialTle, [], parseExpression(read("(if 3 1 2)"))),
  "if condition must evaluate to boolean",
);

checkError(
  () => eval(initialTle, [], parseExpression(read("(number? 1 2)"))),
  "incorrect number of inputs: expected 1 argument",
);

checkError(
  () => eval(initialTle, [], parseExpression(read("(empty?)"))),
  "incorrect number of inputs: expected 1 argument",
);

checkError(
  () => eval(initialTle, [], parseExpression(read("(first empty)"))),
  "this procedure expects a non-empty list input",
);

checkError(
  () =>
    eval(
      initialTle,
      [],
      parseExpression(read("(cond (false 1) ((= 1 2) 3))")),
    ),
  "All conditions resulted in false, so nothing was executed.",
);

checkError(
  () => eval(initialTle, [], parseExpression(read("(+ 1)"))),
  "incorrect number of inputs: expected 2 arguments",
);

checkError(
  () => eval(initialTle, [], parseExpression(read("(+ true 2)"))),
  "Incorrect input type: expected numbers",
);

checkError(
  () =>
    eval(
      initialTle,
      [],
      parseExpression(read("((lambda (x y) (+ x y)) 67)")),
    ),
  "Mismatch in # of formals and actuals",
);

checkError(
  () => eval(initialTle, [], parseExpression(read("((lambda (x) x) 6 7)"))),
  "Mismatch in # of formals and actuals",
);

checkError(
  () => eval(initialTle, [], ApplicationE([])),
  "cannot be empty, this error will already be detected by parse",
);

checkError(
  () => eval(initialTle, [], parseExpression(read("(horse 2 8)"))),
  "the name is not associated to any environment",
);

checkError(
  () => eval(initialTle, [], parseExpression(read("(4 2 0)"))),
  "cannot perform procecure-application on non-procedure values",
);

checkError(
  () =>
    eval(
      initialTle,
      [],
      ApplicationE([
        LambdaE({
          nameList: [Name("a"), Name("b")],
          lambdaBody:
            ApplicationE([
              NameE(Name("+")),
              NameE(Name("a")),
              NameE(Name("b")),
            ]),
        }),
        NumE(5),
      ]),
    ),
  "Mismatch in # of formals and actuals",
);

checkError(
  () =>
    eval(
      initialTle,
      [],
      LambdaE({
        nameList: [Name("a"), Name("a")],
        lambdaBody:
          ApplicationE([NameE(Name("+")), NameE(Name("a")), NumE(1)]),
      }),
    ),
  "lambda has duplicate argument in argument list",
);

checkError(
  () =>
    eval(
      initialTle,
      [],
      LetE({
        letPairs: [
          {pairName: Name("a"), pairExpr: NumE(1)},
          {pairName: Name("a"), pairExpr: NumE(2)},
        ],
        letBody: NameE(Name("a")),
      }),
    ),
  "let has duplicate variable in binding",
);

checkError(
  () =>
    eval(
      initialTle,
      [],
      ApplicationE([
        LambdaE({
          nameList: [Name("a"), Name("b")],
          lambdaBody:
            ApplicationE([
              NameE(Name("/")),
              NameE(Name("a")),
              NameE(Name("b")),
            ]),
        }),
        NumE(5),
      ]),
    ),
  "Mismatch in # of formals and actuals",
);

checkExpect(eval(initialTle, [], NumE(5)), NumV(5), "Evaluates number");

checkExpect(
  eval(initialTle, [], BoolE(true)),
  BoolV(true),
  "Evaluates true",
);
checkExpect(
  eval(initialTle, [], BoolE(false)),
  BoolV(false),
  "Evaluates false",
);
checkExpect(
  eval(initialTle, [], EmptyE),
  ListV([]),
  "Evaluates empty to empty list",
);

checkExpect(
  eval(
    initialTle,
    [],
    IfE({boolExpr: BoolE(true), trueExpr: NumE(1), falseExpr: NumE(2)}),
  ),
  NumV(1),
  "If true branch works",
);

checkExpect(
  eval(
    initialTle,
    [],
    IfE({boolExpr: BoolE(false), trueExpr: NumE(1), falseExpr: NumE(2)}),
  ),
  NumV(2),
  "If false branch works",
);

checkExpect(
  eval(initialTle, [], AndE(BoolE(true), BoolE(false))),
  BoolV(false),
  "And short-circuits false",
);

checkExpect(
  eval(initialTle, [], AndE(BoolE(true), BoolE(true))),
  BoolV(true),
  "And both true",
);

checkExpect(
  eval(initialTle, [], OrE(BoolE(true), BoolE(false))),
  BoolV(true),
  "Or short-circuits true",
);

checkExpect(
  eval(initialTle, [], OrE(BoolE(false), BoolE(true))),
  BoolV(true),
  "Or evaluates second expression",
);

checkExpect(
  eval(
    initialTle,
    [],
    CondE([
      {
        conditionExpr: ApplicationE([NameE(Name(">")), NumE(3), NumE(2)]),
        resultExpr: NumE(1),
      },
      {
        conditionExpr: ApplicationE([NameE(Name("<")), NumE(3), NumE(2)]),
        resultExpr: NumE(0),
      },
    ]),
  ),
  NumV(1),
  "Cond picks first true branch",
);

checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([
      LambdaE({
        nameList: [Name("x")],
        lambdaBody:
          ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(1)]),
      }),
      NumE(5),
    ]),
  ),
  NumV(6),
  "Single-argument lambda application",
);

checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([
      LambdaE({
        nameList: [Name("x"), Name("y")],
        lambdaBody:
          ApplicationE([
            NameE(Name("+")),
            NameE(Name("x")),
            NameE(Name("y")),
          ]),
      }),
      NumE(3),
      NumE(4),
    ]),
  ),
  NumV(7),
  "Multi-argument lambda application",
);

checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([
      LambdaE({
        nameList: [Name("x")],
        lambdaBody:
          ApplicationE([
            LambdaE({
              nameList: [Name("y")],
              lambdaBody:
                ApplicationE([
                  NameE(Name("+")),
                  NameE(Name("x")),
                  NameE(Name("y")),
                ]),
            }),
            NumE(2),
          ]),
      }),
      NumE(5),
    ]),
  ),
  NumV(7),
  "Closure retains outer x binding",
);

checkExpect(
  eval(
    initialTle,
    [],
    LetE({
      letPairs: [
        {pairName: Name("x"), pairExpr: NumE(5)},
        {pairName: Name("y"), pairExpr: NumE(10)},
      ],
      letBody:
        ApplicationE([
          NameE(Name("+")),
          NameE(Name("x")),
          NameE(Name("y")),
        ]),
    }),
  ),
  NumV(15),
  "Let binds local variables and evaluates body",
);

checkExpect(
  eval(
    initialTle,
    [],
    LetE({
      letPairs: [{pairName: Name("x"), pairExpr: NumE(2)}],
      letBody:
        LetE({
          letPairs: [{pairName: Name("x"), pairExpr: NumE(3)}],
          letBody:
            ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(4)]),
        }),
    }),
  ),
  NumV(7),
  "Inner let shadows outer let variable",
);

checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([
      NameE(Name("*")),
      ApplicationE([NameE(Name("+")), NumE(1), NumE(2)]),
      ApplicationE([NameE(Name("-")), NumE(5), NumE(3)]),
    ]),
  ),
  NumV(6),
  "Nested application evaluates correctly",
);

checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([NameE(Name("cons")), NumE(1), EmptyE]),
  ),
  ListV([NumV(1)]),
  "Cons builds a single-element list",
);

checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([
      NameE(Name("first")),
      ApplicationE([
        NameE(Name("cons")),
        NumE(1),
        ApplicationE([NameE(Name("cons")), NumE(2), EmptyE]),
      ]),
    ]),
  ),
  NumV(1),
  "First returns head of list",
);

checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([
      NameE(Name("rest")),
      ApplicationE([
        NameE(Name("cons")),
        NumE(1),
        ApplicationE([NameE(Name("cons")), NumE(2), EmptyE]),
      ]),
    ]),
  ),
  ListV([NumV(2)]),
  "Rest returns tail of list",
);

checkExpect(
  eval(initialTle, [], ApplicationE([NameE(Name("empty?")), EmptyE])),
  BoolV(true),
  "Empty? returns true on empty list",
);

checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([NameE(Name("<")), NumE(2), NumE(3)]),
  ),
  BoolV(true),
  "Less-than comparison works",
);

checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([NameE(Name(">")), NumE(4), NumE(2)]),
  ),
  BoolV(true),
  "Greater-than comparison works",
);

checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([NameE(Name("=")), NumE(5), NumE(5)]),
  ),
  BoolV(true),
  "Equality comparison works",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(if true 1 2)"))),
  NumV(1),
  "If true branch works",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(if false 1 2)"))),
  NumV(2),
  "If false branch works",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(and true false)"))),
  BoolV(false),
  "And short-circuits false",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(and true true)"))),
  BoolV(true),
  "And both true",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(or true false)"))),
  BoolV(true),
  "Or short-circuits true",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(or false true)"))),
  BoolV(true),
  "Or evaluates second expression",
);

checkExpect(
  eval(
    initialTle,
    [],
    parseExpression(read("(cond ((> 3 2) 1) ((< 3 2) 0))")),
  ),
  NumV(1),
  "Cond picks first true branch",
);

checkExpect(
  eval(
    initialTle,
    [],
    parseExpression(
      ListC([
        SymbolC("cond"),
        ListC([
          ListC([SymbolC("<"), NumberC(2), NumberC(3)]),
          NumberC(1),
        ]),
      ]),
    ),
  ),
  NumV(1),
  "Cond picks only true branch",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("((lambda (x) (+ x 1)) 5)"))),
  NumV(6),
  "Single-argument lambda application",
);

checkExpect(
  eval(
    initialTle,
    [],
    parseExpression(read("((lambda (x y) (+ x y)) 3 4)")),
  ),
  NumV(7),
  "Multi-argument lambda application",
);

checkExpect(
  eval(
    initialTle,
    [],
    parseExpression(read("((lambda (x) ((lambda (y) (+ x y)) 2)) 5)")),
  ),
  NumV(7),
  "Closure retains outer x binding",
);

checkExpect(
  eval(
    initialTle,
    [],
    parseExpression(read("(let ((x 5) (y 10)) (+ x y))")),
  ),
  NumV(15),
  "Let binds local variables and evaluates body",
);

checkExpect(
  eval(
    initialTle,
    [],
    parseExpression(read("(let ((x 2)) (let ((x 3)) (+ x 4)))")),
  ),
  NumV(7),
  "Inner let shadows outer let variable",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(* (+ 1 2) (- 5 3))"))),
  NumV(6),
  "Nested application evaluates correctly",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(cons 1 empty)"))),
  ListV([NumV(1)]),
  "Cons builds a single-element list",
);

checkExpect(
  eval(
    initialTle,
    [],
    parseExpression(read("(first (cons 1 (cons 2 empty)))")),
  ),
  NumV(1),
  "First returns head of list",
);

checkExpect(
  eval(
    initialTle,
    [],
    parseExpression(read("(rest (cons 1 (cons 2 empty)))")),
  ),
  ListV([NumV(2)]),
  "Rest returns tail of list",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(empty? empty)"))),
  BoolV(true),
  "Empty? returns true on empty list",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(< 2 3)"))),
  BoolV(true),
  "Less-than comparison works",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(> 4 2)"))),
  BoolV(true),
  "Greater-than comparison works",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(= 5 5)"))),
  BoolV(true),
  "Equality comparison works",
);

/* Design Recipe
   Data Definitions:
   environment:
     a list of binding lists which represents all the given built-in procedures
   which are applicable into Rackette.
   name:
     a representation of a string but with a type of Name
   expression:
     many possible expressions that a Rackette program can take.

   Example Data:
   environment: initialTLE
   name: Name("Apple"), Name("x")
   expression: NumE(4), EmptyE, OrE(BoolE(true), BoolE(false))

   Input:
   env: the current environment
   id: the name of the new definition
   expression: the expression of the new definition which will be associated to id

   Output: a new environment with the new definition added to the
   previous, inputted environment, if it was not already binded.

    */
let addDefinition: (environment, (name, expression)) => environment =
  (env, (id, expr)) => {
    switch (lookup(env, id)) {
    | None =>
      let x = eval(env, [], expr);
      switch (env) {
      | [bindingList] => [[(id, x)] @ bindingList]
      | _ =>
        failwith(
          "Error: env cannot be empty because initialTle already has a binding.",
        )
      };
    | Some(_) => failwith("Error: name is already in tle")
    };
  };

checkError(
  () => addDefinition([[(Name("x"), NumV(1))]], (Name("x"), NumE(2))),
  "Error: name is already in tle",
);

checkError(
  () => addDefinition([], (Name("z"), NumE(3))),
  "Error: env cannot be empty because initialTle already has a binding.",
);

checkExpect(
  addDefinition([[]], (Name("x"), NumE(5))),
  [[(Name("x"), NumV(5))]],
  "addDefinition: basic numeric binding",
);

checkExpect(
  addDefinition([[]], (Name("bool"), BoolE(false))),
  [[(Name("bool"), BoolV(false))]],
  "addDefinition: boolean definition",
);

checkExpect(
  addDefinition([[]], (Name("lst"), EmptyE)),
  [[(Name("lst"), ListV([]))]],
  "addDefinition: define empty list",
);

checkExpect(
  addDefinition([[]], parseDefinition(read("(define a 10)"))),
  [[(Name("a"), NumV(10))]],
  "addDefinition: basic numeric binding",
);

checkExpect(
  lookup(addDefinition(initialTle, (Name("a"), NumE(5))), Name("a")),
  Some(NumV(5)),
  "addDefinition: adds numeric definition to top-level env",
);

checkExpect(
  lookup(
    addDefinition(initialTle, (Name("bool"), BoolE(true))),
    Name("bool"),
  ),
  Some(BoolV(true)),
  "addDefinition: adds boolean definition",
);

checkExpect(
  lookup(
    addDefinition(
      initialTle,
      (
        Name("numbers"),
        ApplicationE([
          NameE(Name("cons")),
          NumE(5),
          ApplicationE([NameE(Name("cons")), NumE(10), EmptyE]),
        ]),
      ),
    ),
    Name("numbers"),
  ),
  Some(ListV([NumV(5), NumV(10)])),
  "addDefinition: creates list using cons",
);

checkExpect(
  lookup(
    addDefinition(
      initialTle,
      (Name("sum"), ApplicationE([NameE(Name("+")), NumE(2), NumE(3)])),
    ),
    Name("sum"),
  ),
  Some(NumV(5)),
  "addDefinition: adds evaluated sum",
);

/* Design Recipe
   Data Definitions:
   value:
     a value represents an output caused by evaluating a parsed Rackette expression.
     Possible types for value include
     NumV(int), which represents integers
     BoolV(bool), which represents true or false
     ListV(list(value)), which represents a list of values
     BuiltinV(builtinData), which represents a builtin, and
     ClosureV(closureData), depicting a closure output

   Example Data:
   value: NumV(4), BoolV(2), ListV([NumV(3), NumV(2), NumV(64)])

   Input:
   aValue: represents a value, which is defined above
   Output:
   a string representation of the value inputted

   Recursion Diagram #1:
   Original Input: NumV(5)
      Recursive Input: NA
      Recursive Output: NA
        Ideation: NA
   Original Output: "5"

   Recursion Diagram #2:
   Original Input: BoolV(true)
      Recusrive Input: NA
      Recursive Output: NA
        Ideation: NA
   Original Output: "true"

   Recursion Diagram #3:
   Original Input: ClosureV({cNameList: [Name("y")], cExpr: NumE(10), cEnv: []})
      Recursive Input: NA!
      Recursive Output: NA!
        Ideation: return pre-defined string output (no recursion)
   Original Output: "<User-defined procedure>"

   */
let rec stringOfValue: value => string =
  aValue =>
    switch (aValue) {
    | NumV(x) => string_of_int(x)
    | BoolV(x) => string_of_bool(x)
    | BuiltinV(x) => x.printedRep
    | ClosureV(_) => "<User-defined procedure>"
    | ListV(x) =>
      switch (x) {
      | [] => "'()"
      | [hd, ...tl] =>
        "(list "
        ++ List.fold_left(
             (y, z) => y ++ " " ++ stringOfValue(z),
             stringOfValue(hd),
             tl,
           )
        ++ ")"
      }
    };

/* Test cases for stringOfValue */
checkExpect(stringOfValue(NumV(42)), "42", "Evaluates a num");

checkExpect(stringOfValue(BoolV(true)), "true", "Evaluates a boolean");

checkExpect(stringOfValue(BoolV(false)), "false", "Evaluates a boolean");

checkExpect(
  stringOfValue(
    ClosureV({cNameList: [Name("x")], cExpr: NumE(10), cEnv: []}),
  ),
  "<User-defined procedure>",
  "Evaluates a user defined closure",
);

checkExpect(stringOfValue(ListV([])), "'()", "Evaluates an empty list");

checkExpect(
  stringOfValue(ListV([NumV(1), NumV(2), NumV(3)])),
  "(list 1 2 3)",
  "Evaluates a list",
);

checkExpect(
  stringOfValue(ListV([NumV(1), BoolV(true), NumV(5)])),
  "(list 1 true 5)",
  "Evaluates a list of mixed types",
);

checkExpect(
  stringOfValue(ListV([NumV(1), ListV([NumV(2), NumV(3)]), NumV(4)])),
  "(list 1 (list 2 3) 4)",
  "Evaluates a nested list",
);

checkExpect(
  stringOfValue(eval(initialTle, [], NumE(42))),
  "42",
  "Evaluates a num",
);

checkExpect(
  stringOfValue(eval(initialTle, [], BoolE(true))),
  "true",
  "Evaluates a boolean true",
);

checkExpect(
  stringOfValue(eval(initialTle, [], BoolE(false))),
  "false",
  "Evaluates a boolean false",
);

checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      LambdaE({nameList: [Name("x")], lambdaBody: NumE(10)}),
    ),
  ),
  "<User-defined procedure>",
  "Evaluates a user-defined closure",
);

checkExpect(
  stringOfValue(eval(initialTle, [], EmptyE)),
  "'()",
  "Evaluates an empty list",
);

/* process: this procedure processes the abstract program
 * representation of a Rackette program following the
 * Rackette rules of processing
 * I/P: pieces, an abstract program representation of a Rackette program
 * O/P: the list of values corresponding to
 * the evaluation of any expressions present in pieces */
let process: abstractProgram => list(value) =
  pieces => {
    let rec processHelper: (environment, abstractProgram) => list(value) =
      (tle, pieces) =>
        switch (pieces) {
        | [] => []
        | [Definition(d), ...tl] => processHelper(addDefinition(tle, d), tl)
        | [Expression(e), ...tl] => [
            eval(tle, [], e),
            ...processHelper(tle, tl),
          ]
        };
    processHelper(initialTle, pieces);
  };

/* Check Expects for process: */

checkExpect(
  process([
    Expression(ApplicationE([NameE(Name("+")), NumE(2), NumE(3)])),
  ]),
  [NumV(5)],
  "Evaluates a simple operation",
);

checkExpect(
  process([
    Expression(ApplicationE([NameE(Name("+")), NumE(1), NumE(1)])),
    Expression(ApplicationE([NameE(Name("*")), NumE(3), NumE(4)])),
  ]),
  [NumV(2), NumV(12)],
  "Evaluates multiple operations",
);

checkExpect(
  process([
    Definition((Name("x"), NumE(10))),
    Expression(
      ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(5)]),
    ),
  ]),
  [NumV(15)],
  "Adds a definition and evaluates an expression using it",
);

checkExpect(
  process([
    Definition((
      Name("square"),
      LambdaE({
        nameList: [Name("n")],
        lambdaBody:
          ApplicationE([
            NameE(Name("*")),
            NameE(Name("n")),
            NameE(Name("n")),
          ]),
      }),
    )),
    Expression(ApplicationE([NameE(Name("square")), NumE(6)])),
  ]),
  [NumV(36)],
  "Defines a function and then makes use of it",
);

checkExpect(
  process([
    Definition((Name("a"), NumE(2))),
    Definition((Name("b"), NumE(3))),
    Expression(
      ApplicationE([
        NameE(Name("+")),
        NameE(Name("a")),
        NameE(Name("b")),
      ]),
    ),
    Expression(
      ApplicationE([
        NameE(Name("*")),
        NameE(Name("a")),
        NameE(Name("b")),
      ]),
    ),
  ]),
  [NumV(5), NumV(6)],
  "Tests multiple definitions and their uses",
);

checkExpect(
  process([
    Definition((
      Name("makeAdder"),
      LambdaE({
        nameList: [Name("x")],
        lambdaBody:
          LambdaE({
            nameList: [Name("y")],
            lambdaBody:
              ApplicationE([
                NameE(Name("+")),
                NameE(Name("x")),
                NameE(Name("y")),
              ]),
          }),
      }),
    )),
    Expression(
      ApplicationE([
        ApplicationE([NameE(Name("makeAdder")), NumE(10)]),
        NumE(5),
      ]),
    ),
  ]),
  [NumV(15)],
  "Tests scope and closure evaluation",
);

checkExpect(
  process([
    Expression(
      IfE({boolExpr: BoolE(true), trueExpr: NumE(1), falseExpr: NumE(2)}),
    ),
  ]),
  [NumV(1)],
  "Tests conditional expression",
);

checkExpect(
  process(parse([ListC([SymbolC("+"), NumberC(2), NumberC(3)])])),
  [NumV(5)],
  "Evaluates a simple operation",
);

checkExpect(
  process(
    parse([
      ListC([SymbolC("+"), NumberC(1), NumberC(1)]),
      ListC([SymbolC("*"), NumberC(3), NumberC(4)]),
    ]),
  ),
  [NumV(2), NumV(12)],
  "Evaluates multiple operations",
);

checkExpect(
  process(
    parse([
      ListC([SymbolC("define"), SymbolC("x"), NumberC(10)]),
      ListC([SymbolC("+"), SymbolC("x"), NumberC(5)]),
    ]),
  ),
  [NumV(15)],
  "Adds a definition and evaluates an expression using it",
);

checkExpect(
  process(
    parse([
      ListC([
        SymbolC("define"),
        SymbolC("square"),
        ListC([
          SymbolC("lambda"),
          ListC([SymbolC("n")]),
          ListC([SymbolC("*"), SymbolC("n"), SymbolC("n")]),
        ]),
      ]),
      ListC([SymbolC("square"), NumberC(6)]),
    ]),
  ),
  [NumV(36)],
  "Defines a function and then makes use of it",
);

checkExpect(
  process(
    parse([
      ListC([SymbolC("define"), SymbolC("a"), NumberC(2)]),
      ListC([SymbolC("define"), SymbolC("b"), NumberC(3)]),
      ListC([SymbolC("+"), SymbolC("a"), SymbolC("b")]),
      ListC([SymbolC("*"), SymbolC("a"), SymbolC("b")]),
    ]),
  ),
  [NumV(5), NumV(6)],
  "Tests multiple definitions and their uses",
);

checkExpect(
  process(
    parse([
      ListC([
        SymbolC("define"),
        SymbolC("makeAdder"),
        ListC([
          SymbolC("lambda"),
          ListC([SymbolC("x")]),
          ListC([
            SymbolC("lambda"),
            ListC([SymbolC("y")]),
            ListC([SymbolC("+"), SymbolC("x"), SymbolC("y")]),
          ]),
        ]),
      ]),
      ListC([ListC([SymbolC("makeAdder"), NumberC(10)]), NumberC(5)]),
    ]),
  ),
  [NumV(15)],
  "Tests scope and closure evaluation",
);

checkExpect(
  process(
    parse([
      ListC([SymbolC("if"), SymbolC("true"), NumberC(1), NumberC(2)]),
    ]),
  ),
  [NumV(1)],
  "Tests conditional expression",
);

checkExpect(
  process(parse(readAll("(+ 2 3)"))),
  [NumV(5)],
  "Evaluates a simple operation",
);

checkExpect(
  process(parse(readAll("(+ 1 1) (* 3 4)"))),
  [NumV(2), NumV(12)],
  "Evaluates multiple operations",
);

checkExpect(
  process(parse(readAll("(define x 10) (+ x 5)"))),
  [NumV(15)],
  "Adds a definition and evaluates an expression using it",
);

checkExpect(
  process(
    parse(readAll("(define square (lambda (n) (* n n))) (square 6)")),
  ),
  [NumV(36)],
  "Defines a function and then makes use of it",
);

checkExpect(
  process(parse(readAll("(define a 2) (define b 3) (+ a b) (* a b)"))),
  [NumV(5), NumV(6)],
  "Tests multiple definitions and their uses",
);

checkExpect(
  process(
    parse(
      readAll(
        "(define makeAdder (lambda (x) (lambda (y) (+ x y)))) ((makeAdder 10) 5)",
      ),
    ),
  ),
  [NumV(15)],
  "Tests scope and closure evaluation",
);

checkExpect(
  process(parse(readAll("(if true 1 2)"))),
  [NumV(1)],
  "Tests conditional expression",
);

/* rackette: this procedure will interpret a Rackette program
 * and return its value as a string, if it has one
 * I/P: a Rackette program represented as a raw program, program
 * O/P: a list of the string representations of
 *      the evaluated Rackette expressions in programs */
let rackette: rawProgram => list(string) =
  program => List.map(stringOfValue, process(parse(readAll(program))));

/* Check expects for rackette */

checkExpect(rackette("(+ 2 3)"), ["5"], "Checks basic arithmetic");

checkExpect(
  rackette("(* 2 3) (+ 10 5)"),
  ["6", "15"],
  "Tests multiple expressions",
);

checkExpect(
  rackette("(define x 10) (+ x 5)"),
  ["15"],
  "Tests simple variable definition and use",
);

checkExpect(rackette("(if true 42 0)"), ["42"], "Tests conditionals");

checkExpect(
  rackette("
    (define a 2)
    (define b 3)
    (+ a b)
    (* a b)
  "),
  ["5", "6"],
  "Tests multiple defenitions and sequential printing",
);

checkExpect(
  rackette("(and true (or false true))"),
  ["true"],
  "Tests boolean logic",
);

checkExpect(rackette("empty"), ["'()"], "Checks empty list");

checkExpect(
  rackette("(cond ((> 3 2) 1) ((< 3 2) 0))"),
  ["1"],
  "Cond chooses the first true branch",
);

checkExpect(
  rackette("(cond ((< 3 2) 0) ((= 4 4) 99))"),
  ["99"],
  "Cond skips false branch and uses the true one",
);

checkExpect(
  rackette("(if false (+ 1 2) (+ 3 4))"),
  ["7"],
  "If expression evaluates the false branch only",
);

checkExpect(
  rackette("(if true (+ 10 2) (/ 1 0))"),
  ["12"],
  "If short-circuits and ignores false branch evaluation",
);

checkExpect(
  rackette("(let ((x 5) (y 10)) (+ x y))"),
  ["15"],
  "Let binds local variables and evaluates body",
);

checkExpect(
  rackette("(let ((x 2)) (let ((x 3)) (+ x 4)))"),
  ["7"],
  "Inner let shadows outer let variable",
);

checkExpect(
  rackette("(let ((a 1) (b 2) (c 3)) (* (+ a b) c))"),
  ["9"],
  "Let handles multiple bindings and nested arithmetic",
);

checkExpect(
  rackette("((lambda (x) (+ x 1)) 5)"),
  ["6"],
  "Simple single-argument lambda application",
);

checkExpect(
  rackette("((lambda (x y) (+ x y)) 3 4)"),
  ["7"],
  "Multi-argument lambda application",
);

checkExpect(
  rackette("((lambda (x) ((lambda (y) (+ x y)) 2)) 5)"),
  ["7"],
  "Nested lambda application",
);

checkExpect(
  rackette(
    "
    (define makeAdder (lambda (x) (lambda (y) (+ x y))))
    ((makeAdder 10) 5)
  ",
  ),
  ["15"],
  "Defines a function with a lmbda",
);

checkExpect(
  rackette(
    "
    (define square (lambda (n) (* n n)))
    (define applyTwice (lambda (f x) (f (f x))))
    (applyTwice square 2)
  ",
  ),
  ["16"],
  "Defines two functions",
);

checkExpect(
  rackette("(cons 1 empty)"),
  ["(list 1)"],
  "Cons builds a single-element list",
);

checkExpect(
  rackette("(first (cons 1 (cons 2 empty)))"),
  ["1"],
  "First returns head of list",
);

checkExpect(
  rackette("(rest (cons 1 (cons 2 empty)))"),
  ["(list 2)"],
  "Rest returns tail of list",
);

checkExpect(
  rackette("(empty? empty)"),
  ["true"],
  "Empty? correctly identifies an empty list",
);

checkExpect(
  rackette("(empty? (cons 1 empty))"),
  ["false"],
  "Empty? correctly identifies a non-empty list",
);

checkExpect(
  rackette(
    "
    (define x 5)
    (define addX (lambda (y) (+ x y)))
    (let ((x 100)) (addX 2))
  ",
  ),
  ["7"],
  "Closure retains outer variable value",
);

checkExpect(
  rackette(
    "
    (define factorial
      (lambda (n)
        (if (= n 0)
            1
            (* n (factorial (- n 1))))))
    (factorial 5)
  ",
  ),
  ["120"],
  "Recursive factorial definition and call",
);
