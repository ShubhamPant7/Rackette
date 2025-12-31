/* Example Data for rawProgram:
 * 1. "(+ 4 9)"
 * 2. "(define a 7)"
 * 3. "(define z 4) (+ z 5)"
 * 4. "(+ 2 (* (- 3 1) 1))"
 */
type rawProgram = string;

/* Example Data for concreteProgramPiece:
 * 1. NumberC(23)
 * 2. NumberC(-8)
 * 3. SymbolC("define")
 * 4. SymbolC("*")
 * 5. SymbolC("false")
 * 6. ListC([ListC([SymbolC("*"), NumberC(8), NumberC(2)])
 */
type concreteProgramPiece =
  | NumberC(int)
  | SymbolC(string)
  | ListC(list(concreteProgramPiece));

/* Example Data for concreteProgram:
 * 1. [NumberC(23)]
 * 2. [SymbolC("false")]
 * 3. [ListC([SymbolC("*"), NumberC(8), NumberC(2)])]
 * 4. [ListC([SymbolC("or"), SymbolC("false"), SymbolC("false")])]
 * 5. [ListC([SymbolC("define"), SymbolC("a"), NumberC(8)])]
 * 6. [ListC([SymbolC("define"), SymbolC("a"), NumberC(9)], SymbolC("a")])]
 */
type concreteProgram = list(concreteProgramPiece);

/* a Rackette name */
/* Example Data for name:
 * 1. Name("x")
 * 2. Name("proc")
 * 3. Name("y")
 * 4. Name("*")
 * 5. Name("cons")
 */
type name =
  | Name(string);

/* a Rackette expression */
/* Example Data for expression:
 * 1. NumE(15)
 * 2. BoolE(false)
 * 3. EmptyE
 * 4. AndE(BoolE(true), OrE(BoolE(false), BoolE(true)))
 * 5. OrE(BoolE(false), BoolE(false))
 * 6. NameE(Name("+"))
 * 7. CondE([
 *      {
 *        conditionExpr: BoolE(true),
 *        resultExpr: NumE(0),
 *      },
 *      {
 *        conditionExpr: BoolE(false),
 *        resultExpr: NumE(5),
 *      }
 *    ])
 * 8. ifE({
 *      ifConditionExpr: BoolE(true),
 *      ifExprTrue: NumE(1),
 *      ifExprFalse: NumE(2),
 *    })
 * 9. LambdaE({
 *      nameList: [Name("a")],
 *      lambdaBody: ApplicationE([NameE(Name("+")), NameE(Name("a")), NumE(1)])
 *    })
 * 10. LetE({
 *       letPairs: [{
 *                   pairName: Name("a"),
 *                   pairExpr: NumE(10)
 *                  },
 *                  {
 *                   pairName: Name("b"),
 *                   pairExpr: NumE(20)
 *                  }]
 *       letBody: ApplicationE([NameE(Name("+")),
 *                              NameE(Name("a")),
 *                              NameE(Name("b"))])
 *     })
 * 11. ApplicationE([[NameE(Name("+")), NameE(Name("a")), NumE(1)])
 */
type expression =
  | NumE(int)
  | BoolE(bool)
  | EmptyE
  | NameE(name)
  | AndE(expression, expression)
  | OrE(expression, expression)
  | IfE(ifData)
  | CondE(list(condData))
  | LambdaE(lambdaData)
  | LetE(letData)
  | ApplicationE(list(expression))

/* Example Data for ifData:
 * 1. {
 *      ifConditionExpr: BoolE(true),
 *      ifTrueExpr: NumE(5),
 *      ifFalseExpr: NumE(-5),
 *    }
 * 2. {
 *      ifConditionExpr: ApplicationE([[NameE(Name("=")),
 *                                      NameE(Name("a")),
 *                                      NameE(Name("b"))]),
 *      ifTrueExpr: NumE(0),
 *      ifFalseExpr: NumE(100),
 *    }
 */
and ifData = {
  boolExpr: expression,
  trueExpr: expression,
  falseExpr: expression,
}

/* Example Data for condData:
 * 1. {
 *      ConditionExpr: BoolE(true),
 *      resultExpr: NumE(10)
 *    }
 * 2. {
 *      ConditionExpr: BoolE(false),
 *      resultExpr: NumE(0)
 *    }
 */
and condData = {
  conditionExpr: expression,
  resultExpr: expression,
}

/* Example Data for lambdaData:
 * 1. {
 *      nameList: [Name("a")],
 *      lambdaBody: ApplicationE([NameE(Name("+")), NameE(Name("a")), NumE(1)])
 *    }
 * 2. {
 *      nameList: [Name("a"), Name("b")],
 *      lambdaBody: ApplicationE([NameE(Name("*")),
 *                                NameE(Name("a")),
 *                                NameE(Name("b"))])
 *    }
 */
and lambdaData = {
  nameList: list(name),
  lambdaBody: expression,
}

/* Example Data for letPair:
 * 1. {
 *      pairName: Name("a"),
 *      pairExpr: NumE(4)
 *    }
 * 2. {
 *      pairName: Name("b"),
 *      pairExpr: BoolE(false)
 *    }
 */
and letPair = {
  pairName: name,
  pairExpr: expression,
}

/* Example Data for letData:
 * 1. {
 *       letPairs: [{
 *                   pairName: Name("a"),
 *                   pairExpr: NumE(5)
 *                  },
 *                  {
 *                   pairName: Name("b"),
 *                   pairExpr: NumE(15)
 *                  }]
 *       letBody: ApplicationE([NameE(Name("+")),
 *                              NameE(Name("a")),
 *                              NameE(Name("b"))])
 *    }
 * 2. {
 *       letPairs: [{
 *                   pairName: Name("a"),
 *                   pairExpr: NumE(12)
 *                  },
 *                  {
 *                   pairName: Name("b"),
 *                   pairExpr: NumE(12)
 *                  }]
 *       letBody: ApplicationE([NameE(Name("*")),
 *                              NameE(Name("a")),
 *                              NameE(Name("b"))])
 *    }
 */
and letData = {
  letPairs: list(letPair),
  letBody: expression,
};

/* a Rackette definition */
/* Example Data for definition:
 * 1. (Name("a"), NumE(5))
 * 2. (Name("b"), BoolE(true))
 */
type definition = (name, expression);

/* a piece of Rackette that can be processed:
 * either a definition or an expression */
/* Example Data for abstractProgramPiece:
 * 1. Definition((Name("a"), NumE(5)))
 * 2. Definition((Name("b"), BoolE(true)))
 * 3. Expression(ApplicationE([[NameE(Name("+")), NameE(Name("a")), NumE(5)]))
 * 4. Expression(NumE(40))
 */
type abstractProgramPiece =
  | Definition(definition)
  | Expression(expression);

/* a representation of a Rackette program -
 * any number of pieces */
/* Example Data for abstractProgram:
 * 1. [Definition((Name("a"), NumE(5))),
 *     Definition((Name("b"), NumE(10))),
 *     Expression(ApplicationE([[NameE(Name("+")), NameE(Name("a")), NumE(10)])),
 *     Expression(ApplicationE([[NameE(Name(">")),
 *                               NameE(Name("a")),
 *                               NameE(Name("b")]
 * 2. [Definition((Name("a"), NumE(10))),
 *     Definition((Name("b"), NumE(20))),
 *     Expression(ApplicationE([[NameE(Name("+")), NameE(Name("a")), NumE(10)])),
 *     Expression(ApplicationE([[NameE(Name(">")),
 *                               NameE(Name("a")),
 *                               NameE(Name("b")]
 */
type abstractProgram = list(abstractProgramPiece);

/* a Rackette value: the result of evaluating a Rackette expression */
/* Example Data for value:
 * 1. NumV(15)
 * 2. BoolV(true)
 * 3. ListV([NumV(20), BoolV(false)])
 * 4. BuiltinV({
 *      printedRep: "<builtin-proc-+>",
 *      bProc: {
 *        let plusProc: list(value) => value =
 *          fun
 *          | [NumV(n1), NumV(n2)] => NumV(n1 + n2)
 *          | _ =>
 *            failwith("incorrect number of inputs or incorrect input types");
 *          plusProc;
 *       },
 *    })
 * 5. ClosureV({
 *      cNameList: [Name("a")],
 *      cExpr: ApplicationE([[NameE(Name("+")), NameE(Name("a")), NumE(5)]),
 *      cEnv: [[]],
 *    })
 */
type value =
  | NumV(int)
  | BoolV(bool)
  | ListV(list(value))
  | BuiltinV(builtinData)
  | ClosureV(closureData)

/* Example Data for builtinData:
 * 1. {
 *      printedRep: "<builtin-proc-+>",
 *      bProc: {
 *        let plusProc: list(value) => value =
 *          fun
 *          | [NumV(n1), NumV(n2)] => NumV(n1 + n2)
 *          | _ =>
 *            failwith("incorrect number of inputs or incorrect input types");
 *          plusProc;
 *       },
 *    }
 * 2. {
 *      printedRep: "<builtin-proc-*>",
 *      bProc: {
 *        let multProc: list(value) => value =
 *          fun
 *          | [NumV(n1), NumV(n2)] => NumV(n1 * n2)
 *          | _ =>
 *            failwith("incorrect number of inputs or incorrect input types");
 *          multProc;
 *       },
 *    }
 */
and builtinData = {
  printedRep: string,
  bProc: list(value) => value,
}

/* Example Data for closureData:
 * 1. {
 *      cNameList: [Name("a")],
 *      cExpr: ApplicationE([[NameE(Name("+")), NameE(Name("a")), NumE(5)]),
 *      cEnv: [[]],
 *    }
 * 2. {
 *      cNameList: [Name("b")],
 *      cExpr: ApplicationE([[NameE(Name("*")), NameE(Name("b")), NumE(10)]),
 *      cEnv: [[]],
 *    }
 */
and closureData = {
  cNameList: list(name),
  cExpr: expression,
  cEnv: environment,
}
/* Environments, bindingLists, and bindings aren't values
   But we use "and" here so closures have access to environments,
   bindings have access to values, etc. */

/* Example Data for environment:
 * 1. [[]]
 * 2. [[(
      Name("*"),
      BuiltinV({
        printedRep: "<builtin-proc-*>",
        bProc: {
          let plusProc: list(value) => value =
            fun
            | [NumV(n1), NumV(n2)] => NumV(n1 * n2)
            | _ =>
              failwith("incorrect number of inputs or incorrect input types");
          multProc;
        },
      }),
    )]]
 */
and environment = list(bindingList)

/* Example Data for bindinglist:
 * 1. []
 * 2. [(
      Name("+"),
      BuiltinV({
        printedRep: "<builtin-proc-+>",
        bProc: {
          let plusProc: list(value) => value =
            fun
            | [NumV(n1), NumV(n2)] => NumV(n1 + n2)
            | _ =>
              failwith("incorrect number of inputs or incorrect input types");
          plusProc;
        },
      }),
    )
 */
and bindingList = list(binding)

/* Example Data for binding:
 * 1. (
      Name("+"),
      BuiltinV({
        printedRep: "<builtin-proc-+>",
        bProc: {
          let plusProc: list(value) => value =
            fun
            | [NumV(n1), NumV(n2)] => NumV(n1 + n2)
            | _ =>
              failwith("incorrect number of inputs or incorrect input types");
          plusProc;
        },
      }),
    )
  2. (
      Name("*"),
      BuiltinV({
        printedRep: "<builtin-proc-*>",
        bProc: {
          let multProc: list(value) => value =
            fun
            | [NumV(n1), NumV(n2)] => NumV(n1 * n2)
            | _ =>
              failwith("incorrect number of inputs or incorrect input types");
          multProc;
        },
      }),
    )
 */
and binding = (name, value);
