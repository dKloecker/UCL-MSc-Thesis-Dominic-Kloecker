>|| There will be lots of comments in this file, so we use the "Literate script" style (indicated where the first character
>|| of this file is '>').  With this style, everything is a comment EXCEPT lines starting with a '>' character. There should
>|| be a blank line separator between commentary and code.  Normal comments can still be used within code lines.

This new program addresses the issue of parser construction.  What we need for a CNL may be different to what is needed for
other parsing projects, since the CNL may have great flexibility and therefore we need to emphasise code construction
methods that work best for lots of alternatives. To give this flexibility we can structure the solution as follows:
* the BNF grammar of the CNL is directly reflected in the code
* the code will produce a parse tree that represents the logic (we define an algebraic type for this)
* we will define general-purpose functions (combinators) to help make the code look like the BNF for the CNL

================================================================================
First we describe our approach to the code:

1. To parse a non-terminal in the BNF that has several alternative definitions we can generate a list of results, where
   the list has one item for each alternative.
2. We will use the new type "maybe *" to permit each list item to return either "Just" the result or "Nothing" if it fails,
   and then select the first non-Nothing item as the chosen correct parsing.

>maybe * ::= Just * | Nothing

3. We will define a parser type to take as input a [*] and return as output a [([*],**)] - that is, it outputs a list
   of 2-tuples each of which contains the remainder of the input not yet parsed ([*]) and the parsed output (**).
   Note the polymorphic types * and ** - we are keeping this as generic as possible.
   Also note that we will probably instantiate ** as "maybe ***" where *** is either the type of a contract or the
   type of a component, etc.
   The input type * will probably be "token"

   Here is the definition of the type t_parser * (note that we now start type names with "t_" to differentiate between type names
   and variable names):

>t_parser * ** == [*] -> [([*],**)]

4. We will need to be able to say "parse this followed by that", so we need a sequencer operation
   For generality, we use an additional function parameter to determine how we want to combine the result of p1 with
   the result of p2 - this occurs as the first argument, so that we can pre-define a partial application for repeated
   occurrences of the same "then f"

>then :: (**->**->**) -> (t_parser * **) -> (t_parser * **) -> (t_parser * **)
>then f p1 p2 = (concat.((map g).p1))
>               where
>               g (rem,res)  = [(rem2, f res res2) | (rem2, res2) <- (p2 rem)]

5. We will need to be able to say "parse this or that", so we need an alternator operation

>alt :: t_parser * ** -> t_parser * ** -> t_parser * **
>alt p1 p2 = g
>            where
>            g ip = (p1 ip)++(p2 ip)

6. We will define the BNF for the CNL, and we will define a separate function to parse each non-terminal and each terminal
   in that BNF.  The parser  functions at the contract level will have type "t_parser token t_contract" and the parser
   functions at the component level will have type "t_parser token t_component" and we will need type conversions to control
   results from the lower levels being incorporated into the results for the higher levels.


================================================================================
Now we describe the BNF, the code and the algebraic types for the logic at each level

1. Contract level

   BNF description:
   <contract> ::  <empty_contract> | <component> AND <component>* || NB: we do not OR contracts (but we can OR components within contracts)

   In the code the BNF for the CNL will be expressed directly in the relevant functions
   but those functions will produce results that align with the logic.  We therefore define algebraic types
   for the logic.

   Logic definition (used in the output of the parser functions):
   An empty contract now has an empty list of clauses. TrueContract simplies the code.

>t_contract ::= TrueContract | Contract t_component | Contracts_and [t_component] || list of components implies ANDing of components

>parse_contract :: t_parser token (maybe t_contract)
>parse_contract = alt parse_component (then f parse_component (then g parse_contractAND parse_contract))
>                 where
>                 f Nothing                any                      = Nothing
>                 f any                    Nothing                  = Nothing
>                 f (Just (Contract x))    (Just (Contracts_and y)) = Just (Contracts_and (x:y))
>                 f (Just (Contract x))    (Just (Contract y))      = Just (Contracts_and [x, y])
>                 g Nothing                any                      = Nothing
>                 g any                    Nothing                  = Nothing
>                 g x                      y                        = y

         The function parse_contractAND is used by parse_contract to parse the contract 'AND' terminal:

>parse_contractAND :: t_parser token (maybe t_contract)
>parse_contractAND (ContractAND:rest) = [(rest,Just (TrueContract))]
>parse_contractAND any                = [(any ,Nothing)]

2. Component level

    BNF description:
    <component> :: <definition> | <expressional-definition> | <statement> | <conditional-statement>

    Logic definition (used in the output of the parser functions). Notice how one definition in the logic (CompCondStatement)
    captures two alternatives in the BNF for the CNL.

    NB: Both CompCondition and CompExpression can not appear on their own in the CNL. It is only used when parsing conditions.

    The function parse_component can alternatively succeed with parsing a definition, or a statement, or a condition,
    or a conditional statement, or an expressional definition.

>t_component  ::= CompDefinition t_definition | CompStatement t_statement | CompCondition t_condition |
>                 CompCondStatement t_condition t_statement | CompStatStatement t_statement t_statement | TrueComponent | || TrueComponent simplifies the code
>                 CompExprDefinition t_expression t_definition | CompExpression t_expression

>parse_component :: t_parser token (maybe t_contract)
>parse_component = maybecomponent2maybecontract . (alt parse_definition (alt parse_statement (alt parse_condition (alt parse_conditional_statement parse_expressional_definition))))
>                  where
>                  maybecomponent2maybecontract ops = filter ((~=Nothing).snd) (map g ops)
>                                                     where
>                                                     g (rem, Nothing) = (rem, Nothing)
>                                                     g (rem, Just s)  = (rem, Just (Contract s))

3. Definitions

    BNF description:
    <definition> :: <ID> DEF-IS <subject> <subject> | <ID> DEF-EQ <object> <numerical-expression>
    <numerical-expression> :: num | <object> | <numerical-expression> <operator> <numerical-expression>

>t_definition ::= Def_IS t_ID t_subject t_subject | Def_EQ t_ID t_object t_numericalexpr | Definitions_and [t_definition]

>t_numericalexpr ::= NumericalExprNum t_num | NumericalExprObject t_object | NumericalExprExpr t_numericalexpr t_operator t_numericalexpr | NoNumericalExpr || NoNumericalExpr needed for intermediate parsing
>t_operator      ::= PLUS | MINUS | TIMES | DIVIDE | NoOperator || NoOperator needed for intermediate parsing
>empty_operator = NoOperator

>parse_definition :: t_parser token (maybe t_component)
>parse_definition = alt parse_simpledefinition (then f parse_simpledefinition (then g parse_componentAND parse_definition))
>                   where
>                   f Nothing                   any                                          = Nothing
>                   f any                       Nothing                                      = Nothing
>                   f (Just (CompDefinition x)) (Just (CompDefinition (Definitions_and y)))  = Just (CompDefinition (Definitions_and (x:y)))
>                   f (Just (CompDefinition x)) (Just (CompDefinition y))                    = Just (CompDefinition (Definitions_and [x, y]))
>                   g Nothing                   any                                          = Nothing
>                   g any                       Nothing                                      = Nothing
>                   g x                         y                                            = y

>parse_simpledefinition :: t_parser token (maybe t_component)
>parse_simpledefinition = alt parse_simpledefinitionIS parse_simpledefinitionEQ

>parse_simpledefinitionIS :: t_parser token (maybe t_component)
>parse_simpledefinitionIS =
>    maybedefinition2maybecomponent . (then a parse_compID_defIS (d_parse "s1ISs2"))
>    where
>    a x       Nothing = Nothing
>    a Nothing y       = Nothing
>    a (Just (Def_IS id1 s11 s12)) (Just (Def_IS id2 s21 s22)) = Just (Def_IS id1 s21 s22) || Def_IS2 but CompID1 overrides
>    d_parse "s1ISs2" = then (merge "s1") parse_s12d (then (merge "s2") parse_IS parse_s22d)
>    d_parse any = g
>    g ip = [([], Nothing)]
>    merge c x Nothing = Nothing
>    merge c Nothing y = Nothing
>    merge "s2" (Just (Def_IS id1 s11 s12)) (Just (Def_IS id2 s21 s22)) = Just (Def_IS id1 s11 s22) || first definition with second s2
>    merge "s1" (Just (Def_IS id1 s11 s12)) (Just (Def_IS id2 s21 s22)) = Just (Def_IS id2 s11 s22) || second definition with first s1
>    maybedefinition2maybecomponent ops = filter ((~=Nothing).snd) (map g ops)
>                                         where
>                                         g (rem, Nothing) = (rem, Nothing)
>                                         g (rem, Just d)  = (rem, Just (CompDefinition d))

>|| Parsing the first subject into a defintion
>parse_s12d :: t_parser token (maybe t_definition)
>parse_s12d = maybesubject2maybedefinition . (then f parse_THE_subject parse_subject)
>             where
>             f x       Nothing = Nothing
>             f Nothing y       = y
>             f x       y       = y
>             maybesubject2maybedefinition ops = filter ((~=Nothing).snd) (map g ops)
>                                                where
>                                                g (rem, Nothing) = (rem, Nothing)
>                                                g (rem, Just s1) = (rem, Just (Def_IS empty_ID s1 empty_subject))
>|| Parsing the second subject into a defintion
>parse_s22d :: t_parser token (maybe t_definition)
>parse_s22d = maybesubject2maybedefinition . (then f parse_THE_subject parse_subject)
>             where
>             f x       Nothing = Nothing
>             f Nothing y       = y
>             f x       y       = y
>             maybesubject2maybedefinition ops = filter ((~=Nothing).snd) (map g ops)
>                                                where
>                                                g (rem, Nothing) = (rem, Nothing)
>                                                g (rem, Just s2) = (rem, Just (Def_IS empty_ID empty_subject s2))

>parse_compID_defIS :: t_parser token (maybe t_definition)
>parse_compID_defIS ((CompID x): rest) = [(rest, Just (Def_IS x empty_subject empty_subject))]
>parse_compID_defIS any                = [(any, Nothing)]

>parse_IS :: t_parser token (maybe t_definition)
>parse_IS (ComponentDEFIS:rest) = [(rest,Just (Def_IS empty_ID empty_subject empty_subject))]
>parse_IS any                   = [(any, Nothing)]

>parse_simpledefinitionEQ :: t_parser token (maybe t_component)
>parse_simpledefinitionEQ =
>    maybedefinition2maybecomponent . (then a parse_compID_defEQ (d_parse "oEQne"))
>    where
>    a x Nothing = Nothing
>    a Nothing y = Nothing
>    a (Just (Def_EQ id1 o1 n1)) (Just (Def_EQ id2 o2 n2)) = Just (Def_EQ id1 o2 n2) || Def_EQ2 but CompID1 overrides
>    d_parse "oEQne"  = then (merge "o") parse_o2d (then (merge "ne") parse_EQ parse_ne2d)
>    d_parse any = g
>    g ip = [([], Nothing)]
>    merge c x Nothing = Nothing
>    merge c Nothing y = Nothing
>    merge "ne"  (Just (Def_EQ id1 o1 n1)) (Just (Def_EQ id2 o2 n2)) = Just (Def_EQ id1 o1 n2)
>    merge "o"   (Just (Def_EQ id1 o1 n1)) (Just (Def_EQ id2 o2 n2)) = Just (Def_EQ id2 o1 n2)
>    maybedefinition2maybecomponent ops = filter ((~=Nothing).snd) (map g ops)
>                                         where
>                                         g (rem, Nothing) = (rem, Nothing)
>                                         g (rem, Just d)  = (rem, Just (CompDefinition d))

>|| Parsing the object into a defintion
>parse_o2d :: t_parser token (maybe t_definition)
>parse_o2d = maybeobject2maybedefinition . (then f (alt (parse_A) (parse_AN)) parse_object)
>            where
>            f x       Nothing = Nothing
>            f Nothing y       = y
>            f x       y       = y
>            maybeobject2maybedefinition ops = filter ((~=Nothing).snd) (map g ops)
>                                              where
>                                              g (rem, Nothing) = (rem, Nothing)
>                                              g (rem, Just o) = (rem, Just (Def_EQ empty_ID o NoNumericalExpr))

>|| Parsing the numerical expression into a defintion
>parse_ne2d :: t_parser token (maybe t_definition)
>parse_ne2d = maybenumericalexpr2maybedefinition . parse_numercialexpr
>             where
>             maybenumericalexpr2maybedefinition ops = filter ((~=Nothing).snd) (map g ops)
>                                                      where
>                                                      g (rem, Nothing) = (rem, Nothing)
>                                                      g (rem, Just ne) = (rem, Just (Def_EQ empty_ID empty_object ne))

>parse_numercialexpr :: t_parser token (maybe t_numericalexpr)
>parse_numercialexpr ((NumericalExpr ne): rest) = [(rest, Just (ne))]
>parse_numercialexpr any                        = [(any, Nothing)]

>parse_compID_defEQ :: t_parser token (maybe t_definition)
>parse_compID_defEQ ((CompID x): rest) = [(rest, Just (Def_EQ x empty_object NoNumericalExpr))]
>parse_compID_defEQ any                = [(any, Nothing)]

>parse_EQ :: t_parser token (maybe t_definition)
>parse_EQ (ComponentDEFEQ:rest) = [(rest,Just (Def_EQ empty_ID empty_object NoNumericalExpr))]
>parse_EQ any                   = [(any, Nothing)]

4. Expressional definitions

    BNF description:
    <expressional-defintion> :: <definition> IF <expression> | IF <expression> THEN <definition>
    <expression> :: <simple-expression> | <simple-expression> AND <simple-expression>* | <simple-expression> OR <simple-expression>*
    <simple-expression> :: <ID> <holds>? <subject> <verb-status> <comparison> <subject>

>t_expression ::= Expression t_ID t_holds t_subject t_verb_status t_comparison t_subject | Expressions_or [t_expression] | Expressions_and [t_expression]
>t_comparison ::= LessThan | EqualTo | MoreThan | NoComparison
>empty_comparison = NoComparison

>parse_expressional_definition :: t_parser token (maybe t_component)
>parse_expressional_definition
>  = alt (then f parse_definition (then h parse_IF parse_expression)) (then g (then h parse_IF parse_expression) (then h parse_THEN parse_definition))
>    where
>    f Nothing                   y                         = Nothing
>    f x                         Nothing                   = Nothing
>    f (Just (CompDefinition x)) (Just (CompExpression y)) = Just (CompExprDefinition y x)
>    g Nothing                   y                         = Nothing
>    g x                         Nothing                   = Nothing
>    g (Just (CompExpression y)) (Just (CompDefinition x)) = Just (CompExprDefinition y x)
>    h Nothing                   y                         = Nothing
>    h x                         Nothing                   = Nothing
>    h x                         y                         = y

>parse_expression :: t_parser token (maybe t_component)
>parse_expression = alt parse_simpleexpression (alt (then f parse_simpleexpression (then g parse_componentOR parse_expression)) (then h parse_simpleexpression (then g parse_componentAND parse_expression)))
>                   where
>                   f Nothing any = Nothing
>                   f any Nothing = Nothing
>                   f (Just (CompExpression x)) (Just (CompExpression (Expressions_or y))) = Just (CompExpression (Expressions_or (x:y)))
>                   f (Just (CompExpression x)) (Just (CompExpression y)) = Just (CompExpression (Expressions_or [x, y]))
>                   g Nothing any = Nothing
>                   g any Nothing = Nothing
>                   g x y = y
>                   h Nothing any = Nothing
>                   h any Nothing = Nothing
>                   h (Just (CompExpression x)) (Just (CompExpression (Expressions_and y))) = Just (CompExpression (Expressions_and (x:y)))
>                   h (Just (CompExpression x)) (Just (CompExpression y)) = Just (CompExpression (Expressions_and [x, y]))

>parse_componentOR :: t_parser token (maybe t_component)
>parse_componentOR ((ComponentOR):rest) = [(rest, Just (TrueComponent))]
>parse_componentOR any                  = [(any, Nothing)]

>parse_componentAND :: t_parser token (maybe t_component)
>parse_componentAND ((ComponentAND):rest) = [(rest, Just (TrueComponent))]
>parse_componentAND any                   = [(any, Nothing)]

>parse_simpleexpression :: t_parser token (maybe t_component)
>parse_simpleexpression = maybeexpression2maybecomponent . (then a parse_compID_expression (then b parse_holds_expression (e_parse "svcs")))
>                         where
>                         a Nothing any = Nothing
>                         a any Nothing = Nothing
>                         a (Just (Expression id1 h1 s11 vs1 c1 s12)) (Just (Expression id2 h2 s21 vs2 c2 s22)) = Just (Expression id1 h2 s21 vs2 c2 s22)
>                         b Nothing any = any || holds is optional
>                         b any Nothing = Nothing
>                         b (Just (Expression id1 h1 s11 vs1 c1 s12)) (Just (Expression id2 h2 s21 vs2 c2 s22)) = Just (Expression id2 h1 s21 vs2 c2 s22)
>                         e_parse "svcs" = then (merge "s1") parse_s12e (then (merge "cs2") parse_v2e (then (merge "s2") parse_c2e parse_s22e))
>                         e_parse any    = g
>                         g ip = [([], Nothing)]
>                         merge c x Nothing = Nothing
>                         merge c Nothing y = Nothing
>                         merge "s2"  (Just (Expression id1 h1 s11 vs1 c1 s12)) (Just (Expression id2 h2 s21 vs2 c2 s22)) = Just (Expression id1 h1 s11 vs1 c1 s22)
>                         merge "cs2" (Just (Expression id1 h1 s11 vs1 c1 s12)) (Just (Expression id2 h2 s21 vs2 c2 s22)) = Just (Expression id1 h1 s11 vs1 c2 s22)
>                         merge "s1"  (Just (Expression id1 h1 s11 vs1 c1 s12)) (Just (Expression id2 h2 s21 vs2 c2 s22)) = Just (Expression id2 h2 s11 vs2 c2 s22)
>                         maybeexpression2maybecomponent ops = filter ((~=Nothing).snd) (map g ops)
>                                                              where
>                                                              g (rem, Nothing) = (rem, Nothing)
>                                                              g (rem, Just e)  = (rem, Just (CompExpression e))

>parse_holds_expression :: t_parser token (maybe t_expression)
>parse_holds_expression (ComponentAffirmation:rest) = [(rest,Just (Expression empty_ID Holds empty_subject empty_verb_status empty_comparison empty_subject))]
>parse_holds_expression (ComponentNegation:rest)    = [(rest,Just (Expression empty_ID NotHolds empty_subject empty_verb_status empty_comparison empty_subject))]
>parse_holds_expression any                         = [(any,Nothing)]

>parse_compID_expression :: t_parser token (maybe t_expression)
>parse_compID_expression ((CompID x): rest) = [(rest, Just (Expression x empty_holds empty_subject empty_verb_status empty_comparison empty_subject))]
>parse_compID_expression any                = [(any, Nothing)]

>|| Parsing the first subject into an expression
>parse_s12e :: t_parser token (maybe t_expression)
>parse_s12e = maybesubject2maybeexpression . (then f parse_THE_subject parse_subject)
>             where
>             f x       Nothing = Nothing
>             f Nothing y       = y
>             f x       y       = y
>             maybesubject2maybeexpression ops = filter ((~=Nothing).snd) (map g ops)
>                                                where
>                                                g (rem, Nothing) = (rem, Nothing)
>                                                g (rem, Just s1) = (rem, Just (Expression empty_ID empty_holds s1 empty_verb_status empty_comparison empty_subject))

>|| Parsing a verb status into an expression
>parse_v2e :: t_parser token (maybe t_expression)
>parse_v2e = maybeverb_status2maybeexpression . parse_verb_status
>            where
>            maybeverb_status2maybeexpression ops = filter ((~=Nothing).snd) (map g ops)
>                                                   where
>                                                   g (rem, Nothing) = (rem, Nothing)
>                                                   g (rem, Just vs) = (rem, Just (Expression empty_ID empty_holds empty_subject vs empty_comparison empty_subject))

>|| Parsing a comparison into an expression
>parse_c2e :: t_parser token (maybe t_expression)
>parse_c2e = maybecomparison2maybeexpression . parse_comparison
>            where
>            maybecomparison2maybeexpression ops = filter ((~=Nothing).snd) (map g ops)
>                                                  where
>                                                  g (rem, Nothing) = (rem, Nothing)
>                                                  g (rem, Just c)  = (rem, Just (Expression empty_ID empty_holds empty_subject empty_verb_status c empty_subject))

>parse_comparison :: t_parser token (maybe t_comparison)
>parse_comparison ((Comparison x):rest) = [(rest, Just (x))]
>parse_comparison any                   = [(any, Nothing)]

>|| Parsing the second subject into an expression
>parse_s22e :: t_parser token (maybe t_expression)
>parse_s22e = maybesubject2maybeexpression . (then f parse_THE_subject parse_subject)
>             where
>             f x       Nothing = Nothing
>             f Nothing y       = y
>             f x       y       = y
>             maybesubject2maybeexpression ops = filter ((~=Nothing).snd) (map g ops)
>                                                where
>                                                g (rem, Nothing) = (rem, Nothing)
>                                                g (rem, Just s2) = (rem, Just (Expression empty_ID empty_holds empty_subject empty_verb_status empty_comparison s2))

4. Statements

    BNF description:
    <statement> :: <simplestatement> | <simplestatement> OR <simplestatement>* | <simplestatement> AND <simplestatement>*
    <simplestatement> :: <ID> <holds>? <statement-type> <date-phrase> <subject-phrase> <verb> <object-phrase>
                       | <ID> <holds>? <statement-type> <date-phrase> <object-phrase> <verb> <subject-phrase>
                       | <ID> <holds>? <statement-type> <subject-phrase> <date-phrase> <verb> <object-phrase>
                       | <ID> <holds>? <statement-type> <subject-phrase> <verb> <object-phrase> <date-phrase>
                       | <ID> <holds>? <statement-type> <object-phrase> <verb> <subject-phrase> <date-phrase>
                       | <ID> <holds>? <statement-type> <object-phrase> <verb> <date-phrase> <subject-phrase>
                       | <ID> <holds>? <statement-type> <subject-phrase> <verb> <object-phrase>
                       | <ID> <holds>? <statement-type> <object-phrase> <verb> <subject-phrase>

    <statement-type> :: Obligation | Permission | Prohibition
    <date-phrase>    :: ON THE <day> <month> <year>
    <subject-phrase> :: THE <subject>
    <object-phrase>  :: A <object> | AN <object>
    <verb>           :: DELIVER | PAY | CHARGE

    When parsing, we parse the <statement-type> and <verb> together, using <verb-phrase>
    <verb-phrase>    :: SHALL <verb> | MAY <verb> | FORBIDDEN <verb>

    Note how the CNL allows different ways of expressing the same thing

>t_statement ::= Statement t_ID t_holds t_statement_type t_date t_subject t_verb t_object | Statements_or [t_statement] | Statements_and [t_statement]
>t_ID == [char]
>t_holds ::= Holds | NotHolds | UnknownHolds
>t_statement_type ::= Obligation | Permission | Prohibition | NoStatementType || We need NoType during intermediate parsing
>t_date == (num,num,num) || day, month, year
>t_other_date ::= ANYDATE | ADATE | THEDATE | NoOtherDate                       || NEW
>t_num == num
>t_subject == [char]
>t_object ::= Pounds num | Dollars num | Euros num | SomeCurrency [char] | Report [char] | NamedObject [char] | OtherObject [char] | NoObject || We need NoObject during intermediate parsing
>t_verb_phrase == (t_statement_type, t_verb)
>t_verb ::= Deliver | Pay | Charge | NoVerb || We need NoVerb during intermediate parsing

>empty_statement :: t_statement
>empty_statement = Statement empty_ID empty_holds empty_statement_type empty_date empty_subject empty_verb empty_object
>empty_ID = ""
>empty_holds :: t_holds
>empty_holds = UnknownHolds
>empty_statement_type :: t_statement_type
>empty_statement_type = NoStatementType
>empty_date :: t_date
>empty_date = (0,0,0)
>empty_other_date :: t_other_date   || NEW
>empty_other_date = NoOtherDate     || NEW
>empty_subject :: t_subject
>empty_subject = ""
>empty_verb :: t_verb
>empty_verb = NoVerb
>empty_object :: t_object
>empty_object = NoObject

>parse_statement :: t_parser token (maybe t_component)
>parse_statement = alt parse_simplestatement (alt (then f parse_simplestatement (then g parse_componentOR parse_statement)) (then h parse_simplestatement (then g parse_componentAND parse_statement)))
>                   where
>                   f Nothing                   any                                          = Nothing
>                   f any                       Nothing                                      = Nothing
>                   f (Just (CompStatement x))  (Just (CompStatement (Statements_or y)))     = Just (CompStatement (Statements_or (x:y)))
>                   f (Just (CompStatement x))  (Just (CompStatement y))                     = Just (CompStatement (Statements_or [x, y]))
>                   g Nothing                   any                                          = Nothing
>                   g any                       Nothing                                      = Nothing
>                   g x                         y                                            = y
>                   h Nothing                   any                                          = Nothing
>                   h any                       Nothing                                      = Nothing
>                   h (Just (CompStatement x))  (Just (CompStatement (Statements_and y)))    = Just (CompStatement (Statements_and (x:y)))
>                   h (Just (CompStatement x))  (Just (CompStatement y))                     = Just (CompStatement (Statements_and [x, y]))

    Code for parse_simplestatement shows a specific coding style (collecting intermediate results into a t_statement
    and merging them progressively):

>parse_simplestatement :: t_parser token (maybe t_component)
>parse_simplestatement = maybestatement2maybecomponent . (then a parse_compID_statement (then f parse_holds_statement (alt (s_parse "dsvo")
>                                                                                                                          (alt (s_parse "dovs")
>                                                                                                                               (alt (s_parse "sdvo")
>                                                                                                                                    (alt (s_parse "svod")
>                                                                                                                                         (alt (s_parse "ovsd") (s_parse "ovds"))))))))
>                        where
>                        a Nothing any = Nothing
>                        a any Nothing = Nothing
>                        a (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id1 h2 ty2 da2 su2 ve2 ob2) || s2 but s1 CompID overrides
>                        f Nothing any = Nothing
>                        f any Nothing = Nothing
>                        f (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id2 h1 ty2 da2 su2 ve2 ob2) || s2 but s1 hold overrides
>                        s_parse "dsvo" = then (merge "D") parse_d (then (merge "vo") parse_s (then (merge "o") parse_v parse_o))
>                        s_parse "dovs" = then (merge "D") parse_d (then (merge "vs") parse_o (then (merge "s") parse_v parse_s))
>                        s_parse "sdvo" = then (merge "S") parse_s (then (merge "vo") parse_d (then (merge "o") parse_v parse_o))
>                        s_parse "svod" = then (merge "S") parse_s (then (merge "od") parse_v (then (merge "d") parse_o parse_d))
>                        s_parse "ovsd" = then (merge "O") parse_o (then (merge "sd") parse_v (then (merge "d") parse_s parse_d))
>                        s_parse "ovds" = then (merge "O") parse_o (then (merge "sd") parse_v (then (merge "s") parse_d parse_s))
>                        s_parse any    = g
>                        g ip = [([], Nothing)]
>                        merge c    x                                      Nothing                                = Nothing
>                        merge c    Nothing                                y                                      = Nothing
>                        merge "o"  (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id1 h1 ty1 da1 su1 ve1 ob2) || keep h1
>                        merge "s"  (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id1 h1 ty1 da1 su2 ve1 ob1) || keep h1
>                        merge "d"  (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id1 h1 ty1 da2 su1 ve1 ob1) || keep h1
>                        merge "vo" (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id1 h1 ty2 da1 su1 ve2 ob2) || keep h1
>                        merge "vs" (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id1 h1 ty2 da1 su2 ve2 ob1) || keep h1
>                        merge "od" (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id1 h1 ty1 da2 su1 ve1 ob2) || keep h1
>                        merge "sd" (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id1 h1 ty1 da2 su2 ve1 ob1) || keep h1
>                        merge "D"  (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id2 h2 ty2 da1 su2 ve2 ob2) || keep h2
>                        merge "S"  (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id2 h2 ty2 da2 su1 ve2 ob2) || keep h2
>                        merge "O"  (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id2 h2 ty2 da2 su2 ve2 ob1) || keep h2
>                        maybestatement2maybecomponent ops = filter ((~=Nothing).snd) (map g ops)
>                                                            where
>                                                            g (rem, Nothing) = (rem, Nothing)
>                                                            g (rem, Just s)  = (rem, Just (CompStatement s))

>parse_holds_statement :: t_parser token (maybe t_statement)
>parse_holds_statement (ComponentAffirmation:rest) = [(rest,Just (Statement empty_ID Holds empty_statement_type empty_date empty_subject empty_verb empty_object))]
>parse_holds_statement (ComponentNegation:rest)    = [(rest,Just (Statement empty_ID NotHolds empty_statement_type empty_date empty_subject empty_verb empty_object))]
>parse_holds_statement any                         = [(any,Nothing)]

>parse_compID_statement :: t_parser token (maybe t_statement)
>parse_compID_statement ((CompID x): rest) = [(rest, Just (Statement x empty_holds empty_statement_type empty_date empty_subject empty_verb empty_object))]
>parse_compID_statement any                = [(any, Nothing)]

>|| In the following code notice how h encodes the year, month and day backwards: day + 1000*(month + 1000*year)
>|| and g decodes this to a t_date, and finally the whole t_date result is converted into a t_statement for use above.
>|| parse_THE, parse_day parse_month and parse_year must all return type t_num so they can be arguments to "then"
>|| parse_ON_date returns type t_date so it can be used with the result of g as arguments to "then"
>parse_d :: t_parser token (maybe t_statement) || date_phrase
>parse_d = maybedate2maybestatement.(then f parse_ON_date (then g parse_THE_date (maybenum2maybedate . (then h parse_day (then h parse_month parse_year)))))
>          where
>          f x       Nothing  = Nothing
>          f Nothing y        = Nothing
>          f x       y        = y
>          g x       Nothing  = Nothing
>          g Nothing y        = Nothing
>          g x       y        = y
>          h Nothing y        = Nothing
>          h x       Nothing  = Nothing
>          h (Just x)(Just y) = Just (x + y*1000)
>          maybenum2maybedate ops = filter ((~=Nothing).snd) (map g ops)
>                                   where
>                                   g (rem, Nothing) = (rem, Nothing)
>                                   g (rem, Just y)  = (rem, Just (y mod 1000, (y div 1000) mod 1000, (y div 1000000)))
>          maybedate2maybestatement ops = filter ((~=Nothing).snd) (map g ops)
>                                         where
>                                         g (rem, Nothing) = (rem, Nothing)
>                                         g (rem, Just d)  = (rem, Just (Statement empty_ID empty_holds empty_statement_type d empty_subject empty_verb empty_object))





>parse_ON_date :: t_parser token (maybe t_date)
>parse_ON_date (ComponentON:rest) = [(rest,Just empty_date)] || the value is not used
>parse_ON_date any                = [(any, Nothing)]

>parse_THE_date :: t_parser token (maybe t_date)
>parse_THE_date (ComponentTHE:rest) = [(rest,Just empty_date)] || the value is not used
>parse_THE_date any                 = [(any, Nothing)]

>parse_THE_subject :: t_parser token (maybe t_subject)
>parse_THE_subject (ComponentTHE:rest) = [(rest,Just empty_subject)] || the value is not used
>parse_THE_subject any                 = [(any, Nothing)]

>parse_day :: t_parser token (maybe t_num)
>parse_day = parse_number

>|| For now we only permit a month to be entered as a number
>parse_month :: t_parser token (maybe t_num)
>parse_month = parse_number

>parse_year :: t_parser token (maybe t_num)
>parse_year = parse_number

>parse_number :: t_parser token (maybe t_num)
>parse_number ((Number x): rest)= [(rest, Just x)]
>parse_number any               = [(any, Nothing)]

>|| The following code deals with the parsing of objects to statements
>parse_o :: t_parser token (maybe t_statement)
>parse_o = maybeobject2maybestatement . (then f (alt (parse_A) (parse_AN)) parse_object)
>          where
>          f x       Nothing = Nothing
>          f Nothing y       = y
>          f x       y       = y
>          maybeobject2maybestatement ops = filter ((~=Nothing).snd) (map g ops)
>                                           where
>                                           g (rem, Nothing) = (rem, Nothing)
>                                           g (rem, Just o)  = (rem, Just (Statement empty_ID empty_holds empty_statement_type empty_date empty_subject empty_verb o))

>parse_A :: t_parser token (maybe t_object)
>parse_A (ComponentA:rest) = [(rest, Just NoObject)] || for intermediate parsing
>parse_A any               = [(any, Nothing)]

>parse_AN :: t_parser token (maybe t_object)
>parse_AN (ComponentAN:rest) = [(rest, Just NoObject)] || for intermediate parsing
>parse_AN any                = [(any, Nothing)]

>parse_object :: t_parser token (maybe t_object)
>parse_object ((Object x):rest) = [(rest, Just (x))]
>parse_object any               = [(any, Nothing)]

>|| The following code deals with the parsing of subjects to statements
>parse_s :: t_parser token (maybe t_statement)
>parse_s = maybesubject2maybestatement . (then f parse_THE_subject parse_subject)
>          where
>          f x       Nothing = Nothing
>          f Nothing y       = y
>          f x       y       = y
>          maybesubject2maybestatement ops = filter ((~=Nothing).snd) (map g ops)
>                                            where
>                                            g (rem, Nothing) = (rem, Nothing)
>                                            g (rem, Just s)  = (rem, Just (Statement empty_ID empty_holds empty_statement_type empty_date s empty_verb empty_object))

>parse_subject :: t_parser token (maybe t_subject)
>parse_subject ((Person x):rest) = [(rest, Just (x))]
>parse_subject any               = [(any, Nothing)]

>|| The following code deals with the parsing of verbs to statements
>parse_v :: t_parser token (maybe t_statement)
>parse_v = maybeverb_phrase2maybestatement . (then f parse_MODAL parse_verb)
>          where
>          f x              Nothing        = Nothing
>          f Nothing        y              = Nothing
>          f (Just (x1,y1)) (Just (x2,y2)) = Just (x1, y2)
>          maybeverb_phrase2maybestatement ops = filter ((~=Nothing).snd) (map g ops)
>                                                where
>                                                g (rem, Nothing) = (rem, Nothing)
>                                                g (rem, Just (st_type, verb)) = (rem, Just (Statement empty_ID empty_holds st_type empty_date empty_subject verb empty_object))

>parse_MODAL :: t_parser token (maybe t_verb_phrase)
>parse_MODAL ((Modal_verb x):rest) = [(rest, Just (x, NoVerb))]
>parse_MODAL any                   = [(any, Nothing)]

>parse_verb :: t_parser token (maybe t_verb_phrase)
>parse_verb ((Verb x):rest) = [(rest, Just (NoStatementType, x))]
>parse_verb any             = [(any, Nothing)]

6. Conditional statements

    BNF description:
    <conditional-statement> :: <statement> IF <condition> | IF <condition> THEN <statement>
    We are also allowing: IF <statement> THEN <statement> in order to capture ISDA 2c.

    <condition> :: <simplecondition> | <simplecondition> OR <simplecondition>* | <simplecondition> AND <simplecondition>*
    <simplecondition> :: <ID> <holds>? <date-phrase> <subject-phrase> <verb-status-phrase> <object-phrase>
                       | <ID> <holds>? <date-phrase> <object-phrase> <verb-status-phrase> <subject-phrase>
                       | <ID> <holds>? <subject-phrase> <date-phrase> <verb-status-phrase> <object-phrase>
                       | <ID> <holds>? <subject-phrase> <verb-status-phrase> <object-phrase> <date-phrase>
                       | <ID> <holds>? <object-phrase> <verb-status-phrase> <subject-phrase> <date-phrase>
                       | <ID> <holds>? <object-phrase> <verb-status-phrase> <date-phrase> <subject-phrase>
                       | <ID> <holds>? <subject-phrase> <verb-status-phrase> <object-phrase>
                       | <ID> <holds>? <object-phrase> <verb-status-phrase> <subject-phrase>

   <date-phrase>        :: ON THE <day> <month> <year> || for reference
   <subject-phrase>     :: THE <subject>               || for reference
   <object-phrase>      :: A <object> | AN <object>    || for reference
   <verb-status-phrase> :: PAID | DELIVERED | CHARGED

   We use some of the same definitions as in <statement>.

>t_condition ::= Condition t_ID t_holds t_date t_subject t_verb_status t_object | Conditions_or [t_condition] | Conditions_and [t_condition]
>t_verb_status ::= Delivered | Paid | Charged | NoStatus
>empty_condition :: t_condition
>empty_condition = Condition empty_ID empty_holds empty_date empty_subject empty_verb_status empty_object
>empty_verb_status :: t_verb_status
>empty_verb_status = NoStatus

>parse_conditional_statement :: t_parser token (maybe t_component)
>parse_conditional_statement
>  = alt (then f parse_statement (then h parse_IF parse_condition)) (alt (then g (then h parse_IF parse_condition) (then h parse_THEN parse_statement)) (then j (then h parse_IF parse_statement) (then h parse_THEN parse_statement)))
>    where
>    f Nothing                  y                        = Nothing
>    f x                        Nothing                  = Nothing
>    f (Just (CompStatement x)) (Just (CompCondition y)) = Just (CompCondStatement y x)
>    g Nothing                  y                        = Nothing
>    g x                        Nothing                  = Nothing
>    g (Just (CompCondition y)) (Just (CompStatement x)) = Just (CompCondStatement y x)
>    h Nothing                  y                        = Nothing
>    h x                        Nothing                  = Nothing
>    h x                        y                        = y
>    j Nothing                  y                        = Nothing
>    j x                        Nothing                  = Nothing
>    j (Just (CompStatement y)) (Just (CompStatement x)) = Just (CompStatStatement y x)

>parse_condition :: t_parser token (maybe t_component)
>parse_condition = alt parse_simplecondition (alt (then f parse_simplecondition (then g parse_componentOR parse_condition)) (then h parse_simplecondition (then g parse_componentAND parse_condition)))
>                   where
>                   f Nothing                   any                                          = Nothing
>                   f any                       Nothing                                      = Nothing
>                   f (Just (CompCondition x))  (Just (CompCondition (Conditions_or y)))     = Just (CompCondition (Conditions_or (x:y)))
>                   f (Just (CompCondition x))  (Just (CompCondition y))                     = Just (CompCondition (Conditions_or [x, y]))
>                   g Nothing                   any                                          = Nothing
>                   g any                       Nothing                                      = Nothing
>                   g x                         y                                            = y
>                   h Nothing                   any                                          = Nothing
>                   h any                       Nothing                                      = Nothing
>                   h (Just (CompCondition x))  (Just (CompCondition (Conditions_and y)))    = Just (CompCondition (Conditions_and (x:y)))
>                   h (Just (CompCondition x))  (Just (CompCondition y))                     = Just (CompCondition (Conditions_and [x, y]))

   Code for parse_simplecondition. The dynamic aspect was attempted to be used for parsing what looks like a statement but is indeed a condition (see start of ISDA 2c).

>parse_simplecondition :: t_parser token (maybe t_component)
>parse_simplecondition = maybecondition2maybecomponent . (then a parse_compID_condition (then f parse_holds_condition (alt (s_parse "dsvo")
>                                                                                                                          (alt (s_parse "dovs")
>                                                                                                                               (alt (s_parse "sdvo")
>                                                                                                                                    (alt (s_parse "svod")
>                                                                                                                                         (alt (s_parse "ovsd") (s_parse "ovds"))))))))
>                        where
>                        a Nothing any = Nothing
>                        a any Nothing = Nothing
>                        a (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id1 h2 da2 su2 vs2 ob2)
>                        f Nothing any = Nothing
>                        f any Nothing = Nothing
>                        f (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id1 h1 da2 su2 vs2 ob2)
>                        s_parse "dsvo" = then (merge "D") parse_d2c (then (merge "vo") parse_s2c (then (merge "o") parse_v2c parse_o2c))
>                        s_parse "dovs" = then (merge "D") parse_d2c (then (merge "vs") parse_o2c (then (merge "s") parse_v2c parse_s2c))
>                        s_parse "sdvo" = then (merge "S") parse_s2c (then (merge "vo") parse_d2c (then (merge "o") parse_v2c parse_o2c))
>                        s_parse "svod" = then (merge "S") parse_s2c (then (merge "od") parse_v2c (then (merge "d") parse_o2c parse_d2c))
>                        s_parse "ovsd" = then (merge "O") parse_o2c (then (merge "sd") parse_v2c (then (merge "d") parse_s2c parse_d2c))
>                        s_parse "ovds" = then (merge "O") parse_o2c (then (merge "sd") parse_v2c (then (merge "s") parse_d2c parse_s2c))
>                        s_parse any    = g
>                        g ip = [([], Nothing)]
>                        merge c    x                                      Nothing                        = Nothing
>                        merge c    Nothing                                y                              = Nothing
>                        merge "o"  (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id1 h1 da1 su1 vs1 ob2)
>                        merge "s"  (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id1 h1 da1 su2 vs1 ob1)
>                        merge "d"  (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id1 h1 da2 su1 vs1 ob1)
>                        merge "vo" (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id1 h1 da1 su1 vs2 ob2)
>                        merge "vs" (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id1 h1 da1 su2 vs2 ob1)
>                        merge "od" (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id1 h1 da2 su1 vs1 ob2)
>                        merge "sd" (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id1 h1 da2 su2 vs1 ob1)
>                        merge "D"  (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id2 h2 da1 su2 vs2 ob2)
>                        merge "S"  (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id2 h2 da2 su1 vs2 ob2)
>                        merge "O"  (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id2 h2 da2 su2 vs2 ob1)
>                        maybecondition2maybecomponent ops = filter ((~=Nothing).snd) (map g ops)
>                                                            where
>                                                            g (rem, Nothing) = (rem, Nothing)
>                                                            g (rem, Just x)  = (rem, Just (CompCondition x))

>parse_holds_condition :: t_parser token (maybe t_condition)
>parse_holds_condition (ComponentAffirmation:rest) = [(rest,Just (Condition empty_ID Holds empty_date empty_subject empty_verb_status empty_object))]
>parse_holds_condition (ComponentNegation:rest)    = [(rest,Just (Condition empty_ID NotHolds empty_date empty_subject empty_verb_status empty_object))]
>parse_holds_condition any                         = [(any,Nothing)]

>parse_compID_condition :: t_parser token (maybe t_condition)
>parse_compID_condition ((CompID x): rest) = [(rest, Just (Condition x empty_holds empty_date empty_subject empty_verb_status empty_object))]
>parse_compID_condition any                = [(any, Nothing)]

>|| The following code deals with the parsing of dates into conditions
>parse_d2c :: t_parser token (maybe t_condition) || date_phrase
>parse_d2c = maybedate2maybecondition . (then f parse_ON_date  (then g parse_THE_date (maybenum2maybedate . (then h parse_day (then h parse_month parse_year)))))
>            where
>            f x       Nothing = Nothing
>            f Nothing y       = Nothing
>            f x       y       = y
>            g x       Nothing = Nothing
>            g Nothing y       = Nothing
>            g x       y       = y
>            h x       Nothing = Nothing
>            h Nothing y       = Nothing
>            h (Just x)(Just y)= Just (x + y*1000)
>            maybenum2maybedate ops = filter ((~=Nothing).snd) (map g ops)
>                                     where
>                                     g (rem, Nothing) = (rem, Nothing)
>                                     g (rem, Just y)  = (rem, Just (y mod 1000, (y div 1000) mod 1000, (y div 1000000)))
>            maybedate2maybecondition ops = filter ((~=Nothing).snd) (map g ops)
>                                           where
>                                           g (rem, Nothing) = (rem, Nothing)
>                                           g (rem, Just d)  = (rem, Just (Condition empty_ID empty_holds d empty_subject empty_verb_status empty_object))

>|| The following code deals with the parsing of objects to conditions
>parse_o2c :: t_parser token (maybe t_condition)
>parse_o2c = maybeobject2maybecondition . (then f (alt (parse_A) (parse_AN)) parse_object)
>            where
>            f x       Nothing = Nothing
>            f Nothing y       = y
>            f x       y       = y
>            maybeobject2maybecondition ops = filter ((~=Nothing).snd) (map g ops)
>                                             where
>                                             g (rem, Nothing) = (rem, Nothing)
>                                             g (rem, Just o)  = (rem, Just (Condition empty_ID empty_holds empty_date empty_subject empty_verb_status o))

>|| The following code deals with the parsing of subjects to conditions
>parse_s2c :: t_parser token (maybe t_condition)
>parse_s2c = maybesubject2maybecondition . (then f parse_THE_subject parse_subject)
>            where
>            f x       Nothing = Nothing
>            f Nothing y       = y
>            f x       y       = y
>            maybesubject2maybecondition ops = filter ((~=Nothing).snd) (map g ops)
>                                              where
>                                              g (rem, Nothing) = (rem, Nothing)
>                                              g (rem, Just s)  = (rem, Just (Condition empty_ID empty_holds empty_date s empty_verb_status empty_object))

>|| The following code deals with the parsing of a verb status to a condition
>parse_v2c :: t_parser token (maybe t_condition)
>parse_v2c = maybeverb_status2maybecondition . parse_verb_status
>            where
>            maybeverb_status2maybecondition ops = filter ((~=Nothing).snd) (map g ops)
>                                                  where
>                                                  g (rem, Nothing) = (rem, Nothing)
>                                                  g (rem, Just vs)  = (rem, Just (Condition empty_ID empty_holds empty_date empty_subject vs empty_object))

>parse_verb_status :: t_parser token (maybe t_verb_status)
>parse_verb_status ((Verb_status x):rest) = [(rest, Just (x))]
>parse_verb_status any                    = [(any, Nothing)]

>|| The following code deals with the parsing of IF into components
>parse_IF :: t_parser token (maybe t_component)
>parse_IF = maybecondition2maybecomponent . p_IF
>           where
>           p_IF (ComponentIF:rest) = [(rest, Just empty_condition)]
>           p_IF any                = [(any, Nothing)]
>           maybecondition2maybecomponent ops
>             = filter ((~=Nothing).snd) (map g ops)
>               where
>               g (rem, Nothing) = (rem, Nothing)
>               g (rem, Just x)  = (rem, Just (CompCondition x))

>|| The following code deals with the parsing of THEN into components
>parse_THEN :: t_parser token (maybe t_component)
>parse_THEN = maybestatement2maybecomponent . p_THEN
>             where
>             p_THEN (ComponentTHEN:rest) = [(rest, Just empty_statement)]
>             p_THEN any                  = [(any, Nothing)]
>             maybestatement2maybecomponent ops
>               = filter ((~=Nothing).snd) (map g ops)
>                 where
>                 g (rem, Nothing) = (rem, Nothing)
>                 g (rem, Just x)  = (rem, Just (CompStatement x))

    Here are the tokens we expect from the lexer:

>token ::= ContractAND | ComponentOR | ComponentAND | ComponentIF | ComponentTHEN | ComponentDEFIS | ComponentDEFEQ | Word [char] | Number num
>          | ComponentON | ComponentTHE | ComponentA | ComponentAN | Modal_verb t_statement_type | Verb t_verb | Object t_object | Person [char]
>          | Verb_status t_verb_status | ComponentAffirmation | ComponentNegation | CompID [char] | Comparison t_comparison | NumericalExpr t_numericalexpr
>          | Operator t_operator

>t_modal ::= May | Shall | Forbidden

>pos (x:xs) x           = 0
>pos (x:xs) y           = 1 + (pos xs y)
>modalverbs             = ["may", "shall", "must", "is forbidden to"]
>modalconstructors      = [May, Shall, Shall, Forbidden] || duplicates for lexer indexing
>verbs                  = ["pay", "pays", "deliver", "delivers", "charge", "charges"]
>verbconstructors       = [Pay, Pay, Deliver, Deliver, Charge, Charge] || duplicates for lexer indexing
>verbstatuses           = ["paid", "delivered", "charged"]
>verbstatusconstructors = [Paid, Delivered, Charged]
>pounds                 = ["pounds", "Pounds", "GBP", "quid", "pounds sterling"]
>dollars                = ["dollars", "Dollars", "USD"]
>euros                  = ["euros", "Euros", "EUR"]
>currenciesconstructors = [Pounds, Pounds, Dollars, Dollars]
>indefarticles          = ["An", "an", "A", "a"]
>defarticles            = ["The", "the"]
>operators              = ["PLUS", "MINUS", "TIMES", "DIVIDE"]
>operatorconstructors   = [PLUS, MINUS, TIMES, DIVIDE]
>
>v2t Shall = Obligation
>v2t May = Permission
>v2t Forbidden = Prohibition

>lexer :: [char] -> [token]
>lexer input = concat (map (xlexer . split_into_words) (split_into_sentences input))
>               where
>               xlexer []                                 = []
>               xlexer ([]: rest)                         = xlexer rest        || skip empty words if they occur
>               xlexer ("IF"  :rest)                      = (ComponentIF   : xlexer rest)
>               xlexer ("THEN":rest)                      = (ComponentTHEN : xlexer rest)
>               xlexer ("more":("than":rest))             = ((Comparison MoreThan) : xlexer rest)
>               xlexer ("equal":("to":rest))              = ((Comparison EqualTo) : xlexer rest)
>               xlexer ("less":("than":rest))             = ((Comparison LessThan) : xlexer rest)
>               xlexer ("On":("the":(dd:(mm:(yy:rest))))) = ComponentON : ComponentTHE : (Number (numval dd)) : (Number (conv mm)) : (Number (numval yy)) : xlexer rest
>               xlexer ("on":("the":(dd:(mm:(yy:rest))))) = ComponentON : ComponentTHE : (Number (numval dd)) : (Number (conv mm)) : (Number (numval yy)) : xlexer rest
>               xlexer ("It":("is":("the":("case":("that":rest)))))         = ComponentAffirmation: xlexer rest
>               xlexer ("it":("is":("the":("case":("that":rest)))))         = ComponentAffirmation: xlexer rest
>               xlexer ("It":("is":("not":("the":("case":("that":rest)))))) = ComponentNegation: xlexer rest
>               xlexer ("it":("is":("not":("the":("case":("that":rest)))))) = ComponentNegation: xlexer rest
>               xlexer ("is":("forbidden":("to":rest)))   = (Modal_verb Prohibition):(xlexer rest)
>               xlexer ("AmountA":rest)                            = (Object (SomeCurrency "AmountA")):(xlexer rest)
>               xlexer ("AmountB":rest)                            = (Object (SomeCurrency "AmountB")):(xlexer rest)
>               xlexer ("AmountC":rest)                            = (Object (SomeCurrency "AmountC")):(xlexer rest)
>               xlexer ("some":("amount":("of":rest)))             = (Object (SomeCurrency "SomeAmount")):(xlexer (tl rest)), if ((hd rest)="currency")
>               xlexer ("the":("same":("amount":("of":rest))))     = (Object (SomeCurrency "SomeAmt")):(xlexer (tl rest)), if ((hd rest)="currency")
>               xlexer ("the":("excess":("amount":("of":rest))))   = (Object (SomeCurrency "ExcessAmount")):(xlexer (tl rest)), if ((hd rest)="currency")
>               xlexer ("IS"      :rest)                  = (ComponentDEFIS: xlexer rest)
>               xlexer ("EQUALS"  :rest)                  = (ComponentDEFEQ : NumericalExpr (NumericalExprNum (numval (hd rest))) : xlexer (tl rest)), if ((isdigit (hd (hd rest))) & (tl rest)=[])
>               xlexer ("EQUALS"  :rest)                  = (ComponentDEFEQ : NumericalExpr (NumericalExprObject (OtherObject (hd rest))) : xlexer (tl rest)), if ((isupper (hd rest)) & (tl rest)=[])
>               xlexer ("EQUALS"  :rest)                  = (ComponentDEFEQ : NumericalExpr (NumericalExprExpr (NumericalExprObject (OtherObject (hd rest))) (operatorconstructors!(pos operators (hd (tl rest)))) (NumericalExprNum (numval (hd (tl (tl rest)))))) : xlexer (tl (tl (tl rest)))), if ((member operators (hd (tl rest))) & (isdigit (hd (hd (tl (tl rest))))))
>               xlexer ("EQUALS"  :rest)                  = (ComponentDEFEQ : NumericalExpr (NumericalExprExpr (NumericalExprObject (OtherObject (hd rest))) (operatorconstructors!(pos operators (hd (tl rest)))) (NumericalExprObject (OtherObject (hd (tl (tl rest)))))) : xlexer (tl (tl (tl rest)))), if (member operators (hd (tl rest)))
>               xlexer (word : rest)                      = (CompID (f word)   : (xlexer rest)), if ((hd word)='[') & ((last word)=']') || e.g., [123]
>                                                         = (ContractAND   : (xlexer rest)), if (word="<AND>")
>                                                         = (ComponentOR   : (xlexer rest)), if (word="OR")
>                                                         = (ComponentAND  : (xlexer rest)), if (word="AND")
>                                                         = (ComponentA) : (Object (Report (hd (tl rest)))): (xlexer (tl (tl rest))),
>                                                           if (word="A" \/ word="a") & ((hd rest)="report") & (rest ~= [])
>                                                         = (ComponentAN) : (Object (Report (hd (tl rest)))): (xlexer (tl (tl rest))),
>                                                           if (word="An" \/ word="an") & ((hd rest)="report") & (rest ~= [])
>                                                         = (ComponentA) : (Object (OtherObject (hd rest))): (xlexer (tl rest)),
>                                                           if (word="A" \/ word="a") & (rest ~= [])
>                                                         = (ComponentAN) : (Object (OtherObject (hd rest))): (xlexer (tl rest)),
>                                                           if (word="An" \/ word="an") & (rest ~= [])
>                                                         = error "Sentence ends with indefinite article",
>                                                           if (member indefarticles word) & (rest=[])
>                                                         = (Modal_verb (v2t (modalconstructors!(pos modalverbs word)))):(xlexer rest),
>                                                           if (member modalverbs word)
>                                                         = (Verb (verbconstructors!(pos verbs word))):(xlexer rest),
>                                                           if (member verbs word)
>                                                         = (Verb_status (verbstatusconstructors!(pos verbstatuses word))):(xlexer rest),
>                                                           if (member verbstatuses word)
>                                                         = (Person (hd rest)): (xlexer (tl rest)),
>                                                           if ((member defarticles word) & (rest ~= []) & ~(isdigit (hd (hd rest))) & (isupper (hd rest)))
>                                                         = (Person (word)): (xlexer rest),
>                                                           if (isupper word)
>                                                         = error "Expected The (or the) to be followed by a number.",
>                                                           if (member defarticles word) & (rest=[])
>                                                         = error "Sentence ends with definite article",
>                                                           if (member defarticles word) & (rest=[])
>                                                         = (Object (Pounds (numval word))): (xlexer (tl rest)),
>                                                           if (rest ~=[]) & (isdigit (hd word)) & (member pounds (hd rest))
>                                                         = (Object (Dollars (numval word))): (xlexer (tl rest)),
>                                                           if (rest ~=[]) & (isdigit (hd word)) & (member dollars (hd rest))
>                                                         = (Object (Euros (numval word))): (xlexer (tl rest)),
>                                                           if (rest ~=[]) & (isdigit (hd word)) & (member euros (hd rest))
>                                                         = (Number (numval word)) : (xlexer rest),
>                                                           if (isdigit (hd word))
>                                                         = (Word (word)) : (xlexer rest), otherwise
>                                                           where
>                                                           f word = xf (tl word)
>                                                                    where
>                                                                    xf [] = []
>                                                                    xf [']'] = []
>                                                                    xf (x:xs) = x:(xf xs)
>               conv mm = [1,2,3,4,5,6,7,8,9,10,11,12]!(pos ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"] mm)
>
>
>split :: char -> [char] -> [[char]]
>split c input = xsplit c input []
>                where
>                xsplit c []       []  = []
>                xsplit c []       acc = [acc]
>                xsplit c (c:rest) acc = [acc] ++ (xsplit c rest [])     || in Miranda, if a variable appears >once in a pattern there's an implied equality test
>                xsplit c (x:rest) acc = xsplit c rest (acc ++ [x])

  That function is so flexible that we can now define two specialised versions - one splits a [char] into sentences, and the other splits a sentence into words.
  Each is a partial application of the function "split" (passing only its first argument - so each is a function that needs to be applied to the second argument before it can
  return a result:

>split_into_sentences :: [char] -> [[char]]
>split_into_sentences = split '.'

>split_into_words :: [char] -> [[char]]
>split_into_words = split ' '

  We can also use the build-in function "member" to test whether a thing is a member of a list of things, so we can use this to check for words:

>contains_word :: [[char]] -> [char] -> bool
>contains_word sentence word = member sentence word

>isdigit :: char -> bool
>isdigit x = (code '0') <= (code x) <= (code '9')

>isupper :: [char] -> bool
>isupper name = ((hd name) >= 'A') & ((hd name) <= 'Z')

  Parse functions, in order of appearance:

    Contracts

     parse_contract :: t_parser token (maybe t_contract)
     parse_contractAND :: t_parser token (maybe t_contract)
     parse_component :: t_parser token (maybe t_contract)

    Definitions

     parse_definition :: t_parser token (maybe t_component)
     parse_simpledefinition :: t_parser token (maybe t_component)
     parse_simpledefinitionIS :: t_parser token (maybe t_component)
     parse_s12d :: t_parser token (maybe t_definition)
     parse_s22d :: t_parser token (maybe t_definition)
     parse_compID_defIS :: t_parser token (maybe t_definition)
     parse_IS :: t_parser token (maybe t_definition)
     parse_simpledefinitionEQ :: t_parser token (maybe t_component)
     parse_o2d :: t_parser token (maybe t_definition)
     parse_ne2d :: t_parser token (maybe t_definition)
     parse_numercialexpr :: t_parser token (maybe t_numericalexpr)
     parse_compID_defEQ :: t_parser token (maybe t_definition)
     parse_EQ :: t_parser token (maybe t_definition)

    Expressional Definitions

     parse_expressional_definition :: t_parser token (maybe t_component)
     parse_expression :: t_parser token (maybe t_component)
     parse_componentOR :: t_parser token (maybe t_component)
     parse_componentAND :: t_parser token (maybe t_component)
     parse_simpleexpression :: t_parser token (maybe t_component)
     parse_holds_expression :: t_parser token (maybe t_expression)
     parse_compID_expression :: t_parser token (maybe t_expression)
     parse_s12e :: t_parser token (maybe t_expression)
     parse_v2e :: t_parser token (maybe t_expression)
     parse_c2e :: t_parser token (maybe t_expression)
     parse_comparison :: t_parser token (maybe t_comparison)
     parse_s22e :: t_parser token (maybe t_expression)

    Statements

     parse_statement :: t_parser token (maybe t_component)
     parse_simplestatement :: t_parser token (maybe t_component)
     parse_holds_statement :: t_parser token (maybe t_statement)
     parse_compID_statement :: t_parser token (maybe t_statement)
     parse_d :: t_parser token (maybe t_statement)
     parse_ON_date :: t_parser token (maybe t_date)
     parse_THE_date :: t_parser token (maybe t_date)
     parse_THE_subject :: t_parser token (maybe t_subject)
     parse_day :: t_parser token (maybe t_num)
     parse_month :: t_parser token (maybe t_num)
     parse_year :: t_parser token (maybe t_num)
     parse_number :: t_parser token (maybe t_num)
     parse_o :: t_parser token (maybe t_statement)
     parse_A :: t_parser token (maybe t_object)
     parse_AN :: t_parser token (maybe t_object)
     parse_object :: t_parser token (maybe t_object)
     parse_s :: t_parser token (maybe t_statement)
     parse_subject :: t_parser token (maybe t_subject)
     parse_v :: t_parser token (maybe t_statement)
     parse_MODAL :: t_parser token (maybe t_verb_phrase)
     parse_verb :: t_parser token (maybe t_verb_phrase)

    Conditional Statements

     parse_conditional_statement :: t_parser token (maybe t_component)
     parse_condition :: t_parser token (maybe t_component)
     parse_simplecondition :: t_parser token (maybe t_component)
     parse_holds_condition :: t_parser token (maybe t_condition)
     parse_compID_condition :: t_parser token (maybe t_condition)
     parse_d2c :: t_parser token (maybe t_condition)
     parse_o2c :: t_parser token (maybe t_condition)
     parse_s2c :: t_parser token (maybe t_condition)
     parse_v2c :: t_parser token (maybe t_condition)
     parse_verb_status :: t_parser token (maybe t_verb_status)

    Misc

     parse_IF :: t_parser token (maybe t_component)
     parse_THEN :: t_parser token (maybe t_component)


    Show functions, used for testing

>my_show1 :: ([([token], maybe t_object)], [char]) -> [char]
>my_show1 (x, y) = show (x) ++ "\n     " ++ show (y)
>my_show2 :: ([([token], maybe (t_statement_type, t_verb))], [char]) -> [char]
>my_show2 (x, y) = show (x) ++ "\n     " ++ show (y)
>my_show3 :: ([([token], maybe num)], [char]) -> [char]
>my_show3 (x, y) = show (x) ++ "\n     " ++ show (y)
>||my_show4 :: ([([token], maybe t_name)], [char]) -> [char] || t_name not used; wasted effort to change further numberings.
>||my_show4 (x, y) = show (x) ++ "\n     " ++ show (y)
>my_show5 :: ([([token], maybe t_component)], [char]) -> [char]
>my_show5 (x, y) = show (x) ++ "\n     " ++ show (y)
>my_show6 :: ([([token], maybe t_statement)], [char]) -> [char]
>my_show6 (x, y) = show (x) ++ "\n     " ++ show (y)
>my_show7 :: ([([token], maybe t_subject)], [char]) -> [char]
>my_show7 (x, y) = show (x) ++ "\n     " ++ show (y)
>my_show8 :: ([([token], maybe t_verb_phrase)], [char]) -> [char]
>my_show8 (x, y) = show (x) ++ "\n     " ++ show (y)
>my_show9 :: ([([token], maybe t_date)], [char]) -> [char]
>my_show9 (x, y) = show (x) ++ "\n     " ++ show (y)
>my_show10 :: ([([token], maybe t_verb_status)], [char]) -> [char]
>my_show10 (x, y) = show (x) ++ "\n     " ++ show (y)
>my_show11 :: ([([token], maybe t_condition)], [char]) -> [char]
>my_show11 (x, y) = show (x) ++ "\n     " ++ show (y)
>my_show12 :: ([([token], maybe t_contract)], [char]) -> [char]
>my_show12 (x, y) = show (last x) ++ "\n     " ++ show (y)
>my_show13 :: ([([token], maybe t_expression)], [char]) -> [char]
>my_show13 (x, y) = show (x) ++ "\n     " ++ show (y)

>vanilla_statement1  = [CompID "1", ComponentAffirmation, ComponentTHE, Person "Consultant", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "r"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020]
>vanilla_condition1  = [CompID "1", ComponentAffirmation, ComponentTHE, Person "Consultant", Verb_status Delivered, ComponentA, Object (Report "r"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020]
>vanilla_expression1 = [CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Verb_status Paid, Comparison LessThan, ComponentTHE, Person "ConsultantB"]

Parser Unit Tests

    (i) Component Level Testing

>test_AN1                   = my_show1 (parse_AN [],                                                   "[([],Nothing)]")
>test_AN2                   = my_show1 (parse_AN [ComponentAN],                                        "[([],Just NoObject)]")

>test_A1                    = my_show1 (parse_A  [],                                                   "[([],Nothing)]")
>test_A2                    = my_show1 (parse_A  [ComponentA],                                         "[([],Just NoObject)]")

>test_ON_date1              = my_show9 (parse_ON_date [],                                              "[([],Nothing)]")
>test_ON_date2              = my_show9 (parse_ON_date [ComponentON],                                   "[([],Just (0,0,0))]")

>test_THE_date1             = my_show9 (parse_THE_date [],                                             "[([],Nothing)]")
>test_THE_date2             = my_show9 (parse_THE_date [ComponentTHE],                                 "[([],Just (0,0,0))]")

>test_THE_subject1          = my_show7 (parse_THE_subject [],                                          "[([],Nothing)]")
>test_THE_subject2          = my_show7 (parse_THE_subject [ComponentTHE],                              "[([],Just [])]")

>test_day1                  = my_show3 (parse_day [],                                                  "[([],Nothing)]")
>test_day2                  = my_show3 (parse_day [Number 1],                                          "[([],Just 1)]")
>test_day3                  = my_show3 (parse_day [Number 1, Number 2],                                "[([Number 2],Just 1)]")

>test_month1                = my_show3 (parse_month [],                                                "[([],Nothing)]")
>test_month2                = my_show3 (parse_month [Number 12],                                       "[([],Just 12)]")
>test_month3                = my_show3 (parse_month [Number 12, Number 11],                            "[([Number 11],Just 12)]")

>test_year1                 = my_show3 (parse_year [],                                                 "[([],Nothing)]")
>test_year2                 = my_show3 (parse_year [Number 2020],                                      "[([],Just 2020)]")
>test_year3                 = my_show3 (parse_year [Number 2020, Number 2021],                         "[([Number 2021],Just 2020)]")

>test_number1               = my_show3 (parse_number [],                                               "[([],Nothing)]")
>test_number2               = my_show3 (parse_number [Number 999],                                     "[([],Just 999)]")

>test_IF1                   = my_show5 (parse_IF [],                                                   "[]")
>test_IF2                   = my_show5 (parse_IF [ComponentIF],                                        "[([],Just (CompCondition (Condition [] UnknownHolds (0,0,0) [] NoStatus NoObject)))]")

>test_THEN1                 = my_show5 (parse_THEN [],                                                 "[]")
>test_THEN2                 = my_show5 (parse_THEN [ComponentTHEN],                                    "[([],Just (CompStatement (Statement [] UnknownHolds NoStatementType (0,0,0) [] NoVerb NoObject)))]")

>test_MODAL1                = my_show2 (parse_MODAL [],                                                "[([],Nothing)]")
>test_MODAL2                = my_show2 (parse_MODAL [Modal_verb Obligation],                           "[([],Just (Obligation,NoVerb))]")
>test_MODAL3                = my_show2 (parse_MODAL [Modal_verb Permission],                           "[([],Just (Permission,NoVerb))]")
>test_MODAL4                = my_show2 (parse_MODAL [Modal_verb Prohibition],                          "[([],Just (Prohibition,NoVerb))]")
>test_MODAL5                = my_show2 (parse_MODAL [Modal_verb NoStatementType],                      "[([],Just (NoStatementType,NoVerb))]")

>test_verb_status1          = my_show10 (parse_verb_status [],                                         "[([],Nothing)]")
>test_verb_status2          = my_show10 (parse_verb_status [Verb_status Delivered],                    "[([],Just Delivered)]")
>test_verb_status3          = my_show10 (parse_verb_status [Verb_status Paid],                         "[([],Just Paid)]")
>test_verb_status4          = my_show10 (parse_verb_status [Verb_status NoStatus],                     "[([],Just NoStatus)]")

>test_verb1                 = my_show2 (parse_verb [],                                                 "[([],Nothing)]" )
>test_verb2                 = my_show2 (parse_verb [Verb Deliver],                                     "[([],Just (NoStatementType,Deliver))]" )
>test_verb3                 = my_show2 (parse_verb [Verb Pay],                                         "[([],Just (NoStatementType,Pay))]" )
>test_verb4                 = my_show2 (parse_verb [Verb NoVerb],                                      "[([],Just (NoStatementType,NoVerb))]" )

>test_subject1              = my_show7 (parse_subject [],                                              "[([],Nothing)]")
>test_subject2              = my_show7 (parse_subject [Person "Consultant"],                           "[([],Just Consultant)]")
>test_subject3              = my_show7 (parse_subject [Person "ConsultantA", Person "ConsultantB"],    "[([Person ConsultantB],Just ConsultantA)]")

>test_object1               = my_show1 (parse_object [],                                               "[([],Nothing)]")
>test_object2               = my_show1 (parse_object [Object (Report "r")],                            "[([],Just (Report r))]")
>test_object3               = my_show1 (parse_object [Object (Pounds 1)],                              "[([],Just (Pounds 1))]")
>test_object4               = my_show1 (parse_object [Object (Report "r"), Object (Pounds 1)],         "[([Object (Pounds 1)],Just (Report r))]")
>test_object5               = my_show1 (parse_object [Object (Report "r1"), Object (Report "r2"), Object (Report "r3")],
>                                                                                                      "[([Object (Report r2), Object (Report r3)],Just (Report r1))]")

>test_componentOR1          = my_show5 (parse_componentOR [],                                          "[([],Nothing)]")
>test_componentOR2          = my_show5 (parse_componentOR [ComponentOR],                               "[([],Just TrueComponent)]")

>test_componentAND1         = my_show5 (parse_componentAND [],                                         "[([],Nothing)]")
>test_componentAND2         = my_show5 (parse_componentAND [ComponentAND],                             "[([],Just TrueComponent)]")

>test_holds_statement1      = my_show6 (parse_holds_statement [],                                      "[([],Nothing)]")
>test_holds_statement2      = my_show6 (parse_holds_statement [ComponentAffirmation],                  "[([],Just (Statement [] Holds NoStatementType (0,0,0) [] NoVerb NoObject))]")
>test_holds_statement3      = my_show6 (parse_holds_statement [ComponentNegation],                     "[([],Just (Statement [] NotHolds NoStatementType (0,0,0) [] NoVerb NoObject))]")

>test_holds_condition1      = my_show11 (parse_holds_condition [],                                     "[([],Nothing)]")
>test_holds_condition2      = my_show11 (parse_holds_condition [ComponentAffirmation],                 "[([],Just (Condition [] Holds (0,0,0) [] NoStatus NoObject))]")
>test_holds_condition3      = my_show11 (parse_holds_condition [ComponentNegation],                    "[([],Just (Condition [] NotHolds (0,0,0) [] NoStatus NoObject)))]")

>test_definitionIS1         = my_show5 (parse_simpledefinitionIS [],                                   "[]")
>test_definitionIS2         = my_show5 (parse_simpledefinitionIS [Person "P1", ComponentDEFIS, Person "A"],
>                                                                                                      "[]") || no CompID
>test_definitionIS3         = my_show5 (parse_simpledefinitionIS [CompID "1", Person "P1", ComponentDEFIS, Person "A"],
>                                                                                                      "[([],Just (CompDefinition (Def_IS 1 P1 A)))]")

>test_definitionEQ1         = my_show5 (parse_simpledefinitionEQ [],                                   "[]")
>test_definitionEQ2         = my_show5 (parse_simpledefinitionEQ [Object (NamedObject "InterestRate"), ComponentDEFEQ, NumericalExpr (NumericalExprNum 1)],
>                                                                                                      "[]") || no CompID
>test_definitionEQ3         = my_show5 (parse_simpledefinitionEQ [CompID "1", Object (NamedObject "InterestRate"), ComponentDEFEQ, NumericalExpr (NumericalExprNum 1)],
>                                                                                                      "[([],Just (CompDefinition (Def_EQ 1 (NamedObject InterestRate) (NumericalExprNum 1)))]")
>test_definitionEQ4         = my_show5 (parse_simpledefinitionEQ [CompID "1", Object (SomeCurrency "AmountA"), ComponentDEFEQ, NumericalExpr (NumericalExprObject (SomeCurrency "AmountB"))],
>                                                                                                      "[([],Just (CompDefinition (Def_EQ 1 (SomeCurrency AmountA) (NumericalExprObject (SomeCurrency AmountB))))]")
>test_definitionEQ5         = my_show5 (parse_simpledefinitionEQ [CompID "1", Object (SomeCurrency "AmountA"), ComponentDEFEQ, NumericalExpr (NumericalExprObject (OtherObject "AmountB"))], || showing that it works for different objects
>                                                                                                      "[([],Just (CompDefinition (Def_EQ 1 (SomeCurrency AmountA) (NumericalExprObject (OtherObject AmountB))))]")
>test_definitionEQ6         = my_show5 (parse_simpledefinitionEQ [CompID "1", Object (SomeCurrency "AmountA"), ComponentDEFEQ, NumericalExpr (NumericalExprExpr (NumericalExprObject (SomeCurrency "AmountB")) PLUS (NumericalExprNum 1))],
>                                                                                                      "[([],Just (CompDefinition (Def_EQ 1 (SomeCurrency AmountA) (NumericalExprExpr (NumericalExprObject (SomeCurrency AmountB)) PLUS (NumericalExprNum 1))))]")
>test_definitionEQ7         = my_show5 (parse_simpledefinitionEQ [CompID "1", Object (SomeCurrency "AmountA"), ComponentDEFEQ, NumericalExpr (NumericalExprExpr (NumericalExprObject (SomeCurrency "AmountB")) PLUS (NumericalExprObject (SomeCurrency "AmountC")))],
>                                                                                                      "[([],Just (CompDefinition (Def_EQ 1 (SomeCurrency AmountA) (NumericalExprExpr (NumericalExprObject (SomeCurrency AmountB)) PLUS (NumericalExprObject (SomeCurrency AmountC)))))]")

>test_expression1           = my_show5 (parse_expression [], "[]")
>test_expression2           = my_show5 (parse_expression [Word "party", ComponentDEFIS, Word "Fred"], "[]")
>test_expression3           = my_show5 (parse_expression
>                                       vanilla_expression1,
>                                     "[([],Just (CompExpression (Expression 1 Holds ConsultantA Paid LessThan ConsultantB)))]")
>test_expression4           = my_show5 (parse_expression
>                                      [CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Verb_status Paid, Comparison LessThan, ComponentTHE, Person "ConsultantB",
>                                       ComponentOR,
>                                       CompID "2", ComponentAffirmation, ComponentTHE, Person "ConsultantC", Verb_status Paid, Comparison MoreThan, ComponentTHE, Person "ConsultantD"],
>                                     "[([],Just (CompExpression (Expressions_or [Expression 1 Holds ConsultantA Paid LessThan ConsultantB,Expression 2 Holds ConsultantC Paid MoreThan ConsultantD])))]")

>test_simpleexpression1     = my_show5 (parse_simpleexpression [], "[]")
>test_simpleexpression2     = my_show5 (parse_simpleexpression [Word "party", ComponentDEFIS, Word "Fred"], "[]")
>test_simpleexpression3     = my_show5 (parse_simpleexpression
>                                        vanilla_expression1,
>                                      "[([],Just (CompExpression (Expression 1 Holds ConsultantA Paid LessThan ConsultantB)))]")

>test_expressional_definition1 = my_show5 (parse_expressional_definition || definition if expression
>                                          [CompID "1b", Person "PartyB", ComponentDEFIS, Person "Debtor",
>                                           ComponentIF,
>                                           CompID "1a", ComponentAffirmation, Person "PartyA", Verb_status Paid, Comparison MoreThan, Person "PartyB"],
>                                       "[([],Just (CompExprDefinition (Expression 1a Holds PartyA Paid MoreThan PartyB) (Def_IS 1b PartyB Debtor)))]")
>test_expressional_definition2 = my_show5 (parse_expressional_definition || if expression then definition
>                                          [ComponentIF,
>                                           CompID "1a", ComponentAffirmation, Person "PartyA", Verb_status Paid, Comparison MoreThan, Person "PartyB",
>                                           ComponentTHEN,
>                                           CompID "1b", Person "PartyB", ComponentDEFIS, Person "Debtor"],
>                                       "[([],Just (CompExprDefinition (Expression 1a Holds PartyA Paid MoreThan PartyB) (Def_IS 1b PartyB Debtor)))]")

>test_statement1            = my_show5 (parse_statement [],                                            "[]")
>test_statement2            = my_show5 (parse_statement || statement
>                                       vanilla_statement1,
>                                      "[([],Just (CompStatement (Statement 1 Holds Permission (31,3,2020) Consultant Deliver (Report r))))]")
>test_statement3            = my_show5 (parse_statement || negated statement
>                                       [CompID "1", ComponentNegation, ComponentTHE, Person "Consultant", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "r"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                      "[([],Just (CompStatement (Statement 1 NotHolds Permission (31,3,2020) Consultant Deliver (Report r))))]")
>test_statement4            = my_show5 (parse_statement || statement1 or statement2
>                                       [CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "r1"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020,
>                                        ComponentOR,
>                                        CompID "2", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "r2"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                      "[([],Just (CompStatement (Statements_or [Statement 1 Holds Permission (31,3,2020) ConsultantA Deliver (Report r1),Statement 2 Holds Permission (31,3,2020) Consultant2 Deliver (Report r2)]))]")
>test_statement5            = my_show5 (parse_statement || statement1 and statement2
>                                      [CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "r1"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020,
>                                       ComponentAND,
>                                       CompID "2", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "r2"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                     "[([],Just (CompStatement (Statements_and [Statement 1 Holds Permission (31,3,2020) Consultant1 Deliver (Report r1),Statement 2 Holds Permission (31,3,2020) Consultant2 Deliver (Report r2)]))]")
>test_statement6            = my_show5 (parse_statement || statement1 or statement2 or statement3
>                                      [CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "r1"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020,
>                                       ComponentOR,
>                                       CompID "2", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "r2"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020,
>                                       ComponentOR,
>                                       CompID "3", ComponentAffirmation, ComponentTHE, Person "ConsultantC", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "r3"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                     "[([],Just (CompStatement (Statements_or [Statement 1 Holds Permission (31,3,2020) Consultant1 Deliver (Report r1),Statement 2 Holds Permission (31,3,2020) Consultant2 Deliver (Report r2),Statement 3 Holds Permission (31,3,2020) Consultant3 Deliver (Report r3)])))]")
>test_statement7            = my_show5 (parse_statement || statement1 and statement2 and statement3
>                                      [CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "r1"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020,
>                                       ComponentAND,
>                                       CompID "2", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "r2"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020,
>                                       ComponentAND,
>                                       CompID "3", ComponentAffirmation, ComponentTHE, Person "ConsultantC", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "r3"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                     "[([],Just (CompStatement (Statements_and [Statement 1 Holds Permission (31,3,2020) Consultant1 Deliver (Report r1),Statement 2 Holds Permission (31,3,2020) Consultant2 Deliver (Report r2),Statement 3 Holds Permission (31,3,2020) Consultant3 Deliver (Report r3)])))]")

>test_simplestatement1      = my_show5 (parse_simplestatement [],                                            "[]")
>test_simplestatement2      = my_show5 (parse_simplestatement [Word "party", ComponentDEFIS, Word "Fred"],   "[]")
>test_simplestatement3      = my_show5 (parse_simplestatement
>                                       vanilla_statement1,
>                                      "[([],Just (CompStatement (Statement 1 Holds Permission (31,3,2020) Consultant Deliver (Report r))))]")

>test_condition1            = my_show5 (parse_condition [],                                                  "[]")
>test_condition2            = my_show5 (parse_condition || condition
>                                       vanilla_condition1,
>                                      "[([],Just (CompCondition (Condition 1 Holds (31,3,2020) Consultant Delivered (Report r))))]")
>test_condition3            = my_show5 (parse_condition || negated condition
>                                       [CompID "1", ComponentNegation, ComponentTHE, Person "Consultant", Verb_status Delivered, ComponentA, Object (Report "r"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                      "[([],Just (CompCondition (Condition 1 NotHolds (31,3,2020) Consultant Delivered (Report r))))]")
>test_condition4            = my_show5 (parse_condition || condition1 or condition2
>                                      [CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Verb_status Delivered, ComponentA, Object (Report "r1"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020,
>                                       ComponentOR,
>                                       CompID "2", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Verb_status Delivered, ComponentA, Object (Report "r2"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                     "[([],Just (CompCondition (Conditions_or [Condition 1 Holds (31,3,2020) ConsultantA Delivered (Report r1),Condition 2 Holds (31,3,2020) ConsultantB Delivered (Report r2)]))]")
>test_condition5            = my_show5 (parse_condition || condition1 and condition2
>                                      [CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Verb_status Delivered, ComponentA, Object (Report "r1"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020,
>                                       ComponentAND,
>                                       CompID "2", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Verb_status Delivered, ComponentA, Object (Report "r2"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                     "[([],Just (CompCondition (Conditions_and [Condition 1 Holds (31,3,2020) ConsultantA Delivered (Report r1),Condition 2 Holds (31,3,2020) ConsultantB Delivered (Report r2)]))]")
>test_condition6            = my_show5 (parse_condition || condition1 or condition2 or condition3
>                                      [CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Verb_status Delivered, ComponentA, Object (Report "r1"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020,
>                                       ComponentOR,
>                                       CompID "2", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Verb_status Delivered, ComponentA, Object (Report "r2"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020,
>                                       ComponentOR,
>                                       CompID "3", ComponentAffirmation, ComponentTHE, Person "ConsultantC", Verb_status Delivered, ComponentA, Object (Report "r3"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                     "[([],Just (CompCondition (Conditions_or [Condition 1 Holds (31,3,2020) ConsultantA Delivered (Report r1),Condition 2 Holds (31,3,2020) ConsultantB Delivered (Report r2),Condition 3 Holds (31,3,2020) ConsultantC Delivered (Report r3)])))]")
>test_condition7            = my_show5 (parse_condition || condition1 and condition2 and condition3
>                                      [CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Verb_status Delivered, ComponentA, Object (Report "r1"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020,
>                                       ComponentAND,
>                                       CompID "2", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Verb_status Delivered, ComponentA, Object (Report "r2"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020,
>                                       ComponentAND,
>                                       CompID "3", ComponentAffirmation, ComponentTHE, Person "ConsultantC", Verb_status Delivered, ComponentA, Object (Report "r3"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                     "[([],Just (CompCondition (Conditions_and [Condition 1 Holds (31,3,2020) ConsultantA Delivered (Report r1),Condition 2 Holds (31,3,2020) ConsultantB Delivered (Report r2),Condition 3 Holds (31,3,2020) ConsultantC Delivered (Report r3)])))]")

>test_simplecondition1      = my_show5 (parse_simplecondition [],                                            "[]")
>test_simplecondition2      = my_show5 (parse_simplecondition [Word "party", ComponentDEFIS, Word "Fred"],   "[]")
>test_simplecondition3      = my_show5 (parse_simplecondition
>                                       vanilla_condition1,
>                                     "[([],Just (CompCondition (Condition 1 Holds (31,3,2020) Consultant Delivered (Report r))))]")

>test_v1                    = my_show6 (parse_v [Modal_verb Permission, Verb Deliver],
>                                               "[([],Just (Statement [] UnknownHolds Permission (0,0,0) [] Deliver NoObject))]")
>test_d1                    = my_show6 (parse_d [ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                               "[([],Just (Statement [] UnknownHolds NoStatementType (31,3,2020) [] NoVerb NoObject))]")
>test_o1                    = my_show6 (parse_o [ComponentA, Object (Pounds 50)],
>                                               "[([],Just (Statement [] UnknownHolds NoStatementType (0,0,0) [] NoVerb (Pounds 50)))]")
>test_s1                    = my_show6 (parse_s [ComponentTHE, Person "Consultant"],
>                                               "[([],Just (Statement [] UnknownHolds NoStatementType (0,0,0) Consultant NoVerb NoObject))]")
>test_do1                   = my_show6 ((then (mymerge_s "d") parse_o parse_d)
>                                       [ComponentA, Object (Pounds 50), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                       "[([],Just (Statement [] UnknownHolds NoStatementType (31,3,2020) [] NoVerb (Pounds 50)))]")
>test_dov1                  = my_show6 ((then (mymerge_s "od") parse_v (then (mymerge_s "d") parse_o parse_d))
>                                       [Modal_verb Permission, Verb Deliver, ComponentA, Object (Pounds 50), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                       "[([],Just (Statement [] UnknownHolds Permission (31,3,2020) [] Deliver (Pounds 50)))]")
>test_dovs1                 = my_show6 ((then (mymerge_s "S") parse_s (then (mymerge_s "od") parse_v (then (mymerge_s "d") parse_o parse_d)))
>                                       [ComponentTHE, Person "Consultant", Modal_verb Permission, Verb Deliver, ComponentA, Object (Pounds 50), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                       "[([],Just (Statement [] UnknownHolds Permission (31,3,2020) Consultant Deliver (Pounds 50)))]")

>test_v2c1                  = my_show11 (parse_v2c [Verb_status Delivered],
>                                                  "[([],Just (Condition [] UnknownHolds (0,0,0) [] Delivered NoObject))]")
>test_d2c1                  = my_show11 (parse_d2c [ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                                  "[([],Just (Condition [] UnknownHolds (31,3,2020) [] NoStatus NoObject))]" )
>test_o2c1                  = my_show11 (parse_o2c [ComponentA, Object (Pounds 50)],
>                                                  "[([],Just (Condition [] UnknownHolds (0,0,0) [] NoStatus (Pounds 50)))]")
>test_s2c1                  = my_show11 (parse_s2c [ComponentTHE, Person "Consultant"],
>                                                  "[([],Just (Condition [] UnknownHolds (0,0,0) Consultant NoStatus NoObject))]" )
>test_do2c1                 = my_show11 ((then (mymerge_c "d") parse_o2c parse_d2c)
>                                        [ComponentA, Object (Pounds 50), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                        "[([],Just (Condition [] UnknownHolds (31,3,2020) [] NoStatus (Pounds 50)))]")
>test_dov2c1                = my_show11 ((then (mymerge_c "od") parse_v2c (then (mymerge_c "d") parse_o2c parse_d2c))
>                                       [Verb_status Delivered, ComponentA, Object (Pounds 50), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                       "[([],Just (Condition [] UnknownHolds (31,3,2020) [] Delivered (Pounds 50)))]")
>test_dovs2c1               = my_show11 ((then (mymerge_c "S") parse_s2c (then (mymerge_c "od") parse_v2c (then (mymerge_c "d") parse_o2c parse_d2c)))
>                                       [ComponentTHE, Person "Consultant", Verb_status Delivered, ComponentA, Object (Pounds 50), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                       "[([],Just (Condition [] UnknownHolds (31,3,2020) Consultant Delivered (Pounds 50)))]")

>test_conditional_statement1  = my_show5 (parse_conditional_statement [],                 "[]")
>test_conditional_statement2  = my_show5 (parse_conditional_statement [Word "party", ComponentDEFIS, Word "Fred"], "[]") || not a statement or a condition
>test_conditional_statement3  = my_show5 (parse_conditional_statement vanilla_statement1, "[]") || only a statement
>test_conditional_statement4  = my_show5 (parse_conditional_statement vanilla_condition1, "[]") || only a condition
>test_conditional_statement5  = my_show5 (parse_conditional_statement || statement if condition
>                                       [CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "r1"), ComponentON, ComponentTHE, Number 30, Number 2, Number 2020,
>                                        ComponentIF,
>                                        CompID "2", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Verb_status Paid, ComponentA, Object (Report "r2"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2021],
>                                       "[([],Just (CompCondStatement (Condition 2 Holds (31,3,2021) ConsultantB Paid (Report r2)) (Statement 1 Holds Permission (30,2,2020), ConsultantA Deliver (Report r1))))]")
>test_conditional_statement6  = my_show5 (parse_conditional_statement || if condition then statement
>                                       [ComponentIF,
>                                        CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Verb_status Delivered, ComponentA, Object (Report "r1"), ComponentON, ComponentTHE, Number 30, Number 2, Number 2020,
>                                        ComponentTHEN,
>                                        CompID "2", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "r2"), ComponentON, ComponentTHE, Number 30, Number 2, Number 2020],
>                                       "[([],Just (CompCondStatement (Condition 1 Holds (30,2,2021) ConsultantA Delivered (Report r1)) (Statement 2 Holds Permission (30,2,2020), ConsultantB Deliver (Report r2))))]")
>test_conditional_statement7  = my_show5 (parse_conditional_statement || if condition1 or condition2 then statement
>                                       [ComponentIF,
>                                        CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Verb_status Paid, ComponentA, Object (Pounds 10), ComponentON, ComponentTHE, Number 31, Number 3, Number 2021,
>                                        ComponentOR,
>                                        CompID "2", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Verb_status Paid, ComponentA, Object (Pounds 20), ComponentON, ComponentTHE, Number 31, Number 4, Number 2021,
>                                        ComponentTHEN,
>                                        CompID "3", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Modal_verb Obligation, Verb Deliver, ComponentA, Object (Report "r"), ComponentON, ComponentTHE, Number 1, Number 5, Number 2021],
>                                       "[([],Just (CompCondStatement (Conditions_or [Condition 1 Holds (31,3,2021) ConsultantB Paid (Pounds 10),Condition 2 Holds (31,4,2021) ConsultantB Paid (Pounds 20)]) (Statement 3 Holds Obligation (1,5,2021) ConsultantA Deliver (Report r))))]")
>test_conditional_statement8  = my_show5 (parse_conditional_statement || if condition1 and condition2 then statement
>                                       [ComponentIF,
>                                        CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Verb_status Paid, ComponentA, Object (Pounds 10), ComponentON, ComponentTHE, Number 31, Number 3, Number 2021,
>                                        ComponentAND,
>                                        CompID "2", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Verb_status Paid, ComponentA, Object (Pounds 20), ComponentON, ComponentTHE, Number 31, Number 4, Number 2021,
>                                        ComponentTHEN,
>                                        CompID "3", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Modal_verb Obligation, Verb Deliver, ComponentA, Object (Report "r"), ComponentON, ComponentTHE, Number 1, Number 5, Number 2021],
>                                       "[([],Just (CompCondStatement (Conditions_and [Condition 1 Holds (31,3,2021) ConsultantB Paid (Pounds 10),Condition 2 Holds (31,4,2021) ConsultantB Paid (Pounds 20)]) (Statement 3 Holds Obligation (1,5,2021) ConsultantA Deliver (Report r))))]")
>test_conditional_statement9  = my_show5 (parse_conditional_statement || statement if condition1 or condition2
>                                       [CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Modal_verb Obligation, Verb Deliver, ComponentA, Object (Report "r"), ComponentON, ComponentTHE, Number 1, Number 5, Number 2021,
>                                        ComponentIF,
>                                        CompID "2", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Verb_status Paid, ComponentA, Object (Pounds 10), ComponentON, ComponentTHE, Number 31, Number 3, Number 2021,
>                                        ComponentOR,
>                                        CompID "3", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Verb_status Paid, ComponentA, Object (Pounds 20), ComponentON, ComponentTHE, Number 31, Number 4, Number 2021],
>                                       "([],Just (CompCondStatement (Conditions_or [Condition 2 Holds (31,3,2021) ConsultantB Paid (Pounds 10),Condition 3 Holds (31,4,2021) ConsultantB Paid (Pounds 20)]) (Statement 1 Holds Obligation (1,5,2021) ConsultantA Deliver (Report r))))]")
>test_conditional_statement10 = my_show5 (parse_conditional_statement || statement if condition1 and condition2
>                                       [CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Modal_verb Obligation, Verb Deliver, ComponentA, Object (Report "r"), ComponentON, ComponentTHE, Number 1, Number 5, Number 2021,
>                                        ComponentIF,
>                                        CompID "2", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Verb_status Paid, ComponentA, Object (Pounds 10), ComponentON, ComponentTHE, Number 31, Number 3, Number 2021,
>                                        ComponentAND,
>                                        CompID "3", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Verb_status Paid, ComponentA, Object (Pounds 20), ComponentON, ComponentTHE, Number 31, Number 4, Number 2021],
>                                       "([],Just (CompCondStatement (Conditions_and [Condition 2 Holds (31,3,2021) ConsultantB Paid (Pounds 10),Condition 3 Holds (31,4,2021) ConsultantB Paid (Pounds 20)]) (Statement 1 Holds Obligation (1,5,2021) ConsultantA Deliver (Report r))))]")

>test_component1            = my_show12 (parse_component [CompID "[1]", ComponentTHE, Person "Consultant", ComponentDEFIS, Person "PersonA"],  "([],Just (Contract [CompDefinition (Def_IS [1] Consultant PersonA)]))")
>test_component2            = my_show12 (parse_component [CompID "[1]", ComponentA, Object (OtherObject "NumberOfParties"), ComponentDEFEQ, NumericalExpr (NumericalExprNum 5)],       "([],Just (Contract [CompDefinition (Def_EQ [1] (OtherObject NumberOfParties) (NumericalExprNum 5))]))")
>test_component3            = my_show12 (parse_component || statement
>                                        vanilla_statement1,
>                                       "([],Just (Contract [CompStatement (Statement 1 Holds Permission (31,3,2020) Consultant Deliver (Report r))]))")
>test_component4            = my_show12 (parse_component || condition
>                                        vanilla_condition1,
>                                       "([],Just (Contract [CompCondition (Condition 1 Holds (31,3,2020) Consultant Delivered (Report r))]))")
>test_component5            = my_show12 (parse_component || statement if condition (conditional statement)
>                                       [CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "r"), ComponentON, ComponentTHE, Number 30, Number 2, Number 2020,
>                                        ComponentIF,
>                                        CompID "2", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Verb_status Paid, ComponentA, Object (Pounds 10), ComponentON, ComponentTHE, Number 31, Number 3, Number 2021],
>                                      "([],Just (Contract [CompCondStatement (Condition 2 Holds (31,3,2021) ConsultantB Paid (Pounds 10)) (Statement 1 Holds Permission (30,2,2020), ConsultantA Deliver (Report r))]))")
>test_component6            = my_show12 (parse_component || if condition then statement (conditional statement)
>                                       [ComponentIF,
>                                        CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Verb_status Paid, ComponentA, Object (Pounds 10), ComponentON, ComponentTHE, Number 31, Number 3, Number 2021,
>                                        ComponentTHEN,
>                                        CompID "2", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "r"), ComponentON, ComponentTHE, Number 30, Number 2, Number 2020],
>                                       "([],Just (Contract [CompCondStatement (Condition 1 Holds (31,3,2021) ConsultantB Paid (Pounds 10)) (Statement 2 Holds Permission (30,2,2020) ConsultantA Deliver (Report r))]))")

>mymerge_s c    x                                      Nothing                                = Nothing
>mymerge_s c    Nothing                                y                                      = Nothing
>mymerge_s "o"  (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id1 h1 ty1 da1 su1 ve1 ob2)
>mymerge_s "s"  (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id1 h1 ty1 da1 su2 ve1 ob1)
>mymerge_s "d"  (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id1 h1 ty1 da2 su1 ve1 ob1)
>mymerge_s "vo" (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id1 h1 ty2 da1 su1 ve2 ob2)
>mymerge_s "vs" (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id1 h1 ty2 da1 su2 ve2 ob1)
>mymerge_s "od" (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id1 h1 ty1 da2 su1 ve1 ob2)
>mymerge_s "sd" (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id1 h1 ty1 da2 su2 ve1 ob1)
>mymerge_s "D"  (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id2 h2 ty2 da1 su2 ve2 ob2)
>mymerge_s "S"  (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id2 h2 ty2 da2 su1 ve2 ob2)
>mymerge_s "O"  (Just (Statement id1 h1 ty1 da1 su1 ve1 ob1)) (Just (Statement id2 h2 ty2 da2 su2 ve2 ob2)) = Just (Statement id2 h2 ty2 da2 su2 ve2 ob1)

>mymerge_c c    x                                      Nothing                        = Nothing
>mymerge_c c    Nothing                                y                              = Nothing
>mymerge_c "o"  (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id1 h1 da1 su1 vs1 ob2)
>mymerge_c "s"  (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id1 h1 da1 su2 vs1 ob1)
>mymerge_c "d"  (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id1 h1 da2 su1 vs1 ob1)
>mymerge_c "vo" (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id1 h1 da1 su1 vs2 ob2)
>mymerge_c "vs" (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id1 h1 da1 su2 vs2 ob1)
>mymerge_c "od" (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id1 h1 da2 su1 vs1 ob2)
>mymerge_c "sd" (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id1 h1 da2 su2 vs1 ob1)
>mymerge_c "D"  (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id2 h2 da1 su2 vs2 ob2)
>mymerge_c "S"  (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id2 h2 da2 su1 vs2 ob2)
>mymerge_c "O"  (Just (Condition id1 h1 da1 su1 vs1 ob1)) (Just (Condition id2 h2 da2 su2 vs2 ob2)) = Just (Condition id2 h2 da2 su2 vs2 ob1)

>component_tests = "\n\n\n Component Level Tests \n\n" ++ layn
>                  [test_AN1, test_AN2, test_A1, test_A2, test_ON_date1, test_ON_date2, test_THE_date1, test_THE_date2, test_THE_subject1, test_THE_subject2,
>                   test_day1, test_day2, test_day3, test_month1, test_month2, test_month3, test_year1, test_year2, test_year3,
>                   test_number1, test_number2, test_IF1, test_IF2, test_THEN1, test_THEN2,
>                   test_MODAL1, test_MODAL2, test_MODAL3, test_MODAL4, test_MODAL5,
>                   test_verb_status1, test_verb_status2, test_verb_status3, test_verb_status4,
>                   test_verb1, test_verb2, test_verb3, test_verb4,
>                   test_subject1, test_subject2, test_subject3,
>                   test_object1, test_object2, test_object3, test_object4, test_object5, || #46
>                   test_componentOR1, test_componentOR2,
>                   test_componentAND1, test_componentAND2,
>                   test_holds_statement1, test_holds_statement2, test_holds_statement3,
>                   test_holds_condition1, test_holds_condition2, test_holds_condition3,
>                   test_definitionIS1, test_definitionIS2, test_definitionIS3,
>                   test_definitionEQ1, test_definitionEQ2, test_definitionEQ3, test_definitionEQ4,
>                   test_definitionEQ5, test_definitionEQ6, test_definitionEQ7,
>                   test_expression1, test_expression2, test_expression3, test_expression4,
>                   test_simpleexpression1, test_simpleexpression2, test_simpleexpression3,
>                   test_expressional_definition1, test_expressional_definition2,
>                   test_statement1, test_statement2, test_statement3, test_statement4, test_statement5, test_statement6, test_statement7,
>                   test_simplestatement1, test_simplestatement2, test_simplestatement3,
>                   test_condition1, test_condition2, test_condition3, test_condition4, test_condition5, test_condition6, test_condition7,
>                   test_simplecondition1, test_simplecondition2, test_simplecondition3, || #49
>                   test_v1, test_d1, test_o1, test_s1, test_do1, test_dov1, test_dovs1,
>                   test_v2c1, test_d2c1, test_o2c1, test_s2c1, test_do2c1, test_dov2c1, test_dovs2c1, || #14
>                   test_conditional_statement1, test_conditional_statement2, test_conditional_statement3, test_conditional_statement4,
>                   test_conditional_statement5, test_conditional_statement6, test_conditional_statement7, test_conditional_statement8,
>                   test_conditional_statement9, test_conditional_statement10,
>                   test_component1, test_component2, test_component3, test_component4, test_component5, test_component6] || #16

    There are a total of 125 component level unit tests above.

    (ii) Contract Level Testing

>test_contractAND1           = my_show12 (parse_contractAND [],                                               "([],Nothing)")
>test_contractAND2           = my_show12 (parse_contractAND [ContractAND],                                    "([],Just TrueContract)")

>test_contract1              = my_show12 (parse_contract || contract
>                                         [CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "r1"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                        "([],Just (Contract [CompStatement (Statement 1 Holds Permission (31,3,2020) ConsultantA Deliver (Report r1))]))")
>test_contract2              = my_show12 (parse_contract || contract <and> contract
>                                         [CompID "1", ComponentAffirmation, ComponentTHE, Person "ConsultantA", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "r1"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020,
>                                          ContractAND,
>                                          CompID "2", ComponentAffirmation, ComponentTHE, Person "ConsultantB", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "r2"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                        "([],Just (Contract [CompStatement (Statement 1 Holds Permission (31,3,2020) ConsultantA Deliver (Report r1)),CompStatement (Statement 2 Holds Permission (31,3,2020) ConsultantB Deliver (Report r2))]))")
>test_contract3              = my_show12 (parse_contract || contract (s if c) <and> contract (s and s) <and> contract (s ors if c ands)
>                                         [CompID "1", ComponentAffirmation, ComponentTHE, Person "C1-A", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "C1-R1"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020,
>                                          ComponentIF,
>                                          CompID "2", ComponentAffirmation, ComponentTHE, Person "C1-B", Verb_status Paid, ComponentA, Object (Report "C1-R2"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2021,
>                                          ContractAND,
>                                          CompID "3", ComponentAffirmation, ComponentTHE, Person "C2-A", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "C2-R1"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020,
>                                          ComponentAND,
>                                          CompID "4", ComponentAffirmation, ComponentTHE, Person "C2-B", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "C2-R2"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020,
>                                          ContractAND,
>                                          CompID "5", ComponentAffirmation, ComponentTHE, Person "C3-A", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "C3-R1"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020,
>                                          ComponentOR,
>                                          CompID "6", ComponentAffirmation, ComponentTHE, Person "C3-B", Modal_verb Permission, Verb Deliver, ComponentA, Object (Report "C3-R2"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020,
>                                          ComponentIF,
>                                          CompID "7", ComponentAffirmation, ComponentTHE, Person "C3-C", Verb_status Delivered, ComponentA, Object (Report "C3-R3"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020,
>                                          ComponentAND,
>                                          CompID "8", ComponentAffirmation, ComponentTHE, Person "C3-D", Verb_status Delivered, ComponentA, Object (Report "C3-R4"), ComponentON, ComponentTHE, Number 31, Number 3, Number 2020],
>                                         "x")

>contract_tests = "\n\n\n Contract Level Tests \n\n" ++ layn [test_contractAND1, test_contractAND2, test_contract1, test_contract2, test_contract3]

    There are a total of 5 contract level unit tests above.

    (iii) Lexer Testing

>my_showlexer :: ([token], [char]) -> [char]
>my_showlexer (x, y) = show (x) ++ "\n     " ++ show (y)

>test_lexer1  = my_showlexer (lexer "[1] The Consultant shall deliver a report r1 on the 23 March 2021", || statement
>                            "[CompID 1,Person consultant,Modal_verb Obligation,Verb Deliver,ComponentA,Object (Report r1),ComponentON,ComponentTHE,Number 23,Number 3,Number 2021]")
>test_lexer2  = my_showlexer (lexer "[1] It is the case that the Consultant shall deliver a report r1 on the 23 March 2021", || affirmed statement
>                            "[CompID 1,ComponentAffirmation,Person consultant,Modal_verb Obligation,Verb Deliver,ComponentA,Object (Report r1),ComponentON,ComponentTHE,Number 23,Number 3,Number 2021]")
>test_lexer3  = my_showlexer (lexer "[1] It is not the case that the consultant shall deliver a report r1 on the 23 March 2021", || negated statement
>                            "[CompID 1,ComponentNegation,Person consultant,Modal_verb Obligation,Verb Deliver,ComponentA,Object (Report r1),ComponentON,ComponentTHE,Number 23,Number 3,Number 2021]")
>test_lexer4  = my_showlexer (lexer "[1] The consultant delivered a report r1 on the 23 March 2021", || condition
>                            "[CompID 1,Person consultant,Verb_status Delivered,ComponentA,Object (Report r1),ComponentON,ComponentTHE,Number 23,Number 3,Number 2021]")
>test_lexer5  = my_showlexer (lexer "[1] It is the case that the consultant delivered a report r1 on the 23 March 2021", || affirmed condition
>                            "[CompID 1,ComponentAffirmation,Person consultant,Verb_status Delivered,ComponentA,Object (Report r1),ComponentON,ComponentTHE,Number 23,Number 3,Number 2021]")
>test_lexer6  = my_showlexer (lexer "[1] It is not the case that the consultant delivered a report r1 on the 23 March 2021", || negated condition
>                            "[CompID 1,ComponentNegation,Person consultant,Verb_status Delivered,ComponentA,Object (Report report),ComponentON,ComponentTHE,Number 23,Number 3,Number 2021]")
>test_lexer7  = my_showlexer (lexer "[1] It is the case that Simon shall pay 20 pounds on the 24 March 2021 IF [2] it is not the case that Simon delivered a report r1 on the 23 March 2021", || statement if condition
>                            "[CompID 1,Person Simon,Modal_verb Obligation,Verb Pay,Object (Pounds 20),ComponentON,ComponentTHE,Number 24,Number 3,Number 2021,ComponentIF,CompID 2,ComponentNegation,Person Simon,Verb_status Delivered,ComponentA,Object (Report r1),ComponentON,ComponentTHE,Number 23,Number 3,Number 2021]")
>test_lexer8  = my_showlexer (lexer "[1] IF it is not the case that the employee delivered a report r1 on the 23 March 2021 THEN [2] it is the case that the employee shall pay 10 pounds on the 24 March 2021", || if condition then statement
>                            "[CompID 1, ComponentIF,ComponentNegation,Person employee,Verb_status Delivered,ComponentA,Object (Report r1),ComponentON,ComponentTHE,Number 23,Number 3,Number 2021,ComponentTHEN,CompID 2,ComponentAffirmation,Person employee,Modal_verb Obligation,Verb Pay,Object (Pounds 10),ComponentON,ComponentTHE,Number 24,Number 3,Number 2021]")
>test_lexer9  = my_showlexer (lexer "[1] The consultant may deliver a report r1 on the 23 March 2021", || use of Permission
>                            "[CompID 1,Person consultant,Modal_verb Permission,Verb Deliver,ComponentA,Object (Report r1),ComponentON,ComponentTHE,Number 23,Number 3,Number 2021]")
>test_lexer10 = my_showlexer (lexer "[1] The consultant is forbidden to deliver a report r1 on the 23 March 2021", || use of Prohibition
>                            "[CompID 1,Person consultant,Modal_verb Prohibition,Verb Deliver,ComponentA,Object (Report r1),ComponentON,ComponentTHE,Number 23,Number 3,Number 2021]")

>lexer_tests = "\n\n\n Lexer Tests \n\n" ++ layn [test_lexer1, test_lexer2, test_lexer3, test_lexer4, test_lexer5, test_lexer6, test_lexer7, test_lexer8, test_lexer9, test_lexer10]

    There are a total of 10 lexer tests above.

    There are a total of 140 unit test (lexer + parser).

    (iv) End-to-End Contract Testing

>my_showsample :: [char] -> [char]
>my_showsample x = show (x) ++ "\n      " ++ show (lexer x) ++ "\n      " ++ show (parse x)

>sample1a = "[1] It is the case that Bob shall deliver a bicycle on the 1 March 2021." || svod (DEFAULT FORMAT)
>sample1b = "[1] It is the case that on the 1 March 2021 Bob shall deliver a bicycle." || dsvo (good)
>sample1c = "[1] It is the case that on the 1 March 2021 a bicycle shall deliver Bob." || dovs (makes no sense but needed for parsing)
>sample1d = "[1] It is the case that Bob on the 1 March 2021 shall deliver a bicycle." || sdvo (good)
>sample1e = "[1] It is the case that a bicycle shall deliver Bob on the 1 March 2021." || ovsd (makes no sense but needed for parsing)
>sample2  = "[1] It is the case that Bob shall deliver a bicycle on the 1 March 2021 OR [2] it is the case that Bob shall deliver a bicycle on the 2 March 2021." || statement or statement
>sample3  = "[1] It is the case that Bob shall deliver a bicycle on the 1 March 2021 OR [2] it is the case that Bob shall deliver a bicycle on the 2 March 2021 OR [3] it is the case that Bob shall deliver a bicycle on the 3 March 2021." || statement or statement or statement
>sample4  = "[1] It is the case that Bob shall deliver a bicycle on the 1 March 2021 OR [2] it is the case that Bob shall deliver a bicycle on the 2 March 2021 IF [3] it is the case that Alice paid 50 pounds on the 31 April 2021." || statement or statement IF condition
>sample5  = "[1] It is the case that Bob shall deliver a bicycle on the 1 March 2021 OR [2] it is the case that Bob shall deliver a bicycle on the 2 March 2021 IF [3] it is the case that Alice paid 50 pounds on the 31 April 2021 OR [4] it is the case that Alice paid 40 pounds on the 30 April 2021." || statement or statement IF condition or condition
>sample6  = "[1] It is the case that Bob shall deliver a bicycle on the 1 March 2021 <AND> [2] It is not the case that Bob shall pay a fee on the 1 March 2021." || contract and contract
>sample7  = "[1] It is the case that the Student must deliver a dissertation on the 31 March 2021."

>test_sample1a = my_showsample sample1a
>test_sample1b = my_showsample sample1b
>test_sample1c = my_showsample sample1c
>test_sample1d = my_showsample sample1d
>test_sample1e = my_showsample sample1e
>test_sample2  = my_showsample sample2
>test_sample3  = my_showsample sample3
>test_sample4  = my_showsample sample4
>test_sample5  = my_showsample sample5
>test_sample6  = my_showsample sample6
>test_sample7  = my_showsample sample7

>endtoend_tests = "\n\n\n End to End Tests \n\n" ++ layn [test_sample1a, test_sample1b, test_sample1c, test_sample1d, test_sample1e, test_sample2, test_sample3, test_sample4, test_sample5, test_sample6, test_sample7]

    There are a total of 11 end-to-end tests above.

    Example Contract Test

>example_contract = "IF [1a] it is the case that Alice paid 100 pounds on the 1 April 2021 OR [1b] it is the case that Alice paid 120 pounds on the 1 April 2021 THEN [2] it is the case that Bob must deliver a bicycle on the 5 April 2021 <AND> [3a] it is the case that Bob may deliver a receipt on the 5 April 2021 AND [3b] it is the case that Bob is forbidden to charge a delivery_fee on the 5 April 2021."

>example_contract_test = "\n\n\n Example Contract Test \n\n" ++ example_contract ++ "\n\n" ++ show (lexer (example_contract)) ++ "\n\n" ++ show (parse (example_contract))

    ISDA testing

>isda  = "IF [1a] it is the case that PartyA paid AmountA on the 01 January 1970 AND [1b] it is the case that PartyB paid AmountB on the 01 January 1970 THEN [2a] it is not the case that PartyA shall pay AmountA on the 01 January 1970 AND [2b] it is not the case that PartyB shall pay AmountB on the 01 January 1970 "
>         ++ "<AND> [3] it is the case that ExcessParty shall pay the excess amount of currency on the 01 January 1970 "
>         ++ "<AND> IF [4a] it is the case that PartyA paid more than PartyB THEN [4bi] ExcessParty IS PartyA AND [4bii] the excess amount of currency EQUALS AmountA MINUS AmountB "
>         ++ "<AND> IF [5a] it is the case that PartyB paid more than PartyA THEN [5bi] ExcessParty IS PartyB AND [5bii] the excess amount of currency EQUALS AmountB MINUS AmountA."

prs_test = parse(guarantor)

>isda_test = "\n\n\n ISDA Master Agreement Section 2(c) Test \n\n" ++ isda ++ "\n\n" ++ show (lexer (isda)) ++ "\n\n" ++ show (parse (isda))

>main = component_tests
>        ++ contract_tests
>        ++ lexer_tests
>        ++ endtoend_tests
>        ++ example_contract_test
>        ++ isda_test

    There are a total of 140 unit tests, 11 end-to-end tests, 1 example contract test, and 1 isda master agreement test implemented.

The function 'parse <input>' should only be used when you know you have a correct CNL input, which will compile to a contract.
Otherwise, use something like: 'parse_contract (lexer <input>)' or 'lexer <input>'
Assuming '<input>' is a string (e.g. "It is the case that ...").

>parse x = snd (last (parse_contract (lexer x)))

This concludes the main program - further experimenting below.

    Scalability testing

        Run `/count`, then `parse scaleX` to see how long each takes.

>scale1   = "[1] It is the case that Alice shall pay 10 pounds on the 1 April 2021."

>scale2   = "[1] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [2] It is the case that Alice shall pay 10 pounds on the 1 April 2021."

>scale3   = "[1] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [2] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [3] It is the case that Alice shall pay 10 pounds on the 1 April 2021."

>scale4   = "[1] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [2] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [3] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [4] It is the case that Alice shall pay 10 pounds on the 1 April 2021."

>scale5   = "[1] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [2] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [3] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [4] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [5] It is the case that Alice shall pay 10 pounds on the 1 April 2021."

>scale6   = "[1] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [2] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [3] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [4] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [5] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [6] It is the case that Alice shall pay 10 pounds on the 1 April 2021."

>scale7   = "[1] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [2] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [3] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [4] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [5] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [6] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [7] It is the case that Alice shall pay 10 pounds on the 1 April 2021."

>scale8   = "[1] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [2] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [3] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [4] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [5] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [6] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [7] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [8] It is the case that Alice shall pay 10 pounds on the 1 April 2021."

>scale9   = "[1] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [2] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [3] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [4] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [5] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [6] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [7] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [8] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [9] It is the case that Alice shall pay 10 pounds on the 1 April 2021."

>scale10  = "[1] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [2] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [3] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [4] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [5] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [6] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [7] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [8] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [9] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [10] It is the case that Alice shall pay 10 pounds on the 1 April 2021."

>scale11  = "[1] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [2] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [3] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [4] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [5] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [6] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [7] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [8] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [9] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [10] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [11] It is the case that Alice shall pay 10 pounds on the 1 April 2021."

>scale12  = "[1] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [2] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [3] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [4] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [5] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [6] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [7] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [8] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [9] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [10] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [11] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [12] It is the case that Alice shall pay 10 pounds on the 1 April 2021."

>scale13  = "[1] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [2] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [3] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [4] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [5] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [6] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [7] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [8] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [9] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [10] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [11] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [12] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [13] It is the case that Alice shall pay 10 pounds on the 1 April 2021."

>scale14  = "[1] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [2] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [3] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [4] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [5] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [6] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [7] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [8] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [9] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [10] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [11] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [12] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [13] It is the case that Alice shall pay 10 pounds on the 1 April 2021 "
>           ++ "<AND> [14] It is the case that Alice shall pay 10 pounds on the 1 April 2021."

>scale15  = "[1] It is the case that Alice shall pay 10 pounds on the 1 April 2021."
>           ++ "<AND> [2] It is the case that Alice shall pay 10 pounds on the 1 April 2021."
>           ++ "<AND> [3] It is the case that Alice shall pay 10 pounds on the 1 April 2021."
>           ++ "<AND> [4] It is the case that Alice shall pay 10 pounds on the 1 April 2021."
>           ++ "<AND> [5] It is the case that Alice shall pay 10 pounds on the 1 April 2021."
>           ++ "<AND> [6] It is the case that Alice shall pay 10 pounds on the 1 April 2021."
>           ++ "<AND> [7] It is the case that Alice shall pay 10 pounds on the 1 April 2021."
>           ++ "<AND> [8] It is the case that Alice shall pay 10 pounds on the 1 April 2021."
>           ++ "<AND> [9] It is the case that Alice shall pay 10 pounds on the 1 April 2021."
>           ++ "<AND> [10] It is the case that Alice shall pay 10 pounds on the 1 April 2021."
>           ++ "<AND> [11] It is the case that Alice shall pay 10 pounds on the 1 April 2021."
>           ++ "<AND> [12] It is the case that Alice shall pay 10 pounds on the 1 April 2021."
>           ++ "<AND> [13] It is the case that Alice shall pay 10 pounds on the 1 April 2021."
>           ++ "<AND> [14] It is the case that Alice shall pay 10 pounds on the 1 April 2021."
>           ++ "<AND> [15] It is the case that Alice shall pay 10 pounds on the 1 April 2021."

=== === === === === === === === === === === === === === === === === === === === === === === === === === === === === ===
                                            ALTERATIONS BY DOMINIC KLOECKER 
                                
                        The previous code is created by Simon Fattal and not part of this project.
=== === === === === === === === === === === === === === === === === === === === === === === === === === === === === ===

> || Type Definitions for intermediate syntax 
>m_test ::= TRUE | FALSE
>
>m_modal_verb ::= M_SHALL | M_SHANT | M_MAY
>
>m_statement ::= M_TemporalActionStatement(m_test, t_subject, m_modal_verb, t_verb, t_object, t_date) 
>
>m_conditional_statement ::= M_ConditionalStatement(m_condition, m_statement)
>
>m_condition ::= M_StatementCondition(m_statement)
>                | M_TemporalActionCondition(m_test, t_subject, t_verb_status, t_object, t_date)
>                | M_ExpressionCondition(m_test, t_subject, t_verb_status, t_comparison, t_subject)
>                | M_AndCondition([m_condition])
>                | M_OrCondition([m_condition])
>
>m_definition ::= M_IsDefinition(t_subject, t_subject) 
>                 | M_EqualsDefinition(t_object, t_numericalexpr) 
>                 | M_DefAnd([m_definition]) 
>
>m_conditional_definition ::= M_ConditionalDefinition(m_condition, m_definition)

>|| Helper Functions to reduce code length
>|| Conditional Statements
>cond_stmt :: m_condition -> m_statement -> m_conditional_statement
>cond_stmt cond stmt = M_ConditionalStatement(cond, stmt)

>|| Conditional Definitions
>cond_def :: m_condition -> m_definition -> m_conditional_definition
>cond_def cond def = M_ConditionalDefinition(cond, def)

>|| Translate parsed cola contract into combined syntax
>m_translate_contract :: maybe t_contract ->  ([m_statement], [m_conditional_statement], [m_definition], [m_conditional_definition])
>m_translate_contract (Nothing)                            = ([], [], [], [])
>m_translate_contract (Just (TrueContract))                = ([], [], [], [])
>m_translate_contract (Just (Contract comp))               = (convert_comonent comp) 
>m_translate_contract (Just (Contracts_and comps))         = combined_components (map convert_comonent comps) ([], [], [], [])

>|| Combine of contract into one tuple
>combined_components [] (s, cs, d, cd) = (s, cs, d, cd) 
>combined_components ((s, cs, d, cd):rest) (sa, csa, da, cda) = combined_components rest ((s++sa), (cs++csa), (d++da), (cd++cda))

>|| Convert parsed CoLa component into combined syntax
>convert_comonent :: t_component -> ([m_statement], [m_conditional_statement], [m_definition], [m_conditional_definition])
>convert_comonent comp 
> = xconvert_component comp 
>   where 
>   || Pattern matching on the different types of components 
>   xconvert_component (CompDefinition t_def)                   = ([], [], [(trans_def (t_def))], [])
>   xconvert_component (CompStatement t_stmt)                   = ((trans_stmt (t_stmt)), [], [], [])
>   xconvert_component (CompCondStatement t_cond t_stmt)        = ([], (trans_cond_stmts t_cond t_stmt) , [], [])
>   xconvert_component (CompStatStatement t_stmt1 t_stmt2   )   = ([], (trans_stmt_stmts t_stmt1 t_stmt2), [], [])
>   xconvert_component (CompExprDefinition t_expr t_def   )     = ([], [], [], [(trans_expr_def t_expr t_def)])
>        
>   || Convert CoLa unconditional definitions 
>   || trans_def :: t_definition -> m_definition
>   trans_def (Definitions_and comps)        = M_DefAnd((map trans_def comps))
>   trans_def (Def_IS t_ID sbj1 sbj2)        = M_IsDefinition(sbj1, sbj2)
>   trans_def (Def_EQ t_ID obj num_exp)      = M_EqualsDefinition(obj, num_exp)
>
>   || Convert CoLa conditional definitions
>   || trans_expr_def :: t_condition -> t_definition -> m_conditional_definition
>   trans_expr_def condExpr defin = xtrans_expr_def (trans_expr_cond condExpr) (trans_def defin)
>   xtrans_expr_def prsedExpr defin = M_ConditionalDefinition(prsedExpr, defin)
>
>   || Convert CoLa expression conditions
>   || trans_expr_cond :: t_condition -> m_condition
>   trans_expr_cond (Expressions_or exprs)  = M_OrCondition( (map trans_expr_cond exprs))
>   trans_expr_cond (Expressions_and exprs) = M_AndCondition( (map trans_expr_cond exprs))
>   trans_expr_cond (Expression tId Holds sbj1 vrb_sts comp sbj2)      = M_ExpressionCondition(TRUE, sbj1, vrb_sts, comp, sbj2)
>   trans_expr_cond (Expression tId NotHolds sbj1 vrb_sts comp sbj2)   = M_ExpressionCondition(FALSE, sbj1, vrb_sts, comp, sbj2)
>
>   || Convert CoLa conditional statements
>   || trans_stmt_stmts :: t_statement -> t_statement -> [m_conditional_statement]
>   trans_stmt_stmts stmt_cond stmt = xtrans_stmt_stmts (trans_stmt_cond stmt_cond) (trans_stmt stmt)
>   xtrans_stmt_stmts prsed_cond stmtlist = (map (cond_stmt prsed_cond) stmtlist)
>
>   || Convert CoLa statement conditions
>   || trans_stmt_cond :: t_condition -> m_condition
>   trans_stmt_cond (Statements_and stmts)      = M_AndCondition((map trans_stmt_cond stmts))
>   trans_stmt_cond (Statements_or  stmts)      = M_OrCondition((map trans_stmt_cond stmts))
>   trans_stmt_cond (Statement tId Holds stmtType date sbj vrb obj)     = M_StatementCondition(M_TemporalActionStatement(TRUE, sbj, (deontic_to_verb stmtType), vrb, obj, date))
>   trans_stmt_cond (Statement tId NotHolds stmtType date sbj vrb obj)  = M_StatementCondition(M_TemporalActionStatement(FALSE, sbj,(deontic_to_verb stmtType), vrb, obj, date))
>
>   || CoLa CoLa conditional statements
>   || trans_cond_stmts :: t_condition -> t_statement -> [m_conditional_statement]
>   trans_cond_stmts cond stmt = xtrans_cond_stmts (trans_conds cond) (trans_stmt stmt)
>   xtrans_cond_stmts prsed_cond stmtlist = (map (cond_stmt prsed_cond) stmtlist)
>
>   || Convert cola conditions 
>   || trans_conds :: t_condition -> m_condition
>   trans_conds (Conditions_or conds)        = M_OrCondition((map trans_conds conds))
>   trans_conds (Conditions_and conds)       = M_AndCondition((map trans_conds conds))
>   trans_conds (Condition tId Holds date subj vrb_sts obj)    = M_TemporalActionCondition(TRUE, subj, vrb_sts, obj, date)
>   trans_conds (Condition tId NotHolds date subj vrb_sts obj) = M_TemporalActionCondition(FALSE, subj, vrb_sts, obj, date)
>
>   || Convert cola statements
>   || trans_stmt :: t_statement -> [m_statement]
>   trans_stmt (Statements_and stmts)      = (concat (map trans_stmt stmts))
>   trans_stmt (Statements_or  stmts)      = (concat (map trans_stmt stmts))
>   trans_stmt (Statement tId Holds stmtType date sbj vrb obj)     = [M_TemporalActionStatement(TRUE, sbj, (deontic_to_verb stmtType), vrb, obj, date)]
>   trans_stmt (Statement tId NotHolds stmtType date sbj vrb obj)  = [M_TemporalActionStatement(FALSE, sbj, (deontic_to_verb stmtType), vrb, obj, date)]
>        
>    || Convert deontic type to modal verb
>    || deontic_to_verb :: t_statement_type -> m_denotic_verbs
>    deontic_to_verb    Obligation  = M_SHALL
>    deontic_to_verb    Permission  = M_MAY
>    deontic_to_verb    Prohibition = M_SHANT
>
>
>abstype m_contract 
>with 
>   translate_contract :: maybe t_contract -> m_contract
>   showm_contract     :: m_contract -> [char]
>
>m_contract_type   ::= M_Contract ([m_statement], [m_conditional_statement], [m_definition], [m_conditional_definition])
>m_contract         == m_contract_type
>
> || Translate parsed cola contract into combined syntax
>translate_contract parsed_cola_contract = M_Contract (m_translate_contract parsed_cola_contract)
> || Show combined syntax contract
>cola_to_python con = translate_contract (parse con)

>showm_contract (M_Contract (stmts, cond_stmts, defin, cond_defins))
>   = "con = Contract()\n\n"
>       ++ (lay (map (print_stmt_call.print_statement) stmts))            
>       ++ (lay (map (print_stmt_call.print_condition_stmt) cond_stmts))      
>       ++ (lay (map (print_def_call.print_definition) defin))                   
>       ++ (lay (map (print_def_call.print_conditionitional_definition) cond_defins))    
>       ++ "con.interaciveSimulation()"

>print_stmt_call :: [char] -> [char]
>print_stmt_call stmt = "con.statement(" ++ stmt ++ ")\n"
>print_def_call def = "con.definition(" ++ def ++ ")\n"
>print_temporal_expression :: t_date -> [char]
>print_temporal_expression date = "TemporalExpression('ON','" ++ (show date) ++ "')"
> 
>print_test TRUE = "True"
>print_test FALSE = "False"
>
>print_modal_verb M_SHALL = "SHALL"
>print_modal_verb M_SHANT = "SHANT"
>print_modal_verb M_MAY = "MAY"
>
>print_statement (M_TemporalActionStatement(m_test, t_subject, m_modal_verb, t_verb, t_object, date))
>    = "TemporalStatement('" ++ (show t_subject) ++ "', '" ++ (print_modal_verb m_modal_verb) 
>        ++ "', '" ++ (show t_verb) ++ "', '" ++ (show t_object) ++ "', " ++ (print_temporal_expression date) ++ ", valid=" ++  (print_test m_test) ++ ")"
>
>print_condition_stmt (M_ConditionalStatement(cond, stmt))
>   = "ConditionalStatement(condition=" ++ (print_condition cond) ++ ", statement=" ++ (print_statement stmt) ++ ")"
>
>print_condition (M_StatementCondition(M_TemporalActionStatement(m_test, t_subject, m_modal_verb, t_verb, t_object, date))) 
>   = "StatementCondition(statement=" ++ (print_statement (M_TemporalActionStatement(m_test, t_subject, m_modal_verb, t_verb, t_object, date)))
>       ++ ", test=" ++ (print_test m_test) ++ ")"
>
>print_condition (M_TemporalActionCondition(m_test, t_subject, t_verb_status, t_object, date))
>   = "TemporalActionCondition('" ++ (show t_subject) ++ "', '" ++ (show t_verb_status) ++ "' , '" 
>       ++ (show t_object) ++ "', " ++ (print_temporal_expression date) ++ ", test=" ++ (print_test m_test) ++")"
>
>print_condition (M_ExpressionCondition(m_test, sbj1, t_verb_status, t_comparison, sbj2)) 
>   = "ExpressionCondition(BooleanExpression('" ++ (show sbj1) ++ "', '" ++ (show t_verb_status)
>        ++ "', '" ++ (show t_comparison) ++ "', '" ++ (show sbj2) ++ "'), test=" ++ (print_test m_test) ++ ")"
>
>print_condition (M_AndCondition(conds))
>   = "AndCondition(conditions=[" ++ (concat (map ((++",\n\t\t").print_condition) conds)) ++ "])"
>
>print_condition (M_OrCondition(conds))
>   = "OrCondition(conditions=[" ++ (concat (map ((++",\n\t\t").print_condition) conds)) ++ "])"
>
>
>print_definition (M_IsDefinition(sbj1, sbj2))   = "IsDefinition('" ++ (show sbj1) ++ "', '" ++ (show sbj2) ++"')"
>print_definition (M_EqualsDefinition(obj, numexp))  = "EqualsDefinition('" ++ (show obj) ++ "', '" ++ (print_expression numexp) ++"')"
>print_definition (M_DefAnd(def))         = "[" ++ (concat (map ((++",\n\t\t").print_definition) def)) ++ "]"

>print_expression (NumericalExprObject obj)   =   (show obj)
>print_expression (NumericalExprNum t_num)    =   (show t_num)
>print_expression (NumericalExprExpr exp1 op exp2)  = (print_expression exp1) ++ " " ++ (show op) ++ " " ++ (print_expression exp2)
>print_expression (NoNumericalExpr) = ""

>print_conditionitional_definition (M_ConditionalDefinition(cond, defs))
>    = "ConditionalDefinition(condition=" ++ (print_condition cond) ++ ", definitions=" ++ (print_definition defs) ++ ")\n"


> || Simple Test Contracts
> || Simple Statement
> test1 = "[1] It is the case that Dominic shall deliver a Report on the 11 September 2023."
> || Simple Forbidden Statement
> test2 = "[1] It is not the case that Dominic shall deliver a Report on the 11 September 2023."
> || Simple Conditional Statement
> test3 = "IF [1] it is the case that Dominic delivered a Report on the 11 September 2023 THEN [2] it is not the case that Dominic shall deliver a Report on the 11 September 2023."
> test4 = "IF [1] It is not the case that Dominic delivered a Report on the 11 September 2023 THEN [2] it is the case that UCL may deliver a Punishment on the 11 September 2023 AND [3] it is the case that Dominic shall deliver a Report on the 12 September 2023."
> || And Condition
> test5 = "IF [1] it is the case that PartyA shall pay AmountA on the 11 September 2023 AND [2] it is the case that PartyB shall pay AmountB on the 11 September 2023 THEN [3] it is not the case that PartyA shall pay AmountA on the 11 September 2023."
> test6 = "IF [1] it is the case that PartyA shall pay AmountA on the 11 September 2023 OR [2] it is the case that PartyB shall pay AmountB on the 11 September 2023 THEN [3] it is not the case that PartyA shall pay AmountA on the 11 September 2023"
> || Seperate Statements
> test7 = "[1] It is the case that Dominic shall deliver a Report on the 11 September 2023."
>              ++ "<AND> [2] It is not the case that Dominic may deliver a Report on the 11 September 2023."

>|| ISDA Contract
> isda_orig = "IF [1] it is the case that PartyA shall pay AmountA on the 01 January 1970 AND [2] it is the case that PartyB shall pay AmountB on the 01 January 1970 THEN [3] it is not the case that PartyA shall pay AmountA on the 01 January 1970 AND [4] it is not the case that PartyB shall pay AmountB on the 01 January 1970 "
>              ++ "<AND> [5] it is the case that ExcessParty shall pay the excess amount of currency on the 01 January 1970 "
>              ++ "<AND> IF [6] it is the case that PartyA paid more than PartyB THEN [7] ExcessParty IS PartyA AND [8] the excess amount of currency EQUALS AmountA MINUS AmountB "
>              ++ "<AND> IF [9] it is the case that PartyB paid more than PartyA THEN [10] ExcessParty IS PartyB AND [11] the excess amount of currency EQUALS AmountB MINUS AmountA."

>|| Modified ISDA Contract 
> isda_modified = "IF [1] it is the case that PartyA shall pay AmountA on the 01 January 1970 AND [2] it is the case that PartyB shall pay AmountB on the 01 January 1970 THEN [3] it is not the case that PartyA shall pay AmountA on the 01 January 1970 AND [4] it is not the case that PartyB shall pay AmountB on the 01 January 1970 AND [5] it is the case that ExcessParty shall pay the excess amount of currency on the 01 January 1970 "
>                 ++ "<AND> IF [6] it is the case that PartyA paid more than PartyB THEN [7] ExcessParty IS PartyA AND [8] the excess amount of currency EQUALS AmountA MINUS AmountB "
>                 ++ "<AND> IF [9] it is the case that PartyB paid more than PartyA THEN [10] ExcessParty IS PartyB AND [11] the excess amount of currency EQUALS AmountB MINUS AmountA."

>|| Bike Delivery Contract
> bike_orig = "IF [1a] it is the case that Alice paid 100 pounds on the 1 April 2021 OR [1b] it is the case that Alice paid 120 pounds on the 1 April 2021 THEN [2] it is the case that Bob must deliver a bicycle on the 5 April 2021"
>              ++ " <AND> [3a] it is the case that Bob may deliver a receipt on the 5 April 2021 AND [3b] it is the case that Bob is forbidden to charge a delivery_fee on the 5 April 2021."

>|| Modified Bike Delivery Contract
> biked_modified = "IF [1a] it is the case that Alice paid 100 pounds on the 1 April 2021 OR [1b] it is the case that Alice paid 120 pounds on the 1 April 2021 THEN [2] it is the case that Bob must deliver a bicycle on the 5 April 2021 AND [3a] it is the case that Bob may deliver a receipt on the 5 April 2021 AND [3b] it is the case that Bob is forbidden to charge a delivery_fee on the 5 April 2021."

>|| Bike Delivery Contract with Sanction
> bike_sanction = "IF [1a] it is the case that Alice paid 100 pounds on the 1 April 2021 OR [1b] it is the case that Alice paid 120 pounds on the 1 April 2021 THEN [2] it is the case that Bob must deliver a bicycle on the 5 April 2021 AND [3a] it is the case that Bob is forbidden to charge a delivery_fee on the 5 April 2021 "
>                  ++ "<AND> IF [6] it is the case that Bob delivered a bicycle on the 5 April 2021 AND [7] it is not the case that Bob charged a delivery_fee on the 5 April 2021 THEN [8] it is the case that Bob may deliver a receipt on the 5 April 2021 "
>                  ++ "<AND> [4] It is the case that Alice may charge 120 pounds on the 1 April 2021 IF [5] it is not the case that Bob delivered a bicycle on the 5 April 2021."

>|| Guarantor Agreement Contract
>guarantor = "[1] It is the case that Landlord shall deliver a Property on the 02 April 2021 "
>            ++ "<AND> IF [2] it is the case that Landlord delivered a demandOfTenantPayment on the 02 March 2021 AND [3] it is not the case that Tenant paid AmountA on the 2 March 2021 THEN [4] it is the case that the Landlord may deliver a demandOfGuarantorPayment on the 03 March 2021 "
>            ++ "<AND> IF [3] it is the case that Landlord delivered a demandOfTenantPayment on the 02 March 2021 AND [4] it is not the case that Tenant paid AmountA on the 2 March 2021 AND [5] it is the case that Landlord delivered a demandOfGuarantorPayment on the 03 March 2021 THEN [6] it is the case that Guarantor shall pay AmountA on the 03 March 2021 "
>            ++ "<AND> IF [7] it is not the case that Tenant paid AmountB on the 10 March 2022 THEN [8] it is the case that the Guarantor shall pay AmountB on the 11 March 2023 "
>            ++ "<AND> IF [9] it is the case that HousingBenefitScheme paid AmountC on the 02 March 2021 AND [10] it is the case that LocalAuthority delivered a overpaymentClaim on the 02 March 2022 THEN [11] it is the case that Guarantor shall pay AmountC on the 01 January 1970."


> python_test1 = cola_to_python test1
> python_test2 = cola_to_python test2
> python_test3 = cola_to_python test3
> python_test4 = cola_to_python test4
> python_test5 = cola_to_python test5
> python_test6 = cola_to_python test6
> python_test7 = cola_to_python test7
> python_isda_orig = cola_to_python isda_orig
> python_isda_modified = cola_to_python isda_modified
> python_bike_orig = cola_to_python bike_orig
> python_bike_modified = cola_to_python biked_modified
> python_bike_sanction = cola_to_python bike_sanction
> python_guarantor = cola_to_python guarantor


