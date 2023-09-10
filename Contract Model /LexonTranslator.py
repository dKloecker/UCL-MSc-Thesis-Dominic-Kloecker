import json
from pprint import pprint
from typing import List, Dict, Tuple, Optional

from Condition import Condition, StateCondition, AndCondition, ExpressionCondition
from Contract import Contract
from Definition import IsDefinition, EqualsDefinition, ConditionalDefinition
from Expression import NumericExpression
from State import State
from Statement import Statement, TwoSubjects, ConditionalStatement


def make_groups(lexons: List[Dict]):
    """Splot lexon list into groups to keep track of subject. 
    Statmenets split by AND are done by the same subject while seqeunce indicates 
    a new subject."""
    groups = []
    group = []
    for lexon in lexons:
        match lexon["stmt"]:
            case "Sequence":
                groups.append(group)
                group = []
            case {"And": _}:
                pass
            case _:
                group.append(lexon)

    groups.append(group)
    return groups

def convert_conditions(condition: Dict) -> Tuple[Condition, Condition]:
    """convert the conditions and return the true and false conditions."""
    condition_operators = condition["ops"]
    expressions = condition["exprs"]

    current_expression = expressions.pop(0)
    if "Cmp" in current_expression:
        result, test = convert_comparison(current_expression["Cmp"]), True
    else:
        result, test = convert_is_expression(current_expression["Is"])

    true_condition = ExpressionCondition(result, test=test)
    false_condition = ExpressionCondition(result, test=test)

    if not condition_operators:
        return true_condition, false_condition

    if condition_operators[0] == "And":
        true_conditions = [true_condition]
        false_conditions = [false_condition]

        while expressions:
            expr = expressions.pop(0)

            if "Cmp" in expr:
                result, test = convert_comparison(expr["Cmp"]), True
            else:
                result, test = convert_is_expression(expr["Is"])

            true_conditions.append(ExpressionCondition(result, test=test))
            false_conditions.append(ExpressionCondition(result, test=test))

        return AndCondition(true_conditions), AndCondition(false_conditions)  # type: ignore

    return true_condition, false_condition


def convert_is_expression(is_expr: Dict) -> tuple[NumericExpression, bool]:
    """convert 'is' expression and return a numeric expression."""
    first_operand = is_expr[0]
    operator = is_expr[1]
    test = True
    if operator == "isnot":
        test = False
        operator = "is"

    second_operand = convert_expression(is_expr[2])

    return NumericExpression(first_operand, operator, second_operand), test


def convert_comparison(comparison: Dict) -> NumericExpression:
    """convert comparison and return a numeric expression."""
    operator = comparison["op"]
    first_operand = convert_expression(comparison["exp1"])
    second_operand = convert_expression(comparison["exp2"])

    return NumericExpression(first_operand, operator, second_operand)


def convert_expression(expr: Dict) -> str:
    """convert general expression and return its string representation."""
    operators = expr["ops"]
    result = convert_terms(expr["terms"].pop(0))

    for op in operators:
        result = f"{result} {op} {convert_terms(expr['terms'].pop(0))}"

    return result


def convert_terms(term: Dict) -> str:
    """convert terms and return its string representation."""
    factors = term["factors"]
    return convert_factors(factors.pop(0))


def convert_factors(factors: Dict) -> str:
    """convert factors and return its string representation."""
    if not factors:
        return ""

    if "Sym" in factors:
        return factors["Sym"]["sym"]
    if "sym" in factors:
        return factors["sym"]
    if "Num" in factors:
        return ""

    # For other types of factors
    key, value = list(factors.items())[0]
    if key == "Time":
        return f"{value}"
    if key == "Remainder":
        return f"{value} {key}"

    return f"{value} {key}"


def convert_payment_statement(pay_stmt: Dict) -> Tuple[str, str, str]:
    """convert payment statement and return pay-to, pay-from, and amount details."""
    pay_from = convert_factors(pay_stmt["from"]) if pay_stmt["from"] else ""
    amount_expression = pay_stmt["exp"]
    pay_to = convert_factors(pay_stmt["to"]) if pay_stmt["to"] else "the other party"

    amount = convert_expression(amount_expression)

    return pay_to, pay_from, amount


def convert_return_statement(ret_stmt: Dict) -> Tuple[str, str]:
    """convert payment statement and return pay-to, pay-from, and amount details."""
    terms = ret_stmt[0][0]["terms"]
    return_what = convert_factors(terms[0]["factors"].pop(0))

    return_to = ret_stmt[1]["sym"] if ret_stmt[1] else "the other party"

    return return_what, return_to


def convert_operation(operations: Dict) -> str:
    """convert operations and return its string representation."""
    terms = operations[1]["terms"]
    factor_operators = operations[1]["ops"]
    factors = terms[0]["factors"]

    result = convert_factors(factors.pop(0))
    object = operations[0]

    while factor_operators:
        op = factor_operators.pop(0)
        result = f"{result} {op} {convert_factors(factors.pop(0))}"

    return f"{result}"


def convert_lexon_group(lexons: List[Dict], con: Contract,
                      pre_condition: Optional[Condition] = None, prev_subject="",
                      modal_verb="Shall", overall_condition: Optional[Condition] = None,
                      fullfillment_conditions: Optional[Condition] = None) -> None:
    prev_subject = prev_subject
    pre_condition = pre_condition

    if fullfillment_conditions == None:
        fullfillment_conditions = []

    while lexons:
        # Pop the next lexon from the list and unpack it
        lexon = lexons.pop(0)
        stmt = lexon["stmt"]
        varname = lexon["varnames"]

        # Reset the statement and definition
        statement = None
        definition = None

        # Pattern match the statement and convert it to appropriate element
        match stmt:
            case "Sequence":
                continue

            case "And":
                continue

            case {"Definition": _}:
                continue

            case {"Increase": _}:
                if prev_subject != "":
                    subject = prev_subject
                else:
                    subject = varname[0]

                operation = convert_operation(stmt["Increase"])
                subject2 = stmt["Increase"][0]
                statement = TwoSubjects(subject, modal_verb, "Increase", subject2, "by", operation)

            case {"Decrease": _}:
                if prev_subject != "":
                    subject = prev_subject
                else:
                    subject = varname[0]

                subject2 = stmt["Decrease"][0]
                operation = convert_operation(stmt["Decrease"])
                statement = TwoSubjects(subject, modal_verb, "Decrease", subject2, "by", operation)

            case {"Pay": _}:
                if prev_subject != "":
                    subject = prev_subject
                else:
                    subject = stmt["Pay"]["who"]["sym"]
                    prev_subject = subject

                pay_to, pay_from, what = convert_payment_statement(stmt["Pay"])
                if pay_from != "" or None:
                    statement = TwoSubjects(subject, modal_verb, "Pay", what, "from" + pay_from + " to", pay_to)
                else:
                    statement = TwoSubjects(subject, modal_verb, "Pay", what, "to", pay_to)

            case {"Return": _}:
                if prev_subject != "":
                    subject = prev_subject
                else:
                    subject = varname[0]
                    prev_subject = subject

                return_what, return_to = convert_return_statement(stmt["Return"])
                statement = TwoSubjects(subject, modal_verb, "Return", return_what, "to", return_to)

            case {"Certify": _}:
                if prev_subject != "":
                    subject = prev_subject
                else:
                    subject = varname[0]
                    prev_subject = subject

                # Combine all the strings in the list (Might be nested lists so flatten it first) 
                # and then join them with a space
                object = " ".join([item for sublist in stmt["Certify"] for item in sublist])
                # object = stmt["Certify"]
                statement = Statement(subject, modal_verb, "certify", object)

            case {"Fix": _}:
                if prev_subject != "":
                    subject1 = prev_subject
                    object = varname[-1]
                else:
                    subject1 = varname[0]
                    object = varname[-1]
                    prev_subject = subject1

                statement = Statement(subject1, modal_verb, "Fix", object)

            case "Appoint":
                if prev_subject != "":
                    subject1 = prev_subject
                    object = varname[0]
                else:
                    subject1 = varname[0]
                    object = varname[1]
                    prev_subject = subject1

                statement = Statement(subject1, modal_verb, "Appoint", object)

            case {"Be": _}:
                subject = varname[0]

                if stmt["Be"]["expression"]:
                    expressions = stmt["Be"]["expression"]
                    expression_string = convert_expression(expressions)

                    object = expression_string
                    definition = EqualsDefinition(subject, object)
                else:
                    expressions = stmt["Be"]["def"]
                    definition = IsDefinition(subject, expressions)

            case {"If": _}:
                conditions = stmt["If"]
                condition_expressions = conditions["cond"]

                on_true_cond, on_false_cond = convert_conditions(condition_expressions)

                # Recursive call to convert nested statements
                if pre_condition is not None:
                    on_true_comb = AndCondition.combined([on_true_cond, pre_condition])
                    on_false_comb = AndCondition.combined([on_false_cond, pre_condition])
                else:
                    on_true_comb = on_true_cond
                    on_false_comb = on_false_cond

                on_true = stmt["If"]["ontrue"]
                convert_lexon_group(lexons=on_true, con=con, pre_condition=on_true_comb, modal_verb=modal_verb,
                                  prev_subject=prev_subject, overall_condition=overall_condition)
                on_false = stmt["If"]["onfalse"]
                # Recursive call to convert nested statements
                convert_lexon_group(lexons=on_false, con=con, pre_condition=on_false_comb, modal_verb=modal_verb,
                                  prev_subject=prev_subject, overall_condition=overall_condition)

            case {"May": _}:
                subject = stmt["May"][0]
                nested_statemtents = stmt["May"][1]
                # Recursive call to convert nested statements
                convert_lexon_group(nested_statemtents, con, pre_condition=pre_condition, prev_subject=subject,
                                  modal_verb="May", overall_condition=overall_condition)

            # General Catch all
            case _:
                pass

        condition = None
        if pre_condition and overall_condition:
            condition = AndCondition.combined([pre_condition, overall_condition])
        elif overall_condition:
            condition = overall_condition

        if statement:
            if condition:
                statement = ConditionalStatement(condition, statement)
                pre_condition = statement.statement().fulfillmentCondition()
                fullfillment_conditions.append(pre_condition)
            else:
                pre_condition = statement.fulfillmentCondition()
                fullfillment_conditions.append(pre_condition)

            con.statement(statement)

        elif definition:
            if condition:
                definition = ConditionalDefinition(condition, [definition])

            con.definition(definition)

        modal_verb = "Shall"


def convert_all_lexon_groups(lexon_groups: List[List[Dict]], contract: Contract,
                           prev_condition: Optional[Condition] = None, overall_condition: Optional[Condition] = None,
                           fullfillment_conditions: List[Condition] = []):
    """convert all the lexon groups."""
    for lexon_group in lexon_groups:
        convert_lexon_group(lexon_group, contract, prev_condition, overall_condition=overall_condition,
                          fullfillment_conditions=fullfillment_conditions)


class LexonTranslator:
    def __init__(self, filename) -> None:
        self._filename = filename
        self._json = {}
        # self._contract = Con

    def read_json_from_file(self) -> None:
        with open(self._filename, "r") as f:
            self._json = json.load(f)[0]

    def pprint_json(self):
        """"Pretty print the JSON"""
        pprint(self._json)

    def _terms_stmts(self):
        """Return the Statement JSONS"""
        return self._json["term_stmts"].copy()

    def _term_clasuses(self):
        """Return the Clauses JSONS"""
        return self._json["term_chpts"].copy()

    def _get_recitals(self) -> list[dict]:
        """Return all recitals"""
        return self._terms_stmts()

    def _get_clauses(self) -> list[dict]:
        """Return all clauses"""
        return self._term_clasuses()

    def _convert(self) -> None:
        """Convert the Contract JSON into equivalent Petri Net"""
        # # Get Recitals
        recitals = self._get_recitals()

        state = State(label="Contract Active", condition=None)
        state_cond = StateCondition(state=state)

        groups = make_groups(recitals)
        con = Contract()
        con.state(state)
        recital_fullfillment = []
        test = convert_all_lexon_groups(groups, con, overall_condition=state_cond,
                                      fullfillment_conditions=recital_fullfillment)
        # recitals_met = AndCondition.combined(recital_fullfillment)
        if recital_fullfillment:
            recitals_met = AndCondition(recital_fullfillment)
            recitals_state = State(label="Recitals Met", condition=recitals_met)
            con.state(recitals_state)
        else:
            recitals_state = State(label="Recitals Met", condition=state_cond)
            con.state(recitals_state)

        recital_cond = StateCondition(state=recitals_state)

        clauses = self._get_clauses()
        for clause in clauses:
            name = clause["name"]
            clause_state = State(label="Clause Invoked: " + name, condition=None)
            clause_state_cond = StateCondition(state=clause_state)
            clause_stmts = clause["statements"]
            clause_groups = make_groups(clause_stmts)
            combined_condition = AndCondition.combined([recital_cond, clause_state_cond])
            convert_all_lexon_groups(clause_groups, con, overall_condition=combined_condition)
            con.state(clause_state)

        con.interaciveSimulation()

    def _simulate_contract(self):
        """Simulate Contract"""
        self.read_json_from_file()
        self._convert()
