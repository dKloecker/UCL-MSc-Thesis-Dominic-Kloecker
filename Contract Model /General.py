from Subject import Subject
from enum import Enum


class Evaluation(Enum):
    true = "TRUE"
    false = "FALSE"
    unknown = "UNKNOWN"

    @staticmethod
    def get_evaluation(evaluation: str) -> "Evaluation":
        """Return the evaluation based on the string"""
        return Evaluation(evaluation.upper())

    @classmethod
    def from_tokens(cls, tokens) -> "Evaluation":
        """Return the evaluation based on the number of tokens in the place"""
        if tokens == -1:
            return Evaluation.unknown
        elif tokens == 0:
            return Evaluation.false
        else:
            return Evaluation.true


class TemporalOperator(Enum):
    during = "during"
    before = "before"
    after = "after"
    within = "within"
    on = "on"
    throughout = "throughout"

    @staticmethod
    def get_temporal_operator(operator: str) -> "TemporalOperator":
        """Return the temporal operator based on the string"""
        return TemporalOperator(operator.lower())


class NumericOperator(Enum):
    moreThan = "morethan"
    lessThan = "lessthan"
    equalTo = "equalto"
    greaterEqual = "greaterequal"
    lessEqual = "lessequal"
    notEqual = "notequal"
    greater = "greater"
    lesser = "lesser"

    @staticmethod
    def get_numeric_operator(operator: str) -> "NumericOperator":
        """Return the numeric operator based on the string"""
        return NumericOperator(operator.lower())


class ModalVerb(Enum):
    shall = "SHALL"
    may = "MAY"
    shant = "SHANT"
    should = "SHOULD"
    must = "MUST"

    @staticmethod
    def get_modal_verb(modal_verb: str) -> "ModalVerb":
        """Return the modal verb based on the string"""
        return ModalVerb(modal_verb.upper())

    def get_modal_type(self) -> str:
        """Return the modal type based on the modal verb"""
        if self == ModalVerb.shall or self == ModalVerb.should or self == ModalVerb.must:
            return "Obligation"
        elif self == ModalVerb.may:
            return "Permission"
        elif self == ModalVerb.shant:
            return "Prohibition"


# Dictionary of verb to verb_status (past tense)
# Used to convert verb to verb_status
verb_to_verb_status = {
    "pay": "paid",
    "deliver": "delivered",
    "transfer": "transferred",
    "fix": "fixed",
    "provide": "provided",
    "issue": "issued",
    "give": "given",
    "notify": "notified",
    "demand": "demanded",
    "appoint": "appointed",
    "envoke": "envoked",
    "increase": "increased",
    "decrease": "decreased",
    "change": "changed",
    "replace": "replaced",
    "charge": "charged",
    "return": "returned",
    "certify": "certified"
}

# Dictionary of verb_status to verb
# Created by reversing the verb_to_verb_status dictionary
verb_status_to_verb = {v: k for k, v in verb_to_verb_status.items()}


class Verb(Enum):
    pay = "pay"
    deliver = "deliver"
    transfer = "transfer"
    fix = "fix"
    provide = "provide"
    issue = "issue"
    give = "give"
    notify = "notify"
    demand = "demand"
    appoint = "appoint"
    envoke = "envoke"
    increase = "increase"
    decrease = "decrease"
    change = "change"
    charge = "charge"
    # Return has to be capitalized because it is a python keyword
    RETURN = "return"
    certify = "certify"

    @staticmethod
    def get_verb(verb: str) -> "Verb":
        return Verb(verb.lower())

    @staticmethod
    def get_verb_status(verb: str) -> "VerbStatus":
        return VerbStatus.get_verb_status(verb_to_verb_status[verb.lower()])


class VerbStatus(Enum):
    paid = "paid"
    delivered = "delivered"
    transferred = "transferred"
    fixed = "fixed"
    provided = "provided"
    issued = "issued"
    given = "given"
    notified = "notified"
    demanded = "demanded"
    appointed = "appointed"
    envoked = "envoked"
    increased = "increased"
    decreased = "decreased"
    changed = "changed"
    charged = "charged"
    RETURNED = "returned"
    certified = "certified"

    @staticmethod
    def get_verb_status(verb_status: str) -> "VerbStatus":
        return VerbStatus(verb_status.lower())

    @staticmethod
    def get_verb(verb_status: str):
        return Verb.get_verb(verb_status_to_verb[verb_status.lower()])


class Relation:
    def __init__(self, relObject1, relObject2, comparisonOperator) -> None:
        self._relObject1 = relObject1
        self._relObject2 = relObject2
        self._comparisonOperator = comparisonOperator

    def relObject1(self):
        return self._relObject1

    def relObject2(self):
        return self._relObject2

    def comparisonOperator(self):
        return self._comparisonOperator

    def __repr__(self) -> str:
        return self.relObject1().label() + " " + self.comparisonOperator().value + " " + self.relObject2().label()


class TemporalRelation(Relation):
    def __init__(self, relObject1: "TemporalObject", relObject2: "TemporalObject",
                 comparisonOperator: "TemporalOperator") -> None:
        super().__init__(relObject1, relObject2, comparisonOperator)


class NumericRelation(Relation):
    def __init__(self, relObject1: "NumericObject", relObject2: "NumericObject",
                 comparisonOperator: "NumericOperator") -> None:
        super().__init__(relObject1, relObject2, comparisonOperator)


# Objects to Represent both Numeric and Temporal Values 

class ReferenceObject:
    def __init__(self, label) -> None:
        self._label = label
        Subject.add_subject(label)

    def label(self) -> str:
        return self._label

    def subject(self) -> "Subject":
        return Subject.get_subject(self.label())

    def __repr__(self) -> str:
        return self.label()


class NumericObject(ReferenceObject):
    def __init__(self, label, value="") -> None:
        super().__init__(label)
        self._value = value
        Subject.redefine_subject(self.label(), value)

    def value(self) -> str:
        return self._value

    def label(self) -> str:
        return self._label


class TemporalObject(ReferenceObject):
    def __init__(self, label, value="") -> None:
        super().__init__(label)
        self._value = value
        Subject.redefine_subject(self.label(), value)

    def value(self) -> str:
        return self._value

    def label(self) -> str:
        return self._label
