from Subject import Subject
from General import ReferenceObject, Relation, TemporalRelation, NumericRelation, TemporalObject, NumericObject, \
    TemporalOperator, NumericOperator
from General import Verb, VerbStatus
from PetriNet import Place, Transition, PetriNet


class Expression:
    def __init__(self):
        self._place = None

    def label(self):
        return "Expression"

    def __repr__(self):
        return self.label()

    def placeNode(self, net: PetriNet) -> Place:
        if self._place is None:
            self._place = net.place(self.label())
        return self._place


# E.g. Amount A is greater than Amount B
class NumericExpression(Expression):
    def __init__(self, numericSubject1: str, numericOperator: str, numericSubject2: str):
        super().__init__()
        self._numericObject = numericSubject1
        self._numericOperator = numericOperator
        self._numericObject2 = numericSubject2

    def object1(self):
        return Subject.get_subject(self._numericObject)

    def object2(self):
        return Subject.get_subject(self._numericObject2)

    def operator(self):
        return self._numericOperator

    def label(self):
        return f"{self.object1().label()} {self.operator()} {self.object2().label()}"


# E.g. Before Time B
class TemporalExpression(Expression):
    def __init__(self, temporalOperator: str, temporalSubject: str):
        super().__init__()
        self._temporalSubject = temporalSubject
        self._temporalOperator = temporalOperator

    def object(self):
        return Subject.get_subject(self._temporalSubject)

    def operator(self):
        return self._temporalOperator

    def label(self):
        return f"{self.operator()} {self.object().label()}"


# E.g. Person A is some status (e.g. "alive" or "dead")
class StatusExpression(Expression):
    def __init__(self, subject: str, status: str):
        super().__init__()
        self._subject = subject
        self._status = status

    def subject(self):
        return Subject.get_subject(self._subject)

    def status(self):
        return self._status

    def label(self):
        return f"{self.subject().label()} is {self.status()}"


class BooleanExpression(Expression):
    def __init__(self, subject1: str, verbStatus: str, operator: str, subject2: str):
        super().__init__()
        self._subject1 = subject1
        self._subject2 = subject2
        self._verbStatus = verbStatus
        self._operator = operator

    def subject1(self):
        return Subject.get_subject(self._subject1)

    def subject2(self):
        return Subject.get_subject(self._subject2)

    def verbStatus(self):
        # print(self._verbStatus)
        return VerbStatus.get_verb_status(self._verbStatus)

    def operator(self):
        return NumericOperator.get_numeric_operator(self._operator)

    def label(self):
        return f"{self.subject1().label()} {self.verbStatus().value} {self.operator().value} {self.subject2().label()}"
