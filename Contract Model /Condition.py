from typing import List

from General import VerbStatus, Evaluation
from PetriNet import Place, Transition, PetriNet
from State import State
from Subject import Subject


class Condition:
    """Base class for Conditions (E.g. Numeric, Temporal, Status)"""

    def __init__(self, test=True):
        self._test = test
        self._place = None
        self._transition = None
        self._eval = Evaluation.unknown

    def label(self) -> str:
        return "Condition"

    def __repr__(self) -> str:
        return self.label()

    def placeNode(self, net: PetriNet, show_label=True) -> Place:
        if self._place is None:
            self._place = net.place(self.label(), show_label)
        return self._place

    def transitionNode(self, net: PetriNet) -> Transition:
        if self._transition is None:
            self._transition = net.transition(f"{self.label()}")
        return self._transition

    def draw(self, net: PetriNet):
        """Draw the condition"""
        self.placeNode(net)

    def updatePlace(self, net: PetriNet):
        """Update the place node from the value of condition"""
        place = self.placeNode(net)
        if self._eval == Evaluation.true:
            place.setTrue()
        elif self._eval == Evaluation.false:
            place.setFalse()
        else:
            place.setUnknown()

    def update_from_place(self, net: PetriNet):
        """Update the condition from the place node (e.g. for a condition that is a state)"""
        place = self.placeNode(net)
        self._eval = Evaluation.from_tokens(place._tokens)

    def conditions(self):
        """Return self as a list of conditions"""
        return [self]

    def test(self):
        """Return the test value of the condition"""
        return self._test


class ActionCondition(Condition):
    def __init__(self, subject: str, verb_status: str, object: str, test=True):
        super().__init__(test)
        self._subject = subject
        self._verb_status = verb_status
        self._object = object

    def subject(self) -> Subject:
        return Subject.get_subject(self._subject)

    def verb_status(self) -> VerbStatus:
        return VerbStatus.get_verb_status(self._verb_status)

    def object(self) -> Subject:
        return Subject.get_subject(self._object)

    def label(self) -> str:
        return f"{self.subject().label()} {self.verb_status().value} {self.object().label()}"


class TemporalActionCondition(ActionCondition):
    def __init__(self, subject: str, verb_status: str, object: str, temporalExpression, test=True):
        super().__init__(subject, verb_status, object, test)
        self._temporalExpression = temporalExpression

    def label(self) -> str:
        return f"{super().label()} {self._temporalExpression}"


class TwoSubjectActionCondition(ActionCondition):
    def __init__(self, subject: str, verb_status: str, object: str, subject2: str, preposition: str, test=True):
        super().__init__(subject, verb_status, object, test)
        self._subject2 = subject2
        self._preposition = preposition

    def subject2(self):
        return Subject.get_subject(self._subject2)

    def preposition(self):
        return self._preposition

    def label(self):
        return f"{super().label()} {self._preposition} {self.subject2().label()}"


class TemporalTwoSubjectActionCondition(TwoSubjectActionCondition):
    def __init__(self, subject: str, verb_status: str, object: str, subject2: str, preposition: str, temporalExpression,
                 test=True):
        super().__init__(subject, verb_status, object, subject2, preposition, test)
        self._temporalExpression = temporalExpression

    def temporalExpression(self):
        return self._temporalExpression

    def label(self):
        return f"{super().label()} {self.temporalExpression()}"


class StatementCondition(Condition):
    def __init__(self, statement, test=True):
        super().__init__(test)
        self._statement = statement

    def statement(self) -> "Statement":  # type: ignore # Avoid circular import error
        return self._statement

    def label(self) -> str:
        return f"{self._statement.label()}"

    def draw(self, net: PetriNet):
        """Statement Condition, does not include the usual connection to the voiding transition"""
        # Draw the statement itself
        self.statement().placeNode(net)


class ExpressionCondition(Condition):
    def __init__(self, expression, test=True):
        super().__init__(test)
        self._expression = expression

    def expression(self) -> "Expression":  # type: ignore # Avoid circular import error
        return self._expression

    def label(self) -> str:
        return f"{self._expression.label()}"


class StateCondition(Condition):
    def __init__(self, state: State, test=True):
        super().__init__(test)
        self._state = state

    def state(self) -> State:
        return self._state

    def label(self) -> str:
        return f"{self._state.label()}"

    def draw(self, net: PetriNet):
        # Draw the state itself
        self._state.draw(net)


class AndCondition(Condition):
    def __init__(self, conditions: List[Condition], test=True):
        super().__init__(test)
        self._conditions = conditions
        # Flatten conditions
        self.flattenConditions()

    def flattenConditions(self) -> List[Condition]:
        """Combine nested AndConditions into a single AndCondition"""
        flattened_conditions = []
        for cond in self._conditions:
            if isinstance(cond, AndCondition):
                flattened_conditions.extend(cond.flattenConditions())
            else:
                flattened_conditions.append(cond)
        # return flattened_conditions
        self._conditions = flattened_conditions

    @classmethod
    def combined(cls, conditions: List[Condition], test=True) -> "AndCondition | Condition":
        """Combine a list of conditions into a single AndCondition"""
        if len(conditions) == 1:
            # If only one condition, return it directly
            return conditions[0]

        conds = []
        for cond in conditions:
            # Flatten the conditions and add them to the list of conditions (avoiding duplicates)
            conds.extend(cond.conditions())

        return cls(conds, test)

    def conditions(self) -> List[Condition]:
        """Return self as a list of all base conditions (i.e. no Or/And Conditions) recursively"""
        conditions = []
        for cond in self._conditions:
            conditions.extend(cond.conditions())
        return conditions

    def label(self) -> str:
        """Return a string representation of the condition"""
        # conditions = self.flattenConditions(
        conditions = self.conditions()
        return " \nAND\n ".join([condition.label() for condition in conditions])

    def draw(self, net: PetriNet):
        """Draw the condition on the PetriNet"""
        # Get Condition Place and Transition Nodes (for all conditions)
        and_place = self.placeNode(net, show_label=False)
        and_trans = self.transitionNode(net)
        # And condition is modelled as a single transition with multiple input places (one for each condition)

        for cond in self._conditions:
            # Draw each condition (recursively) and get the place node
            cond.draw(net)
            cond_place = cond.placeNode(net)

            if cond._test:
                # If condition is testing for True, Connect it to and Transition with read edge
                cond_place.read(and_trans)
            else:
                # If condition is testing for False, Connect it to and Transition with inhibit edge
                cond_place.inhibit(and_trans)

        and_trans.connect(and_place)


class OrCondition(Condition):
    def __init__(self, conditions: List[Condition], test=True):
        super().__init__(test)
        self._conditions = conditions

    @classmethod
    def combined(cls, conditions: List[Condition], test=True) -> "OrCondition | Condition":
        """Combine a list of conditions into a single OrCondition"""
        if len(conditions) == 1:
            return conditions[0]

        conds = []
        for cond in conditions:
            conds.extend(cond.conditions())

        return cls(conds, test)

    def conditions(self) -> List[Condition]:
        """Return self as a list of all base conditions (i.e. no Or/And Conditions) recursively"""
        conditions = []
        for cond in self._conditions:
            conditions.extend(cond.conditions())
        return conditions

    def label(self) -> str:
        """Return a string representation of the condition"""
        return " \nOR\n ".join([condition.label() for condition in self._conditions])

    def draw(self, net: PetriNet):
        """Draw the condition on the PetriNet"""
        # Get Condition Place and Transition Nodes (for all conditions)
        or_place = self.placeNode(net, show_label=False)
        # Or condition is modelled as a single place with multiple input transitions (one for each condition)

        for cond in self._conditions:
            # Draw each condition (recursively) and get the place node
            cond.draw(net)
            # If the condition is a AndCondition, take the transition node instead rather than repeating the condition
            # This is because the AndCondition will already have a transition node and will make the net more readable
            if isinstance(cond, AndCondition):
                cond_tran = cond.transitionNode(net)
                cond_tran.connect(or_place)
                continue

            # Otherwise get the place and transition nodes and connect accordingly
            cond_place = cond.placeNode(net)
            cond_tran = cond.transitionNode(net)

            if cond._test:
                cond_place.read(cond_tran)
            else:
                cond_place.inhibit(cond_tran)

            cond_tran.connect(or_place)
