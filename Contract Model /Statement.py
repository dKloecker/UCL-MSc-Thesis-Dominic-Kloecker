from Condition import Condition, ActionCondition, TemporalActionCondition, TwoSubjectActionCondition, \
    TemporalTwoSubjectActionCondition
from General import Evaluation
from General import ReferenceObject, Verb, ModalVerb
from PetriNet import Place, Transition, PetriNet
from Subject import Subject


class Statement:
    def __init__(self, subject, modalVerb, verb, object, valid=True) -> None:
        self._subject = subject
        self._modalVerb = modalVerb
        self._verb = verb
        self._object = object
        self._valid = valid
        self._eval = Evaluation.unknown
        self._condition = None
        # Initialise Nodes to None (will be set later to avoid repetition)
        self._place = None
        self._valid_transition = None
        self._invalid_transition = None

    def conditions(self):
        return []

    def condition(self):
        return self._condition

    def valid(self):
        return self._valid

    def subject(self):
        return Subject.get_subject(self._subject)

    def fulfillmentCondition(self) -> ActionCondition:
        """Condition that confirms that the statement has been fulfilled"""
        return ActionCondition(subject=self._subject,
                               verb_status=Verb.get_verb_status(self._verb).value,
                               object=self._object)

    def modalVerb(self) -> ModalVerb:
        """Modal verb of statement"""
        return ModalVerb.get_modal_verb(self._modalVerb)

    def verb(self) -> Verb:
        """Verb of statement"""
        return Verb.get_verb(self._verb)

    def object(self) -> ReferenceObject:
        """Object of statement"""
        return Subject.get_subject(self._object)

    def label(self) -> str:
        """Label of statement (for Petri Net)"""
        return f"{self.subject().label()} {self.modalVerb().value} {self.verb().value} {self.object().label()}"

    def __repr__(self) -> str:
        """String representation of statement"""
        return self.label()

    def fulfillmentPlace(self, net: PetriNet) -> Place:
        """Place that confirms that the statement has been fulfilled"""
        return self.fulfillmentCondition().placeNode(net)

    def update_from_place(self, net: PetriNet):
        """Update the statement from the place node"""
        place = self.placeNode(net)
        self._eval = Evaluation.from_tokens(place._tokens)

    def placeNode(self, net: PetriNet) -> Place:
        """Place node of statement"""
        if self._place is None:
            self._place = net.place(self.label())
        return self._place

    def enablingTransition(self, net: PetriNet) -> Transition:
        """Transition that enables the statement"""
        if self._valid_transition is None:
            self._valid_transition = net.transition(f"True That {self.label()}")
        return self._valid_transition

    def voidingTransition(self, net: PetriNet) -> Transition:
        """Transition that voids the statement"""
        if self._invalid_transition is None:
            self._invalid_transition = net.transition(f"Not True That {self.label()}")
        return self._invalid_transition

    def draw(self, net: PetriNet) -> None:
        # Get place node of statement
        place = self.placeNode(net)
        # Connect place to voiding transition (always required)
        voiding_transition = self.voidingTransition(net)
        place.connect(voiding_transition)

        if self._valid:
            # If statement is valid, connect enabling transition to place (to act as token source)
            valid_transition = self.enablingTransition(net)
            valid_transition.connect(place)
            # Connect statement fulfillment place to voiding transition (to remove tokens)
            fulfillment_place = self.fulfillmentPlace(net)
            fulfillment_place.read(voiding_transition)

    def eval(self) -> Evaluation:
        return self._eval

    def statement(self) -> "Statement":
        return self

    def test(self) -> bool:
        return self._valid


class TemporalStatement(Statement):
    def __init__(self, subject, modalVerb, verb, object, temporalExpression, valid=True):
        super().__init__(subject, modalVerb, verb, object, valid)
        self._temporalExpression = temporalExpression
        self._fulfilled_cond = self.fulfillmentCondition()

    def label(self):
        return f"{self.subject().label()} {self.modalVerb().value} {self.verb().value} {self.object().label()} {self.temporalExpression().label()} "

    def temporalExpression(self):
        return self._temporalExpression

    def fulfillmentCondition(self):
        return TemporalActionCondition(subject=self._subject,
                                       verb_status=Verb.get_verb_status(self._verb).value,
                                       object=self._object, temporalExpression=self.temporalExpression())


class TwoSubjects(Statement):
    def __init__(self, subject, modalVerb, verb, object, preposition, subject2, valid=True):
        super().__init__(subject, modalVerb, verb, object, valid)
        self._subject2 = subject2
        self._preposition = preposition
        self._fulfilled_cond = self.fulfillmentCondition()

    def subject2(self):
        return Subject.get_subject(self._subject2)

    def label(self):
        return f"{self.subject().label()} {self.modalVerb().value} {self.verb().value} {self.object().label()} {self._preposition} {self.subject2().label()}"

    def fulfillmentCondition(self):
        return TwoSubjectActionCondition(subject=self._subject, object=self._object,
                                         verb_status=Verb.get_verb_status(self._verb).value,
                                         subject2=self._subject2,
                                         preposition=self._preposition)


class TemporalTwoSubjects(TwoSubjects):
    def __init__(self, subject, modalVerb, verb, object, preposition, subject2, temporalExpression, valid=True):
        super().__init__(subject, modalVerb, verb, object, preposition, subject2, valid)
        self._temporalExpression = temporalExpression
        self._fulfilled_cond = self.fulfillmentCondition()

    def temporalExpression(self):
        return self._temporalExpression

    def label(self):
        return f"{self.subject().label()} {self.modalVerb().value} {self.verb().value} {self.object().label()} {self._preposition} {self.subject2().label()} {self.temporalExpression().label()}"

    def fulfillmentCondition(self):
        return TemporalTwoSubjectActionCondition(subject=self._subject, object=self._object,
                                                 verb_status=Verb.get_verb_status(self._verb).value,
                                                 subject2=self._subject2, preposition=self._preposition,
                                                 temporalExpression=self.temporalExpression())


class ConditionalStatement(Statement):
    def __init__(self, condition: Condition, statement: Statement):
        # super().__init__(None, None, None, None, None, True)
        self._condition = condition
        self._statement = statement
        self._transition = None
        self._valid_transition = None
        self._invalid_transition = None

    def condition(self):
        return self._condition

    def valid(self):
        return self.statement().valid()

    def fulfillmentCondition(self):
        return self._statement.fulfillmentCondition()

    def modalVerb(self):
        return self.statement().modalVerb()

    def statement(self):
        return self._statement

    def conditions(self):
        return self._condition.conditions()

    def label(self):
        return f"IF {self._condition.label()} THEN {self._statement.label()}"

    def transitionNode(self, net: PetriNet) -> Transition:
        # Combines the Condition and Statement into a single Transition Node to enable the Condition to be evaluated
        # before the Statement is executed
        if self._transition is None:
            self._transition = net.transition(self.label())
        return self._transition

    def enablingTransition(self, net: PetriNet) -> Transition:
        if self._valid_transition is None:
            self._valid_transition = net.transition(f"Enabling: {self.label()}")
        return self._valid_transition

    def voidingTransition(self, net: PetriNet) -> Transition:
        if self._invalid_transition is None:
            self._invalid_transition = net.transition(f"Voiding {self.label()}")
        return self._invalid_transition

    def update_from_place(self, net: PetriNet):
        # Update Condition and Statement
        self._condition.update_from_place(net)
        self._statement.update_from_place(net)

    def eval(self):
        return self._statement.eval()

    def draw(self, net: PetriNet):
        # Draw Condition and Any Sub-Conditions
        self.condition().draw(net)
        # Draw Statement

        # Get Conditional Place Node and Statement Place Node
        condition_place = self.condition().placeNode(net)
        statement_place = self.statement().placeNode(net)
        voiding_transition = self.voidingTransition(net)
        statement_place.connect(voiding_transition)

        if self.statement()._valid:
            target_transition = self.enablingTransition(net)
            target_transition.connect(statement_place)
            fulfillment_place = self.statement().fulfillmentPlace(net)
            fulfillment_place.read(voiding_transition)
        else:
            target_transition = voiding_transition
    
        if self.condition().test():
            condition_place.read(target_transition)
        else:
            condition_place.inhibit(target_transition)


