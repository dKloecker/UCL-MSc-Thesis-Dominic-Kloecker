from General import Evaluation
from PetriNet import Place, Transition, PetriNet
from Subject import Subject


class Definition:
    """Base class for Definitions"""
    def __init__(self):
        self._place = None
        self._transition = None
        self._eval = Evaluation.unknown
        self._condition = None

    def eval(self) -> Evaluation:
        return self._eval

    def label(self) -> str:
        return "Definition"

    def condition(self) -> "Condition": # Avoid circular import by using string
        return self._condition

    def __repr__(self) -> str:
        return self.label()

    def placeNode(self, net: PetriNet) -> Place:
        if self._place is None:
            self._place = net.place(self.label())
        return self._place

    def transitionNode(self, net: PetriNet) -> Transition:
        if self._transition is None:
            self._transition = net.transition(f"{self.label()} Transition")
        return self._transition

    def draw(self, net: PetriNet):
        # Draw Definition with enabling transition
        place = self.placeNode(net)
        transition = self.transitionNode(net)
        transition.connect(place)

    def conditions(self):
        return []

    def update_from_place(self, net: PetriNet):
        # Get Updated Evaluation from Place Node
        place = self.placeNode(net)
        self._eval = Evaluation.from_tokens(place._tokens)


class IsDefinition(Definition):
    def __init__(self, subject: str, value: str):
        super().__init__()
        self._subject = subject
        self._value = value

    def subject(self):
        return Subject.get_subject(self._subject)

    def value(self):
        return self._value

    def label(self):
        return f"{self._subject} IS {self._value}"


class EqualsDefinition(Definition):
    def __init__(self, subject: str, expression: str):
        super().__init__()
        self._subject = subject
        self._expression = expression.strip()

    def subject(self) -> Subject:
        return Subject.get_subject(self._subject)

    def expression(self) -> str:
        return self._expression

    def label(self) -> str:
        return f"{self._subject} EQUALS {self._expression}"


class ConditionalDefinition(Definition):
    def __init__(self, condition, definitions):
        self._condition = condition
        self._definitions = definitions

    def update_from_place(self, net: PetriNet):
        """Update the definitions from their place nodes in the net (for simulation)"""
        for definition in self._definitions:
            definition.update_from_place(net)

    def eval(self) -> Evaluation:
        """As all definitions have same condition, just return the first one"""
        return self._definitions[0].eval()

    def condition(self) -> "Condition":
        return self._condition

    def conditions(self) -> ["Condition"]:
        return self._condition.conditions()

    def label(self) -> str:
        """Return the labels of all definitions"""
        return ' AND '.join(["( " + d.label() + " )" for d in self._definitions])

    def draw(self, net: PetriNet):
        """Draw the conditional definition"""
        self._condition.draw(net)
        cond_place = self._condition.placeNode(net)

        for definition in self._definitions:
            def_place = definition.placeNode(net)
            def_transition = definition.transitionNode(net)

            if self._condition.test():
                cond_place.read(def_transition)
            else:
                cond_place.inhibit(def_transition)

            def_transition.connect(def_place)
