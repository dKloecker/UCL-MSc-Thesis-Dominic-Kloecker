from typing import List

from PetriNet import Place, Transition, PetriNet
from General import Evaluation


# States are overall States of the contract (E.g. Default, Breach etc.)
class State:
    def __init__(self, label: str, condition=None) -> None:
        self._condition = condition
        self._label = label
        self._eval = Evaluation.unknown

    def eval(self) -> Evaluation:
        return self._eval

    def label(self) -> str:
        return self._label

    def condition(self) -> "Condition":  # Avoid circular import by using string
        return self._condition

    def conditions(self) -> List["Condition"]:
        if self._condition is None:
            return []
        return self._condition.conditions()

    def update_from_place(self, net: PetriNet):
        """Update the condition from the place node in the net"""
        place = self.placeNode(net)
        self._eval = Evaluation.from_tokens(place._tokens)

    def __repr__(self) -> str:
        return self.label()

    def placeNode(self, net: PetriNet) -> Place:
        return net.place(self.label())

    def transitionNode(self, net: PetriNet) -> Transition:
        return net.transition(f"Void {self.label()} Transition")

    def draw(self, net: PetriNet):
        """Draw the state"""
        # States always have a place and a transition node
        state_place = self.placeNode(net)
        enabling_transition = self.transitionNode(net)

        # Draw the condition if there is one
        if self._condition is None:
            pass
        else:
            # Draw the condition and connect it to the state
            self._condition.draw(net)
            condition_place = self._condition.placeNode(net)
            if self._condition.test():
                condition_place.read(enabling_transition)
            else:

                condition_place.inhibit(enabling_transition)

            enabling_transition.connect(state_place)
