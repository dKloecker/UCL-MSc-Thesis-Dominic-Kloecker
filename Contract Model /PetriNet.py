# -*- coding: utf-8 -*-
import collections
import textwrap
import datetime
from graphviz import Digraph


def node_from_list(node_label: str, node_list: list) -> "Place | Transition":
    """Get node object from list based on node label"""
    ind = [n._label for n in node_list].index(node_label)
    return node_list[ind]


class Edge:
    """Class representing a connection edge in a Petri net."""

    def __init__(self, from_node: "Place | Transition", to_node: "Place | Transition", weight: int = 1):
        self._from_node = from_node
        self._to_node = to_node
        self.weight = weight
        self._hash_id = Edge._hash(from_node, to_node)
        self.arrow_head = "normal"

    def _type(self) -> str:
        """Returns the type of the edge."""
        return "connect"

    def __repr__(self) -> str:
        """Returns a string representation of the edge."""
        return self._hash_id

    @staticmethod
    def _hash(from_node, to_node) -> str:
        """Generate a hash ID for an edge to ensure edge ids are unique."""
        return f"C - {from_node._id} - {to_node._id}"

    @staticmethod
    def connect(from_node, to_node):
        """Connect nodes with an edge if they aren't already connected."""
        edge_hash = Edge._hash(from_node, to_node)
        if edge_hash not in [e._hash_id for e in from_node._out_edges]:
            edge = Edge(from_node, to_node)
            from_node._out_edges.append(edge)
            to_node._in_edges.append(edge)

    def canFire(self) -> bool:
        """Check if the edge can be triggered."""
        return self._from_node.canFire()

    def active(self) -> bool:
        """Check if the edge is active."""
        return self._from_node.active()


class InhibitorEdge(Edge):
    def __init__(self, from_node: "Node | Place",
                 to_node: "Node | Transition", weight: int = 1):
        super().__init__(from_node, to_node, weight)  # type:ignore
        self.arrow_head = "dot"

    def _type(self) -> str: return "inhibit"

    def canFire(self) -> bool:
        """Check if the edge can be triggered."""
        return self._from_node.canFire() == False and self._from_node.active() == True

    @staticmethod
    def _hash(from_node, to_node) -> str:
        """Generate a hash ID for an edge to ensure edge ids are unique."""
        return f"I - {from_node._id} - {to_node._id}"

    @staticmethod
    def connect(from_node: "Node | Place",
                to_node: "Transition | Place") -> None:
        """Connect nodes with an edge if they aren't already connected."""
        edge_hash = InhibitorEdge._hash(from_node, to_node)
        if edge_hash not in [e._hash_id for e in from_node._out_edges]:
            edge = InhibitorEdge(from_node, to_node)
            from_node._out_edges.append(edge)
            to_node._in_edges.append(edge)


class ReadEdge(Edge):
    def __init__(self, from_node: "Node | Place",
                 to_node: "Node |Transition") -> None:
        super().__init__(from_node, to_node)  # type:ignore
        self.arrow_head = "odot"

    def _type(self) -> str: return "read"

    @staticmethod
    def _hash(from_node, to_node) -> str:
        """Generate a hash ID for an edge to ensure edge ids are unique."""
        return f"R - {from_node._id} - {to_node._id}"

    @staticmethod
    def connect(from_node: "Place", to_node: "Transition | Place") -> None:
        """Connect nodes with an edge if they aren't already connected."""
        edge_hash = ReadEdge._hash(from_node, to_node)
        if edge_hash not in [e._hash_id for e in from_node._out_edges]:
            edge = ReadEdge(from_node, to_node)
            from_node._out_edges.append(edge)
            to_node._in_edges.append(edge)


class Node:
    """Base class representing a node in a Petri net."""
    cnt = 0  # Counter for unique node IDs

    def __init__(self, label: str, in_edges: list, out_edges: list, active=True):
        self._id = Node.cnt
        self._label = label
        self._active = active
        self._in_edges = in_edges if in_edges else []
        self._out_edges = out_edges if out_edges else []
        self._color = 'white'

        Node.cnt += 1

    def connect(self, target):
        """Connect to another node through normal edge"""
        Edge.connect(self, target)

    def read(self, target):
        """Connect to another node through read edge"""
        ReadEdge.connect(self, target)  # type:ignore

    def inhibit(self, target):
        """Connect to another node through inhibitor edge"""
        InhibitorEdge.connect(self, target)


class Place(Node):
    def __init__(self, label, in_edges: list[Edge],
                 out_edges: list[Edge | ReadEdge | InhibitorEdge],
                 tokens=-1, substate: bool = False, show_label: bool = True):
        super().__init__(label, in_edges, out_edges)
        self._tokens = tokens
        self._substate = substate
        self._show_label = show_label

    def shape(self) -> str:
        """Return the shape of the place."""
        return "egg" if self._show_label else "circle"  # type:ignore

    def canFire(self) -> bool:
        """Check if the place can be triggered."""
        return self._tokens >= 1

    def active(self) -> bool:
        """Check if the place is active."""
        return self._tokens >= 0

    def setTrue(self) -> None:
        """Set the place to have a token."""
        self._tokens = 1

    def setFalse(self) -> None:
        """Set the place to not have a token."""
        self._tokens = 0

    def setUnknown(self) -> None:
        """Set the place to have an unknown number of tokens."""
        self._tokens = -1

    def color(self) -> str:
        """Return the color of the place."""
        if self._tokens <= -1:
            return "grey"
        elif self._tokens == 0:
            return "white"
        else:
            return "green"

    def label(self):
        """For visual clarity, format the label to be within a 30 character bounding box"""
        if not self._show_label:
            return ""
        label = self._label
        if len(label) > 30:
            label = textwrap.fill(label, 25, break_long_words=False)

        return label

    def fire(self):
        """BFS traversal to fire outgoing transitions, if possible"""
        Q = collections.deque([e for e in self._out_edges])
        while Q:
            edge = Q.popleft()
            transition = edge._to_node
            if transition.canFire():
                transition.fire()

            # add next set of edges to Q
            Q.extend([e for e in transition._out_edges])


class Transition(Node):
    def __init__(self, label: str, in_edges: list[Edge | ReadEdge | InhibitorEdge],
                 out_edges: list[Edge]):
        super().__init__(label, in_edges, out_edges)
        # self.shape = "box"

    def shape(self) -> str:
        """Return the shape of the transition."""
        return "rectangle"

    def canFire(self) -> bool:
        """Check if the transition can be triggered."""
        return all([e.canFire() for e in self._in_edges]) or self._in_edges == []

    def active(self) -> bool:
        """Check if the transition is active."""
        return all([e.active() for e in self._in_edges]) or self._in_edges == []

    def fire(self):
        if self.canFire():
            for e in self._out_edges:
                e._to_node.setTrue()

            for e in self._in_edges:
                # Only consume tokens if the edge is a edge and not a read or inhibitor edge
                if e._type() == "connect":
                    e._from_node.setFalse()


class PetriNet:
    """Class representing a Petri net."""

    def __init__(self):
        self.places = []
        self.transitions = []

    def place(self, label: str, show_label=True) -> Place:
        """Get or create a place in the Petri net with the given label."""
        if any([p._label == label for p in self.places]):
            return node_from_list(label, self.places)  # type: ignore
        else:
            place = Place(label, in_edges=[], out_edges=[], show_label=show_label)
            self.places.append(place)
            return place

    def transition(self, label: str) -> Transition:
        """Get or create a transition in the Petri net with the given label."""
        if any([t._label == label for t in self.transitions]):
            return node_from_list(label, self.transitions)  # type: ignore
        else:
            tran = Transition(label, in_edges=[], out_edges=[])
            self.transitions.append(tran)
            return tran

    @staticmethod
    def draw_edge(dot, edges_drawn, edge: Edge):
        """Draw an edge if it hasn't been drawn already and add it to the list of drawn edges."""
        if edge._hash_id in edges_drawn:
            return
        dot.edge(str(edge._from_node._id), str(edge._to_node._id), arrowhead=edge.arrow_head)
        edges_drawn.append(edge._hash_id)

    def assemble(self, engine: str = 'dot'):
        dot = Digraph(comment='PetriNet')
        dot.engine = engine
        dot.graph_attr['overlap'] = 'false'
        edges_drawn = []

        # Draw places nodes
        for place in self.places:
            dot.node(str(place._id),
                     label=place.label(),
                     style="filled",
                     fillcolor=place.color(),
                     shape=place.shape(),
                     fontsize='30', fontname="Arial", fontweight='bold')

            # Draw edges for place
            for edge in place._in_edges:
                PetriNet.draw_edge(dot, edges_drawn, edge)
            for edge in place._out_edges:
                PetriNet.draw_edge(dot, edges_drawn, edge)

        # Draw transition nodes
        for transition in self.transitions:
            # Draw transition nodes, only if they have least one connection
            if transition._in_edges == [] and transition._out_edges == []:
                continue

            dot.node(str(transition._id),
                     label="",
                     style="filled",
                     fillcolor=transition._color,
                     height="1.5",
                     width="0.15",
                     shape=transition.shape())

            # Draw Edges for transition
            for edge in transition._in_edges:
                PetriNet.draw_edge(dot, edges_drawn, edge)
            for edge in transition._out_edges:
                PetriNet.draw_edge(dot, edges_drawn, edge)

        return dot

    def fire_transitions(self):
        """Trigger all transitions in Petri Net.
         Used for initial value evaluation (i.e. unconditional transitions)"""
        for transition in self.transitions:
            transition.fire()

    def render(self):
        """Render the Petri net using Graphviz."""
        dot = self.assemble()
        # Make the graph left to right (instead of top to bottom)
        dot.attr(rankdir='LR')
        # Reduce the spacing between nodes and edges for visual clarity
        dot.attr(nodesep='0.15')
        dot.attr(ranksep='0.3')
        dot.attr(newrank='true')
        # Make edges thicker for visual clarity
        dot.attr(penwidth='2')

        dot.format = 'png'

        # Add timestamp to filename to avoid overwriting
        filename = f"outputs/_{datetime.datetime.now().strftime('%Y%m%d%H%M%S')}"
        # Render the graph
        dot.render(filename, view=True, cleanup=True)
