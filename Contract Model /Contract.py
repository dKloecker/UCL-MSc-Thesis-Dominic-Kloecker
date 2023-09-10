from typing import Dict

import pandas as pd

from Condition import Condition, AndCondition, \
    OrCondition
from Definition import Definition, ConditionalDefinition
from General import Evaluation
from PetriNet import PetriNet
from State import State
from Statement import Statement, ConditionalStatement


# Helper Function to Truncate Labels (for display)
def truncate_label(label):
    return label[:65] + '...' if len(label) > 65 else label


class Contract:
    def __init__(self) -> None:
        self._definitions: Dict = {}
        self._statements: Dict = {}
        self._states: Dict = {}

        self._petriNet = PetriNet()

        self._all_conditions = []
        self._all_statements = []
        self._all_states = []

        self._events = []

    def statement(self, statement: Statement):
        """Add a statement to the contract"""
        label = f"{statement.label()} {statement.statement().valid()}"
        if not isinstance(statement, ConditionalStatement):
            self._statements[label] = statement
            self._all_statements.append(statement)
            return

        # If Conditional Then Combine the Conditions as Or Conditions
        label = f"{statement.label()} {statement.statement().valid()}"
        if label in self._statements:
            previous = self._statements[label]
            previous._condition = OrCondition([previous.condition(), statement.condition()])
        else:
            self._statements[label] = statement

    def definition(self, definition: Definition):
        """Add a definition to the contract"""
        definition_label = definition.label()
        if not isinstance(definition, ConditionalDefinition):
            self._definitions[definition_label] = definition
            return

        # If Conditional and existst Then Combine the Conditions as Or Conditions
        if definition_label in self._definitions:
            previous = self._definitions[definition_label]
            previous._condition = OrCondition([previous.condition(), definition.condition()])
        else:
            self._definitions[definition_label] = definition

    def state(self, state: State):
        """Add a state to the contract"""
        if state.label() in self._states:
            # print(f"State {state.label()} already exists")
            return
        self._states[state.label()] = state
        self._all_states.append(state)

    def conditions(self) -> list[Condition]:
        # Return all conditions in the contract 
        # If already processed return the list of conditions
        if self._all_conditions:
            return self._all_conditions

        conditions = []
        for definition in self.definitions():
            conditions.extend(definition.conditions())

        for statement in self.statements():
            conditions.extend(statement.conditions())
            if statement.valid():
                conditions.append(statement.fulfillmentCondition())

        for state in self.states():
            conditions.extend(state.conditions())

        unique_conditions = []
        # Get Unique Conditions by Label (as objects are different and can not be compared)
        for condition in conditions:
            if condition.label() not in [c.label() for c in unique_conditions]:
                unique_conditions.append(condition)

        # Sort the conditions by label and then by the evaluation 
        unique_conditions.sort(key=lambda x: (x.label(), x._eval.value))  # type: ignore
        # Update the list of all conditions so it does not need to be processed again
        self._all_conditions = unique_conditions
        return unique_conditions

    def statements(self) -> list[Statement]:
        return list(self._statements.values())

    def definitions(self) -> list[Definition]:
        return list(self._definitions.values())

    def states(self) -> list[State]:
        return list(self._states.values())

    def render(self) -> None:
        # Render the Petri Net
        self._petriNet.render()
        
    def draw(self):
        """Draw all nodes and edges in the Petri Net"""
        for statement in self._statements.values():
            statement.draw(self._petriNet)

        for definition in self._definitions.values():
            definition.draw(self._petriNet)

        for state in self._states.values():
            state.draw(self._petriNet)

    def print_conditions(self):
        # If conditions are empty return empty list
        if len(self.conditions()) == 0:
            return []
        
        headers = ["ID", "Status", "Condition"]
        widths = [3, 10, 70]
        self.print_headers(headers, widths, "Conditions")

        table_values = []
        for i, condition in enumerate(self.conditions()):
            status = condition._eval.value
            short_status = status[0]  # First Letter of the Status (T or F or U)
            # Justify the text to be alligned in columns
            print(f"|_ C{i + 1:<2} _|_ {status:<10} _|_ {condition.label():<70} _|")
            # print(f"{i+1}) {condition._eval} | {condition.label()}")
            table_values.append({"ID": f"C{i + 1}", "Status": short_status, "Label": condition.label()})

        return table_values

    def print_definitions(self):
        # If definitions are empyty return empty list
        if len(self._definitions) == 0:
            return []
        
        headers = ["ID", "Status", "Definition", "Conditions"]
        widths = [3, 15, 125, 30]
        self.print_headers(headers, widths, "Definitions")
        table_values = []
        for i, definition in enumerate(self.definitions()):
            # if definition.eval() == Evaluation.true:
            #     status = "Active"
            #     short_status = "A"
            # else:
            #     status = "Not Active"
            #     short_status = "NA"
            status = definition.eval().value
            short_status = status[0]  # First Letter of the Status (T or F or U)

            print(
                f"|_ D{i + 1:<2} _|_ {status:<15} _|_ {definition.label():<125} _|_ {self.formatConditions(definition):30} _|")

            table_values.append(
                {"ID": f"D{i + 1}", "Status": short_status, "Label": definition.label()})

        return table_values

    def print_statements(self):
        # Sort the statements by the modal verb
        # print("========================= Statements ===============================\n")

        self._all_statements.sort(key=lambda x: x.modalVerb().get_modal_type())  # type: ignore
        headers = ["ID", "Status", "Statement", "Statement Type & Conditions"]
        widths = [3, 10, 80, 64]
        self.print_headers(headers, widths, "Statements")
        # Iterate through the statements and print their details
        # Get status for final table of history
        table_values = []
        for i, statement in enumerate(self.statements()):
            # Get the status of the statement
            status = statement.eval().value
            short_status = status[0]  # First Letter of the Status (T or F or U)


            statement_type = "Enabling"
            if not statement.valid():
                statement_type = "Voiding"

            # Format strings to be alligned in columns by justifying the text
            print(
                f"|_ S{i + 1:<2} _|_ {status:<10} _|_ {statement.statement().label():<80} _|_ {statement_type:<10} <- {self.formatConditions(statement):50} _|")

            table_values.append(
                {"ID": f"S{i + 1}", "Status": short_status, "Label": statement.statement().label()})

        return table_values

    @staticmethod
    def print_headers(headers, widths, table_name=""):
        # Print the headers for the table
        header_string = "| "
        for i, header in enumerate(headers):
            header_string += f" {header:<{widths[i]}}  | "

        print("-" * len(header_string))
        # Print the table name in the middle of the table
        if table_name:
            table_name_string = f"| {table_name:^{len(header_string) - 5}} |"
            print(table_name_string)

        print("-" * len(header_string))
        print(header_string)
        print("-" * len(header_string))

    def processEvent(self, event: "Event"):
        # Update the value of the condition
        event.update_value()
        # Update the Petri Net
        event.update_net(self._petriNet)

    def updateFromNet(self):
        # Update the statements and conditions from the token flow in the Petri Net
        # This is used to be able to test against the Petri Net in the interactive simulation
        for statement in self._statements.values():
            statement.update_from_place(self._petriNet)

        for definition in self._definitions.values():
            definition.update_from_place(self._petriNet)

        for state in self._states.values():
            state.update_from_place(self._petriNet)

    def formatConditions(self, conditional_object):
        # Formatted output of conditions for statements and definitions 
        # This is used to be able to test against the Petri Net in the interactive simulation
        # Show the conditions and their values
        condition = conditional_object.condition()
        if condition is None:
            return "Uncoditional"

        all_conditions = self.conditions()
        condition_labels = [c.label() for c in all_conditions]
        cond_lables = []
        for cond in conditional_object.conditions():
            # Append index of condition
            ind = condition_labels.index(cond.label())
            lbl = f"(C{ind + 1}, {cond.test()})"
            cond_lables.append(lbl)

        # Check if the condition is a special condition e.g. And or Or for formatting
        if isinstance(condition, AndCondition):
            return f"{' & '.join(cond_lables)}"
        elif isinstance(condition, OrCondition):
            return f"{' | '.join(cond_lables)}"
        else:
            return cond_lables[0]

    def performanceHistory(self, history, event):
        """Table to track the history of simulated performance"""

    def interaciveSimulation(self):
        # Render the Petri Net
        self.draw()
        # Trigger unconditional transitions 
        self._petriNet.fire_transitions()
        self.render()
        self.updateFromNet()

        # Index is ID
        performance_history = pd.DataFrame(columns=["ID", "Label", "Status"])
        event_history = pd.DataFrame(columns=["ID", "Old Value", "New Value"])
        event_count = 1

        # Print Initial Conditions
        cond_table_values = self.print_conditions()
        print("\n")
        definition_table_values = self.print_definitions()
        print("\n")
        statement_table_values = self.print_statements()

        # Add the list of dictionaries (rows) to the performance history dataframe for each table
        performance_history = pd.concat([performance_history, pd.DataFrame(cond_table_values)], ignore_index=True)
        performance_history = pd.concat([performance_history, pd.DataFrame(definition_table_values)], ignore_index=True)
        performance_history = pd.concat([performance_history, pd.DataFrame(statement_table_values)], ignore_index=True)

        self.render()

        while True:
            # Ask for the condition to be updated
            condition_index = input("\n\nEnter the ID of the condition to be updated e.g. 7 for C7 or C to Abort: ")
            if condition_index not in [str(i) for i in range(1, len(self.conditions()) + 1)] + ["C", "c"]: 
                # Handle invalid input
                print("Invalid ID. Please try again.")
                continue 
            elif condition_index == "C" or condition_index == "c":
                break
            
            # Get the condition
            condition = self.conditions()[int(condition_index) - 1]
            # Get the value of the condition
            value = input("\n\nEnter the new value: True | False | Unknown: ")
            # Check if the value entered is valid
            if value not in ["True", "False", "Unknown"]:
                print("Invalid value. Please try again.")
                continue
            
            # Create the event
            event = Event(condition, Evaluation(value.upper()))
            # Add the event to the event history
            event_history.loc[event_count] = [
                f"C{condition_index}", condition._eval.value, event.value().value]

            # Process the event and trigger token flow if possible
            self.processEvent(event)
            # Update the statements and conditions from the token flow in the Petri Net
            self.updateFromNet()

            # Add new column to performance history dataframe for each event E1 E2 E3 etc.
            # to contain updated status values
            performance_history[f"E{event_count}"] = ""

            print("\n")
            # Add New Column
            # Print the conditions
            cond_table_values = self.print_conditions()
            # Add new status values of items into same row but new column
            performance_history.loc[
                performance_history["ID"].isin([d['ID'] for d in cond_table_values]), f"E{event_count}"] = [
                d['Status'] for d in cond_table_values]

            print("\n")

            # If definitions are not empty
            if len(self._definitions) > 0:
                definition_table_values = self.print_definitions()
                # Add new status values of items into same row but new column
                performance_history.loc[
                    performance_history["ID"].isin([d['ID'] for d in definition_table_values]), f"E{event_count}"] = [
                    d['Status'] for d in definition_table_values]

            print("\n")

            # Print the statements 
            if len(self._statements) > 0:
                statement_table_values = self.print_statements()
                # Add new status values of items into same row but new column
                performance_history.loc[
                    performance_history["ID"].isin([d['ID'] for d in statement_table_values]), f"E{event_count}"] = [
                    d['Status'] for d in statement_table_values]

            self.render()
            event_count += 1
            

        # For formatting truncate the label to max of 40 characters]
        print("============================== Performance History ==============================")
        performance_history["Label"] = performance_history["Label"].apply(truncate_label)
        print(performance_history)
        print("============================== Event History ==============================")
        print("\n\n")
        print(event_history.transpose())

class Event:
    """
    An event is a condition and its new value.
    """

    def __init__(self, condition: Condition, value: Evaluation) -> None:
        self._condition = condition
        self._value = value

    def condition(self):
        """The condition that is being changed."""
        return self._condition

    def value(self):
        """The new value for the condition."""
        return self._value

    def update_value(self):
        """Update the condition to have the new value."""
        self._condition._eval = self._value

    def update_net(self, net: PetriNet):
        """Update the Petri net to have the new value for the condition."""
        self._condition.updatePlace(net)
        self._condition.placeNode(net).fire()

    def __repr__(self) -> str:
        return f"{self._condition} = {self._value}"
