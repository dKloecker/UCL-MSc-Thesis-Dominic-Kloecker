class Subject:
    all_subjects = {}
    
    def __init__(self, label:str) -> None:
        self._label = label.strip()
        # Capitalize the first letter of the subject
        self._label = self._label[0].upper() + self._label[1:]
        # Add the subject to the list of all subjects if it is not already there 
        Subject.add_subject(self)
            
    # Return the label of the subject
    def label(self) -> str:
        return self._label
    
    # Add a subject to the list of all subjects
    @staticmethod
    def add_subject(new_subject:"Subject"):
        if new_subject.label() not in Subject.all_subjects:
            Subject.all_subjects[new_subject.label()] = new_subject
        
    # Return the subject with the given label
    @staticmethod
    def get_subject(label:str) -> "Subject":
        # Remove any whitespace from the label and capitalize the first letter
        label = label.strip()
        label = label[0].upper() + label[1:]
        if label not in Subject.all_subjects:
            new_subject = Subject(label)
            return new_subject 
        return Subject.all_subjects[label]
    
    # Redefine the subject with the given label
    @staticmethod
    def redefine_subject(label, value) -> None:
        # The redefining of a subject allows us to keep track of active definitions
        # E.g. a  definition statement such as ExcessParty is PartyA, can then make the value of ExcessParty be PartyA
        Subject.all_subjects[label] = value
        
    # Return string representation of the subject
    def __repr__(self) -> str:
        return self.label()
    
    