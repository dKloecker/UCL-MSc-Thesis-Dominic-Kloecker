from Contract import Contract
from Definition import *
from Expression import *
from Statement import *
from Condition import *


def cola_isda_original():
    # isda  = "IF [1a] it is the case that PartyA paid AmountA on the 01 January 1970 AND [1b] it is the case that PartyB paid AmountB on the 01 January 1970 THEN [2a] it is not the case that PartyA shall pay AmountA on the 01 January 1970 AND [2b] it is not the case that PartyB shall pay AmountB on the 01 January 1970 "
    #           ++ "<AND> [3] it is the case that ExcessParty shall pay the excess amount of currency on the 01 January 1970 "
    #           ++ "<AND> IF [4a] it is the case that PartyA paid more than PartyB THEN [4bi] ExcessParty IS PartyA AND [4bii] the excess amount of currency EQUALS AmountA MINUS AmountB "
    #           ++ "<AND> IF [5a] it is the case that PartyB paid more than PartyA THEN [5bi] ExcessParty IS PartyB AND [5bii] the excess amount of currency EQUALS AmountB MINUS AmountA."
    
    con = Contract()

    con.statement(TemporalStatement('"ExcessParty"', 'SHALL', 'Pay', 'SomeCurrency "ExcessAmount"', TemporalExpression('ON','(1,1,1970)'), valid=True))

    con.statement(ConditionalStatement(condition=AndCondition(conditions=[StatementCondition(statement=TemporalStatement('"PartyA"', 'SHALL', 'Pay', 'SomeCurrency "AmountA"', TemporalExpression('ON', '(1,1,1970)'), valid=True), test=True),
                                                                          StatementCondition(statement=TemporalStatement('"PartyB"', 'SHALL', 'Pay', 'SomeCurrency "AmountB"', TemporalExpression('ON','(1,1,1970)'), valid=True), test=True),
                                                                          ]), statement=TemporalStatement('"PartyA"', 'SHALL', 'Pay', 'SomeCurrency "AmountA"', TemporalExpression('ON','(1,1,1970)'), valid=False)))

    con.statement(ConditionalStatement(condition=AndCondition(conditions=[StatementCondition(statement=TemporalStatement('"PartyA"', 'SHALL', 'Pay', 'SomeCurrency "AmountA"', TemporalExpression('ON', '(1,1,1970)'), valid=True), test=True),
                                                                          StatementCondition(statement=TemporalStatement('"PartyB"', 'SHALL', 'Pay', 'SomeCurrency "AmountB"', TemporalExpression('ON','(1,1,1970)'), valid=True), test=True),
                                                                          ]), statement=TemporalStatement('"PartyB"', 'SHALL', 'Pay', 'SomeCurrency "AmountB"', TemporalExpression('ON','(1,1,1970)'), valid=False)))

    con.definition(ConditionalDefinition(condition=ExpressionCondition(BooleanExpression('"PartyB"', 'Paid', 'MoreThan', '"PartyA"'), test=True), definitions=[IsDefinition('"ExcessParty"', '"PartyB"'),
                    EqualsDefinition('SomeCurrency "ExcessAmount"', 'OtherObject "AmountB" MINUS OtherObject "AmountA"'),
                    ])
    )

    con.definition(ConditionalDefinition(condition=ExpressionCondition(BooleanExpression('"PartyA"', 'Paid', 'MoreThan', '"PartyB"'), test=True), definitions=[IsDefinition('"ExcessParty"', '"PartyA"'),
                    EqualsDefinition('SomeCurrency "ExcessAmount"', 'OtherObject "AmountA" MINUS OtherObject "AmountB"'),
                    ])
    )

    con.interaciveSimulation()


def cola_isda_improved():
    # isda_2 = "IF [1] it is the case that PartyA shall pay AmountA on the 01 January 1970 AND [2] it is the case that PartyB shall pay AmountB on the 01 January 1970 THEN [3] it is not the case that PartyA shall pay AmountA on the 01 January 1970 AND [4] it is not the case that PartyB shall pay AmountB on the 01 January 1970 AND [5] it is the case that ExcessParty shall pay the excess amount of currency on the 01 January 1970 "
    #           ++ "<AND> IF [6] it is the case that PartyA paid more than PartyB THEN [7] ExcessParty IS PartyA AND [8] the excess amount of currency EQUALS AmountA MINUS AmountB "
    #           ++ "<AND> IF [9] it is the case that PartyB paid more than PartyA THEN [10] ExcessParty IS PartyB AND [11] the excess amount of currency EQUALS AmountB MINUS AmountA."

    con = Contract()
    con.statement(ConditionalStatement(condition=AndCondition(conditions=[StatementCondition(statement=TemporalStatement('"PartyA"', 'SHALL', 'Pay', 'SomeCurrency "AmountA"', TemporalExpression('ON', '(1,1,1970)'), valid=True), test=True),
                                                                          StatementCondition(statement=TemporalStatement('"PartyB"', 'SHALL', 'Pay', 'SomeCurrency "AmountB"', TemporalExpression('ON','(1,1,1970)'), valid=True), test=True),
                                                                          ]), statement=TemporalStatement('"PartyA"', 'SHALL', 'Pay', 'SomeCurrency "AmountA"', TemporalExpression('ON','(1,1,1970)'), valid=False)))

    con.statement(ConditionalStatement(condition=AndCondition(conditions=[StatementCondition(statement=TemporalStatement('"PartyA"', 'SHALL', 'Pay', 'SomeCurrency "AmountA"', TemporalExpression('ON', '(1,1,1970)'), valid=True), test=True),
                                                                          StatementCondition(statement=TemporalStatement('"PartyB"', 'SHALL', 'Pay', 'SomeCurrency "AmountB"', TemporalExpression('ON','(1,1,1970)'), valid=True), test=True),
                                                                          ]), statement=TemporalStatement('"PartyB"', 'SHALL', 'Pay', 'SomeCurrency "AmountB"', TemporalExpression('ON','(1,1,1970)'), valid=False)))

    con.statement(ConditionalStatement(condition=AndCondition(conditions=[StatementCondition(statement=TemporalStatement('"PartyA"', 'SHALL', 'Pay', 'SomeCurrency "AmountA"', TemporalExpression('ON', '(1,1,1970)'), valid=True), test=True),
                                                                          StatementCondition(statement=TemporalStatement('"PartyB"', 'SHALL', 'Pay', 'SomeCurrency "AmountB"', TemporalExpression('ON','(1,1,1970)'), valid=True), test=True),
                                                                          ]), statement=TemporalStatement('"ExcessParty"', 'SHALL', 'Pay', 'SomeCurrency "ExcessAmount"', TemporalExpression('ON','(1,1,1970)'), valid=True)))

    con.definition(ConditionalDefinition(condition=ExpressionCondition(BooleanExpression('"PartyB"', 'Paid', 'MoreThan', '"PartyA"'), test=True), definitions=[IsDefinition('"ExcessParty"', '"PartyB"'),
                    EqualsDefinition('SomeCurrency "ExcessAmount"', 'OtherObject "AmountB" MINUS OtherObject "AmountA"'),
                    ])
    )

    con.definition(ConditionalDefinition(condition=ExpressionCondition(BooleanExpression('"PartyA"', 'Paid', 'MoreThan', '"PartyB"'), test=True), definitions=[IsDefinition('"ExcessParty"', '"PartyA"'),
                    EqualsDefinition('SomeCurrency "ExcessAmount"', 'OtherObject "AmountA" MINUS OtherObject "AmountB"'),
                    ])
    )

    con.interaciveSimulation()

def cola_bike_delivery_original():
    # bike_delivery = "IF [1a] it is the case that Alice paid 100 pounds on the 1 April 2021 OR [1b] it is the case that Alice paid 120 pounds on the 1 April 2021 THEN [2] it is the case that Bob must deliver a bicycle on the 5 April 2021 
    #                  ++ <AND> [3a] it is the case that Bob may deliver a receipt on the 5 April 2021 AND [3b] it is the case that Bob is forbidden to charge a delivery_fee on the 5 April 2021."

    con = Contract()
    con.statement(TemporalStatement('Bob', 'MAY', 'Deliver', 'OtherObject receipt', TemporalExpression('ON','(5,4,2021)'), valid=True))

    con.statement(TemporalStatement('Bob', 'SHANT', 'Charge', 'OtherObject delivery_fee', TemporalExpression('ON','(5,4,2021)'), valid=True))

    con.statement(ConditionalStatement(condition=OrCondition(conditions=[TemporalActionCondition('Alice', 'Paid', 'Pounds 100', TemporalExpression('ON', '(1,4,2021)'), test=True),
                                                                         TemporalActionCondition('Alice', 'Paid' , 'Pounds 120', TemporalExpression('ON','(1,4,2021)'), test=True),
                                                                         ]), statement=TemporalStatement('Bob', 'SHALL', 'Deliver', 'OtherObject bicycle', TemporalExpression('ON','(5,4,2021)'), valid=True)))

    con.interaciveSimulation()
    
def cola_bike_delivery_improved():
    # bike_delivery = "IF [1a] it is the case that Alice paid 100 pounds on the 1 April 2021 OR [1b] it is the case that Alice paid 120 pounds on the 1 April 2021 THEN [2] it is the case that Bob must deliver a bicycle on the 5 April 2021 AND [3a] it is the case that Bob may deliver a receipt on the 5 April 2021 AND [3b] it is the case that Bob is forbidden to charge a delivery_fee on the 5 April 2021."
    con = Contract()

    con.statement(ConditionalStatement(condition=OrCondition(conditions=[TemporalActionCondition('"Alice"', 'Paid', 'Pounds 100', TemporalExpression('ON', '(1,4,2021)'), test=True),
                                                                         TemporalActionCondition('"Alice"', 'Paid' , 'Pounds 120', TemporalExpression('ON','(1,4,2021)'), test=True),
                                                                         ]), statement=TemporalStatement('"Bob"', 'SHALL', 'Deliver', 'OtherObject "bicycle"', TemporalExpression('ON','(5,4,2021)'), valid=True)))

    con.statement(ConditionalStatement(condition=OrCondition(conditions=[TemporalActionCondition('"Alice"', 'Paid', 'Pounds 100', TemporalExpression('ON', '(1,4,2021)'), test=True),
                                                                         TemporalActionCondition('"Alice"', 'Paid' , 'Pounds 120', TemporalExpression('ON','(1,4,2021)'), test=True),
                                                                         ]), statement=TemporalStatement('"Bob"', 'MAY', 'Deliver', 'OtherObject "receipt"', TemporalExpression('ON','(5,4,2021)'), valid=True)))

    con.statement(ConditionalStatement(condition=OrCondition(conditions=[TemporalActionCondition('"Alice"', 'Paid', 'Pounds 100', TemporalExpression('ON', '(1,4,2021)'), test=True),
                                                                         TemporalActionCondition('"Alice"', 'Paid' , 'Pounds 120', TemporalExpression('ON','(1,4,2021)'), test=True),
                                                                         ]), statement=TemporalStatement('"Bob"', 'SHANT', 'Charge', 'OtherObject "delivery_fee"', TemporalExpression('ON','(5,4,2021)'), valid=True)))
    con.interaciveSimulation()

    
    

def cola_example_contract_sanction():
    # bike_delivery = "IF [1a] it is the case that Alice paid 100 pounds on the 1 April 2021 OR [1b] it is the case that Alice paid 120 pounds on the 1 April 2021 THEN [2] it is the case that Bob must deliver a bicycle on the 5 April 2021 AND [3a] it is the case that Bob may deliver a receipt on the 5 April 2021 AND [3b] it is the case that Bob is forbidden to charge a delivery_fee on the 5 April 2021 "
    #                  ++ "<AND> [4] It is the case that Alice may charge 120 pounds on the 6 April 2021 IF [5] it is not the case that Bob delivered a bicycle on the 5 April 2021."

    con = Contract()
    con.statement(ConditionalStatement(condition=TemporalActionCondition('"Bob"', 'Delivered', 'OtherObject "bicycle"', TemporalExpression('ON', '(5,4,2021)'), test=False), statement=TemporalStatement('"Alice"', 'MAY', 'Charge', 'Pounds 120', TemporalExpression('ON', '(1,4,2021)'), valid=True)))

    con.statement(ConditionalStatement(condition=OrCondition(conditions=[TemporalActionCondition('"Alice"', 'Paid', 'Pounds 100', TemporalExpression('ON', '(1,4,2021)'), test=True),
                                                                         TemporalActionCondition('"Alice"', 'Paid' , 'Pounds 120', TemporalExpression('ON','(1,4,2021)'), test=True),
                                                                         ]), statement=TemporalStatement('"Bob"', 'SHALL', 'Deliver', 'OtherObject "bicycle"', TemporalExpression('ON','(5,4,2021)'), valid=True)))

    con.statement(ConditionalStatement(condition=OrCondition(conditions=[TemporalActionCondition('"Alice"', 'Paid', 'Pounds 100', TemporalExpression('ON', '(1,4,2021)'), test=True),
                                                                         TemporalActionCondition('"Alice"', 'Paid' , 'Pounds 120', TemporalExpression('ON','(1,4,2021)'), test=True),
                                                                         ]), statement=TemporalStatement('"Bob"', 'MAY', 'Deliver', 'OtherObject "receipt"', TemporalExpression('ON','(5,4,2021)'), valid=True)))

    con.statement(ConditionalStatement(condition=OrCondition(conditions=[TemporalActionCondition('"Alice"', 'Paid', 'Pounds 100', TemporalExpression('ON', '(1,4,2021)'), test=True),
                                                                         TemporalActionCondition('"Alice"', 'Paid' , 'Pounds 120', TemporalExpression('ON','(1,4,2021)'), test=True),
                                                                         ]), statement=TemporalStatement('"Bob"', 'SHANT', 'Charge', 'OtherObject "delivery_fee"', TemporalExpression('ON','(5,4,2021)'), valid=True)))
    con.interaciveSimulation()




def cola_guarantor_agreement():
    # guaran = "[1] It is the case that Landlord shall deliver a Property on the 02 April 2021 "
    #         ++ "<AND> IF [2] it is the case that Landlord delivered a demandOfTenantPayment on the 02 March 2021 AND [3] it is not the case that Tenant paid AmountA on the 2 March 2021 THEN [4] it is the case that the Landlord may deliver a demandOfGuarantorPayment on the 03 March 2021 "
    #         ++ "<AND> IF [2] it is the case that Landlord delivered a demandOfTenantPayment on the 02 March 2021 AND [3] it is not the case that Tenant paid AmountA on the 2 March 2021 AND [4] it is the case that Landlord delivered a demandOfGuarantorPayment on the 03 March 2021 THEN [5] it is the case that Guarantor shall pay AmountA on the 03 March 2021 "
    #         ++ "<AND> IF [6] it is not the case that Tenant paid AmountB on the 10 March 2022 THEN [7] it is the case that the Guarantor shall pay AmountB on the 11 March 2023 "
    #         ++ "<AND> IF [8a] it is the case that HousingBenefitScheme paid AmountC on the 02 March 2021 AND [9] it is the case that LocalAuthority delivered a overpaymentClaim on the 02 March 2022 THEN [10] it is the case that Guarantor shall pay AmountC on the 01 January 1970."

    con = Contract()
    con.statement(TemporalStatement('"Landlord"', 'SHALL', 'Deliver', 'OtherObject "Property"', TemporalExpression('ON','(2,4,2021)'), valid=True))

    con.statement(ConditionalStatement(condition=AndCondition(conditions=[TemporalActionCondition('"HousingBenefitScheme"', 'Paid', 'SomeCurrency "AmountC"', TemporalExpression('ON', '(2,3,2021)'), test=True),
                                                                          TemporalActionCondition('"LocalAuthority"', 'Delivered' , 'OtherObject "overpaymentClaim"', TemporalExpression('ON','(2,3,2022)'), test=True),
                                                                          ]), statement=TemporalStatement('"Guarantor"', 'SHALL', 'Pay', 'SomeCurrency "AmountC"', TemporalExpression('ON','(1,1,1970)'), valid=True)))

    con.statement(ConditionalStatement(condition=TemporalActionCondition('"Tenant"', 'Paid', 'SomeCurrency "AmountB"', TemporalExpression('ON', '(10,3,2022)'), test=False), statement=TemporalStatement('"Guarantor"', 'SHALL', 'Pay', 'SomeCurrency "AmountB"', TemporalExpression('ON', '(11,3,2023)'), valid=True)))

    con.statement(ConditionalStatement(condition=AndCondition(conditions=[TemporalActionCondition('"Landlord"', 'Delivered', 'OtherObject "demandOfTenantPayment"', TemporalExpression('ON', '(2,3,2021)'), test=True),
                                                                          TemporalActionCondition('"Tenant"', 'Paid' , 'SomeCurrency "AmountA"', TemporalExpression('ON','(2,3,2021)'), test=False),
                                                                          TemporalActionCondition('"Landlord"', 'Delivered' , 'OtherObject "demandOfGuarantorPayment"', TemporalExpression('ON','(3,3,2021)'), test=True),
                                                                          ]), statement=TemporalStatement('"Guarantor"', 'SHALL', 'Pay', 'SomeCurrency "AmountA"', TemporalExpression('ON','(3,3,2021)'), valid=True)))

    con.statement(ConditionalStatement(condition=AndCondition(conditions=[TemporalActionCondition('"Landlord"', 'Delivered', 'OtherObject "demandOfTenantPayment"', TemporalExpression('ON', '(2,3,2021)'), test=True),
                                                                          TemporalActionCondition('"Tenant"', 'Paid' , 'SomeCurrency "AmountA"', TemporalExpression('ON','(2,3,2021)'), test=False),
                                                                          ]), statement=TemporalStatement('"Landlord"', 'MAY', 'Deliver', 'OtherObject "demandOfGuarantorPayment"', TemporalExpression('ON','(3,3,2021)'), valid=True)))

    con.interaciveSimulation()

