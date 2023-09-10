LEX UCC Financing Statement.
LEXON: 0.2.12

"Financing Statement" is this contract.
"File Number" is data.
"Initial Statement Date" is a time.
"Filer" is a person.
"Debtor" is a person.
"Secured Party" is a person.
"Filing Office" is a person.
"Collateral" is data.
"Digital Asset Collateral" is an amount.
"Reminder Fee" is an amount.
"Continuation Window Start" is a time.
"Continuation Statement Date" is a time.
"Continuation Statement Filing Number" is data.
"Lapse Date" is a time.
"Default" is a binary.
"Continuation Statement" is a binary.
"Termination Statement" is a binary.
"Termination StatementTime" is a time.
"Notification Statement" is a text.
"Default" is a binary.


The Filer fixes the Filing Office, 
fixes the Debtor,
fixes the Secured Party, 
and fixes the Collateral.

Clause: Certify.
The Filing Office may certify the File Number.

Clause: Set File Date.
The Filing Office may fix the Initial Statement Date as the current time.

Clause: Set Lapse.
The Filing Office may fix the Lapse Date as now.

Clause: Pay Escrow In.
The Debtor may pay the Digital Asset Collateral into escrow.

Clause: Fail to Pay.
The Secured Party may fix Default as true.

Clause: Take Possession.
If Default is true then the Filing Office may pay the Digital Asset Collateral to the Secured Party.

Clause: File Continuation.
The Secured Party may fix the Continuation Statement as true.

Clause: Set Continuation Lapse.
If the Continuation Statement is true then the Filing Office may fix the Continuation Statement Date. 

Clause: File Termination.
The Secured Party may fix Termination Statement as true,
and fix the Termination StatementTime as the current time.

Clause: Release Escrow.
If the Termination Statement is true then The Filing Office may pay the Digital Asset Collateral to the Debtor.

Clause: Release ReminderFee.
If the Termination Statement is true then The Filing Office may pay the Reminder Fee to the Secured Party.
