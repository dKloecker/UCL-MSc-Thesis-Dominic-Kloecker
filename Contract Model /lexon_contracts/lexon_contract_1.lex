LEX Returnable Bet.
LEXON: 0.2.12
COMMENT: 6.c - a bet between two parties, with odds
TERMS:
“Placer” is a person.
“Holder” is a person.
“Judge” is a person.
“Closed” is a binary.
“Bet” is this contract.
The Judge is appointed.
CLAUSE: Place Bet.
If the Bet is not Closed then
the Placer may pay an Amount into escrow,
and also fix the Odds.

CLAUSE: Hold Bet.
If the Amount is equal to the escrow times the Odds then
the Holder may pay the Amount into escrow,
and then the Bet is deemed Closed.

CLAUSE: Payout.
The Judge may If the Bet is Closed then pay the escrow to the Placer.
The Judge may If the Bet is Closed then pay the escrow to the Holder.
In any case, afterwards the Bet is terminated.

CLAUSE: Return.
The Judge may
if the Bet is not Closed then
return the escrow to the Placer.