# crypto-binder

## About
The goal of this project is to create a uniform E-DSL for various cryptocurrency exchanges. This could have many uses from arbritaging to algorithmic trading. I will initially only be implementing a select set of functions, but the hope is to extend it in the future.

Contributions are welcomed with open arms :)

## Implementation
There is a shared *Types.hs* file. This file contains ADT's that will be uniform accross all exchanges.
Each of the exchanges contains a custom `ToJSON` instance in order to wrangle the data into these ADT's. 

Each exchange requires its own functions to call the Api, but they follow a similar pattern.

I hope to extraxt as much functioanlity as possible and I expect a lot of refactoring as they project progresses.

## Progress
Exchange | getTicker | getBalance 
---|---|---
Kraken | Yes | Yes
Bittrex | Yes | No*

*New accounts are currently blocked 
