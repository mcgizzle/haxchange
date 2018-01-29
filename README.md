# crypto-binder 🤑

## About
The goal of this project is to create a uniform E-DSL for various cryptocurrency exchanges. This could have many uses from arbritaging to algorithmic trading. I will initially only be implementing a select set of functions, but the hope is to extend it in the future.

## Templates

There is a template folder which contains the three modules necessary for adding a new exchange, along with most of the bolier plate. To contribute simple copy that folder and rename all tags
`<newmodule>` to `<exchange-name>`

Contributions are welcomed with open arms :)

## Implementation
There are shared ADT's that are uniform accross all exchanges. Each exchange implements custom `FromJSON` instances as appropriate.

In order to improve effeciency, the data types are mostly [Text](http://sorryiwillinsertalinkatsomestage.com).

There is an *Api* type class which has the following functions
```Haskell
class Api where
  toText :: a -> Text
  fromText :: Text -> a
```

Each exchange then implements its own type class
```Haskell
class <Exchange> where
  toText :: a -> Text
```
This helps with wrangling data into the unified model and vice-versa. The `toText` function is used to override the general version for special cases. 

Implementations of the exchanges will often follow the following pattern:
```Haskell
import qualified Types as T
class Kraken where
  toText (COIN BTC) = "XBT"
  toText a          = T.toText a
  
```
Kraken refers to Bitcoin as XBT whereas most exchanges refer to it as BTC


## Progress

Exchange | getTicker | getBalance | buyLimit | sellLimit 
---|---|---|---|---
Kraken | Yes | Yes | Yes | Yes
Binance | Yes | No | No | No
Bittrex | Yes | No* | No* | No*

*New accounts are currently blocked 
