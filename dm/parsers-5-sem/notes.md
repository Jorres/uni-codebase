### structures
```
class Node {
    children:  list<Node>
}

class Context {
    string token;
    void next_token();
}
```

```
// define function for each nonterminal, invariant - token on first symbol from this nonterminal
// output - char that follows last that expanded from this nonterminal 
function A(): Node {
    res = new Node(A)
    // FIRST' = FIRST(A) - eps UNION (FOLLOW(A) if eps in FIRST(A))
    // unique set that does not overlap with other rules from this terminal
    switch (token) {
        // for all rules in A -> alpha, find rule where
        case FIRST'(alpha): // expand A to alpha
            // alpha = XYcZd, for example
            {
                t = X()
                res.children.add(t)
                t = Y()
                res.children.add(t)
                assert(t == c), t = next_token()
                res.children.add(new Node(c))
                t = Z()
                res.children.add(t)
                assert(t == d), t = next_token()
                res.children.add(new Node(d))
                next_token()
                return 
            }
        case FIRST'(beta): ...
        // mutually recursive functions, looks like fun!
    }
}
```

### good rules:
    - create exceptions, they suit well for this kind of asserts
    - recursive descent for 2nd lab
    - create proper tokenizer, do not include rules in GRAMMAR for number structure
      you just need one terminal for the number, the inner structure of the number 
      should be in tokenizer
    - AntLR
    - Bison
    - Happy
    - make your lexical analyzer lazy, active on `nextToken`

### simplifications
    - inline functions for nonterminals where they are not needed outside 
```
    A()                 A -> alpha A`
     |                  A` -> beta  R1
     |  alpha           A` -> gamma R2
     |          
     case token
        FIRST(beta)
        |       
        |  beta
        |       
        FIRST(gamma)
        |       
        |  gamma
        |       
```
    - inline loops where rules look like `A -> b a*`
```
                        
    A()
     |  switch()
     |      beta1
     |      beta2
        while token in FIRST(a) // <==> a*
            process alpha
```

### Bison documentation details

GLR - generalized LR - clones the parser when ambiguity is found, and than it can either vanish or
get merged back with some other parser. User defined actions are recorded but executed later.
