# Runic - I write the funny symbols so you don't have to

## What is it? 
Runic is a command-line utility for solving systems of equations with a Python-like syntax. Users can define their own constants and functions, declare domains and initial guess values, and then sit back and watch their systems be solved quickly and efficiently by Runic's problem constrainer and newton-raphson solver. Example:

```
keep x positive
guess 4 for x

const PI = 22 / 7

fn circumference(r)
    return 2 * PI * r
    
fn area(r)
    return PI * r ^ 2

system:
// find the radius of a circle whose 
// circumference and area sum up to 69:
circumference(x) + area(x) = 69
```

## How does it work? 
The Runic solver works in a few distinct steps. First it evaluates constant and function declarations for quick usage later in a solution. When preparing to solve a system, Runic builds a dependency tree of equations to find what equations and subsystems can be solved in parallel. This prepares the runtime to give detailed feedback on why certain systems may have failed to solve compared to previous implementations of this project. 
