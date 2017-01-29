# Calculator
### Build and tests status
master | [![Build Status](https://travis-ci.org/sirIrishman/calculator.svg?branch=master)](https://travis-ci.org/sirIrishman/calculator)
---|---
---

A simple mathematical expressions evaluator written in Scala. 

It converts an input infix expression into the [Reverse Polish notation (RPN) form](https://en.wikipedia.org/wiki/Reverse_Polish_notation) using the [Shunting-yard algorithm](https://en.wikipedia.org/wiki/Shunting-yard_algorithm) and then evaluates the postfix expression using the [Postfix algorithm](https://en.wikipedia.org/wiki/Reverse_Polish_notation#Postfix_algorithm).

## Changelog
### 0.1 version:
* integer and float numbers;
* positive and negative numbers;
* algebraic operators;
  * `+` addition;
  * `-` subtraction;
  * `/` division;
  * `*` multiplication;
* `()` parentheses (with no limits for nesting).

## Plans for future releases:
* improve error handling (handle all types of invalid expressions with providing meaningful messages);
* extend number of operators (`%` modulo, `^` power, bit operations, ...);
* predefined mathematical functions (trigonometric: `sin(x)`, `tan(x)`, ...; roots: `sqrt(x)`, `root(x,y)`; ...);
* constants (`pi`, `e`, ...);
* variables.
