Names: Atharv Parashar & Shubham Pant


How a user would interact with our program: 
A user gives the program a piece of Rackette code. Our program will then read the code and figure out what needs to be outputted, such as a number or even a closure. Some examples of code which a user could input include integer addition, code which involves checking conditions such as the usage of if and cons, and function creation using lambda and let. What will be outputted is the value of the code in a string format or an error message if they inputted their Rackette code incorrectly in the beginning. 


How the pieces of our program work: 
After the user inputs a program, the read or readAll function will convert the string into a concreteProgramPiece or a concreteProgram, respectively. Then, this program will be parsed by our parse and its corresponding methods on all of the pieces, and concreteProgramPiece and concreteProgram are respectively turned into abstractProgramPiece, which is either a Definition or Expression, and abstractProgram, representing syntax our eval and addDefinition procedures can understand. If the abstractProgramPiece is a Definition, we use addDefinition to extend our top level environment. If abstractProgramPiece an expression, we use eval to get an output, and we use the top level and local environment as a reference lis for when we evaluate these expressions. Finally, the values produced from eval are inputted into the stringOfValue procedure, which outputs value in a string form! 

We couldn't find any possible major bugs. Moreover, there are no extra features. 
