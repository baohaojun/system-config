grammar Test1;

head : .* WORD {System.out.println("" + input.LT(1));} .*;

ID : 'x' ;
WORD : 'y';
