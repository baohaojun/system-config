// START: header
tree grammar Def;
options {
  tokenVocab = Jnu;
  ASTLabelType = JnuAST;
  filter = true;
}
@members {
    SymbolTable symtab;
    Scope currentScope;
    public Def(TreeNodeStream input, SymbolTable symtab) {
        this(input);
        this.symtab = symtab;
        currentScope = symtab.globals;
    }
}
// END: header

topdown
    :   enterBlock
    |   enterPackage
    |   enterImport
    |   enterMethod
    |   enterClass
    |   enterClassRef
    |   varDeclaration
    |   atoms
    ;

bottomup
    :   exitBlock
    |   exitMethod
    |   exitClass
    ;

// S C O P E S
enterPackage
    :   ^('package' qualifiedName)
    ;

enterImport
    :   ^('import' qualifiedName '*'?)
    ;

qualifiedName 
    :   IDENTIFIER 
    |   ^('.' e+=IDENTIFIER+)
    ;

enterBlock
    :   BLOCK {
            currentScope = new LocalScope(currentScope);
        }
    ;
exitBlock
    :   BLOCK
        {
            currentScope = currentScope.getEnclosingScope();    // pop scope
        }
    ;

// START: class
enterClass
    : 	^('class' name=IDENTIFIER ^(CLASS_PARAM .*) ^(SUPER type+=.*) ^(CLASS_BODY .*))
        {
            if ($type != null) {
                for (int i = 0; i < $type.size(); i++) {
                    ((JnuAST)$type.get(i)).scope = currentScope;
                }
            }

            ClassSymbol cs = new ClassSymbol($name.text,currentScope,null);
            cs.def = $name;
            $name.symbol = cs;
            currentScope.define(cs);
            currentScope = cs;

        }
    ;

enterClassRef
    :   ^(CLASS_REF ^(QUALIFIED e+=IDENTIFIER+) .*)
        {
        }
    ;
            

exitClass
    :   'class'
        {
            currentScope = currentScope.getEnclosingScope();    // pop scope
        }
    ;

enterMethod
    : 	^(METHOD_DECL type=. name=IDENTIFIER .*)
        {
            $type.scope = currentScope;
            MethodSymbol ms = new MethodSymbol($name.text,null,currentScope);
            ms.def = $name;            // track AST location of def's ID
            $name.symbol = ms;         // track in AST
            currentScope.define(ms); // def method in globals
            currentScope = ms;       // set current scope to method scope
        }

    |   ^(CONSTRUCTOR_DECL name=IDENTIFIER .*)
        {
            MethodSymbol ms = new MethodSymbol($name.text,null,currentScope);
            ms.def = $name;
            $name.symbol = ms;
            currentScope.define(ms);
            currentScope = ms;
        }
    ;
exitMethod
    :   METHOD_DECL
        {
            currentScope = currentScope.getEnclosingScope();    // pop arg scope
        }
    ;

// START: atoms
/** Set scope for any identifiers in expressions or assignments */
atoms
@init {JnuAST t = (JnuAST)input.LT(1);}
    :  {t.hasAncestor(EXPR)}? ('this'|ID)
       {t.scope = currentScope;}
    ;
//END: atoms

// START: var
varDeclaration // global, parameter, or local variable
    :   ^((FIELD_DECL|VAR_DECL|ARG_DECL) type=. id+=IDENTIFIER+)
        {
            $type.scope = currentScope;

            for (int i = 0; i < $id.size(); i++) {
                VariableSymbol vs = new VariableSymbol(((JnuAST)$id.get(i)).getText(),null);
                vs.def = ((JnuAST)$id.get(i));            // track AST location of def's ID
                ((JnuAST)$id.get(i)).symbol = vs;         // track in AST
                currentScope.define(vs);
            }
        }
    ;
// END: field
