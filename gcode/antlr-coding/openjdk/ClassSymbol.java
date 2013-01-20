/***
 * Excerpted from "Language Implementation Patterns",
 * published by The Pragmatic Bookshelf.
 * Copyrights apply to this code. It may not be used to create training material, 
 * courses, books, articles, and the like. Contact us if you are in doubt.
 * We make no guarantees that this code is fit for any purpose. 
 * Visit http://www.pragmaticprogrammer.com/titles/tpdsl for more book information.
***/
import java.util.*;

public class ClassSymbol extends ScopedSymbol implements Scope, Type {
    /** This is the superclass not enclosingScope field. We still record
     *  the enclosing scope so we can push in and pop out of class defs.
     */
    ClassSymbol superClass;
    /** List of all fields and methods */
    public Map<String,Symbol> members=new LinkedHashMap<String,Symbol>();

    public ClassSymbol(String name, Scope enclosingScope, ClassSymbol superClass) {
        super(name, enclosingScope);
        this.superClass = superClass;
    }

    public Scope getParentScope() {
        if ( superClass==null ) return enclosingScope; // globals
        return superClass; // if not root object, return super
    }

    /** For a.b, only look in a's class hierarchy to resolve b, not globals */
    public Symbol resolveMember(String name) {
        Symbol s = members.get(name);
        if ( s!=null ) return s;
        // if not here, check just the superclass chain
        if ( superClass != null ) {
            return superClass.resolveMember(name);
        }
        return null; // not found
    }

    public Map<String, Symbol> getMembers() { return members; }
    public String toString() {
        return "class "+name+":{"+
               stripBrackets(members.keySet().toString())+"}";
    }
}
