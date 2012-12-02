/***
 * Excerpted from "Language Implementation Patterns",
 * published by The Pragmatic Bookshelf.
 * Copyrights apply to this code. It may not be used to create training material, 
 * courses, books, articles, and the like. Contact us if you are in doubt.
 * We make no guarantees that this code is fit for any purpose. 
 * Visit http://www.pragmaticprogrammer.com/titles/tpdsl for more book information.
***/
import java.util.LinkedHashMap;
import java.util.Map;

public abstract class BaseScope implements Scope {
	Scope enclosingScope; // null if global (outermost) scope
	Map<String, Symbol> symbols = new LinkedHashMap<String, Symbol>();

    public BaseScope(Scope parent) { this.enclosingScope = parent;	}

    public Symbol resolve(String name) {
		Symbol s = symbols.get(name);
        if ( s!=null ) return s;
		// if not here, check any enclosing scope
		if ( getParentScope() != null ) return getParentScope().resolve(name);
		return null; // not found
	}

	public void define(Symbol sym) {
		symbols.put(sym.name, sym);
		sym.scope = this; // track the scope in each symbol
	}

    public Scope getParentScope() { return getEnclosingScope(); }
    public Scope getEnclosingScope() { return enclosingScope; }

	public String toString() { return symbols.keySet().toString(); }
}
