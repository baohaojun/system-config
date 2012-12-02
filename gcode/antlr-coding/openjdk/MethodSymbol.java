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

public class MethodSymbol extends ScopedSymbol {
	Map<String, Symbol> orderedArgs = new LinkedHashMap<String, Symbol>();

	public MethodSymbol(String name, Type retType, Scope parent) {
        super(name, retType, parent);
    }

    public Map<String, Symbol> getMembers() { return orderedArgs; }

    public String getName() {
        return name+"("+stripBrackets(orderedArgs.keySet().toString())+")";
    }
}
