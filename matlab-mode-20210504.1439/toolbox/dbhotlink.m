function dbhotlink()
% Display text that EMACS can interpret as a hotlink
% so the debugger can auto move to the right spot.
% Input L is the stack frame to specify.
% If L is not provided, then use the current stack frame.
    
   [ST, I] = dbstack('-completenames');
   
   disp('(progn ');
   es = getappdata(groot, 'EmacsStack');
   es.updateForHotLinks(ST, I);
   bp = getappdata(groot, 'EmacsBreakpoints');
   bp.updateForHotLinks();
   disp(')');

end
