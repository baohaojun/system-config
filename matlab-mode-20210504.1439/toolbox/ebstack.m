function ebstack(FRAMEIDX)
% Emacs version of dbstack.  Updates Emacs for where we are in the stack.
    
    [ST, I] = dbstack('-completenames');
    
   % Send emacs our updated stack
   es = getappdata(groot, 'EmacsStack');

   if nargin == 1
       I = FRAMEIDX;
   end
       
   es.updateEmacs(ST, I);

end
