function ebclear(varargin)
% Emacs version of dbstop.  Tells emacs which breakpoints are active.
    
    dbclear(varargin{:});
    
    % Send emacs some breakpoints
    bp = getappdata(groot, 'EmacsBreakpoints');
    bp.updateEmacs;

end

