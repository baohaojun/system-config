function ebstop(varargin)
% Emacs version of dbstop.  Tells emacs which breakpoints are active.
    
    dbstop(varargin{:});
    
    % Send emacs some breakpoints
    bp = getappdata(groot, 'EmacsBreakpoints');
    bp.updateEmacs;

end

