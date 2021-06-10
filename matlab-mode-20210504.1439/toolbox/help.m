function [out, docTopic] = help(varargin)
% Provide help, augmented so Emacs picks it up to display in a special buffer.
% See the help for the built-in help command by asking for "help help" in
% MATLAB, which will redirect to the correct location.

    origPath = path;
    cleanup = onCleanup(@()path(origPath));
    me = mfilename('fullpath');

    % Recursion Detection.  (Not sure why we sometimes recurse in the first call to help.)
    [ST] = dbstack('-completenames',1);
    files = { ST.file };
    mask = strncmp(files, me, length(me));
    if any(mask)
        disp('MATLAB Emacs help override recursion detected.  Exiting.');
        return;
    end

    % Remove this dir from path so we can get to the built-in version of help.
    myDir = fileparts(me);
    rmpath(myDir);

    builtinHelp = which('help');
    clear cleanup;
    
    helpPath = fileparts(builtinHelp);

    % Cd to where built-in help is so we call that first.  On cleanup restore
    % old working directory.
    oldCWD = pwd;
    cd(helpPath);
    cleanup = onCleanup(@()cd(oldCWD));

    args = varargin;
    
    nso = emacsnetshell('fetch');
    
    if isempty(nso)
        cookie = true;
    else
        cookie = false;
    end
    
    if nargin > 0 && strcmp(args{1}, '-emacs')
        cookie=false;
        args = args(2:end);
    end

    switch nargout
      case 0
        if cookie
            disp(['<EMACSCAP>(*MATLAB Help: ' args{:} '*)']);
        end
        try
            help(args{:});
        catch ERR
            disp(ERR)
        end
        if cookie
            disp('</EMACSCAP>');
        end
      case 1
        [out] = help(args{:});
      case 2
        [out, docTopic] = help(args{:});
    end
end