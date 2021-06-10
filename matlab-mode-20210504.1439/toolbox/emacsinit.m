function emacsinit()
% EMACSINIT Initialize the current MATLAB session for matlab-shell-mode
%

    me = mfilename('fullpath');
    % This command can also update the path for the emacs Toolbox directory.
    % This will make it possible to use this one command from a standalone
    % MATLAB and setup netshell.
    [ myDir ] = fileparts(me);

    if ~contains(path, myDir)

        disp(['Updating MATLAB Path to support Emacs toolbox: addpath(' myDir ')']);

        addpath(myDir,'-begin');
        rehash;
    end

    if usejava('jvm')
        %{
        % Leaving in old hot-link code and description (see below)
        % in case someone with older MATLAB's need to use this.

        v = ver('MATLAB');
        if str2double(v.Version) < 8.5
            % In 8.5 (R2015b) the MATLAB removed the ability to display hot link's when
            % debugging in -nodesktop mode. In R2015a (8.4) MATLAB would display in -nodesktop
            % mode something like:
            %   >> dbstop in testit
            %   >> testit
            %   >> <a href="matlab: opentoline('/path/to/testit.m',3,1)">3   </a>    a = 10 * 10;
            %   K>>
            % and emacs then would use the hot link to drive debugging from within emacs.
            % Given that R2015b and later does not have hot links, we use the graphical debugging
            % by leaving EditorGraphicalDebugging set to true.

            % Use the desktop hotlinking system in MATLAB Shell.  matlab-shell
            % will interpret them, and provide clickable areas.
            % NOTE: This doesn't work in all cases where HotLinks are used.
            feature('HotLinks','on');

            % Disable built-in editor showing up for debugging
            com.mathworks.services.Prefs.setBooleanPref('EditorGraphicalDebugging', false);
        end
        %}

        % Starting in matlab-emacs v 4.0, we can simulate debugging
        % hot links, so we should always disable graphical
        % debugging.

        % Disable built-in editor showing up for debugging
        com.mathworks.services.Prefs.setBooleanPref('EditorGraphicalDebugging', false);

        % Disable wrapping of text lines.  Emacs will wrap or not based on user preference.
        com.mathworks.services.Prefs.setBooleanPref('WrapLines',false)
    end

    % Check if we're running inside emacs.  If we are NOT, then force the enablement of
    % the netshell interface to Emacs.
    emacs_env = getenv('INSIDE_EMACS');

    if isempty(emacs_env)
        startnetshell = true;
    else
        startnetshell = false;
    end

    % If requested, start the Emacs netshell interface.
    if startnetshell
        nso = emacsnetshell('init');
    else
        nso = [];
    end

    % Initialize Emacs breakpoint handler.
    bp = emacs.Breakpoints(nso);
    setappdata(groot, 'EmacsBreakpoints', bp);

    % Initialize Emacs stack handler.
    st = emacs.Stack(nso);
    setappdata(groot, 'EmacsStack', st);

end

% LocalWords:  netshell nodesktop testit hotlinking
