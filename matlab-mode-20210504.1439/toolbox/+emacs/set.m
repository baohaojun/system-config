function set(varargin)
% Setup an Emacs option based on Name/Value pairs.
% Valid options include:
%
%  netshell - Initialize a netshell connection.
%  clientcmd - What to use for `edit' client command

    P = inputParser;
    addParameter(P, 'netshell', 0, @isnumeric)
    addParameter(P, 'clientcmd', "", @ischar)
    addParameter(P, 'followstack', -1, @isnumeric)

    parse(P, varargin{:});

    clientcommand = P.Results.clientcmd;
    netshellport = P.Results.netshell;
    followstack = P.Results.followstack;

    %% Client Command
    if ~isempty(clientcommand)
        if usejava('jvm')
            % Use clientcommand (e.g. emacsclient -n) for text editing
            if verLessThan('MATLAB','9.4')
                % Before settings API was introduced
                com.mathworks.services.Prefs.setBooleanPref('EditorBuiltinEditor',false);
            else
                s = settings;
                s.matlab.editor.UseMATLABEditor.TemporaryValue = 0;
            end
            if verLessThan('MATLAB','9.9')
                % Before OtherEditor was read from settings API
                com.mathworks.services.Prefs.setStringPref('EditorOtherEditor', clientcommand);
            else
                s = settings;
                s.matlab.editor.OtherEditor.TemporaryValue = clientcommand;
            end
        end
        if isunix
            % In some cases 'edit foo.m' requires EDITOR to be set so we see foo.m in Emacs.
            %  - Consider 'emacs -nw' which and running without Java on Linux.
            %  - On Mac EDITOR is always used, but it must be a single executable w/o switches
            %  - Any system('command') that uses opens files should open in Emacs.
            setenv('EDITOR_EMACSCLIENT', clientcommand);
            p1 = fileparts(mfilename('fullpath')); % p1 /path/to/matlab-emacs/toolbox/+emacs
            p2 = fileparts(p1); % p2 == /path/to/matlab-emacs/toolbox/+emacs
            p3 = fileparts(p2); % p3 = /path/to/matlab-emacs/

            % We cannot run using the full path to matlab-emacsclient.sh because
            % checkMacApp(applicationName,'emacs') will fail if the path contains the string
            % "/emacs", e.g. /path/to/emacs/packages/matlab-emacs. Therefore, update PATH to make
            % matlab-emacsclient.sh available.
            setenv('EDITOR', 'matlab-emacsclient.sh');
            setenv('PATH',[getenv('PATH'),':',p3,'/bin']);
        end
    end

    %% Netshell Support
    if netshellport

        nso = emacsnetshell('init');

        % Assign netshell into our reporter objects.
        bp = getappdata(groot, 'EmacsBreakpoints');
        bp.NetShellObject = nso;

        st = getappdata(groot, 'EmacsStack');
        st.NetShellObject = nso;

    end

    %% Follow Stack settings
    if followstack == 0 || followstack == 1
        EMACSSERVER = getappdata(groot, 'EmacsNetShell');
        EMACSSERVER.FollowStack = followstack;
    end

end

% LocalWords:  netshell clientcmd followstack clientcommand emacsclient nw
