function nso = emacsnetshell(cmd, data)
% Create a connection to an EMACS editor server.
%
% emacsnetshell('init') - Initialize the connection with Emacs.
%    emacs will send commands to MATLAB with additional connectivity
%    information.
%
% emacsnetshell('ack') - Send ack to Emacs, it should echo back.
%
% emacsnetshell('output',data) - Send DATA as output to display in Emacs.
% emacsnetshell('error',data) - Send DATA as error description to Emacs.
% emacsnetshell('eval',data) - Send DATA - a string containing an Emacs
%    lisp form which will be evaluated in Emacs.
    

    EMACSSERVER = getappdata(groot, 'EmacsNetShell');

    if nargin == 0
        if isempty(EMACSSERVER)
            cmd = 'init';
        else
            cmd = 'fetch';
        end
    end

    if strcmp(cmd, 'fetch')
        % Fetch means to get the server and return it.  Do not create
        % the server on fetch - otherwise no way to know if a server was started
        % or not.
        nso = EMACSSERVER;
        return;
        
    elseif strcmp(cmd, 'shutdown')
        % Shutdown our connection to emacs.
        setappdata(groot, 'EmacsNetShell', []);

        if ~isempty(EMACSSERVER)
            delete(EMACSSERVER);
        end
    else
        if ~isempty(EMACSSERVER) && ~isvalid(EMACSSERVER)
            EMACSSERVER = [];
            setappdata(groot, 'EmacsNetShell', EMACSSERVER);
        end

        if isempty(EMACSSERVER)
            EMACSSERVER = emacs.EmacsServer();
            setappdata(groot, 'EmacsNetShell', EMACSSERVER);
            sendinit = false;
        else
            sendinit = true;
        end

        if ~ischar(cmd)
            error('Command must be a char vector.');
        end

        if ~strcmp(cmd,'init') || sendinit
            if nargin == 2
                EMACSSERVER.SendCommand(cmd, data);
            else
                EMACSSERVER.SendCommand(cmd);
            end
        end
    end
    
    if nargout == 1
        nso = EMACSSERVER;
    end

end
