function emacsdocomplete(substring)
% Ask for completions of SUBSTRING from MATLAB.
% This is used by Emacs TAB in matlab-shell to provide possible
% completions.  This hides the differences between versions
% for the calls needed to do completions.

% Custom completions for Emacs can be added to MATLAB (*.m) files by:
%   1. In a comment, place the string "SUPPORTS_DASH_COMPLETE"
%   2. Handle the -complete argument which produces completion strings
%      of the form:
%          'CMD_TEXT_TO_REPLACE' --> 'REPLACEMENT_TEXT'
%              'OPTION1'
%              'OPTION2'
%              ...
%      See details in `matlab-shell-completion-list'.
    
    persistent completeSw; % if completeSw(cmd), then supports -complete
    if isempty(completeSw)
        completeSw=containers.Map();
    end

    cmd=regexp(substring,'^(\w+)\s+[^\)]','tokens');
    if length(cmd)==1
        cmd=cmd{1}{1};

        if completeSw.isKey(cmd)
            supportsDashComplete = completeSw(cmd);
        else
            supportsDashComplete = false; % assume
            f=which(cmd);
            if regexp(f,'\.m$')
                fid=fopen(f,'r');
                if fid ~= -1
                    while true
                        l = fgetl(fid);
                        if ~ischar(l), break, end
                        if regexp(l,'SUPPORTS_DASH_COMPLETE')
                            supportsDashComplete = true;
                            break
                        end
                    end
                    fclose(fid);
                end
            end
            completeSw(cmd) = supportsDashComplete;
        end

        if supportsDashComplete
            % For /path/to/cmd.ext we have /path/to/cmd.complete which
            % signals that we can get the completions by calling
            %    CMD -complete ARGS
            completeCmd = regexprep(substring,'^(\w+)','$1 -complete');
            disp('emacs_completions_output =');
            evalin('base',completeCmd);
            return
        end
    end

    v = ver('MATLAB');

    if str2double(v.Version) < 8.4

        % Pre R2014b: partial_string
        extracmd = '';

    else

        % Post R2014b: partial_string, caret, num
        extracmd = [ ', ' num2str(length(substring)) ',0' ];

        % DEV NOTE: If you find a test failure, contact Eric Ludlam
        % to also update matlab-emacs SF repository.

    end

    substringQuoted = strrep(substring, '''', '''''');

    command = [ 'matlabMCRprocess_emacs = com.mathworks.jmi.MatlabMCR;' ...
                'emacs_completions_output = matlabMCRprocess_emacs.mtFindAllTabCompletions(''' ...
                substringQuoted '''' extracmd '),' ...
                'clear(''matlabMCRprocess_emacs'',''emacs_completions_output'');' ];

    % Completion engine needs to run in the base workspace to know
    % what the variables you have to work with are.
    evalin('base',command);

end
