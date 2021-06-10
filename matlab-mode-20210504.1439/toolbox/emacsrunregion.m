function emacsrunregion(file, startchar, endchar)
% Run code from FILE between STARTCHAR and ENDCHAR.
% Command sent by Emacs for run-cell & run-region functionality.

    % Filter out emacs tramp file path prefix
    trampMatch = regexp(file, {'/*:',':/'});
    if (~isempty(trampMatch{1}))
        file = file((trampMatch{2}+1):end);
    end

    if ~exist(file,'file')
        error('You must save your region into a file accessible by MATLAB process.');
    end

    % Now figure out if shortFileName is on the path.
    [ fullFilePath, shortFileName ] = fileparts(file);
    onpath = ~isempty(which(shortFileName));

    % If not on the path, temporarilly switch to that directory so it and an files it references are
    % accessible
    if ~onpath
        oldpath = pwd;
        cd(fullFilePath);
        cleanup = onCleanup(@()cd(oldpath));
    end

    txt = fileread(file);
    evalTxt = txt(startchar:min(endchar,length(txt)));
    evalin('base',evalTxt);
end
