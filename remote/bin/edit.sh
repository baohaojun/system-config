. ~/.bashrc_edit

function editnw()
{
    wait_for_emacs=-w edit "$@"
}

editnw "$@"
