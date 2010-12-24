#!/usr/bin/env perl
%jira_help = ("--action" => " <action>
        Requested operation to perform. Valid actions (not case sensitive) are:

                addAttachment addComment addComponent 
                addGroup addUser addUserToGroup 
                addUserToGroupWithFile addUserWithFile addVersion 
                copyComponents createIssue createProject 
                deleteComponent deleteProject getAttachment 
                getAttachmentList getAvailableSteps getCommentList 
                getComments getFieldValue getIssue 
                getIssueList getPluginDownload getPluginList 
                getProjectList getServerInfo getUserList 
                linkIssue login logout 
                progressIssue removeGroup removeUser 
                removeUserFromGroup removeUserFromGroupWithFile removeUserWithFile 
                setFieldValue updateIssue",

              "login" => "Login to remote server. Returns login token.
        	 Required parameters: password
        	 Optional parameters: user",

              "logout" => "Logout of remote server.",

              "getServerInfo" => "Get information about the JIRA server.",

              "createProject" => "Create a new project with key provided by project parameter.
        	 Required parameters: project, lead
        	 Optional parameters: name, permissionScheme, notificationScheme,
        	 issueSecurityScheme",

              "deleteProject" => "Delete a project.
        	 Required parameters: project",

              "getProjectList" => "List defined projects.
        	 Optional parameters: file",

              "addVersion" => "Add a new version to a project.
        	 Required parameters: project, name
        	 Optional parameters: after, date, dateFormat",

              "addComponent" => "Add component to a project.
        	 Required parameters: project, name
        	 Optional parameters: description, lead",

              "deleteComponent" => "Delete component from a project.
        	 Required parameters: project, name",

              "copyComponents" => "Copy all or some components from one project to
        another.
        	 Required parameters: project, toProject
        	 Optional parameters: components",

              "createIssue" => "Create a new issue for a project.
        	 Required parameters: project, type, summary
        	 Optional parameters: parent, priority, reporter, assignee,
        	 date, dateFormat, description, components,
        	 affectsVersions, fixVersions, autoVersion, autoComponent, environment,
        custom, file, findReplace",

              "updateIssue" => "Update an existing issue.
        	 Required parameters: issue
        	 Optional parameters: type, summary, priority, reporter, assignee,
        	 resolution, date, dateFormat, description, components,
        	 affectsVersions, fixVersions, autoVersion, autoComponent, environment,
                 custom, file, findReplace",

              "linkIssue" => "Link an issue to another issue.
        	 Required parameters: issue, toIssue, link
        	 Optional parameters: comment",

              "addComment" => "Add a comment to an issue.
        	 Required parameters: issue
        	 Optional parameters: comment, file, role, group, findReplace",

              "addAttachment" => "Add an attachment to an issue.
        	 Required parameters: issue, file, findReplace
        	 Optional parameters: name",

              "progressIssue" => "Progress issue through workflow.
        	 Required parameters: issue, step
        	 Optional parameters: resolution, type, summary, priority, reporter,
        	 assignee, date, dateFormat, description, environment, custom",

              "getIssue" => "Get information about an existing issue.
        	 Required parameters: issue
        	 Optional parameters: file, dateFormat",

              "getFieldValue" => "Get field value for an issue.
        	 Required parameters: issue, field
        	 Optional parameters: file, dateFormat",

              "setFieldValue" => "Set custom field value for an issue.
        	 Required parameters: issue, field, file or values
        	 Optional parameters: asVersion, asComponent",

              "getAvailableSteps" => "Get available workflow steps for issue.
        	 Required parameters: issue
        	 Optional parameters: file",

              "getIssueList" => "List issues for a filter.
        	 Required parameters: filter
        	 Optional parameters: file, dateFormat",

              "getCommentList" => "List of comment information (id, dates, ...) for an
        issue.
        	 Required parameters: issue
        	 Optional parameters: file, dateFormat",

              "getComments" => "Get a formatted string of all comment text for an issue.
        	 Required parameters: issue
        	 Optional parameters: file, dateFormat",

              "getAttachment" => "Get lastest attachment by name or id for an issue.
        	 Required parameters: issue, file
        	 Optional parameters: name",

              "getAttachmentList" => "List all attachments for an issue.
        	 Required parameters: issue, file
        	 Optional parameters: dateFormat",

              "addUser" => "Add a new user.
        	 Required parameters: userId
        	 Optional parameters: userFullName, userEmail, userPassword",

              "addUserWithFile" => "Add users from comma separated file.
        	 Required parameters: file",

              "removeUser" => "Remove a user.
        	 Required parameters: userId",

              "removeUserWithFile" => "Remove users from comma separate file.
        	 Required parameters: file",

              "addGroup" => "Add a new group.
        	 Required parameters: group",

              "removeGroup" => "Remove a group.
        	 Required parameters: group
        	 Optional parameters: defaultGroup",

              "addUserToGroup" => "Add user to a group.
        	 Required parameters: userId, group
        	 Optional parameters: autoGroup",

              "addUserToGroupWithFile" => "Add users to groups from comma separated file.
        	 Required parameters: file
        	 Optional parameters: autoGroup",

              "removeUserFromGroup" => "Remove user from a group.
        	 Required parameters: userId, group",

              "removeUserFromGroupWithFile" => "Remove users from groups from comma
        separated file.
        	 Required parameters: file",

              "getUserList" => "List users in a group.
        	 Required parameters: group
        	 Optional parameters: file",

              "getPluginList" => "Get list of plugins. Use outputFormat=2 for extended
        information (longer time required).
        	 Optional parameters: plugin, file, outputFormat, count",

              "getPluginDownload" => " Get url to download the plugin version. Version
        defaults to the latest version.
        	 Required parameters: plugin
        	 Optional parameters: version",



              "--file" => " <file>
        Path to file based content or result output",

              "--debug" => "
        Requests debug output, example: stack traces.",

              "--verbose" => "
        Requests verbose output.",

              "--server" => " <server>
        Server URL.",

              "--user" => " <user>
        User name for remote login. (default: automation)",

              "--password" => " <password>
        User password for remote login.",

              "--login" => "<login>
        Login token from previous login request.",

              "--service" => "<service>
        Service address extension. (default: /rpc/soap/jirasoapservice-v2)",

              "--loginFromStandardInput" => "
        Get login token from standard input.",

              "--project" => "<project>
        Project name, key, or id",

              "--toProject" => "<toProject>
        Project name, key, or id to copy to",

              "--name" => "<name>
        Name",

              "--description" => "<description>
        Description",

              "--url" => "<url>
        URL",

              "--lead" => "<lead>
        Project lead user id",

              "--after" => "<after>
        Version name or id to add a version after",

              "--issue" => "<issue>
        Issue key",

              "--toIssue" => "<toIssue>
        Link destination issue key",

              "--parent" => "<parent>
        Parent issue key",

              "--summary" => "<summary>
        Summary of issue",

              "--priority" => "<priority>
        Issue priority - name or id",

              "--reporter" => "<reporter>
        Issue reporter user id",

              "--assignee" => "<assignee>
        Issue assignee user id",

              "--environment" => "<environment>
        Issue environment",

              "--components" => "<components>
        Components - comma separated names or ids",

              "--affectsVersions" => "<affectsVersions>
        Affects versions - comma separated names or ids",

              "--fixVersions" => "<fixVersions>
        Fix versions - comma separated names or ids",

              "--custom" => "<custom>
        Custom fields - comma separated key:value pairs. Key can be field name
        or id. Single quote the key:value pair if it contains a comma (,)",

              "--field" => "<field>
        Field name or id. For some actions, this parameter must be a custom
        field.",

              "--date" => "<date>
        Date for version or due date for issue",

              "--dateFormat" => "<dateFormat>
        Format string for date in Java SimpleDateFormat, default is client date
        format",

              "--type" => "<type>
        Issue type - name or id",

              "--resolution" => "<resolution>
        Resolution name or id",

              "--step" => "<step>
        Workflow step - name or id",

              "--comment" => "<comment>
        Comment for an issue",

              "--filter" => "<filter>
        Filter id or favorite filter name.",

              "--link" => "<link>
        Link description.",

              "--values" => "<values>
        Comma separated list of field values",

              "--count" => "<count>
        Maximum number of entries to return. (default: 2147483647)",

              "--outputFormat" => "<outputFormat>
        Specify output format for an action. (default: 1)",

              "--plugin" => "<plugin>
        Plugin key or partial key for matching.",

              "--version" => "<version>
        Plugin version.",

              "--role" => "<role>
        User role in project",

              "--group" => "<group>
        Group name",

              "--defaultGroup" => "<defaultGroup>
        Default group to move users on removeGroup action.",

              "--userId" => "<userId>
        User id for user management and other actions",

              "--userFullName" => "<userFullName>
        User name for user management actions",

              "--userEmail" => "<userEmail>
        User email for user management actions",

              "--userPassword" => "<userPassword>
        User password for user management actions",

              "--permissionScheme" => "<permissionScheme>
        Permission scheme name or id (default: Default Permission Scheme)",

              "--notificationScheme" => "<notificationScheme>
        Notification scheme name or id",

              "--issueSecurityScheme" => "<issueSecurityScheme>
        Issue security scheme name or id",

              "--findReplace" => "<findReplace>
        Find and replace text (separated by :).",

              "--autoVersion" => "
        Automatically add versions used in affectsVersions and fixVersions
        parameters.",

              "--autoComponent" => "
        Automatically add components used in components parameter.",

              "--autoGroup" => "
        Groups are automatically added when referenced in add user functions.",

              "--asVersion" => "
        Interpret values parameter as version values and convert each to the
        version id.",

              "--asComponent" => "
        Interpret values parameter as component values and convert each to the
        component id.

");

$jira_help{"-a"} = $jira_help{"--action"};

for (@ARGV) {
  next unless exists $jira_help{$_};
  print "Help: " . $_ . " " . $jira_help{$_};
  print "\n\n\n";
}
