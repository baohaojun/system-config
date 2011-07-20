#!/usr/bin/env perl
%confluence_help = ("--action" => " <action>
        Requested operation to perform. Valid actions (not case sensitive) are:

		login logout run
		run getServerInfo removeTrash
		addPage storePage removePage
		copyPage movePage renamePage
		getSource render watchPage
		watchPage addAttachment getAttachment
		removeAttachment copyAttachments addComment
		copyComments getComments addLabels
		removeLabels addNews storeNews
		removeNews getNewsSource renderNews
		addUser addUserWithFile removeUser
		removeUserWithFile addGroup removeGroup
		addUserToGroup addUserToGroupWithFile removeUserFromGroup
		removeUserFromGroupWithFile addPermissions removePermissions
		copyPermissions removeAllPermissionsForGroup addSpace
		removeSpace copySpace getSpace
		getSpaceList getPageList getNewsList
		getCommentList getLabelList getPermissionList
		getUserList getGroupList getPluginList
		getPluginDownload installPlugin checkPlugin
		exportSpace exportSite loadFiles",

	      "login" => "Login to remote server. Returns login token.
        	 Required parameters: password
        	 Optional parameters: user",

	      "logout" => "Logout of remote server.",

	      "run" => "Run script.
        	 Required parameters: file
        	 Optional parameters: continue",

	      "run" => "Run script generated from SQL.
        	 Required parameters: sql or file, url or host, driver, database
        	 Optional parameters: host, port, url, continue",

	      "getServerInfo" => "Get information about the Confluence server.",

	      "removeTrash" => "Permanently remove trash item.
        	 Required parameters: space, id",

	      "addPage" => "Add a new page.
        	 Required parameters: space, title, content, file
        	 Optional parameters: parent, labels, replace, findReplace",

	      "storePage" => "Create or update a page.
        	 Required parameters: space, title, content, file
        	 Optional parameters: parent, labels, findReplace",

	      "removePage" => "Remove a page and, optionally, all descendents.
        	 Required parameters: space, title
        	 Optional parameters: descendents",

	      "copyPage" => "Copy contents to another page.
        	 Required parameters: space, title, newSpace or newTitle
        	 Optional parameters: parent, children, descendents, replace, findReplace, copyAttachments, copyComments, copyLabels,
        targetServer",

	      "movePage" => "Move a page.
        	 Required parameters: space, title
        	 Optional parameters: parent",

	      "renamePage" => "Rename or move a page.
        	 Required parameters: space, title
        	 Optional parameters: newTitle, parent",

	      "getSource" => "Get page or news wiki text. Put to a file if specified.
        	 Required parameters: space, title
        	 Optional parameters: file, news, dayOfMonth",

	      "render" => "Render page or news.
        	 Required parameters: space, title
        	 Optional parameters: file, news, dayOfMonth",

	      "watchPage" => "Watch a page.
        	 Optional parameters: space, title, id",

	      "watchPage" => "Stop watching a page.
        	 Optional parameters: space, title, id",

	      "addAttachment" => "Add an attachment.
        	 Required parameters: space, title, file or content and name
        	 Optional parameters: mime, comment",

	      "getAttachment" => "Get an attachment and put to a file.
        	 Required parameters: space, title or id, file
        	 Optional parameters: name",

	      "removeAttachment" => "Remove an attachment.
        	 Required parameters: space, title, name",

	      "copyAttachments" => "Copy all page attachments to another page.
        	 Required parameters: space, title
        	 Optional parameters: newSpace, newTitle, targetServer",

	      "addComment" => "Add a comment to a page or news.
        	 Required parameters: space, title, comment or content or file
        	 Optional parameters: findReplace",

	      "copyComments" => "Copy all page comments to another page.
        	 Required parameters: space, title
        	 Optional parameters: newSpace, newTitle, targetServer",

	      "getComments" => "Get a formatted string of all comment text for a page.
        	 Required parameters: space, title
        	 Optional parameters: file, dateFormat",

	      "addLabels" => "Add labels to a page, news or space.
        	 Required parameters: space, labels
        	 Optional parameters: title, news, dayOfMonth",

	      "removeLabels" => "Remove labels to a page, news or space.
        	 Required parameters: space, labels
        	 Optional parameters: title, news, dayOfMonth",

	      "addNews" => "Add a news (blog) entry.
        	 Required parameters: space, title, file or content
        	 Optional parameters: dayOfMonth, labels, replace",

	      "storeNews" => "Add or update a news (blog) entry.
        	 Required parameters: space, title, file or content
        	 Optional parameters: dayOfMonth, labels, replace",

	      "removeNews" => "Remove a news (blog) entry.
        	 Required parameters: space, title
        	 Optional parameters: dayOfMonth",

	      "getNewsSource" => "Get wiki text for a news (blog) entry. Put to a file if specified.
        	 Required parameters: space, title
        	 Optional parameters: dayOfMonth, file",

	      "renderNews" => "Render a news (blog) entry. Put to a file if specified.
        	 Required parameters: space, title
        	 Optional parameters: dayOfMonth, file",

	      "addUser" => "Add a new user.
        	 Required parameters: userId
        	 Optional parameters: userFullName, userEmail, userPassword",

	      "addUserWithFile" => "Add users from comma separated file.
        	 Required parameters: file",

	      "removeUser" => "Remote a user.
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

	      "removeUserFromGroupWithFile" => "Remove users from groups from comma separated file.
        	 Required parameters: file",

	      "addPermissions" => "Add permissions to page or space.
        	 Required parameters: space, permissions, userId or group
        	 Optional parameters: title, descendents",

	      "removePermissions" => "Remove permissions from page or space.
        	 Required parameters: space, permissions, userId or group
        	 Optional parameters: title, descendents",

	      "copyPermissions" => "Copy page permissions from a page to another page.
        	 Required parameters: space, title
        	 Optional parameters: newSpace, newTitle",

	      "removeAllPermissionsForGroup" => "Remove all permissions for a group.
        	 Required parameters: group",

	      "addSpace" => "Add a new space.
        	 Required parameters: space or userId
        	 Optional parameters: title, comment",

	      "removeSpace" => "Remove a space.
        	 Required parameters: space or userId",

	      "copySpace" => "Copy space information and pages to another space.
        	 Required parameters: space, newSpace
        	 Optional parameters: title, comment, findReplace, replace, copyAttachments, copyLabels, copyComments, targetServer",

	      "getSpace" => "Get space information.
        	 Required parameters: space",

	      "getSpaceList" => "Get list of spaces. Put to a file if specified.
        	 Required parameters: space
        	 Optional parameters: file",

	      "getPageList" => "Get list of pages. Put to a file if specified.
        	 Required parameters: space
        	 Optional parameters: title, ancestors, descendents, children, file",

	      "getNewsList" => "Get list of news items. Put to a file if specified.
        	 Required parameters: space
        	 Optional parameters: ancestors, descendents, children, file",

	      "getCommentList" => "List of comment information (id, dates, ...) for a page.
        	 Required parameters: space, title
        	 Optional parameters: file, dateFormat",

	      "getLabelList" => "Get list of labels. Default is mostPopular. Put to a file if specified.
        	 Optional parameters: space, title, news, mostPopular, recentlyUpdated, file, count",

	      "getPermissionList" => "Get list of all available, space, or page permissions. Put to a file if specified.
        	 Optional parameters: space, title, userId, file",

	      "getUserList" => "Get list of users. Put to a file if specified.
        	 Optional parameters: file",

	      "getGroupList" => "Get list of groups. Put to a file if specified.
        	 Required parameters: userId
        	 Optional parameters: file",

	      "getPluginList" => "Get list of plugins. Use outputFormat=2 for extended information (longer time required).
        	 Optional parameters: plugin, file, outputFormat, count",

	      "getPluginDownload" => "Get url to download the plugin version. Version defaults to the latest version.
        	 Required parameters: plugin
        	 Optional parameters: version",

	      "installPlugin" => "Install plugin.
        	 Required parameters: file",

	      "checkPlugin" => "Verify that plugin is installed and enabled.
        	 Required parameters: plugin",

	      "exportSpace" => "Export a space to a file
        	 Required parameters: space, exportType, file",

	      "exportSite" => "Export site backup to a file
        	 Required parameters: file
        	 Optional parameters: exportAttachments",

	      "loadFiles" => "BETA. Load directory and files into a page hierarchy. HTM, HTML, and text files converted to pages. All other
        files added as attachments.
        	 Required parameters: space, file
        	 Optional parameters: title, content, parent, userid, group, replace",



	      "--file" => "<file>
        Path to file based content or result output
",

	      "--encoding" => "<encoding>
        Character encoding (character set) for text based file content - must be an encoding supported by your JAVA platform.
",

	      "--debug" => "
        Requests debug output, example: stack traces.
",

	      "--verbose" => "
        Requests verbose output.
",

	      "--sql" => "<sql>
        SQL select statement used to generate a run script.
",

	      "--driver" => "<driver>
        JDBC driver class or predefined value: postgresql, mysql, mssql, oracle, or db2400. Required for SQL actions.
",

	      "--url" => "<url>
        Action specific setting. Example: Database access url for SQL actions. Optional when host is provided.
",

	      "--host" => "<host>
        Database host server for SQL actions. Not used if url is provided. (default: localhost)
",

	      "--port" => "<port>
        Database host port for SQL actions. Optional, defaults to database default. Not used if url is provided.
",

	      "--database" => "<database>
        Database name is required for SQL actions.
",

	      "--continue" => "
        Continue processing even after errors are encountered.
",

	      "--server" => "<server>
        Server URL.
",

	      "--user" => "<user>
        User name for remote login. (default: automation)
",

	      "--password" => "<password>
        User password for remote login.
",

	      "--login" => "<login>
        Login token from previous login request.
",

	      "--service" => "<service>
        Service address extension. (default: /rpc/soap-axis/confluenceservice-v1)
",

	      "--loginFromStandardInput" => "
        Get login token from standard input.
",

	      "--title" => "<title>
        Page or news title.
",

	      "--content" => "<content>
        Content for page, attachment or comment. (default: )
",

	      "--comment" => "<comment>
        Text for comment or attachment comment.
",

	      "--parent" => "<parent>
        Parent page title.
",

	      "--name" => "<name>
        File name for attachment.
",

	      "--labels" => "<labels>
        Comma separated list of labels.
",

	      "--newTitle" => "<newTitle>
        New title of copied or renamed page.
",

	      "--space" => "<space>
        Space key.
",

	      "--newSpace" => "<newSpace>
        New space key for copied or moved page.
",

	      "--targetServer" => "<targetServer>
        Target server url for copy requests.
",

	      "--permissions" => "<permissions>
        Comma separated list of permissions.
        	Page permissions:
        		view, edit
        	Space permissions:
        		viewsspace, editspace, comment, editblog, createattachment,
        		removepage, removecomment, removeblog, removeattachment, removemail,
        		setpagepermissions, setspacepermissions,
        		exportpage, exportSpace
",

	      "--plugin" => "<plugin>
        Plugin key or partial key for matching.
",

	      "--version" => "<version>
        Item version.
",

	      "--dateFormat" => "<dateFormat>
        Format string for date in Java SimpleDateFormat, default is client date format.
",

	      "--dayOfMonth" => "<dayOfMonth>
        Day of month for news entry. Use negative values for going back to previous months. (default: 0)
",

	      "--count" => "<count>
        Maximum number of entries to return. (default: 2147483647)
",

	      "--id" => "<id>
        Numeric id of an item.
",

	      "--exportType" => "<exportType>
        Export type (XML, HTML, PDF) for space export. (default: XML)
",

	      "--mime" => "<mime>
        Attachment mime type if you want to override determination by file extension.
",

	      "--findReplace" => "<findReplace>
        Find and replace text - separated by :.
",

	      "--outputFormat" => "<outputFormat>
        Specify output format for an action. (default: 1)
",

	      "--group" => "<group>
        Group name.
",

	      "--defaultGroup" => "<defaultGroup>
        Default group to move users on removeGroup action.
",

	      "--userId" => "<userId>
        User id for user management and other actions.
",

	      "--userFullName" => "<userFullName>
        User name for user management actions.
",

	      "--userEmail" => "<userEmail>
        User email for user management actions.
",

	      "--userPassword" => "<userPassword>
        User password for user management actions.
",

	      "--news" => "
        Parameters represent a news item.
",

	      "--exportAttachments" => "
        Export attachments for site export.
",

	      "--mostPopular" => "
        Request most popular labels.
",

	      "--recentlyUsed" => "
        Request recently used labels.
",

	      "--ancestors" => "
        Ancestors for a page.
",

	      "--descendents" => "
        All descendents for a page.
",

	      "--children" => "
        Immediate children for a page.
",

	      "--replace" => "
        Replace existing entity.
",

	      "--copyAttachments" => "
        Copy attachments when copying a page.
",

	      "--copyComments" => "
        Copy comments when copying a page.
",

	      "--copyLabels" => "
        Copy labels when copying a page or space.
",

	      "--autoGroup" => "
        Groups are automatically added when referenced in add user functions.
",



	     );

$confluence_help{"-a"} = $confluence_help{"--action"};

for (@ARGV) {
  next unless exists $confluence_help{$_};
  print "Help: " . $_ . " " . $confluence_help{$_};
  print "\n\n\n";
}
