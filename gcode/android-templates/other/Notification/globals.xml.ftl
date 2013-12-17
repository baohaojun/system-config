<?xml version="1.0"?>
<globals>
    <global id="srcOut" value="src/${slashedPackageName(packageName)}" />
    <global id="notificationName" value="${className?replace('notification','','i')}" />
    <global id="notification_name" value="${camelCaseToUnderscore(className?replace('notification','','i'))}" />
    <global id="display_title" value="${camelCaseToUnderscore(className?replace('notification','','i'))?replace('_',' ')?cap_first}" />

    <global id="icon_resource" value="ic_stat_${camelCaseToUnderscore(className?replace('notification','','i'))}" />
</globals>
