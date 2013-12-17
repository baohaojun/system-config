<resources>
    <#if !isNewProject>
    <string name="title_${activityToLayout(activityClass)}">${escapeXmlString(activityTitle)}</string>
    </#if>

    <string name="action_settings">Settings</string>

    <#if navType != "none">
    <string name="title_section1">Section 1</string>
    <string name="title_section2">Section 2</string>
    <string name="title_section3">Section 3</string>
    <#else>
    <string name="hello_world">Hello world!</string>
    </#if>
</resources>
