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

    <#if navType == "navigation_drawer">
    <string-array name="planets_array">
    <item>Mercury</item>
    <item>Venus</item>
    <item>Earth</item>
    <item>Mars</item>
    <item>Jupiter</item>
    <item>Saturn</item>
    <item>Uranus</item>
    <item>Neptune</item>
    </string-array>
    <string name="drawer_open">Open navigation drawer</string>
    <string name="drawer_close">Close navigation drawer</string>
    <string name="action_websearch">Web search</string>
    <string name="app_not_available">Sorry, there\'s no web browser available</string>
    </#if>
</resources>
