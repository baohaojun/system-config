<?xml version="1.0"?>
<recipe>
    <merge from="AndroidManifest.xml.ftl" />
    <instantiate from="src/app_package/BroadcastReceiver.java.ftl"
                   to="${srcOut}/${className}.java" />
    <open file="${srcOut}/${className}.java" />
</recipe>
