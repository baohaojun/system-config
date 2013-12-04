package ${packageName};

import android.os.Bundle;
import android.app.Activity;
import android.view.Menu;
<#if parentActivityClass != "">
import android.view.MenuItem;
import android.support.v4.app.NavUtils;
<#if minApiLevel < 11>
import android.annotation.TargetApi;
import android.os.Build;
</#if>
</#if>

public class ${activityClass} extends Activity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.${layoutName});
        <#if parentActivityClass != "">
        // Show the Up button in the action bar.
        setupActionBar();
        </#if>
    }

    <#if parentActivityClass != "">
    /**
     * Set up the {@link android.app.ActionBar}<#if minApiLevel < 11>, if the API is available</#if>.
     */
    <#if minApiLevel < 11>@TargetApi(Build.VERSION_CODES.HONEYCOMB)
    </#if>private void setupActionBar() {
        <#if minApiLevel < 11>if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB) {</#if>
        getActionBar().setDisplayHomeAsUpEnabled(true);
        <#if minApiLevel < 11>}</#if>
    }
    </#if>

    <#include "include_onCreateOptionsMenu.java.ftl">
    <#include "include_onOptionsItemSelected.java.ftl">

}
