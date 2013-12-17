<merge xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:tools="http://schemas.android.com/tools"
    tools:context=".${activityClass}">

    <!-- Login progress -->
    <LinearLayout android:id="@+id/login_status"
        android:visibility="gone"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:layout_gravity="center"
        android:gravity="center_horizontal"
        android:orientation="vertical">
        <ProgressBar style="?android:attr/progressBarStyleLarge"
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:layout_marginBottom="8dp"/>
        <TextView
            android:id="@+id/login_status_message"
            android:textAppearance="?android:attr/textAppearanceMedium"
            android:fontFamily="sans-serif-light"
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:layout_marginBottom="16dp"
            android:text="@string/login_progress_signing_in" />
    </LinearLayout>

    <!-- Login form -->
    <ScrollView
        android:id="@+id/login_form"
        android:layout_width="match_parent"
        android:layout_height="match_parent">

        <LinearLayout style="@style/LoginFormContainer"
            android:orientation="vertical">

            <EditText
                android:id="@+id/email"
                android:singleLine="true"
                android:maxLines="1"
                android:layout_width="match_parent"
                android:layout_height="wrap_content"
                android:inputType="textEmailAddress"
                android:hint="@string/prompt_email" />

            <EditText
                android:id="@+id/password"
                android:singleLine="true"
                android:maxLines="1"
                android:layout_width="match_parent"
                android:layout_height="wrap_content"
                android:hint="@string/prompt_password"
                android:inputType="textPassword"
                android:imeActionLabel="@string/action_sign_in_short"
                android:imeActionId="@+id/login"
                android:imeOptions="actionUnspecified" />

            <Button android:id="@+id/sign_in_button"
                android:layout_width="wrap_content"
                android:layout_height="wrap_content"
                android:layout_marginTop="16dp"
                android:text="@string/action_sign_in_register"
                android:paddingLeft="32dp"
                android:paddingRight="32dp"
                android:layout_gravity="right" />

        </LinearLayout>

    </ScrollView>
</merge>
