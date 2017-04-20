package com.matburt.mobileorg.Gui.Wizard;

import java.io.File;
import java.util.Arrays;

import android.content.Context;
import android.os.Environment;
import android.util.Log;

public class LocalDirectoryBrowser extends DirectoryBrowser<File> {

	public LocalDirectoryBrowser(Context context) {
		super(context);

		browseTo(Environment.getExternalStorageDirectory().getAbsolutePath());
	}
	
	@Override
	public boolean isCurrentDirectoryRoot() {
		return currentDirectory.getParent() == null;
	}

	@Override
	public void browseTo(int position) {
		File newdir = getDir(position);
		browseTo(newdir.getAbsolutePath());
	}

	@Override
	public void browseTo(String directory) {
		currentDirectory = new File(directory);
		directoryNames.clear();
		directoryListing.clear();
		if (currentDirectory.getParent() != null) {
			directoryNames.add(upOneLevel);
			directoryListing.add(currentDirectory.getParentFile());
		}
		File[] tmpListing = currentDirectory.listFiles();
        Log.e("bhj", String.format("%s:%d: directory is %s", "LocalDirectoryBrowser.java", 37, directory));
		// default list order doesn't seem to be alpha
		Arrays.sort(tmpListing);
		for (File dir : tmpListing) {
			if (dir.isDirectory() && dir.canWrite()) {
				directoryNames.add(dir.getName());
				directoryListing.add(dir);
			}
		}
	}
}
