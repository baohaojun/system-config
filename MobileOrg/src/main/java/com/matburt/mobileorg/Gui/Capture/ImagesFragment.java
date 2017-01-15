package com.matburt.mobileorg.Gui.Capture;

import android.app.Activity;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TableLayout;

import android.content.ContentResolver;
import android.content.Context;
import android.database.Cursor;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.net.Uri;
import android.os.Environment;
import android.provider.MediaStore;
import android.util.AttributeSet;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.ViewGroup.LayoutParams;
import android.view.View.MeasureSpec;
import android.widget.BaseAdapter;
import android.widget.GridView;
import android.widget.ImageView;
import android.widget.ListAdapter;
import com.actionbarsherlock.app.SherlockFragment;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuInflater;
import com.actionbarsherlock.view.MenuItem;
import com.matburt.mobileorg.Gui.Capture.DateTableRow.DateTableRowListener;
import com.matburt.mobileorg.OrgData.OrgNodePayload;
import com.matburt.mobileorg.R;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

public class ImagesFragment extends SherlockFragment {
    private static File sdcard = Environment.getExternalStorageDirectory();
    private GridView imagesView;
    private Context mContext;
    private OnImagesModifiedListener mListener;
    private OrgNodePayload payload;
    private PhotoAdapter mPhotoAdapter;

	public interface OnImagesModifiedListener {
		public void onImagesModified();
	}
	
	@Override
	public void onAttach(Activity activity) {
		super.onAttach(activity);

        mContext = activity.getApplicationContext();
		try {
            mListener = (OnImagesModifiedListener) activity;
        } catch (ClassCastException e) {
            throw new ClassCastException(activity.toString() + " must implement OnImagesModifiedListener");
        }
	}

    private final static int imageGridCols = 2;
	
	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container,
			Bundle savedInstanceState) {
		super.onCreateView(inflater, container, savedInstanceState);
        Log.e("bhj", String.format("%s:%d: ", "ImagesFragment.java", 55));
		setHasOptionsMenu(true);
		this.imagesView = new MyGridView(getActivity());
        mPhotoAdapter = new PhotoAdapter(mContext);
        imagesView.setAdapter(mPhotoAdapter);
        imagesView.setLayoutParams(new LayoutParams(LayoutParams.FILL_PARENT, LayoutParams.WRAP_CONTENT));
        imagesView.setNumColumns(imageGridCols);
		return imagesView;
	}
	
	@Override
	public void onActivityCreated(Bundle savedInstanceState) {
		super.onActivityCreated(savedInstanceState);
		EditHost host = (EditHost) getActivity();
        this.payload = host.getController().getOrgNodePayload();
		
		if(savedInstanceState != null)
			restoreInstanceState(savedInstanceState);
		else
			setupImages();
		
		try {
			EditActivity activity = (EditActivity) host;
			activity.invalidateOptionsMenu();
		}
		catch (ClassCastException e) {}
    }

	public void setupImages() {
        for (String img : payload.getImages()) {
            mPhotoAdapter.addPhotos(img);
        }

	}

    public void restoreInstanceState(Bundle savedInstanceState) {
		if(savedInstanceState != null) {
		}
	}


	@Override
	public void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
	}
	
	public void setModifable(boolean enabled) {
	}

	@Override
	public void onCreateOptionsMenu(Menu menu, MenuInflater inflater) {
		inflater.inflate(R.menu.edit_images, menu);
		super.onCreateOptionsMenu(menu, inflater);
	}
	
	@Override
	public void onPrepareOptionsMenu(Menu menu) {
		super.onPrepareOptionsMenu(menu);
	}


	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.menu_nodeedit_addimage:
            Log.e("bhj", String.format("%s:%d: ", "ImagesFragment.java", 200));
			return true;

		default:
			return super.onOptionsItemSelected(item);
		}
	}

    private static Bitmap getThumbnail(ContentResolver cr, String path) throws Exception {
        Cursor ca = cr.query(
            MediaStore.Images.Media.EXTERNAL_CONTENT_URI,
            new String[] { MediaStore.MediaColumns._ID },
            MediaStore.MediaColumns.DATA + "=?",
            new String[] {path},
            null);
        if (ca != null && ca.moveToFirst()) {
            int id = ca.getInt(ca.getColumnIndex(MediaStore.MediaColumns._ID));
            Log.e("bhj", String.format("%s:%d: id is %d for %s", "ImagesFragment.java", 147, id, path));
            ca.close();
            return MediaStore.Images.Thumbnails.getThumbnail(cr, id, MediaStore.Images.Thumbnails.MINI_KIND, null );
        }

        ca.close();
        return null;

    }

    public class PhotoAdapter extends BaseAdapter {

        private ArrayList<String> mPhotos = new ArrayList<String>();
        
        public PhotoAdapter(Context c) {
            mContext = c;
            addPhotos("images/Screenshot_2017-01-14-19-28-35-102.png");
            addPhotos("images/Screenshot_2017-01-14-19-28-35-102.png");
            addPhotos("images/Screenshot_2017-01-14-19-28-35-102.png");
            addPhotos("images/Screenshot_2017-01-14-19-28-35-102.png");
        }

        public int getCount() {
            Log.e("bhj", String.format("%s:%d: count is %d", "ImagesFragment.java", 143, mPhotos.size()));
            return mPhotos.size();
        }

        public Object getItem(int position) {
            return position;
        }

        public long getItemId(int position) {
            return position;
        }

        private HashMap<String, Bitmap> imageBitmapsMap = new HashMap<String, Bitmap>();

        public View getView(int position, View convertView, ViewGroup parent) {
            // Make an ImageView to show a photo
            Log.e("bhj", String.format("%s:%d: ", "ImagesFragment.java", 142));

            ImageView i;
            if (convertView == null) {
                i = new ImageView(mContext);
                i.setAdjustViewBounds(true);
                i.setLayoutParams(new GridView.LayoutParams(LayoutParams.FILL_PARENT, LayoutParams.FILL_PARENT));
                i.setPadding(5, 5, 5, 5);
            } else {
                i = (ImageView) convertView;
            }

            if (imageBitmapsMap.containsKey(mPhotos.get(position))) {
                i.setImageBitmap(imageBitmapsMap.get(mPhotos.get(position)));
                return i;
            }

            File imgFile = new File(sdcard.getAbsolutePath() + "/MobileOrg/" + mPhotos.get(position));
            Log.e("bhj", String.format("%s:%d: imgFile: %s", "ImagesFragment.java", 205, imgFile.getAbsolutePath()));

            
            Bitmap bitmap = null;
            try {
                bitmap = getThumbnail(mContext.getContentResolver(), imgFile.getAbsolutePath());
            } catch (Exception e) {
                Log.e("bhj", String.format("%s:%d: ", "ImagesFragment.java", 209), e);
            }
            if (!imgFile.exists()) {
                return i;
            }

            if (bitmap == null) {
                Log.e("bhj", String.format("%s:%d: ", "ImagesFragment.java", 195));
                bitmap = BitmapFactory.decodeFile(imgFile.getAbsolutePath());
            }
            

            if (bitmap != null) {
                i.setImageBitmap(bitmap);
                imageBitmapsMap.put(mPhotos.get(position), bitmap);
            }
            
            // Give it a nice background
            return i;
        }

        private Context mContext;

        public void addPhotos(String img) {
            mPhotos.add(img);
            notifyDataSetChanged();
        }
    }

    public class MyGridView extends GridView {
        public MyGridView(Context context) {
            super(context);
        }
 
        public MyGridView(Context context, AttributeSet attrs) {
            super(context, attrs);
        }
 
        public MyGridView(Context context, AttributeSet attrs, int defStyle) {
            super(context, attrs, defStyle);
        }
 
        @Override
        protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
            int heightSpec;
 
            if (getLayoutParams().height == LayoutParams.WRAP_CONTENT) {
                // The great Android "hackatlon", the love, the magic.
                // The two leftmost bits in the height measure spec have
                // a special meaning, hence we can't use them to describe height.
                heightSpec = MeasureSpec.makeMeasureSpec(
                    Integer.MAX_VALUE >> 2, MeasureSpec.AT_MOST);
            }
            else {
                // Any other height should be respected as is.
                heightSpec = heightMeasureSpec;
            }
 
            super.onMeasure(widthMeasureSpec, heightSpec);
        }
    }
}
