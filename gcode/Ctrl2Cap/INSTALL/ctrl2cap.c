//----------------------------------------------------------------------
//
// Ctrl2cap Installation Program
//
// This program installs and uninstalls the ctrl2cap keyboard
// filter driver. Its actions vary depending on whether running
// on NT 4 or Win2K.
//
//----------------------------------------------------------------------
#include <windows.h>
#include <stdio.h>

//
// Registry keys we access
//
#define CTRL2CAP_DRIVER_KEY		"System\\CurrentControlSet\\Services\\Ctrl2cap"
#define CTRL2CAP_FILTER_KEY		"System\\CurrentControlSet\\Control\\Class\\{4D36E96B-E325-11CE-BFC1-08002BE10318}"

//----------------------------------------------------------------------
//
// SearchMultiSz
//
// Finds the specified string in a multiSz.
//
//----------------------------------------------------------------------
char *
SearchMultiSz( 
	char *Value, 
	DWORD ValueLength, 
	char *String 
	)
{
	DWORD	len;

	if( ValueLength < strlen( String )) return NULL;

	len = ValueLength - strlen(String);
	do {

		if( !stricmp( &Value[len], String )) return &Value[len];

	} while( len-- );
	return NULL;
}


//----------------------------------------------------------------------
//
// PrintError
//
// Presents an error as an easy to read message.
//
//----------------------------------------------------------------------
void
PrintError(
   DWORD Code
   )
{
   CHAR                                     buffer[80] ;
   DWORD                                    count ;

   //
   // Translate the Win32 error code into a useful message.
   //
   count = FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM,
                          NULL,
                          Code,
                          0,
                          buffer,
                          sizeof (buffer),
                          NULL) ;

   //
   // Make sure that the message could be translated.
   //
   if (count == 0) {

      printf("\nError could not be translated.\n Code: %d\n", Code) ;
      return;
   }
   else {

      //
      // Display the translated error.
      //
      printf("%s\n", buffer) ;
      return;
   }
}


//----------------------------------------------------------------------
// 
// Ctrl2capUninstall
//
// Uninstalls the ctrl2cap driver.
//
//----------------------------------------------------------------------
void 
Ctrl2capUninstall( 
    BOOLEAN Silent,
    DWORD Version, 
    char *SystemDirectory 
    )
{
    HKEY	hKey;
    DWORD	error, type, length;
	char	filters[MAX_PATH], *ptr;

    //
    // Delete the driver file.
    //
    if( !DeleteFile( SystemDirectory ) && !Silent) {

        printf("Error deleting \\system32\\drivers\\ctrl2cap.sys:\n");
        PrintError( GetLastError());
    }

    //
    // Delete the driver Registry key.
    //
	RegDeleteKey( HKEY_LOCAL_MACHINE, CTRL2CAP_DRIVER_KEY"\\Security" );
	RegDeleteKey( HKEY_LOCAL_MACHINE, CTRL2CAP_DRIVER_KEY"\\Enum" );
	if(( error = RegDeleteKey( HKEY_LOCAL_MACHINE, CTRL2CAP_DRIVER_KEY )) !=
		ERROR_SUCCESS && !Silent ) {

		printf("Error deleting ctrl2cap driver key:\n");
		PrintError( error );
	}

	//
	// Delete ctrl2cap from keyboard filter value
	//
	if( Version > 4 ) {

		if( (error = RegOpenKey( HKEY_LOCAL_MACHINE,
						CTRL2CAP_FILTER_KEY,
						&hKey )) != ERROR_SUCCESS ) {

			if( !Silent ) {
				printf("Could not open Ctrl2cap filter Registry key:\n");
				PrintError( error );
			}
			return;			
		}

		length = sizeof( filters );
		if( (error = RegQueryValueEx( hKey, "UpperFilters", 0, &type, filters, &length )) !=
				ERROR_SUCCESS ) {

			if( !Silent ) {

				error = GetLastError();
				printf("Could not query Ctrl2cap filter Registry key:\n");
				PrintError( error );
				RegCloseKey( hKey );
			}
			return;
		} 
		if( ptr = SearchMultiSz( filters, length, "ctrl2cap" )) {

			//
			// Zap it.
			//
			memcpy( ptr, ptr + strlen("ctrl2cap")+1, 
					length - (ptr-filters) - strlen("ctrl2cap") -1 );
			length -= strlen("ctrl2cap")+1;

			if( (error = RegSetValueEx( hKey, "UpperFilters", 0, type,
						filters, length )) != ERROR_SUCCESS ) {

				if( !Silent ) {

					printf("Could not reset Ctrl2cap filter Registry key:\n");
					PrintError( error );
				}
			}
		}
		RegCloseKey( hKey );
	}
	printf("Ctrl2cap uninstalled. You must reboot for this to take effect.\n\n");
}


//----------------------------------------------------------------------
//
// Ctrl2capInstall
//
// Installs the ctrl2cap driver.
//
//----------------------------------------------------------------------
void 
Ctrl2capInstall( 
    DWORD Version, 
    char *SystemDirectory 
    )
{
    HKEY	hKey;
    DWORD	error, value, type, length;
	char	filters[MAX_PATH];

    //
    // First, copy the driver file to the system directory.
    //
   if( Version > 4 ) {

        if( !CopyFile( "ctrl2cap.nt5.sys", SystemDirectory, FALSE )) {

            printf("Could not copy ctrl2cap.nt5.sys to \\system32\\drivers.:\n");
            PrintError( GetLastError());
            exit(1);
        }
    } else {

        if( !CopyFile( "ctrl2cap.nt4.sys", SystemDirectory, FALSE )) {

            printf("Could not copy ctrl2cap.nt4.sys to \\system32\\drivers.:\n");
            PrintError( GetLastError());
            exit(1);
        }
    }

    //
    // Create the driver Registry key.
    //
    if( (error = RegCreateKey( HKEY_LOCAL_MACHINE,
                       CTRL2CAP_DRIVER_KEY,
                       &hKey )) != ERROR_SUCCESS ) {

		error = GetLastError();
        PrintError( error );
        Ctrl2capUninstall( TRUE, Version, SystemDirectory );
        exit(1);
    }

    //
    // Add the appropriate values.
    //
    value = 1;
    RegSetValueEx( hKey, "Type", 0, REG_DWORD, (PCHAR) &value, sizeof(value));
    value = 1;
    RegSetValueEx( hKey, "ErrorControl", 0, REG_DWORD, (PCHAR) &value, sizeof(value));
    
    //
    // On Win2K, add the driver as a keyboard filter.
    //
    if( Version > 4 ) {

		value = 3;
		RegSetValueEx( hKey, "Start", 0, REG_DWORD, (PCHAR) &value, sizeof(value));
		RegCloseKey( hKey );
		if( (error = RegOpenKey( HKEY_LOCAL_MACHINE,
						CTRL2CAP_FILTER_KEY,
						&hKey )) != ERROR_SUCCESS ) {

			printf("Could not create Ctrl2cap filter Registry key:\n");
			goto error;
		}

		length = sizeof( filters );
		if( (error = RegQueryValueEx( hKey, "UpperFilters", 0, &type, filters, &length )) !=
				ERROR_SUCCESS ) {

			error = GetLastError();
			printf("Could not open Ctrl2cap filter Registry key:\n");
			RegCloseKey( hKey );
			goto error;
		}

		// 
		// Append ctrl2cap to the end.
		//
		if( !SearchMultiSz( filters, length, "ctrl2cap" )) {

			strcpy( &filters[length-1], "ctrl2cap");
			length = length + strlen("ctrl2cap");
			filters[ length ] = 0;

			if( (error = RegSetValueEx( hKey, "UpperFilters", 0, type,
				filters, length + 1 )) != ERROR_SUCCESS ) {

				error = GetLastError();
				printf("Could not set Ctrl2cap filter Registry key:\n");
				RegCloseKey( hKey );
				goto error;
			}
		}
		RegCloseKey( hKey );
    } else {

		value = 1;
		RegSetValueEx( hKey, "Start", 0, REG_DWORD, (PCHAR) &value, sizeof(value));
		RegSetValueEx( hKey, "Group", 0, REG_SZ, "KeyboardClass", strlen("KeyboardClass")+1);
		RegCloseKey( hKey );
	}

	printf("Ctrl2cap successfully installed. You must reboot for it to take effect.\n\n");
	return;

error:
	//
	// Cleanup path when we have to uninstall stuff on the way out.
	//
	PrintError( error );
	Ctrl2capUninstall( TRUE, Version, SystemDirectory );
	exit(1);
}


//----------------------------------------------------------------------
//
// Usage
// 
// Tell the user the proper arguments.
//
//----------------------------------------------------------------------
void 
Usage( 
    char *name 
    )
{
    printf("usage: %s [/install | /uninstall]\n\n", name );
    exit(1);
}


//----------------------------------------------------------------------
//
// Main
// 
// Just prints banner and calls the uninstall or install function.
//
//----------------------------------------------------------------------
void 
main( 
    int argc, 
    char *argv[] 
    )
{
    DWORD   version;
    CHAR    systemDirectory[MAX_PATH] ;

    //
    // Print banner
    //
    printf("\nCtrl2cap Installation Applet\n");
    printf("Copyright (C) 1999 Mark Russinovich\n");
    printf("Systems Internals - http://www.sysinternals.com\n\n");
    if( argc != 2 ||
        (stricmp( argv[1], "/install") && stricmp( argv[1], "/uninstall") )) {
        
        Usage( argv[0] );
    }

    //
    // Get NT version
    //
    version = GetVersion();
    if( version >= 0x80000000 ) {

        printf("This version of ctrl2cap works on NT 4 and Win2K.\n"
               "Visit http://www.sysinternals.com for a version that works on Windows 9x\n\n");
        return;
    }

    //
    // Get the system directory
    //
    GetSystemDirectory( systemDirectory, sizeof (systemDirectory)) ;
    strcat( systemDirectory, "\\drivers\\ctrl2cap.sys") ;

    //
    // Do the install or uninstall routine.
    //
    if( !stricmp( argv[1], "/install" )) {

        Ctrl2capInstall( version & 0xFF, systemDirectory );

    } else {
        
        Ctrl2capUninstall( FALSE, version & 0xFF, systemDirectory );
    }
}
