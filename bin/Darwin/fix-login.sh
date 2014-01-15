#!/bin/bash

cat > /tmp/loginfix.sh <<EOF
#!/bin/bash
exec > ~bhj/.logs/fix-login.sh 2>&1
set -x
rm /Users/*/Library/Preferences/ByHost/com.apple.loginwindow.* -v
EOF
sudo mv /tmp/loginfix.sh /usr/bin/loginfix.sh
sudo chmod +x /usr/bin/loginfix.sh
defaults write com.apple.loginwindow LoginHook /usr/bin/loginfix.sh
defaults write com.apple.loginwindow TALLogoutSavesState 0


# defaults delete com.apple.loginwindow LoginHook
