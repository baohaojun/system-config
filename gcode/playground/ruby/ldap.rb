require 'rubygems'
require 'net/ldap'

require File::expand_path("~/src/github/smartcm/builder/app/controllers/smartcm_ldap_helper.rb")

class Test
  include SmartcmLdapHelper
end
if Test.new.ldap_login(ARGV[0], ARGV[1])
  puts "builder: ok"
else
  puts "builder: fail"
end

ldap = Net::LDAP.new
ldap.host = '172.16.21.3'
ldap.port = 3268
ldap.auth "itadmin@smartisan.cn", `cat ~/.ssh/mysql-cmbuild.pass`.chomp

user_email = String.new(ARGV[0])

user_email2 = user_email.sub(/\.com$/, ".cn")

user_email = user_email.sub(/\.cn$/, ".com")


[user_email, user_email2].each { | user |
  result = ldap.bind_as(:base => "dc=smartisan, dc=cn",
                        :filter => "(&(|(mail=#{user})(userPrincipalName=#{user}))(sAMAccountName=#{user.sub(/@.*/, "")}))",
                        :password => ARGV[1])
  if result
    puts "good: #{user}"
  else
    puts "fail: #{user}"
  end

}

