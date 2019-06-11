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
ldap.host = ENV['scm_ldap_host']
ldap.port = ENV['scm_ldap_port']
ldap.auth ENV['scm_ldap_user'], ENV['scm_ldap_password']

user_email = String.new(ARGV[0])

user_email2 = user_email.sub(/\.com$/, ".cn")

user_email = user_email.sub(/\.cn$/, ".com")


[user_email, user_email2].each { | user |
  result = ldap.bind_as(:base => "DC=chj,DC=local",
                        :filter => "(sAMAccountName=#{user.sub(/@.*/, "")})",
                        :password => ARGV[1])
  if result
    puts "good: #{user}"
  else
    puts "fail: #{user}"
  end

}
