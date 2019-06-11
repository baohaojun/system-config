require 'rubygems'
require 'net/ldap'

ldap = Net::LDAP.new
ldap.host = ENV['scm_ldap_host']
ldap.port = ENV['scm_ldap_port']
# ldap.auth ENV['scm_ldap_user'], ENV['scm_ldap_password']

user_email = String.new(ARGV[0])

user_email2 = user_email.sub(/\.com$/, ".cn")

user_email = user_email.sub(/\.cn$/, ".com")


[user_email, user_email2].each { | user |
  result = ldap.bind_as(:base => "DC=nodomain",
                        :filter => "(cn=#{user.sub(/@.*/, "")})",
                        :password => ARGV[1])
  if result
    puts "good: #{user}"
  else
    puts "fail: #{user}"
  end

}
