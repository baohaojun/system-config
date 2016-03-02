require 'rubygems'
require 'net/ldap'

ldap = Net::LDAP.new
ldap.host = '172.16.21.3'
ldap.port = 3268
ldap.auth "itadmin@smartisan.cn", `cat ~/.ssh/mysql-cmbuild.pass`.chomp


['baohaojun@smartisan.cn', 'cmtest@smartisan.cn', 'baohaojun@smartisan.cn'].each { | user |
  result = ldap.bind_as(:base => "dc=smartisan, dc=cn",
                        :filter => "(mail=#{user})",
                        :password => ARGV[0])
  if result
    puts "hello world"
  else
    puts "fucked"
  end

}

