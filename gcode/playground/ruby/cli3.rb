#!/usr/bin/ruby
require 'thor'
class MyCLI < Thor
  option :from
  option :yell, :type => :boolean
  desc "hello NAME", "say hello to NAME"
  def hello(name)
    output = []
    output << "from: #{options[:from]}" if options[:from]
    output << "Hello #{name}"
    output = output.join("\n")
    puts options[:yell] ? output.upcase : output
  end
end
MyCLI.start(ARGV)
