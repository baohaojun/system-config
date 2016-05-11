$LOAD_PATH << "~/system-config/gcode/playground/ruby"

require "MyMixin"
class TestMixin
  include MyMixin
  def yes
    p 'yes'
  end
end

x = TestMixin.new
x.yes
x.hello
